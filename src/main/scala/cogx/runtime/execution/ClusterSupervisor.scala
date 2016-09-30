/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogx.runtime.execution

import akka.actor.{Actor, ActorRef, Props}
import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.OpenCLPlatform
import cogx.runtime.resources.ComputeNode
import cogx.utilities.AnnotateThread

import scala.collection.mutable.HashMap
import cogx.runtime.allocation.{AllocateCluster, AllocationMode}

/** Actor which supervises all computation for a kernel circuit.
  *
  * This maps the kernel circuit onto the available hardware and allocates
  * resources necessary to execute it. The protocol is as follows:
  *
  * 1. In response to a `Step` request, this supervisor will step its children
  * actors. When the children have completed, this supervisor will respond
  * with a `StepDone` to the parent.
  *
  * 2. In response to a `Reset` request, the supervisor will reset it's children
  * actors. When the children have completed, this supervisor will respond
  * with a `ResetDone` to the parent.
  *
  * 3. The parent is not allowed to send a `Step` or `Reset` request to this
  * supervisor until any previous `Step` or `Reset` command has completed and
  * and has been acknowledged with a `StepDone` or `ResetDone` response.
  *
  * 4. The `ProbeField` request is legal at all times.
  *
  * 5. The `FieldData` message is legal at all times.
  *
  * @author Greg Snider
  *
  * @param circuit The DAG of kernels to be evaluated.
  * @param platform The platform upon which the circuit is evaluated.
  * @param mode The specific machines and devices upon which the circuit is evaluated.
  * @param profileSize How often to print out profiling statistics (0 == never)
  */
private[runtime]
class ClusterSupervisor(circuit: KernelCircuit, platform: OpenCLPlatform, mode: AllocationMode, profileSize: Int)
        extends Actor
{
  import SupervisorMessages._

  /** NodeSupervisor actors controlled by this cluster supervisor. */
  var nodeSupervisors: Seq[ActorRef] = null

  /** Map of a kernel ID to the NodeSupervisor actor which produces it. */
  val kernelIDToChild = new HashMap[Int, ActorRef]

  /** Map of a kernel ID to the NodeSupervisor actors which consume but do not
    * produce it (i.e. on receiving FieldData tagged with some kernel ID, this
    * map says where it needs to be forwarded to). */
  val proxyRoutes = new HashMap[Int, Set[ActorRef]]

  /** Number of children which are currently executing a request from self. */
  var busyChildren = 0

  /** The supervisor of this supervisor. Since the supervisor is a typed actor
    * that interacts with self using futures, you can't use context.parent to
    * determine it. Akka uses a *different* actor than the parent in that case.
    *
    * Normal parent/sender for `tell` messages (for example):
    * {{{
    *    akka://CogActors/user/$a
    * }}}
    * Sender for `ask` messages which use futures (for example):
    * {{{
    *    akka://CogActors/temp/$a
    * }}}
    *
    * This is used for both `Step` and `Reset` requests.
    */
  var requestor: ActorRef = null

  /** In accumulating errors from ComputeNodeSupervisors, remember just one (the last). */
  var lastError: Option[Exception] = None

  /** Circuit initialization error, saved to be passed back in response to the first user command. */
  var initError: Option[Exception] = None

  /** Message handling. */
  def receive = {
    case Step =>
      initError match {
        case Some(e) =>
          sender ! StepDone(initError)
        case None =>
          requestor = sender
          require(busyChildren == 0, "protocol failure: Step request while busy")
          lastError = None
          busyChildren = nodeSupervisors.length
          nodeSupervisors.foreach(_ ! Step)
      }

    case StepDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0)
        requestor ! StepDone(lastError)

    case Reset =>
      initError match {
        case Some(e) =>
          sender ! ResetDone(initError)
        case None =>
          requestor = sender
          require(busyChildren == 0, "protocol failure: Reset request while busy")
          lastError = None
          busyChildren = nodeSupervisors.length
          nodeSupervisors.foreach(_ ! Reset)
      }

    case ResetDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0)
        requestor ! ResetDone(lastError)

    case msg @ ProbeField(id) =>
      initError match {
        case Some(e) =>
          sender ! initError.get
        case None =>
          val child = kernelIDToChild.get(id.kernelID) match {
            case Some(kernel) => kernel
            case None => throw new RuntimeException(s"Internal compiler error: probed field $id is unprobeable.")
          }
          child forward ProbeField(id)
      }

    case msg @ FieldData(id, data) =>
      initError match {
        case Some(e) =>
          sender ! initError.get
        case None =>
          val child = kernelIDToChild(id.kernelID)
          child forward msg
      }

    case DebugPrint =>
      println("+++ ClusterSupervisor: DebugPrint")
      initError match {
        case Some(e) =>
          println(s"Initialization failed with exception $e")
        case None =>
          nodeSupervisors.foreach(_ ! DebugPrint)
      }

    case x =>
      throw new Exception("unexpected message: " + x)
  }

  /** Allocate all resources needed to implement the circuit. */
  override def preStart() {
    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName)
    // Here's where we do placement and buffer allocation.
    // We save any exceptions here, rather than throwing them, and throw them in response
    // to the first user command (best for OpenCL resource and Akka actor system cleanup).
    try {
      val cluster = AllocateCluster(circuit, platform, mode)
      val computeNodes: Seq[ComputeNode] = cluster.computeNodes
      nodeSupervisors = computeNodes.zipWithIndex.map {
        case (node, idx) =>
          CogActorSystem.createActor(context, Props(new ComputeNodeSupervisor(node, platform, profileSize)),
            name="ComputeNodeSupervisor-"+idx)
      }
      for (i <- 0 until computeNodes.length) {
        computeNodes(i).gpus.foreach {
          gpu =>
            gpu.circuit.traversePostorder {
              kernel => kernelIDToChild(kernel.id) = nodeSupervisors(i)
            }
        }
      }
    }
    catch {
      case e: Exception =>
        initError = Some(e)
    }
  }
}