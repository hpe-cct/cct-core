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
  */
private[runtime]
class ClusterSupervisor(circuit: KernelCircuit, platform: OpenCLPlatform, mode: AllocationMode)
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

  /** Message handling. */
  def receive = {
    case Step =>
      requestor = sender
      require(busyChildren == 0, "protocol failure: Step request while busy")
      lastError = None
      busyChildren = nodeSupervisors.length
      nodeSupervisors.foreach(_ ! Step)

    case StepDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0)
        requestor ! StepDone(lastError)

    case Reset =>
      requestor = sender
      require(busyChildren == 0, "protocol failure: Reset request while busy")
      lastError = None
      busyChildren = nodeSupervisors.length
      nodeSupervisors.foreach(_ ! Reset)

    case ResetDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0)
        requestor ! ResetDone(lastError)

    case msg @ ProbeField(id) =>
      val child = kernelIDToChild.get(id.kernelID) match {
        case Some(kernel) => kernel
        case None => throw new RuntimeException(s"Internal compiler error: probed field $id is unprobeable.")
      }
      child forward ProbeField(id)

    case msg @ FieldData(id, data) =>
      val child = kernelIDToChild(id.kernelID)
      //child ! ProbeData(id, data)
      child forward msg

    case DebugPrint =>
      println("+++ ClusterSupervisor: DebugPrint")
      nodeSupervisors.foreach(_ ! DebugPrint)

    case x =>
      throw new Exception("unexpected message: " + x)
  }

  /** Allocate all resources needed to implement the circuit. */
  override def preStart() {
    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName)

//    val mode: AllocationMode = Option(System.getProperty("cog.device")) match {
//      case Some("all") =>
//        Console.err.println("[AllocateCluster] Warning: Operating in experimental multi-GPU mode.")
//        AllocationMode.MultiGPU
//      case _ =>
//        AllocationMode.SingleGPU(System.getProperty("cog.device","0").toInt) // Default mode
//    }
    val cluster = AllocateCluster(circuit, platform, mode)
    val computeNodes: Seq[ComputeNode] = cluster.computeNodes
    nodeSupervisors = computeNodes.zipWithIndex.map {
      case (node, idx) =>
        CogActorSystem.createActor(context, Props(new ComputeNodeSupervisor(node, platform)),
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
}