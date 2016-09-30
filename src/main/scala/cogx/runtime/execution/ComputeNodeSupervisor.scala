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

import akka.actor.{Props, ActorRef, Actor}
import cogx.utilities.AnnotateThread
import scala.collection.mutable.HashMap
import cogx.runtime.resources.ComputeNode
import cogx.platform.opencl.OpenCLPlatform
import cogx.runtime.allocation.circuit.{InputProxyKernel, OutputProxyKernel}
import cogx.runtime.FieldID

/** Actor which supervises all GPU computation on a compute node.
  *
  * Each GPU on a compute node is supervised by a GPUSupervisor. This actor
  * creates and manages the GPUSupervisors for the compute node. The protocol
  * is as follows:
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
  * @param computeNode The compute node managed by this actor.
  * @param platform The platform upon which the circuit is evaluated.
  * @param profileSize How often to print out profiling statistics (0 == never)
  *
  * @author Greg Snider
  */
private[runtime]
class ComputeNodeSupervisor(computeNode: ComputeNode, platform: OpenCLPlatform, profileSize: Int)
        extends Actor
{
  import SupervisorMessages._

  // Not used, but left here as a placehold for how to alter the supervisor strategy
//  override def supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 0, withinTimeRange = 0 seconds) {
//    case e: Exception =>
//      println("ComputeNodeSupervisor Actor sees exception " + e)
//      Escalate
//  }

  /** Children actors which supervise the GPUs on this node. */
  var gpuSupervisors: Seq[ActorRef] = null

  /** Map of a kernel ID to the GPU supervisor actor which produces it. */
  val kernelIDToChild = new HashMap[Int, ActorRef]

  /** Routing information for FieldData messages. Messages include the ID of
    * the kernel that produced the data. This maps that ID to the nodes we
    * need to forward the message to. */
  val proxyRoutes = new HashMap[FieldID, Set[ActorRef]]

  /** Number of children which are currently executing a request from self. */
  var busyChildren = 0

  /** In accumulating errors from ComputeNodeSupervisors, remember just one (the last). */
  var lastError: Option[Exception] = None

  /** Message handling. */
  def receive = {
    case Step =>
      require(busyChildren == 0, "protocol failure: Step request while busy")
      lastError = None
      busyChildren = gpuSupervisors.length
      gpuSupervisors.foreach(_ ! Step)
      
    case StepDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0) {
        context.parent ! StepDone(lastError)
      }

    case Reset =>
      require(busyChildren == 0, "protocol failure: Reset request while busy")
      lastError = None
      busyChildren = gpuSupervisors.length
      gpuSupervisors.foreach(_ ! Reset)
      
    case ResetDone(e) =>
      busyChildren -= 1
      if (lastError == None)
        lastError = e
      if (busyChildren == 0)
        context.parent ! ResetDone(lastError)
      
    case msg @ ProbeField(id) =>
      val child = kernelIDToChild(id.kernelID)
      child forward msg

    case msg @ FieldData(id, data) =>
      for (target <- proxyRoutes(id))
        target forward msg

    case DebugPrint =>
      println("+++ ComputeNodeSupervisor: DebugPrint")
      gpuSupervisors.foreach(_ ! DebugPrint)

    case x =>
      throw new Exception("unexpected message: " + x)
  }

  /** Allocate children before starting. */
  override def preStart() {
    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName)

    gpuSupervisors = computeNode.gpus.zipWithIndex.map {
      case (gpu, idx) =>
        require(gpu.circuit != null, "GPU is lacking a bound circuit")
        val device = platform.devices(gpu.deviceIndex)
        CogActorSystem.createActor(context,
          Props(new GPUSupervisorActor(device, gpu, profileSize)), name="GPUSupervisorActor-"+idx)
    }
    for (i <- 0 until computeNode.gpus.length) {
      computeNode.gpus(i).circuit.traversePostorder {
        kernel => kernelIDToChild(kernel.id) = gpuSupervisors(i)
      }
    }


    /// Set up proxy routes ///

    // TODO Avoid unnecessary iteration over the kernel circuit.
    // We've already iterated over the circuit once; we could have already
    // discovered all the input and output proxies.

    // First, set up a route for every input proxy we know about. These routes
    // go down the actor hierarchy.
    for (i <- 0 until computeNode.gpus.length) {
      computeNode.gpus(i).circuit.traversePostorder {
        case ipk: InputProxyKernel =>

          // Create an actor to manage proxy messages
          //val actor = context.actorOf(Props(new InputProxyKernelActor(ipk)), name="InputProxyActor-"+ipk.id)
          val actor = context.actorOf(Props(new InputProxyKernelActorFSM(ipk)), name="InputProxyActorFSM-"+ipk.id)
          ipk.injectActor(actor)

          val dataSource = ipk.proxiedKernelId // <-- Data from here ...
          val sendTo = actor                   // <-- ... goes here.
          val oldRoutes = proxyRoutes.getOrElseUpdate(dataSource, Set.empty[ActorRef])
          proxyRoutes(dataSource) = oldRoutes + sendTo
        case _ =>
      }
    }

    // Then, handle any output proxies we know about. What we're interested in
    // determining here is if we need to hand a message up to our supervisor.
    // If we control all the GPUSupervisors that are interested in the message,
    // there's no need to send it up.
    for (i <- 0 until computeNode.gpus.length) {
      computeNode.gpus(i).circuit.traversePostorder {
        case opk: OutputProxyKernel =>
          val actor = context.actorOf(Props(new OutputProxyKernelActor(opk)), name="OutputProxyActor-"+opk.id)
          opk.injectActor(actor)
          val proxiesDataFor = opk.proxiedKernelId

          // If we don't know about a target, we'll have to let our
          // supervisor handle it.
          if (opk.targetInputProxyIds.exists(kernelIDToChild.get(_) eq None)) {
            val oldRoutes = proxyRoutes.getOrElseUpdate(proxiesDataFor, Set.empty[ActorRef])
            proxyRoutes(proxiesDataFor) = oldRoutes + context.parent
          }

          // Make sure not to forward messages back down the subtree that
          // sent it up in the first place.
          proxyRoutes.get(proxiesDataFor) match {
            case Some(routes) =>
              require(!routes.contains(kernelIDToChild(opk.id)))
            case None => // Nothing to do
          }
        case _ =>
      }
    }

  }
}