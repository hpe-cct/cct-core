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

import akka.actor.{ActorRef, Actor}
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLCpuKernel}
import cogx.platform.opencl.OpenCLCpuKernel.Step
import cogx.utilities.AnnotateThread
import com.jogamp.opencl.CLEvent.ExecutionStatus

/** An actor with its own thread that executes CPU kernels.
  *
  * @author Greg Snider
  */
private[cogx]
class CPUKernelActor(kernel: OpenCLCpuKernel, replyTo: ActorRef) extends Actor {

  val Verbose = false
  if (Verbose)
    println("--- CPUKernelActor: constructing")

  def receive = {
    case Step(inputTriggers, outputTrigger, inputRegisters, outputRegisters) =>

      kernel.waitForEvents(inputTriggers)

      try {
        kernel match {
          case singleOutputKernel: OpenCLCpuSingleOutputKernel =>
            singleOutputKernel.compute(inputRegisters, outputRegisters(0))
          case _ =>
            kernel.compute(inputRegisters, outputRegisters)
        }
        outputTrigger.setComplete()
      }
      catch {
        case e: Exception =>
          println("CPU Kernel Actor " + this + " sees exeption: " + e)
          outputTrigger.setStatus(ExecutionStatus.ERROR)
      }

    case x =>
      println("CPUKernelActor, unexpected message: " + x)
  }

  /** Inject `self` into the kernel so it may invoke it (using the Step
    * message) when needed.
    */
  override def preStart {
    // Actor injection used to occur here, but this is too late.  The
    // GPUSupervisor's creation of this actor via context.actorOf(...)
    // returns immediately, even before this class' constructor completes.
    // Having the GPUSupervisor do the injection is the only race-free
    // approach I found.  The race occurs when the GPUSupervisor, having
    // created the CPUKernelActors, then responds to a Step message by calling
    // kernel.phase1Clock().  The kernel would then message Step to
    // its kernelActor, which might be null if the injection call hadn't
    // happened yet.
    //
    // -RJC

//    kernel.injectActor(self)
    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName + "-" + kernel)

  }

}

