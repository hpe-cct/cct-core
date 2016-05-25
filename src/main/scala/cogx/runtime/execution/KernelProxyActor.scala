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

import cogx.runtime.allocation.circuit.{OutputProxyKernel, InputProxyKernel}
import cogx.platform.opencl.OpenCLCpuKernel.Step
import cogx.runtime.execution.SupervisorMessages.FieldData
import cogx.platform.cpumemory.AbstractFieldMemory
import akka.actor.{FSM, Actor}

/** A local proxy for a kernel that is instantiated on another node. This is
  * necessary because of the predominantly static design of OpenCL.
  *
  * @author Greg Snider
  */
//private[runtime]
//class KernelProxyActor { // extends CPUKernelActor {
//
//}

/* My intial attempts at implementing an actor to manage an InputProxyKernel
 * had a few problems. The actor needs to be able to handle the Step message
 * like every other CPU kernel actor, but it also needs to handle the FieldData
 * message. Actors must completely finish processing one message before moving
 * onto the next. So how to step if the input proxy hasn't received data from
 * its corresponding output proxy yet? We'd need to save our target output
 * register and output triggers until data arrives. But then what if for some
 * reason the GPU supervisor running the input proxy is slow relative to the
 * supervisors, and they send us proxied data before we hear the Step message?
 * The data needs to be set aside somewhere until the message comes in.
 *
 * I found that I was writing out something resembling a finite state machine,
 * but it turns out that there's explicit support for that sort of thing in
 * Akka, so why not use it? */

private[runtime]
object InputProxyKernelActorFSM {
  
  sealed trait State
  case object Idle extends State
  case object WaitingForData extends State
  case object WaitingForStep extends State
  //case object Stepping extends State
  
  sealed trait Data
  case object Unitialized extends Data
  case class FieldDataWrapper(data: FieldData) extends Data
  case class StepWrapper(step: Step) extends Data
  
}
import InputProxyKernelActorFSM._
private[cogx]
class InputProxyKernelActorFSM(proxyKernel: InputProxyKernel)
        extends Actor
        with FSM[State, Data] {

  startWith(Idle, Unitialized)

  when(Idle) {
    case Event(step: Step, Unitialized) =>
      goto(WaitingForData) using StepWrapper(step)
    case Event(fd: FieldData, Unitialized) =>
      goto(WaitingForStep) using FieldDataWrapper(fd)
  }

  when(WaitingForData) {
    case Event(data: FieldData, StepWrapper(step)) =>
      doStep(step, data)
      goto(Idle) using Unitialized
  }

  when(WaitingForStep) {
    case Event(step: Step, FieldDataWrapper(data)) =>
      doStep(step, data)
      goto(Idle) using Unitialized
  }

  initialize

  private def doStep(step: Step, fieldData: FieldData) {
    val outputRegister = step.outputRegisters(0)
    val outBuf = outputRegister.master.cpuMemory.asInstanceOf[AbstractFieldMemory]
    fieldData.data.copyTo(outBuf)
    outputRegister.master.write
    step.outputTrigger.setComplete()
  }

  override def preStart() {
    // Actor injection used to occur here, but this is too late.  The
    // ComputeNodeSupervisor's creation of this actor via context.actorOf(...)
    // returns immediately, even before this class' constructor completes.
    // Having the ComputeNodeSupervisor do the injection is the only race-free
    // approach I found.  The race occurs when the ComputeNodeSupervisor, having
    // created the InputProxyKernelActors, then passes along a Step message to
    // a GPUSupervisor, which calls kernel.phase1Clock().  The kernel would then
    // message Step to its kernelActor, which might be null if the injection call
    // hadn't happened yet.
    //
    // -RJC

//    proxyKernel.injectActor(self)
  }

}

private[runtime]
class OutputProxyKernelActor(proxyKernel: OutputProxyKernel) extends Actor {
  val Verbose = false
  def receive = {
    case Step(inputTriggers, outputTrigger, inputRegisters, outputRegisters) =>
      proxyKernel.waitForEvents(inputTriggers)
      if (Verbose)
        println("OutputProxyKernelActor ("+self.path.name+") Sending data to supervisor: "+proxyKernel.proxiedKernelId)
      context.parent ! FieldData(
        proxyKernel.proxiedKernelId,
        inputRegisters(0).slave.read.asInstanceOf[AbstractFieldMemory]
      )
      proxyKernel.compute(inputRegisters, outputRegisters) // No-op
      outputTrigger.setComplete()
    case x =>
      Console.err.println("OutputProxyKernelActor - Unexpected message: "+x)
  }

  override def preStart() {
    // Actor injection used to occur here, but this is too late.  The
    // ComputeNodeSupervisor's creation of this actor via context.actorOf(...)
    // returns immediately, even before this class' constructor completes.
    // Having the ComputeNodeSupervisor do the injection is the only race-free
    // approach I found.  The race occurs when the ComputeNodeSupervisor, having
    // created the OutputProxyKernelActors, then passes along a Step message to
    // a GPUSupervisor, which calls kernel.phase1Clock().  The kernel would then
    // message Step to its kernelActor, which might be null if the injection call
    // hadn't happened yet.
    //
    // -RJC

//    proxyKernel.injectActor(self)
  }
}