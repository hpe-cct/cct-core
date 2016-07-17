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

import cogx.platform.opencl.OpenCLPlatform
import cogx.utilities.AnnotateThread

import scala.language.postfixOps
import cogx.platform.cpumemory.AbstractFieldMemory
import cogx.compiler.codegenerator.KernelCircuit
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import concurrent.duration._
import cogx.runtime.{EvaluatorInterface, FieldID}
import akka.actor.TypedActor.{PostStop, PreStart, Receiver}
import cogx.platform.types.VirtualFieldRegister
import cogx.runtime.allocation.AllocationMode

/** Object interface to the evaluation system. This uses an Akka "TypedActor"
  * to glue the world of objects to the world of actors.
  *
  * To use this, the caller must do something like this:
  * {{{
  *    actorSystem = ActorSystem("CogActors")
  *    val circuitEvaluator: EvaluatorInterface =
  *      TypedActor(actorSystem).typedActorOf(TypedProps(
  *        classOf[EvaluatorInterface], new CircuitEvaluator(circuit)))
  * }}}
  * then use the circuitEvaluator object's methods.
  *
  * TypedActors are not well documented. It's not clear from the documentation
  * if some of their type rules are really rules (e.g. synchronous methods
  * cannot be of type Unit) or are merely conventions. The book "Akka
  * Essentials" is especially ambiguous when describing Akka in general and
  * should be avoided (lots of forward and dangling references).
  *
  * There are two modes of operation here: single-stepping and running. When
  * single-stepping, a `Step` request causes the supervisor to advance the
  * state of the system by one cycle and then reply with a `StepDone`. This mode
  * is slow since it does not allow pipelining and is used primarily when
  * debugging.
  *
  * Running mode is initiated by a `Run(steps)` request and causes the system
  * to run until `steps` cycles have been executed. Multiple `Run(steps)`
  * requests are cumulative in the number of cycles which will be executed
  * before stopping. When running, `Step` requests increment the number of
  * authorized cycles, but do not generate a `StepDone` response. The system
  * runs until all authorized cycles have been completed.
  *
  * A `Stop` request will stop a running system and do nothing otherwise. In
  * either case, the supervisor responds with `StopDone`.
  *
  * A `Reset` request will stop a running system, forcing it into a non-running
  * state. Regardless of running state at the time of request, it will then
  * force a complete reset of the system, responding with `ResetDone` when it
  * is complete.
  *
  * @author Greg Snider
  *
  * @param circuit The DAG of kernels to be evaluated.
  * @param platform The platform upon which the circuit is evaluated.
  * @param mode The specific machines and devices upon which the circuit is evaluated.
  */
private[cogx]
class CircuitEvaluator(circuit: KernelCircuit, platform: OpenCLPlatform, mode: AllocationMode)
        extends EvaluatorInterface
        with PreStart
        with PostStop
        with Receiver
{
  import SupervisorMessages._
  /** Actor which supervises the entire computation. */
  private val clusterSupervisor =
    CogActorSystem.createActor(TypedActor.context, Props(new ClusterSupervisor(circuit, platform, mode)),
      name="ClusterSupervisor")

  /** How long to wait before assuming something has crashed. */
  implicit val timeout = Timeout(300 seconds)

  val resetTimeout = Timeout(1000 seconds)

  /** True when running, false when idle. */
  private var running = false

  /** Current simulation time. */
  private var time = 0L

  /** When we want to message ourselves, who do we send to?  There might be
    * an easy way to set this internally, but I didn't find it.  -RJC
    */
  private var myActor: ActorRef = null

  /** Flag recording an implicit reset, so we know to ignore an explicit one that follows. */
  private var circuitHasBeenReset = false

  // We no longer reset the computation implicitly to put it in a valid initial state.
  // Instead, we wait for the first user command and (if it's not a reset) perform an implicit reset then.

  /** Reset the computation to an initial state defined by the user
    * (synchronous call).
    *
    * If the computation is running, it is stopped before the initialization
    * takes place.
    *
    * @return Zero (the simulation time after reset).
    */
  @throws(classOf[Exception])
  def reset: Long = {
    running = false
    time = 0L
    val future = clusterSupervisor ? Reset
    Await.result(future, resetTimeout.duration) match {
      case ResetDone(e) => e match {
        case Some(exception) =>
          println(s"CircuitEvaluator sees error in Reset: $exception")
          throw exception
        case None =>
      }
      case x => throw new RuntimeException("Unexpected message: " + x)
    }
    circuitHasBeenReset = true
    time
  }

  /** Step the computation one cycle. Does not alter running state.
    * (synchronous call).
    *
    * @return The simulation time after the step completes.
    */
  @throws(classOf[Exception])
  def step: Long = {
    if (!circuitHasBeenReset)
      reset
    val future = clusterSupervisor ? Step
    try {
      Await.result(future, timeout.duration) match {
        case StepDone(e) => e match {
          case Some(exception) =>
            println(s"CircuitEvaluator sees error in Step: $exception")
            throw exception
          case None => time += 1
        }
        case x => throw new Exception("Unexpected message: " + x)
      }
    }
    catch {
      case e: Exception => //println("CircuitEvaluator.Await sees exception " + e)
        throw e
    }
    time
  }

  /** Step the computation `count` cycles Does not alter running state.
    * (synchronous call).
    *
    * @return The simulation time after the step completes.
    */
  @throws(classOf[Exception])
  def step(count: Long): Long = {
    for (i <- 0L until count)
      step
    time
  }

  /** Start the computation running until `stop` is called (asynchronous call) */
  def run {
    running = true
    myActor ! Run
  }

  def print {
    println("typed actor: " + getClass.getSimpleName)
    clusterSupervisor ! DebugPrint
  }

  /** Stop the computation, returning the simulation time (synchronous call). */
  @throws(classOf[Exception])
  def stop: Long = {
    running = false
    time
  }

  /** Get the current simulation time, in cycles.
    *
    * @param done Callback routine that passes the simulation time as a
    *        parameter.
    */
  def time(done: (Long) => Unit) {
    done(time)
  }

  /** Read a field, calling `done` when the field data is available.
   *
   * CURRENT DEBUGGER INTERFACE.
   *
   * @param virtualFieldRegister Virtual field register driving the field
   * @param to Pre-allocated field memory, passed to the `done` function, that
   *        receives the read field data.
   * @param done Callback which returns memory holding the field data; this
   *        memory must be released before this field can be read again.
   */
  def readField(virtualFieldRegister: VirtualFieldRegister,
                to: AbstractFieldMemory,
                done: (AbstractFieldMemory) => Unit)
  {
    if (!circuitHasBeenReset)
      reset
    val kernel = virtualFieldRegister.source
    val kernelOutput = virtualFieldRegister.sourceOutputIndex match {
      case Some(i) => i
      case None =>
        throw new RuntimeException("Compiler error: missing output field register.")
    }
    synchronized {
      val fieldID = {
        kernel.aliases match {
          case Some(aliases) =>
            require(aliases.size == 1, s"Multiple copies of kernel ${kernel.name} exist; which to probe?")
            FieldID(aliases.head, kernelOutput)
          case None => FieldID(kernel.id, kernelOutput)
        }
      }

      val future = clusterSupervisor ? ProbeField(fieldID)
      Await.result(future, timeout.duration) match {
        case ProbeData(id, data) =>
          require(fieldID == id)
          data.copyTo(to)
          platform.fieldMemoryAllocator.release(data)
        case x => throw new Exception("Unexpected message: " + x)
      }
    }
    done(to)
  }


  /** Read a field, calling `done` when the field data is available.
    *
    * THIS IS PART OF THE USER INTERFACE, NOT FOR THE DEBUGGER.
    *
    * @param virtualFieldRegister Virtual field register driving the field
    * @param done Callback which returns memory holding the field data; this
    *        memory must be released before this field can be read again.
    */
  def readField(virtualFieldRegister: VirtualFieldRegister,
                done: (AbstractFieldMemory) => Unit)
  {
    if (!circuitHasBeenReset)
      reset
    val kernel = virtualFieldRegister.source
    val kernelOutput = virtualFieldRegister.sourceOutputIndex match {
      case Some(i) => i
      case None =>
        throw new RuntimeException("Compiler error: missing output field register.")
    }
    var memory: AbstractFieldMemory = null
    synchronized {
      val fieldID = {
        kernel.aliases match {
          case Some(aliases) =>
            require(aliases.size == 1, s"Multiple copies of kernel ${kernel.name} exist; which to probe?")
            FieldID(aliases.head, kernelOutput)
          case None => FieldID(kernel.id, kernelOutput)
        }
      }

      val future = clusterSupervisor ? ProbeField(fieldID)
      Await.result(future, timeout.duration) match {
        case ProbeData(id, data) =>
          require(fieldID == id)
          memory = platform.fieldMemoryAllocator.copy(data)
          platform.fieldMemoryAllocator.release(data)
        case x => throw new Exception("Unexpected message: " + x)
      }
    }
    done(memory)
  }

  override def preStart() {
    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName)
  }

  override def postStop() {
  }

  /** This is needed so `this` can do a "death watch" on the computeGraphActor
    * and initiate fault recovery.  See p. 90 of the Akka book about getting
    * an ActorRef for `this` so the message can be sent.
    *
    * To run, this actor performs one step, then tells itself to keep running.
    * This allows a stop() method invocation (turned into a message in this
    * actor's queue) to get through and change the running variable.  When such
    * a 'stop' returns synchronously, the next message to this actor will be a
    * stale Run, but since 'running' would be false, this message is ignored.
    */
  @throws(classOf[Exception])
  def onReceive(message: Any, sender: ActorRef) {
    message match {
      case Run =>
        if (running) {
          // Calls the step() method directly, not through a messaging protocol
          try {
            step
            myActor ! Run
          }
          catch {
            case e: Exception =>
              running = false
              println("CircuitEvaluator stopping (further stepping not possible) cause: " + e)
              throw e
          }
        }
      case other =>
        println("^^^^^^^^^^^^^   CircuitEvaluator: message received: " + other)
    }
  }

  /** Informs this actor of its identity for self-messaging */
  def tellIdentity(me: ActorRef) {
    myActor = me
  }

  // See p. 91 and p. 131. of book "Akka Essentials" by M. Gupta.
  // The default strategy is probably good enough here.
  //def supervisorStrategy: SupervisorStrategy = {
  //  null
  //}
}