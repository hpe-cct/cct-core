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

package cogx.runtime

import cogx.platform.types.{VirtualFieldRegister, AbstractKernel}
import cogx.platform.cpumemory.AbstractFieldMemory
import akka.actor.ActorRef

/** Interface to an Evaluator, an object that executes a computation specified
  * in a ComputeGraph.
  *
  * @author Greg Snider
  */
private[cogx]
trait EvaluatorInterface {

  /** Step the computation one cycle, returning the resulting simulation time
    * (synchronous call).
    *
    * If the computation is already running as a result of the `run` method,
    * this still returns the simulation time, but it is likely to be stale by
    * the time the caller receives it.
    *
    * @return Number of steps taken since last reset.
    */
  def step: Long

  /** Step the computation `count` cycles (synchronous call).
    *
    * If the computation is already running as a result of the `run` method,
    * this still returns the simulation time, but it is likely to be stale by
    * the time the caller receives it.
    *
    * @param count Number of steps to take.
    * @return Number of steps taken since last reset.
    */
  def step(count: Long): Long

  /** Start the computation running until `stop` is called (asynchronous call) */
  def run: Unit

  /** Stop the computation, returning the simulation time (synchronous call). */
  def stop: Long

  /** Get the current simulation time (asynchronous call).
    *
    * @param done Callback function with the simulation time
    */
  def time(done: (Long) => Unit)

  /** Reset the computation to an initial state defined by the user (synchronous
    * call).
    *
    * If the computation is running, it is stopped before the initialization
    * takes place.
    *
    * @return Zero (the simulation time after reset).
    */
  def reset: Long

  /** Read a field, calling `done` when the field data is available
    * (asynchronous call).
    *
    * @param virtualFieldRegister The field register of the field being read
    * @param done Callback which returns memory holding the field data; this
    *        memory must be released before this field can be read again.
    */
  def readField(virtualFieldRegister: VirtualFieldRegister,
                done: (AbstractFieldMemory) => Unit): Unit

  /** Read a field, calling `done` when the field data is available.
    *
    * @param virtualFieldRegister The field register of the field being read
    * @param to Pre-allocated field memory, passed to the `done` function, that
    *        receives the read field data.
    * @param done Callback which returns memory holding the field data; this
    *        memory must be released before this field can be read again.
    */
  def readField(virtualFieldRegister: VirtualFieldRegister,
                to: AbstractFieldMemory,
                done: (AbstractFieldMemory) => Unit)

  /** Print out evaluator for debugging. */
  def print: Unit

  def tellIdentity(me: ActorRef)
}
