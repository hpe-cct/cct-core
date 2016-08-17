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

package cogx.platform.types

/** A register than holds the data for a field.
  *
  * The register can act like either a master/slave flip-flop or a latch,
  * depending on how it's constructed. Externally it always looks like a
  * flip-flop (since it has both master and slave methods), but for latches
  * the master and slave are the same.
  *
  * @param buffer0 First buffer, needed by both flip-flops and latches.
  * @param buffer1 Second buffer, needed only by flip-flops.
  *
  * @author Greg Snider
  */
private[cogx]
class FieldRegister[T <: AbstractFieldBuffer](buffer0: T, buffer1: T = null) {
  require(buffer0 != null)
  /** True if this is a flip-flop, false if it's a latch. */
  val isFlipFlop = buffer1 != null
  /** Buffers holding data in register: one (for latch) or two (for flip-flop)*/
  private val buffers =
    if (isFlipFlop)
      Array[AbstractFieldBuffer](buffer0, buffer1)
    else
      Array[AbstractFieldBuffer](buffer0, buffer0)
  /** Name attached to register, used only user-level debugging. */
  var name: String = ""

  /** Get the "master" buffer. */
  def master: T = buffers(0).asInstanceOf[T]

  /** Get the "slave" buffer. */
  def slave:  T = buffers(1).asInstanceOf[T]

  /** Flip the "master" and "slave" buffers. */
  def clock() {
    if (isFlipFlop) {
      val temp = buffers(0)
      buffers(0) = buffers(1)
      buffers(1) = temp
    }
    // The master no longer has valid data, so mark it that way.
  }
}