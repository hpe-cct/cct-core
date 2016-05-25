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

package cogx.cogmath.collection

import scala.collection.mutable.ArrayBuffer

/** An array buffer that uses object identity rather than the equals method
  * for determining containment in an array buffer.
  *
  * @author Greg Snider
  */

private [cogx] class IdentityArrayBuffer[T <: AnyRef] extends Iterable[T] {
  /** Actual buffer used to hold objects. */
  private val buffer = new ArrayBuffer[T]

  /** Access element at `index`. */
  def apply(index: Int) = buffer.apply(index)

  /** Number of elements in the buffer. */
  def length = buffer.length

  /** Iterator over elements in the buffer. */
  def iterator = buffer.iterator

  /** Append `that` to the buffer. Must not already be in the buffer. */
  def +=(that: T) {
    require(that != null)
    require(!this.contains(that))
    val initialSize = this.length
    buffer += that
    require(this.length == initialSize + 1)
  }

  /** Append `that` to the buffer. Must not already be in the buffer. */
  def append(that: T) {
    +=(that)
  }

  /** Check if `that` is in the buffer. */
  def contains(that: T): Boolean = {
    for (element <- buffer)
      if (element eq that)
        return true
    false
  }
}