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

/** A circular buffer which holes Doubles in FIFO (first-in, first-out) order.
  * When the buffer is filled, appending a Double overwrites the oldest
  * entry.
  *
  * @author Greg Snider
  */

private [cogx] class CircularBuffer(val length: Int) {
  private val data = new Array[Float](length)
  private var nextIndex = 0
  private val asArray = new Array[Float](length)

  def append(value: Float) {
    data(nextIndex) = value
    nextIndex += 1
    if (nextIndex >= length)
      nextIndex = 0
  }

  def +=(value: Float) = append(value)

  def toArray: Array[Float] = {
    val beginLength = length - nextIndex
    val endLength = length - (beginLength)
    Array.copy(data, nextIndex, asArray, 0, beginLength)
    Array.copy(data, 0, asArray, beginLength, endLength)
    asArray
  }

  override def toString: String = {
    toArray.map(_.toString).reduceLeft(_ + " " + _)
  }
}
