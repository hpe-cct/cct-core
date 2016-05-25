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

package cogx.platform.cpumemory.readerwriter

/** The interface for reading a scalar field on the CPU.
  *
  * @author Greg Snider
  */
trait ScalarFieldReader
  extends FieldReader
  with Iterable[Float]
{
  /** An iterator over all values in the field, scanning in row-major order. */
  def iterator: Iterator[Float]

  /** Read the single value in a 0D scalar field. */
  def read(): Float

  /** Read the value at (`col`) in a 1D scalar field. */
  def read(col: Int): Float

  /** Read the value at (`row`, `col`) in a 2D scalar field. */
  def read(row: Int, col: Int): Float

  /** Read the value at (`layer`, `row`, `col`) in a 3D scalar field. */
  def read(layer: Int, row: Int, col: Int): Float

  /** Read the value of the 0D or 1D scalar field into an Array[Float]. */
  def get(dst: Array[Float])

  /** Read a portion of the values of the 0D or 1D scalar field into an
    * Array[Float], starting at the source buffer's `srcIndex` position.
    */
  def get(srcIndex: Int, dst: Array[Float])

  /** Read `length` values of the 0D or 1D scalar field into the `dst`
    * Array[Float], starting at the source buffer's `srcIndex` position,
    * and placing the values in the `dst` Array starting at position
    * `dstIndex`.
    */
  def get(srcIndex: Int, dst: Array[Float], dstIndex: Int, length: Int)

  /** Read the value of the 2D scalar field into an Array[Array[Float]]. */
  def get(dst: Array[Array[Float]])

  /** Read the value of the scalar field into an Array[Array[Array[Float]]]. */
  def get(dst: Array[Array[Array[Float]]])

}
