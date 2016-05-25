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

import cogx.platform.types.FieldType
import cogx.cogmath.geometry.Shape

/** Base trait for all field readers, provides information about the type
  * of the field being read.
  *
  * @author Greg Snider
  */
trait FieldReader {

  /** Type of the field. */
  def fieldType: FieldType

  /** Shape of the field. */
  def fieldShape: Shape

  /** Shape of the tensors in the field. */
  def tensorShape: Shape

  /** "Layers" in the field; 1 for 2D, 1D, 0D fields. */
  val layers: Int

  /** "Rows" in the field; 1 for 1D, 0D fields. */
  val rows: Int

  /** "Columns" in the field; 1 for 0D fields. */
  val columns: Int

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float

  /** Test for approximate equality between `this` and `that` using L-infinity norm.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @param maxError Maximum L-infinity error value to consider equal
    * @return true if L_inf(this, that) <= maxError
    */
  def approxEquals(that: FieldReader, maxError: Float = 0.001f): Boolean = {
    compareLinf(that) <= maxError
  }

  /** Test for exact equality between `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return true if every value in `this` and `that` is exactly equal
    */
  def equals(that: FieldReader) = approxEquals(that, maxError = 0f)
}