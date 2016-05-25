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

package libcog.fields

import scala.language.implicitConversions
import cogx.`package`._

/** A 2D field of stick tensors.
  *
  * Each tensor requires only 2 components, so this is implemented with a
  * complex field. This uses the Scala "Enriched" pattern, much like RichStrings,
  * to automatically convert back and forth between complex fields and
  * stick tensor fields.
  *
  * @param complexField The complex field implementing the stick tensor field.
  *
  * @author Greg Snider
  */
class StickTensorField(private val complexField: ComplexField) {

  /** Create a stick tensor field from a complex matrix. Each element of
    * the matrix is used to represent one tensor in the field.
    *
    * @param matrix Complex matrix to convert to a stick tensor field.
    */
  def this(matrix: ComplexMatrix) =
    this(ComplexField(matrix))

  /** Create a stick tensor field.
    *
    * @param rows Rows in the field.
    * @param columns Columns in the field.
    * @param f Function which returns a stick tensor at each point (row, column)
    *        in the field.
    */
  def this(rows: Int, columns: Int, f: (Int, Int) => StickTensor) =
    this(ComplexField(rows, columns, f))


  /** Create a stick tensor field from stickness and ballness fields.
    *
    * @param stickness Scalar field holding stickness attribute
    * @param orientation Scalar field holding orientation values for each
    *        point in the field, in the range (0, Pi]
    */
  def this(stickness: ScalarField, orientation: ScalarField) =
    this(polarComplex(stickness, orientation * 2))


  /** Get the "stickness" of a stick tensor field.
    *
    * @return Stickness as a scalar field.
    */
  def stickness: ScalarField = {
    magnitude(complexField)
  }

  /** Get the orientation in the range (0, Pi] of a stick tensor field.
    *
    * @return Orientation at each point in the field, in the range (0, Pi].
    */
  def orientation: ScalarField = {
    val orientation = phase(complexField) / 2
    val nonPositive = orientation <= 0f
    val normalizedOrientation =
      orientation + nonPositive * math.Pi.toFloat
    normalizedOrientation.asInstanceOf[ScalarField]
  }
}


/** Implicit conversions between stick tensor fields and complex fields. */
object StickTensorField {

  /** ComplexField --> StickTensorField*/
  implicit def toStickTensorField(c: ComplexField) =
    new StickTensorField(c)

  /** StickTensorField --> ComplexField. */
  implicit def fromStickTensorField(s: StickTensorField): ComplexField =
     s.complexField
}
