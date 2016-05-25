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

/** A 2D field of positive, semidefinite, symmetric tensors.
  *
  * Each tensor requires only 3 components, so this is implemented with a
  * vector field. This uses the Scala "Enriched" pattern, much like RichStrings,
  * to automatically convert back and forth between vector fields and
  * symmetric tensor fields.
  *
  * @param vectorField The vector field used to implement the symmetric
  *        tensor field.
  *
  * @author Greg Snider
  */
class SymmetricTensorField(private val vectorField: VectorField) {
  require(vectorField.tensorShape(0) == 3,
    "vector field must have 3 components for use as a symmetric tensor field")

  /** Create a symmetric tensor field.
    *
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which supplies a symmetric tensor for each point
    *       (row, column) in the field.
    */
  def this(rows: Int, columns: Int, f: (Int, Int) => SymmetricTensor) =
    this(VectorField(rows, columns, f))

  /**
    * Compute stickness, ballness and orientation
    */
  def sticknessBallnessOrientation: (ScalarField, ScalarField, ScalarField) = {
    val Axx: ScalarField = vectorElement(vectorField, 0)
    val Axy: ScalarField = vectorElement(vectorField, 1)
    val Ayy: ScalarField = vectorElement(vectorField, 2)
    val trace = Axx + Ayy
    val determinant = Axx * Ayy + Axy * Axy
    val stickness = sqrt(trace * trace - 4 * determinant)
    val ballness = (trace - stickness) / 2
    val orientation = phase(complex(Axx - Ayy, 2 * Axy)) / 2
    (stickness, ballness, orientation)
  }
}



/** Implicit conversions between symmetric tensor fields and vector fields. */
object SymmetricTensorField {

  /** VectorField --> SymmetricTensorField */
  implicit def toSymmetricTensorField(c: VectorField) =
    new SymmetricTensorField(c)

  /** SymmetricTensorField --> VectorField. */
  implicit def fromSymmetricTensorField(s: SymmetricTensorField): VectorField =
    s.vectorField
}
