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

package cogx.compiler.parser.syntaxtree

import cogx.compiler.parser.op._
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.CompilerError
import cogx.cogmath.geometry.Shape
import cogx.platform.types.{Opcode, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op.ConstantScalar0DOp
import cogx.utilities.Random
import cogx.cogmath.algebra.real.Matrix

/** A multidimensional array of scalars.
  *
  * @param operation The operation that creates this field.
  * @param resultType Type of the field.
  *
  * @author Greg Snider
  */
class ScalarField(operation: Operation,
                  resultType: FieldType)
        extends Field(operation, resultType)
        with CompilerError
        with SemanticError
{
  def this(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    this(Operation(opcode, inputs, fieldType), fieldType)

  require(resultType.tensorOrder == 0)
  require(resultType.elementType == Float32)

  /** Create a 0D scalar field filled with `value`. */
  def this(value: Float) =
    this(ConstantScalar0DOp(() => value),
      Array[Field](),
      new FieldType(Shape(), Shape(), Float32))

  /** Create a 1D `columns` size scalar field filled by `f`. */
  def this(columns: Int, f: (Int) => Float) =
    this(ConstantScalar1DOp(f),
      Array[Field](),
      new FieldType(Shape(columns), Shape(), Float32))

  /** Create a 2D (`rows` x `columns`) scalar field filled by `f`. */
  def this(rows: Int, columns: Int, f: (Int, Int) => Float)  =
    this(ConstantScalar2DOp(f),
      Array[Field](),
      new FieldType(Shape(rows, columns), Shape(), Float32))

  /** Create a 3D (`layers` x `rows` x `columns`) scalar field filled by `f`. */
  def this(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Float) =
    this(ConstantScalar3DOp(f),
      Array[Field](),
      new FieldType(Shape(layers, rows, columns), Shape(), Float32))
}

/** Functions for creating constant/recurrent scalar fields.
  */
object ScalarField extends CompilerError {
  /** Random number generator. */
  private val rand = new Random

  /** Create a 0D scalar field filled with `value`. */
  def apply(value: Float): ScalarField =
    new ScalarField(value)

  /** Create a 0D scalar field filled with zero. */
  def apply(): ScalarField =
    apply(0f)

  /** Create a 1D `columns` size scalar field filled by `f`. */
  def apply(columns: Int, f: (Int) => Float): ScalarField  =
    new ScalarField(columns, f)

  /** Create a 1D `columns` size input scalar field filled with zeroes. */
  def apply(columns: Int): ScalarField =
    apply(columns, (_) => 0f)


  /** Create a 2D (`rows` x `columns`) scalar field filled by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Float): ScalarField  =
    new ScalarField(rows, columns, f)

  /** Create a 2D (`rows` x `columns`) scalar field filled with zeroes. */
  def apply(rows: Int, columns: Int): ScalarField =
    apply(rows, columns, (_, _) => 0f)

  /** Create a 2D (`rows` x `columns`) scalar field from a matrix. */
  def apply(matrix: Matrix): ScalarField =
    apply(matrix.rows, matrix.columns, (row, col) => matrix(row, col))

  /** Create a 3D (`layers` x `rows` x `columns`) scalar field filled by `f`. */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Float): ScalarField  =
    new ScalarField(layers, rows, columns, f)

  /** Create a 3D (`layers` x `rows` x `columns`) scalar field with zeroes. */
  def apply(layers: Int, rows: Int, columns: Int): ScalarField  =
    apply(layers, rows, columns, (_, _, _) => 0f)

  /** Create a zero-filled scalar field with dimensions `fieldShape`. */
  def apply(fieldShape: Shape): ScalarField = {
    val shape = fieldShape
    fieldShape.dimensions match {
      case 0 => apply()
      case 1 => apply(shape(0))
      case 2 => apply(shape(0), shape(1))
      case 3 => apply(shape(0), shape(1), shape(2))
      case x => internalError("illegal number of dimensions"); null
    }
  }

  /** Create a 2D scalar field from a sequence of sequences of floats. */
  def apply(values: Seq[Float]*): ScalarField = {
    val data: Array[Array[Float]] = values.toArray.map(_.toArray)
    val rows = data.length
    val columns = data(0).length
    for (row <- 0 until rows)
      require(data(row).length == columns, "Non-rectangular field specified")
    apply(rows, columns, (r, c) => data(r)(c))
  }

  /** Create a random-filled scalar field with dimensions `sizes`. */
  def random(sizes: Int*): ScalarField = random(Shape(sizes: _*))

  /** Create a random-filled scalar field with dimensions `fieldShape`. */
  def random(fieldShape: Shape): ScalarField = {
    val shape = fieldShape
    val rand = new Random()

    fieldShape.dimensions match {
      case 0 => apply(rand.nextFloat)
      case 1 => apply(shape(0),
        (c) => rand.nextFloatResetFirstIf(c==0))
      case 2 => apply(shape(0), shape(1),
        (r,c) => rand.nextFloatResetFirstIf(r==0 && c==0))
      case 3 => apply(shape(0), shape(1), shape(2),
        (l,r,c) => rand.nextFloatResetFirstIf(l==0 && r==0 && c==0))
      case x => internalError("illegal number of dimensions"); null
    }
  }
}




