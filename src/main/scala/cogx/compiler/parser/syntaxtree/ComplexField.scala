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
import cogx.cogmath.algebra.complex.{ComplexMatrix, Complex}
import cogx.platform.types.{Opcode, FieldType}
import cogx.platform.types.ElementTypes.Complex32
import cogx.utilities.Random
import cogx.cogmath.algebra.real.Matrix

/** A multidimensional array of complex numbers.
  *
  * @param operation The operation that creates this field.
  * @param fieldType Type of the field.
  *
  * @author Greg Snider
  */
class ComplexField(operation: Operation,
                   fieldType: FieldType)
        extends Field(operation, fieldType)
        with CompilerError
        with SemanticError
{
  def this(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    this(Operation(opcode, inputs, fieldType), fieldType)

  require(fieldType.tensorOrder == 0)
  require(fieldType.elementType == Complex32)

  /** Create a 0D field of complex numbers.
    *
    * @param value: The single complex value in the field
    */
  def this(value: Complex) =
    this(ConstantComplex0DOp(() => value),
      Array[Field](),
      new FieldType(Shape(), Shape(), Complex32)
    )

  /** Create a 1D field of complex numbers.
    *
    * @param columns Columns in field.
    * @param f Function which returns a complex number for every (column).
    */
  def this(columns: Int, f: (Int) => Complex) =
    this(ConstantComplex1DOp(f),
      Array[Field](),
      new FieldType(Shape(columns), Shape(), Complex32)
    )

  /** Create a 2D field of complex numbers.
    *
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which returns a complex number for every (row, column).
    */
  def this(rows: Int, columns: Int, f: (Int, Int) => Complex) =
    this(ConstantComplex2DOp(f), Array[Field](),
      new FieldType(Shape(rows, columns), Shape(), Complex32)
    )

  /** Create a 3D field of complex numbers.
    *
    * @param layers Layers in field.
    * @param rows Rows in field.
    * @param columns Columns in field.
    * @param f Function which returns a complex number for every
    *       (layer, row, column).
    */
  def this(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Complex) =
    this(ConstantComplex3DOp(f),
      Array[Field](),
      new FieldType(Shape(layers, rows, columns), Shape(), Complex32)
    )
}

/** Functions for creating constant/recurrent complex fields.
  */
object ComplexField extends CompilerError {

  /** Create a 0D complex field filled with `value`. */
  def apply(value: Complex): ComplexField =
    new ComplexField(value)

  /** Create a 1D `columns` size complex field filled by `f`. */
  def apply(columns: Int, f: (Int) => Complex): ComplexField  =
    new ComplexField(columns, f)

  /** Create a 2D (`rows` x `columns`) complex field filled by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Complex): ComplexField  =
    new ComplexField(rows, columns, f)

  /** Create a 2D (`rows` x `columns`) complex field from a complex matrix. */
  def apply(matrix: ComplexMatrix): ComplexField =
    apply(matrix.rows, matrix.columns, (row, col) => matrix(row, col))

  /** Create a 2D (`rows` x `columns`) complex field from a real matrix. */
  def apply(matrix: Matrix): ComplexField =
    apply(new ComplexMatrix(matrix))

  /** Create a 3D (`layers` x `rows` x `columns`) complex field filled by `f`. */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Complex): ComplexField  =
    new ComplexField(layers, rows, columns, f)

  /** Create a zero-filled complex field with dimensions `fieldShape`. */
  def apply(fieldShape: Shape): ComplexField = {
    val zero = new Complex(0f, 0f)
    val shape = fieldShape
    fieldShape.dimensions match {
      case 0 => apply(zero)
      case 1 => apply(shape(0), (_) => zero)
      case 2 => apply(shape(0), shape(1), (_,_) => zero)
      case 3 => apply(shape(0), shape(1), shape(2), (_,_,_) => zero)
      case x => internalError("illegal number of dimensions"); null
    }
  }


  /** Create a random-filled complex field with dimensions `sizes`. */
  def random(sizes: Int*): ComplexField = random(Shape(sizes: _*))

  /** Create a random-filled complex field with dimensions `fieldShape`. */
  def random(fieldShape: Shape): ComplexField = {
    val shape = fieldShape

    val rand = new Random()

    def complex(firstFieldPoint: Boolean) = {
      new Complex(rand.nextFloatResetFirstIf(firstFieldPoint), rand.nextFloat)
    }

    fieldShape.dimensions match {
      case 0 => apply(Complex.random)
      case 1 => apply(shape(0),
        (c) => complex(c==0))
      case 2 => apply(shape(0), shape(1),
        (r,c) => complex(r==0 && c==0))
      case 3 => apply(shape(0), shape(1), shape(2),
        (l,r,c) => complex(l==0 && r==0 && c==0))
      case x => internalError("illegal number of dimensions"); null
    }
  }
}

