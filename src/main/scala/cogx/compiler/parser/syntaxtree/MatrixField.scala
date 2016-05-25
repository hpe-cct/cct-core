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
import cogx.cogmath.algebra.real.Matrix
import cogx.platform.types.{Opcode, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.utilities.Random

/** A multidimensional array of matrices.
  *
  * @param operation The operation that creates this field.
  * @param resultType Type of the field.
  *
  * @author Greg Snider
  */
class MatrixField(operation: Operation,
                  resultType: FieldType)
        extends Field(operation, resultType)
        with CompilerError
        with SemanticError
{
  def this(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    this(Operation(opcode, inputs, fieldType), fieldType)

  require(resultType.tensorOrder == 2)
  require(resultType.elementType == Float32)

  /** Create a 0D matrix field filled with `value`. */
  def this(value: Matrix) =
    this(ConstantMatrix0DOp(() => value),
      Array[Field](),
      new FieldType(Shape(), value.shape, Float32))

  /** Create a 1D `columns` size matrix field filled by `f`. */
  def this(columns: Int, f: (Int) => Matrix)  =
    this(ConstantMatrix1DOp(f),
      Array[Field](),
      new FieldType(Shape(columns), f(0).shape, Float32))

  /** Create a 2D (`rows` x `columns`) matrix field filled by `f`. */
  def this(rows: Int, columns: Int, f: (Int, Int) => Matrix) =
    this(ConstantMatrix2DOp(f),
      Array[Field](),
      new FieldType(Shape(rows, columns), f(0, 0).shape, Float32))

  /** Create a 3D (`layers` x `rows` x `columns`) matrix field filled by `f`. */
  def this(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Matrix)  =
    this(ConstantMatrix3DOp(f),
      Array[Field](),
      new FieldType(Shape(layers, rows, columns), f(0, 0, 0).shape, Float32))
}

/** Functions for creating constant/recurrent matrix fields.
  */
object MatrixField extends CompilerError {

  /** Create a 0D matrix field filled with `value`. */
  def apply(value: Matrix): MatrixField =
    new MatrixField(value)

  /** Create a 1D `columns` size matrix field filled by `f`. */
  def apply(columns: Int, f: (Int) => Matrix): MatrixField  =
    new MatrixField(columns, f)

  /** Create a 2D (`rows` x `columns`) matrix field filled by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Matrix): MatrixField  =
    new MatrixField(rows, columns, f)

  /** Create a 3D (`layers` x `rows` x `columns`) matrix field filled by `f`. */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Matrix): MatrixField  =
    new MatrixField(layers, rows, columns, f)

  /** Create a zero-filled matrix field with field dimensions `fieldShape`
    * and tensor dimensions `tensorShape` */
  def apply(fieldShape: Shape, tensorShape: Shape): MatrixField = {
    if (tensorShape.dimensions != 2)
      internalError("Expect 2D tensor shape, found: " + tensorShape)
    val zeroMatrix = new Matrix(tensorShape(0), tensorShape(1))
    val shape = fieldShape
    fieldShape.dimensions match {
      case 0 => apply(zeroMatrix)
      case 1 => apply(shape(0), (_) => zeroMatrix)
      case 2 => apply(shape(0), shape(1), (_,_) => zeroMatrix)
      case 3 => apply(shape(0), shape(1), shape(2), (_,_,_) => zeroMatrix)
      case x => internalError("illegal number of dimensions"); null
    }
  }

  /** Create a randomly-filled matrix field with field dimensions `fieldShape`
    * and tensor dimensions `tensorShape`
    */
  def random(fieldShape: Shape, tensorShape: Shape): MatrixField = {
    if (tensorShape.dimensions != 2)
      internalError("Expect 2D tensor shape, found: " + tensorShape)
    val matrixRows = tensorShape(0)
    val matrixColumns = tensorShape(1)
    val shape = fieldShape

    val rand = new Random()

    def matrix(firstFieldPoint: Boolean) =
      Matrix(matrixRows, matrixColumns,
        (r,c) => rand.nextFloatResetFirstIf(firstFieldPoint && r==0 && c==0))

    fieldShape.dimensions match {
      case 0 => apply(Matrix.random(matrixRows, matrixColumns))
      case 1 => apply(shape(0),
        (c) => matrix(c==0))
      case 2 => apply(shape(0), shape(1),
        (r,c) => matrix(r==0 && c==0))
      case 3 => apply(shape(0), shape(1), shape(2),
        (l,r,c) => matrix(l==0 && r==0 && c==0))
      case x => internalError("illegal number of dimensions"); null
    }
  }
}

