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
import cogx.cogmath.algebra.real.Vector
import cogx.platform.types.{Opcode, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.utilities.Random

/** A multidimensional array of vectors.
  *
  * @param operation The operation that creates this field.
  * @param resultType Type of the field.
  *
  * @author Greg Snider
  */
class VectorField(operation: Operation,
                  resultType: FieldType)
        extends Field(operation, resultType)
        with CompilerError
        with SemanticError
{
  def this(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    this(Operation(opcode, inputs, fieldType), fieldType)

  require(resultType.tensorOrder == 1)
  require(resultType.elementType == Float32)

  /** Create a 0D vector field filled with `value`. */
  def this(value: Vector) =
    this(ConstantVector0DOp(() => value),
      Array[Field](),
      new FieldType(Shape(), value.shape, Float32))

  /** Create a 1D `columns` size vector field filled by `f`. */
  def this(columns: Int, f: (Int) => Vector) =
    this(ConstantVector1DOp(f),
      Array[Field](),
      new FieldType(Shape(columns), f(0).shape, Float32))

  /** Create a 2D (`rows` x `columns`) vector field filled by `f`. */
  def this(rows: Int, columns: Int, f: (Int, Int) => Vector)  =
    this(ConstantVector2DOp(f),
      Array[Field](),
      new FieldType(Shape(rows, columns), f(0, 0).shape, Float32))

  /** Create a 3D (`layers` x `rows` x `columns`) vector field filled by `f`. */
  def this(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Vector) =
    this(ConstantVector3DOp(f),
      Array[Field](),
      new FieldType(Shape(layers, rows, columns), f(0, 0, 0).shape, Float32))
}

/** Functions for creating constant/recurrent vector fields.
  */
object VectorField extends CompilerError {
  /** Random number generator. */
  private val rand = new Random

  /** Create a 0D vector field filled with `value`. */
  def apply(value: Vector): VectorField =
    new VectorField(value)

  /** Create a 1D `columns` size vector field filled by `f`. */
  def apply(columns: Int, f: (Int) => Vector): VectorField  =
    new VectorField(columns, f)

  /** Create a 2D (`rows` x `columns`) vector field filled by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Vector): VectorField  =
    new VectorField(rows, columns, f)

  /** Create a 3D (`layers` x `rows` x `columns`) vector field filled by `f`. */
  def apply(layers: Int, rows: Int, columns: Int, f: (Int, Int, Int) => Vector): VectorField  =
    new VectorField(layers, rows, columns, f)

  /** Create a zero-filled vector field with field dimensions `fieldShape`
    * and tensor dimensions `tensorShape` */
  def apply(fieldShape: Shape, tensorShape: Shape): VectorField = {
    if (tensorShape.dimensions != 1)
      internalError("Expect 1D tensor shape, found: " + tensorShape)
    val zeroVector = new Vector(tensorShape(0))
    val shape = fieldShape
    fieldShape.dimensions match {
      case 0 => apply(zeroVector)
      case 1 => apply(shape(0), (_) => zeroVector)
      case 2 => apply(shape(0), shape(1), (_,_) => zeroVector)
      case 3 => apply(shape(0), shape(1), shape(2), (_,_,_) => zeroVector)
      case x => internalError("illegal number of dimensions"); null
    }
  }

  /** Create a randomly-filled VectorField with field dimensions `fieldShape`
    * and tensor dimensions `tensorShape`.
    */
  def random(fieldShape: Shape, tensorShape: Shape): VectorField = {
    if (tensorShape.dimensions != 1)
      internalError("Expect 1D tensor shape, found: " + tensorShape)
    val vectorLength = tensorShape(0)
    val shape = fieldShape

    val rand = new Random()

    def vector(firstFieldPoint: Boolean) =
      Vector(vectorLength,
        (i) => rand.nextFloatResetFirstIf(firstFieldPoint && i==0))

    fieldShape.dimensions match {
      case 0 => apply(Vector.random(vectorLength))
      case 1 => apply(shape(0),
        (c) => vector(c==0))
      case 2 => apply(shape(0), shape(1),
        (r,c) => vector(r==0 && c==0))
      case 3 => apply(shape(0), shape(1), shape(2),
        (l,r,c) => vector(l==0 && r==0 && c==0))
      case x => internalError("illegal number of dimensions"); null
    }
  }
}




