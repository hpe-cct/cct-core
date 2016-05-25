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
import cogx.platform.types.{Pixel, Opcode, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.utilities.Random

/** A color image.
  *
  * @param operation The operation that creates this field.
  * @param resultType Type of the field.
  *
  * @author Greg Snider
  */
class ColorField(operation: Operation,
                 resultType: FieldType)
        extends Field(operation, resultType)
        with CompilerError
        with SemanticError
{
  def this(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    this(Operation(opcode, inputs, fieldType), fieldType)

  // Color fields look like vector fields.
  require(resultType.tensorOrder == 1)
  require(resultType.tensorShape.points == ColorField.Colors)


  // These keywords are only defined for ColorFields, so they are listed here.
  // Fields only recognized by the compiler as being of the superclass 'Field'
  // will be implicitly converted by asInstanceOf[ColorField].

  def red = UnaryOperator(ColorPlaneRedOp, this)
  def green = UnaryOperator(ColorPlaneGreenOp, this)
  def blue = UnaryOperator(ColorPlaneBlueOp, this)
  def luminance = UnaryOperator(ColorPlaneLuminanceOp, this)
}

/** Function for creating constant/recurrent color fields.
  */
object ColorField extends CompilerError {
  private lazy val random = new java.util.Random
  val Colors = 3

  /** Create a 2D (`rows` x `columns`) color field filled by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Pixel): ColorField  =
    new ColorField(ConstantColorOp(f),
      Array(),
      new FieldType(Shape(rows, columns), Shape(Colors), Uint8Pixel))

  /** Create a 2D (`rows` x `columns`) color field filled with "black". */
  def apply(rows: Int, columns: Int): ColorField =
    apply(rows, columns, (_, _) => new Pixel())

  /** Create a 2D (`rows` x `columns`) random color field. */
  def random(rows: Int, columns: Int): ColorField = {
    val rand = new Random()

    def pixel(firstFieldPoint: Boolean) = {
      new Pixel(
        rand.nextIntResetFirstIf(firstFieldPoint).toByte,
        rand.nextInt.toByte,
        rand.nextInt.toByte)
    }

    apply(rows, columns, (r,c) => pixel(r==0 && c==0))
  }
}




