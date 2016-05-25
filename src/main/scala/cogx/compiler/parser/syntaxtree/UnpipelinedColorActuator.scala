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
import cogx.cogmath.geometry.Shape
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.platform.types.{Pixel, FieldType}

/** An output from a Cog computation, called an actuator.
  *
  * An output color field value is generated on every cycle of the
  * simulation. The user function `newOutput` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * @param op The actuator opcode for this field.
  * @param source The color field driving this actuator.
  *
  * @author Dick Carter
  */

class UnpipelinedColorActuator private[cogx] (op: UnpipelinedColorActuatorOp, source: Field)
        extends Operation(op, Array(source), Array[FieldType]())
        with SemanticError
        with RestoreHooks
{
  require(source.fieldType.fieldShape.dimensions == 2,
    "ColorActuator source field must have 2 dimensions, found: " + source.fieldType.fieldShape.dimensions)
  require(source.fieldType.tensorShape == Shape(3))
  require(source.fieldType.elementType == Uint8Pixel,
    "ColorActuator source field must be a color field, found element type: " + source.fieldType.elementType)

  // Ensure that the field that is "actuated" is not eliminated by the merger.
  source.markProbed()

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new UnpipelinedColorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit) =
    this(UnpipelinedColorActuatorOp(update, () => {}), source)

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new UnpipelinedColorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit, resetHook: () => Unit) =
    this(UnpipelinedColorActuatorOp(update, resetHook), source)

}

/** Factory for creating actuators that write fields to Scala arrays.
  */
object UnpipelinedColorActuator extends SemanticError {

  // 2D color field actuator factory methods

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array of Pixels where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Array[Pixel]],
            resetHook: () => Unit): UnpipelinedColorActuator = {
    checkType(source, output)
    def update(iterator: Iterator[Byte]) {
      val rows = output.length
      val columns = output(0).length
      for (row <- 0 until rows; col <- 0 until columns) {
        output(row)(col) = new Pixel(iterator.next(), iterator.next(), iterator.next())
      }
    }
    new UnpipelinedColorActuator(UnpipelinedColorActuatorOp(update _, resetHook), source)
  }

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Array[Pixel]]): UnpipelinedColorActuator =
  apply(source, output, () => {})

  /** Type checking for ColorActuator creation.
   *
    * Verifies that `source` is a colorfield type, and that the shape of `source`
    * exactly matches the shape of `output`, necessary since `output` will
    * be written with the state of the field each cycle.
    */
  def checkType(source: Field, output: Array[_]) {
    // Check that source is a color field.
    val fieldShape = source.fieldType.fieldShape
    val tensorShape = source.fieldType.tensorShape
    val elementType = source.fieldType.elementType
    if (fieldShape.dimensions != 2)
      actuatorTypeError(source.fieldType)
    if (tensorShape.dimensions != 1 || tensorShape.points != 3)
      actuatorTypeError(source.fieldType)
    if (elementType != Uint8Pixel)
      actuatorTypeError(source.fieldType)

    // Check that source and output have the same shape. Note that we need to
    // coerce a 0D field to have a 1D shape since Scala doesn't support the
    // notion of a 0D array holding any value.
    val arrayShape = getArrayShape(output)
    if (!(fieldShape == arrayShape))
      actuatorArrayMatchError(fieldShape, output)
  }

  /** Get the shape of multidimensional `array`. */
  private def getArrayShape(array: Array[_]): Shape = {
    array match {
      case array: Array[Array[Pixel]] =>
        // 2D field
        val out = array
        Shape(out.length, out(0).length)
      case x =>
        internalError(s"unexpected field dimensionality or type of receiving array, expecting Array[Array[Pixel]], found $x.");
        null
    }
  }
}


