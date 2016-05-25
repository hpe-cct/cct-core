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
import ColorActuator.zeroInitOp
import cogx.platform.types.Pixel

/** An output from a Cog computation, called an actuator, here pipelined to overlap CPU and GPU work.
  *
  * An output color field value is generated on every cycle of the
  * simulation. The user function `update` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * @param op The actuator opcode for this field.
  * @param source The color field driving this actuator.
  * @param update A user-supplied callback function that is provided an iterator over the actuator's new data.
  * @param resetHook An optional callback function that is called upon reset.
  *
  * @author Dick Carter
  */

class ColorActuator private[cogx] (op: ConstantOp, source: Field, update: (Iterator[Byte]) => Unit, resetHook: () => Unit)
        extends ColorField(op, Array[Field](), source.fieldType)
        with SemanticError
        with RestoreHooks
{
  require(source.fieldType.tensorShape.dimensions == 1)
  // Create this field as a pipeline stage fed by the source
  this <== source
  // The update function is driven by the pipeline stage, not the source directly.
  // This achieves an overlap between CPU and GPU work needed for peak simulation performance.
  private[cogx] val unpipedColorActuator =  new UnpipelinedColorActuator(this, update, resetHook) {
    // What's going on here with the RestoreHooks is a bit tricky- the user has subclassed ColorActuator and provided
    // overridden restoreParameter() and possibly restoringClass() methods.  But the actual UnpipelinedActuatorKernel doing the
    // saving is attached to the `unpipedColorActuator` operation. Thus, we link the hooks of the `unpipedColorActuator` to the hooks
    // of `this`.
    override def restoreParameters: String = ColorActuator.this.restoreParameters
    override def restoringClass = ColorActuator.this.restoringClass
  }

  // Use the factory object for making actuators of all dimensions

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Byte].
    *
    * This preserves the legacy constructor "new ColorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit) =
    this(zeroInitOp(source), source, update, () => {})

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Byte].
    *
    * This preserves the legacy constructor "new ColorActuator(source, update, resetHook)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit, resetHook: () => Unit) =
    this(zeroInitOp(source), source, update, resetHook)

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Byte].
    *
    * This preserves the legacy constructor "new ColorActuator(source, update, init(_,_) => Float)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit, init: (Int, Int) => Pixel) =
    this(ConstantColorOp(init), source, update, () => {})

  /** Create an actuator for a 2D color field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Byte].
    *
    * This preserves the legacy constructor "new ColorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new color values (row-major, rgba).
    * @param init Function providing initial value upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Byte]) => Unit, init: (Int, Int) => Pixel, resetHook: () => Unit) =
    this(ConstantColorOp(init), source, update, resetHook)
}

/** Factory for creating actuators that write fields to Scala arrays of Pixels.
  */
object ColorActuator extends SemanticError {

  // 2D color field actuator factory methods

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param init Function providing initial values upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Pixel]],
            init: (Int, Int) => Pixel, resetHook: () => Unit): ColorActuator = {
    UnpipelinedColorActuator.checkType(source, output)
    def update(iterator: Iterator[Byte]) {
      val rows = output.length
      val columns = output(0).length
      for (row <- 0 until rows; col <- 0 until columns) {
        output(row)(col) = new Pixel(iterator.next(), iterator.next(), iterator.next())
      }
    }
    new ColorActuator(ConstantColorOp(init), source, update _, resetHook)
  }

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param init Function providing initial values upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Pixel]],
            init: (Int, Int) => Pixel): ColorActuator = apply(source, output, init, () => {})

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    * The actuator outputs 0's upon reset.
    *
    * Init supplied:  NO
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Pixel]],
            resetHook: () => Unit): ColorActuator = {
    val zeroPixel = new Pixel(0,0,0)
    apply(source, output,  (_,_) => zeroPixel, resetHook)
  }

  /** Create an actuator for a 2D color field `source` that writes the
    * output of that field to the array `output` which the user can access.
    * The actuator outputs 0's upon reset.
    *
    * Init supplied:  NO
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Pixel]]): ColorActuator = {
    val zeroPixel = new Pixel(0,0,0)
    apply(source, output,  (_,_) => zeroPixel, () => {})
  }

  /** Create a ConstantColorOp that inits the field to all zeros. */
  def zeroInitOp(field: Field) = {
    val zeroPixel = new Pixel(0,0,0)
    field.dimensions match {
      case 2 => ConstantColorOp((_,_) => zeroPixel)
      case x => throw new RuntimeException(s"Expecting field of dimension 2, found: ${field.fieldType}")
    }
  }
}


