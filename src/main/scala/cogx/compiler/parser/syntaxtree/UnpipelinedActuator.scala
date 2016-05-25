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
import cogx.platform.cpumemory.readerwriter.ScalarFieldReader
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.types.FieldType
import Actuator.wrap

import scala.reflect.ClassTag

/** An output from a Cog computation, called an actuator.
  *
  * An output scalar field value is generated on every cycle of the
  * simulation. The user function `newOutput` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * This class needs some clean-up, since Actuators are created through both the 'new' keyword and factory object
  * apply() methods.  The apply methods are better on the one hand for isolating user code from changes in the
  * platform implementation.  However, the recommended approach for saving/restoring Actuators has the user
  * create a subclass of Actuator with restoreParameters and restoringClass overridden.

  * @param op The actuator opcode for this field.
  * @param source The scalar field driving this actuator.
  * @author Dick Carter
  */

class UnpipelinedActuator private[cogx] (op: UnpipelinedActuatorOp, source: Field)
        extends Operation(op, Array(source), Array[FieldType]())
        with SemanticError
        with RestoreHooks
{

  // Ensure that the field that is "actuated" is not eliminated by the merger.
  source.markProbed()

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit) =
    this(UnpipelinedActuatorOp(wrap(update), () => {}), source)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit) =
    this(UnpipelinedActuatorOp(wrap(update), resetHook), source)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed a ScalarFieldReader.
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * Reset supplied: NO
    *
    * Implicit ClassTag included to disambiguate the signature from another using Function1[Iterator[Float],Unit]
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (ScalarFieldReader) => Unit, resetHook: () => Unit)(implicit tt: ClassTag[Function1[ScalarFieldReader, Unit]])
  =
    this(UnpipelinedActuatorOp(update, resetHook), source)

  require(source.fieldType.tensorShape.dimensions == 0)
}

/** Factory for creating actuators that write fields to Scala arrays.
  */
object UnpipelinedActuator extends SemanticError {


  import Actuator.ScalarFieldReaderFunc
  import Actuator.IteratorOfFloatFunc

  // 0D or 1D scalar field actuator factory methods

  /** Create an actuator for a 0D or 1D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: YES
    *
    * @param source The field to be written to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Float],
            resetHook: () => Unit): UnpipelinedActuator = {
    checkType(source, output)
    def update(iterator: Iterator[Float]) {
      val columns = output.length
      for (col <- 0 until columns)
        output(col) = iterator.next()
    }
    new UnpipelinedActuator(UnpipelinedActuatorOp(wrap(update), resetHook), source)
  }

  /** Create an actuator for a 0D or 1D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: NO
    *
    * @param source The field to be written to an array each clock cycle.
    * @param output The output array where field is written.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Float]): UnpipelinedActuator = apply(source, output, () => {})

  // 2D scalar field actuator factory methods

  /** Create an actuator for a 2D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Array[Float]],
            resetHook: () => Unit): UnpipelinedActuator = {
    checkType(source, output)
    def update(iterator: Iterator[Float]) {
      val rows = output.length
      val columns = output(0).length
      for (row <- 0 until rows; col <- 0 until columns)
        output(row)(col) = iterator.next()
    }
    new UnpipelinedActuator(UnpipelinedActuatorOp(wrap(update), resetHook), source)
  }

  /** Create an actuator for a 2D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field,
            output: Array[Array[Float]]): UnpipelinedActuator =
  apply(source, output, () => {})

  // 3D scalar field actuator factory methods

  /** Create an actuator for a 3D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Array[Float]]],
            resetHook: () => Unit): UnpipelinedActuator = {
    checkType(source, output)
    def update(iterator: Iterator[Float]) {
      val layers = output.length
      val rows = output(0).length
      val columns = output(0)(0).length
      for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
        output(layer)(row)(col) = iterator.next()
    }
    new UnpipelinedActuator(UnpipelinedActuatorOp(wrap(update), resetHook), source)
  }

  /** Create an actuator for a 3D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Array[Float]]]): UnpipelinedActuator =
    apply(source, output, () => {})


  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed a ScalarFieldReader.
    *
    * This preserves the legacy constructor "new Actuator(source, update, resetHook)"
    *
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: ScalarFieldReaderFunc, resetHook: () => Unit) =
    new UnpipelinedActuator(UnpipelinedActuatorOp(update, resetHook), source)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed a ScalarFieldReader.
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: ScalarFieldReaderFunc): UnpipelinedActuator =
    apply(source, update, () => {})


  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed a ScalarFieldReader.
    *
    * This preserves the legacy constructor "new Actuator(source, update, resetHook)"
    *
    * Reset supplied: YES
    *
    * Implicit ClassTag included to disambiguate the signature from another using Function1[Iterator[Float],Unit]
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: IteratorOfFloatFunc, resetHook: () => Unit)(implicit tt: ClassTag[IteratorOfFloatFunc]) =
    new UnpipelinedActuator(UnpipelinedActuatorOp(wrap(update), resetHook), source)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed a ScalarFieldReader.
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * Reset supplied: NO
    *
    * Implicit ClassTag included to disambiguate the signature from another using Function1[Iterator[Float],Unit]
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: IteratorOfFloatFunc)(implicit tt: ClassTag[IteratorOfFloatFunc]): UnpipelinedActuator =
    apply(source, update, () => {})

  /** Type checking for Actuator creation.
   *
    * Verifies that `source` is a scalar type, and that the shape of `source`
    * exactly matches the shape of `output`, necessary since `output` will
    * be written with the state of the field each cycle.
    */
  private def checkType(source: Field, output: Array[_]) {
    // Check that source is a scalar field.
    val fieldShape = source.fieldType.fieldShape
    val tensorShape = source.fieldType.tensorShape
    val elementType = source.fieldType.elementType
    if (tensorShape.dimensions != 0)
      actuatorTypeError(source.fieldType)
    if (elementType != Float32)
      actuatorTypeError(source.fieldType)

    // Check that source and output have the same shape. Note that we need to
    // coerce a 0D field to have a 1D shape since Scala doesn't support the
    // notion of a 0D array holding any value.
    val arrayShape = getArrayShape(output)
    val coercedFieldShape = if (fieldShape == Shape()) Shape(1) else fieldShape
    if (!(coercedFieldShape == arrayShape))
      actuatorArrayMatchError(coercedFieldShape, output)
  }

  /** Get the shape of multidimensional `array`. */
  private def getArrayShape(array: Array[_]): Shape = {
    array match {
      case array: Array[Float] =>
        // 0D or 1D field
        Shape(array.length)

      case array: Array[Array[Float]] =>
        // 2D field
        val out = array
        Shape(out.length, out(0).length)

      case array: Array[Array[Array[Float]]] =>
        // 3D field
        val out = array
        Shape(out.length, out(0).length, out(0)(0).length)
      case _ =>
        internalError("unexpected field dimensionality.");
        null
    }
  }
}


