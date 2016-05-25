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
import Actuator.{zeroInitOp, wrap}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/** An output from a Cog computation, called an actuator, here pipelined to overlap CPU and GPU work.
  *
  * An output scalar field value is generated on every cycle of the
  * simulation. The user function `update` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * Implicit ClassTag included to disambiguate the signature from another using Function1[Iterator[Float],Unit]
  *
  * This class needs some clean-up, since Actuators are created through both the 'new' keyword and factory object
  * apply() methods.  The apply methods are better on the one hand for isolating user code from changes in the
  * platform implementation.  However, the recommended approach for saving/restoring Actuators has the user
  * create a subclass of Actuator with restoreParameters and restoringClass overridden.
  *
  * @param op The actuator opcode for this field.
  * @param source The scalar field driving this actuator.
  * @param update A user-supplied callback function that is provided an iterator over the actuator's new data.
  * @param resetHook An optional callback function that is called upon reset.
  * @author Dick Carter
  */

class Actuator private[cogx] (op: ConstantOp,
                              source: Field,
                              update: (ScalarFieldReader) => Unit,
                              resetHook: () => Unit)
                              (implicit tt: ClassTag[Function1[ScalarFieldReader, Unit]])
        extends ScalarField(op, Array[Field](), source.fieldType)
        with SemanticError
        with RestoreHooks
{
  require(source.fieldType.tensorShape.dimensions == 0)
  // Create this field as a pipeline stage fed by the source
  this <== source
  // The update function is driven by the pipeline stage, not the source directly.
  // This achieves an overlap between CPU and GPU work needed for peak simulation performance.
  private[cogx] val unpipedActuator = new UnpipelinedActuator(this, update, resetHook) {
    // What's going on here with the RestoreHooks is a bit tricky- the user has subclassed Actuator and provided
    // overridden restoreParameter() and possibly restoringClass() methods.  But the actual UnpipelinedActuatorKernel doing the
    // saving is attached to the `unpipedActuator` operation. Thus, we link the hooks of the `unpipedActuator` to the hooks
    // of `this`.
    override def restoreParameters: String = Actuator.this.restoreParameters
    override def restoringClass = Actuator.this.restoringClass
  }

  // Use the factory object for making actuators of all dimensions

  private def this(op: ConstantOp, source: Field, update: Function1[Iterator[Float],Unit], resetHook: () => Unit) =
    this(op, source, wrap(update), resetHook)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit) =
    this(zeroInitOp(source), source, update, () => {})

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update, resetHook)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit) =
    this(zeroInitOp(source), source, update, resetHook)

  /** Create an actuator for a 2D scalar field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update, init(_,_) => Float)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, init: (Int, Int) => Float) =
    this(ConstantScalar2DOp(init), source, update, () => {})

  /** Create an actuator for a 2D scalar field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Float].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param init Function providing initial value upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, init: (Int, Int) => Float, resetHook: () => Unit) =
    this(ConstantScalar2DOp(init), source, update, resetHook)
}

/** Factory for creating actuators that write fields to Scala arrays.
  */
object Actuator extends SemanticError {

  type ScalarFieldReaderFunc = Function1[ScalarFieldReader, Unit]
  type IteratorOfFloatFunc = Function1[Iterator[Float], Unit]

  // 0D scalar field actuator factory methods

  /** Create an actuator for a 0D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES (supplied as () => Float)
    * Reset supplied: YES
    *
    * The use of TypeTag's here was used to coerce what would have been two distinct apply()
    * method signatures into one.  The problem with having two signatures is that the Scala compiler
    * then started refusing to convert fairly generic init function actual arguments of the form (say):
    *
    * () => 1
    *
    * into the required Function0[Float] type, requiring the user to instead write:
    *
    * () => 1f
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Float] or function of a ScalarFieldReader.
    * @param init Function providing initial value upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
                        init: () => Float, resetHook: () => Unit): Actuator = {
    val updateFunction = targetToUpdateFunction(source, target)
    new Actuator(ConstantScalar0DOp(init), source, updateFunction, resetHook)
  }

  /** Create an actuator for a 0D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: NO
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Array[Float] or function of a ScalarFieldReader.
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
                        init: () => Float): Actuator =
    apply(source, target, init, () => {})

  /** Create an actuator for a 0D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES (supplied as Float)
    * Reset supplied: YES
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Float] or function of a ScalarFieldReader.
    * @param initVal Initial value upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
            initVal: Float, resetHook: () => Unit): Actuator =
    apply(source, target, () => initVal, resetHook)

  /** Create an actuator for a 0D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES (supplied as Float)
    * Reset supplied: NO
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Array[Float] or function of a ScalarFieldReader.
    * @param initVal Initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
            initVal: Float): Actuator =
  apply(source, target, () => initVal, () => {})

  /** Create an actuator for a 1D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: YES
    *
    * The use of TypeTag's here was used to coerce what would have been two distinct apply()
    * method signatures into one.  The problem with having two signatures is that the Scala compiler
    * then started refusing to convert fairly generic init function actual arguments of the form (say):
    *
    * (c) => c + 1
    *
    * into the required Function1[Int,Float] type, requiring the user to instead write:
    *
    * (c:Int) => c + 1f
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Float] or function of a ScalarFieldReader.
    * @param init Function providing initial values upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
            init: (Int) => Float, resetHook: () => Unit): Actuator = {
    val updateFunction = targetToUpdateFunction(source, target)
    new Actuator(ConstantScalar1DOp(init), source, updateFunction, resetHook)
  }

  /** Create an actuator for a 1D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: NO
    *
    * @param source The field to be written to an array each clock cycle.
    * @param target Receiver of the data, either an Array[Float] or function of a ScalarFieldReader.
    * @param init Function providing initial values upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T, init: (Int) => Float): Actuator =
    apply(source, target, init, () => {})

  /** Create an actuator for a 0D or 1D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    * The actuator outputs 0's upon reset.
    *
    * Init supplied:  NO
    * Reset supplied: YES
    *
    * Some extra implicit arg had to added here to disambiguate this constructor's signature from the above-seen:
    *
    * apply(source: Field, output: Array[Float], init: () => Float)
    *
    * @param source The field to be written to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Float], resetHook: () => Unit)(implicit tt: ClassTag[Function0[Unit]]): Actuator = {
    source.dimensions match {
      case 0 => this(source, output, () => 0f, resetHook)
      case 1 => this(source, output, (_) => 0f, resetHook)
      case x => throw new RuntimeException("Array[Float] argument requires 1D or 0D field, found: " + x + "D")
    }
  }

  // 2D scalar field actuator factory methods

  /** Create an actuator for a 2D scalar field `source` that invokes a
    * callback function 'update' that is passed a ScalarFieldReader.
    *
    * Init supplied:  YES
    * Reset supplied: YES
    *
    * The use of TypeTag's here was used to coerce what would have been two distinct apply()
    * method signatures into one.  The problem with having two signatures is that the Scala compiler
    * then started refusing to convert fairly generic init function actual arguments of the form (say):
    *
    * (r,c) => r + c
    *
    * into the required Function2[Int,Int,Float] type, requiring the user to instead write:
    *
    * (r:Int, c:Int) => (r + c).toFloat
    *
    * Rather than break the existing codebase, we put up with the fact that the type checking
    * on the target parameter is now done at runtime, rather than at compile time.
    *
    * @param source The field to written output to an array each clock cycle.
    * @param target Receiver of the data, either an Array[ Array[Float] ] or function of a ScalarFieldReader.
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T, init: (Int, Int) => Float, resetHook: () => Unit) = {
    val updateFunction = targetToUpdateFunction(source, target)
    new Actuator(ConstantScalar2DOp(init), source, updateFunction, resetHook)
  }

  /** Create an actuator for a 2D scalar field `source` that invokes a
    * callback function 'update' that is passed a ScalarFieldReader.
    *
    * Init supplied:  YES
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param target Receiver of the data, either an Array[ Array[Float] ] or function of a ScalarFieldReader.
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T, init: (Int, Int) => Float): Actuator =
    apply(source, target, init, () => {})

  /** Create an actuator for a 2D scalar field `source` that writes the
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
  def apply(source: Field, output: Array[Array[Float]],
            resetHook: () => Unit): Actuator = apply(source, output,  (_,_) => 0f, resetHook)

  // 3D scalar field actuator factory methods

  /** Create an actuator for a 3D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: YES
    *
    * The use of TypeTag's here was used to coerce what would have been two distinct apply()
    * method signatures into one.  The problem with having two signatures is that the Scala compiler
    * then started refusing to convert fairly generic init function actual arguments of the form (say):
    *
    * (l,r,c) => l + r + c
    *
    * into the required Function3[Int,Int,Int,Float] type, requiring the user to instead write:
    *
    * (l:Int, r:Int, c:Int) => (l + r + c).toFloat
    *
    * Rather than break the existing codebase, we put up with the fact that the type checking
    * on the target parameter is now done at runtime, rather than at compile time.
    *
    * @param source The field to written output to an array each clock cycle.
    * @param target Receiver of the data, either an Array[ Array[ Array[Float] ] ] or function of a ScalarFieldReader.
    * @param init Function providing initial values upon reset.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T,
            init: (Int, Int, Int) => Float, resetHook: () => Unit): Actuator = {
    val updateFunction = targetToUpdateFunction(source, target)
    new Actuator(ConstantScalar3DOp(init), source, updateFunction, resetHook)
  }

  /** Create an actuator for a 3D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  YES
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param target Receiver of the data, either an Array[ Array[ Array[Float] ] ] or function of a ScalarFieldReader.
    * @param init Function providing initial values upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T, init: (Int, Int, Int) => Float): Actuator =
    apply(source, target, init, () => {})

  /** Create an actuator for a 3D scalar field `source` that writes the
    * output of that field to the array `output` which the user can access.
    *
    * Init supplied:  NO
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param output The output array where field is written.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, output: Array[Array[Array[Float]]],
            resetHook: () => Unit): Actuator =
    apply(source, output, (_,_,_) => 0f, resetHook)

  /** Create an actuator for an nD scalar field `source` whose target can be
    * a function(ScalarFieldReader), function(Iterator[Float]) or an nD array of Floats.
    *
    * Init supplied:  NO
    * Reset supplied: NO
    *
    * @param source The field to written output to an array each clock cycle.
    * @param target Receiver of the data, a function of a ScalarFieldReader or Iterator[Float].
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply[T: TypeTag](source: Field, target: T): Actuator = {
    val updateFunction = targetToUpdateFunction(source, target)
    new Actuator(zeroInitOp(source), source, updateFunction, () => {})
  }

  /** Create an actuator for an nD scalar field `source` whose update callback
    * function takes a ScalarFieldReader as an argument.
    *
    * Init supplied:  NO
    * Reset supplied: YES
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback function that is called after each new value is available.
    * @param resetHook An callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: ScalarFieldReaderFunc, resetHook: () => Unit): Actuator = {
    new Actuator(zeroInitOp(source), source, update, resetHook)
  }

  /** Create an actuator for an nD scalar field `source` whose update callback
    * function takes an Iterator[Float] as an argument.
    *
    * Init supplied:  NO
    * Reset supplied: YES
    *
    * Some extra implicit arg had to added here to disambiguate this constructor's signature from the above-seen:
    *
    * apply(source: Field, update: ScalarFieldReaderFunc, resetHook: () => Unit)
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback function that is called after each new value is available.
    * @param resetHook An callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def apply(source: Field, update: IteratorOfFloatFunc, resetHook: () => Unit)(implicit tt: ClassTag[IteratorOfFloatFunc]): Actuator = {
    new Actuator(zeroInitOp(source), source, wrap(update), resetHook)
  }

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

  /** Create a ConstantScalarOp that inits the field to all zeros. */
  def zeroInitOp(field: Field) = {
    field.dimensions match {
      case 0 => ConstantScalar0DOp(() => 0f)
      case 1 => ConstantScalar1DOp((_) => 0f)
      case 2 => ConstantScalar2DOp((_,_) => 0f)
      case 3 => ConstantScalar3DOp((_,_,_) => 0f)
    }
  }

  /** Adapter useful in expanding the functionality of Actuators.  Originally, the Actuator
    * update() was a user function that would take an Iterator[Float].  To enable bulk copies
    * of the Actuator data to a user Array[Float], the callback has been generalized from
    * an Iterator[Float] to a ScalarFieldReader (from which one can extract an iterator if one
    * wants, or invoke the bulk copier function get()).
    *
    * This wrapper converts the legacy 'update' function signature to the current more
    * capable one.
    *
    * @param f Old style form of update() that takes an Iterator[Float]
    * @return new style form of update() that takes a ScalarFieldReader
    */
  def wrap(f: (Iterator[Float]) => Unit) = {
    def wrapped(sfr: ScalarFieldReader) {
      f(sfr.iterator)
    }
    wrapped _
  }

  /** Create an actuator "update" function that takes a ScalarFieldReader and returns Unit.
    *
    * @param source The field supplying the data seen by the Actuator.
    * @param target Receiver of the data, either an nD Float Array or function of a ScalarFieldReader.
    * @return The update function
    */
  def targetToUpdateFunction[T: TypeTag](source: Field, target: T): ScalarFieldReaderFunc = {
    // This could be implemented as one big match on typeOf[T], but we make use of the "first-class"
    // types (like Array[Float]) that aren't erased to simplify the code.
    val updateFunction = target match {
      case a: Array[Float] =>
        checkType(source, a)
        (reader: ScalarFieldReader) => reader.get(a)
      case aa: Array[Array[Float]] =>
        checkType(source, aa)
        (reader: ScalarFieldReader) => reader.get(aa)
      case aaa: Array[Array[Array[Float]]] =>
        checkType(source, aaa)
        (reader: ScalarFieldReader) => reader.get(aaa)
      case f: Function1[_,_] =>
        typeOf[T] match {
          case t if t =:= typeOf[ScalarFieldReaderFunc] => target.asInstanceOf[ScalarFieldReaderFunc]
          case t if t =:= typeOf[IteratorOfFloatFunc] => wrap(target.asInstanceOf[IteratorOfFloatFunc])
          case _ => throw new RuntimeException("Unexpected Actuator target type: " + typeOf[T] + "." +
            "  Expecting a function: (ScalarFieldReader) => Unit.")
        }
      case _ => throw new RuntimeException("Unexpected Actuator target type: " + typeOf[T] + ".")
    }
    updateFunction
  }

}


