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
import cogx.cogmath.algebra.real.Vector
import cogx.platform.types.ElementTypes.Float32
import VectorActuator.zeroInitOp

import scala.reflect.ClassTag

/** An output from a Cog computation, called an actuator, here pipelined to overlap CPU and GPU work.
  *
  * An output vector field value is generated on every cycle of the
  * simulation. The user function `update` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * @param op The actuator opcode for this field.
  * @param source The vector field driving this actuator.
  *
  * @author Dick Carter
  */

class VectorActuator private[cogx] (op: ConstantOp, source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit)
        extends VectorField(op, Array[Field](), source.fieldType)
        with SemanticError
        with RestoreHooks
{
  require(source.fieldType.tensorShape.dimensions == 1)
  // Create this field as a pipeline stage fed by the source
  this <== source
  // The update function is driven by the pipeline stage, not the source directly.
  // This achieves an overlap between CPU and GPU work needed for peak simulation performance.
  private[cogx] val unpipedVectorActuator = new UnpipelinedVectorActuator(this, update, resetHook) {
    // What's going on here with the RestoreHooks is a bit tricky- the user has subclassed VectorActuator and provided
    // overridden restoreParameter() and possibly restoringClass() methods.  But the actual UnpipelinedActuatorKernel doing the
    // saving is attached to the `unpipedVectorActuator` operation. Thus, we link the hooks of the `unpipedVectorActuator` to the hooks
    // of `this`.
    override def restoreParameters: String = VectorActuator.this.restoreParameters
    override def restoringClass = VectorActuator.this.restoringClass
  }

  // Use the factory object for making actuators of all dimensions

  /** Create an actuator for a nD vector field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new VectorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit) =
    this(zeroInitOp(source), source, update, () => {})

  /** Create an actuator for a nD vector field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new VectorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit) =
    this(zeroInitOp(source), source, update, resetHook)

  /** Create an actuator for a nD vector field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new VectorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param init Function providing initial value upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, init: (Int, Int) => Vector) =
    this(zeroInitOp(source), source, update, () => {})

  /** Create an actuator for a nD vector field `source` that invokes a
    * callback function 'update' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new VectorActuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, init: (Int, Int) => Vector, resetHook: () => Unit) =
    this(zeroInitOp(source), source, update, resetHook)
}

/** Factory for creating actuators that write fields to Scala arrays.
  */
object VectorActuator extends SemanticError {

  /** Create a ConstantVectorOp that inits the field to all-zero vectors. */
  def zeroInitOp(field: Field) = {
    val v = new Vector(field.tensorShape.points)
    field.dimensions match {
      case 0 => ConstantVector0DOp(() => v)
      case 1 => ConstantVector1DOp((_) => v)
      case 2 => ConstantVector2DOp((_,_) => v)
      case 3 => ConstantVector3DOp((_,_,_) => v)
    }
  }

}
