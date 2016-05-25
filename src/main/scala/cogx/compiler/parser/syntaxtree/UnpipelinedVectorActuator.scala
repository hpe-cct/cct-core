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
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.types.FieldType
import cogx.cogmath.algebra.real.Vector

/** An output from a Cog computation, called an actuator.
  *
  * An output scalar field value is generated on every cycle of the
  * simulation. The user function `newOutput` is called when the output is
  * ready, so that the user may use that information elsewhere.
  *
  * @param op The actuator opcode for this field.
  * @param source The scalar field driving this actuator.
  *
  * @author Dick Carter
  */

class UnpipelinedVectorActuator private[cogx] (op: UnpipelinedVectorActuatorOp, source: Field)
        extends Operation(op, Array(source), Array[FieldType]())
        with SemanticError
        with RestoreHooks
{

  // Ensure that the field that is "actuated" is not eliminated by the merger.
  source.markProbed()

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit) =
    this(UnpipelinedVectorActuatorOp(update, () => {}), source)

  /** Create an actuator for a nD scalar field `source` that invokes a
    * callback function 'newOutput' that is passed an Iterator[Vector].
    *
    * This preserves the legacy constructor "new Actuator(source, update)"
    *
    * @param source The field to written output to an array each clock cycle.
    * @param update A callback that is passed an iterator over the new values.
    * @param resetHook An optional callback function that is called upon reset.
    * @return An actuator that writes `source` to `output` each cycle.
    */
  def this(source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit) =
    this(UnpipelinedVectorActuatorOp(update, resetHook), source)

  require(source.fieldType.tensorShape.dimensions == 1)
}
