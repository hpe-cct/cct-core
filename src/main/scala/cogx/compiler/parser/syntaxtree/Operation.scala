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

import cogx.cogmath.hypercircuit.{Hyperedge, Hypernode}
import cogx.platform.types.{FieldType, Opcode}

/** A node in the syntax tree.
  *
  * @param _inputs The input Fields to the operation.
  * @param fieldTypes The FieldTypes of the outputs of the operation.
  *
  * @author Dick Carter
  */
private[cogx]
class Operation(private[this] var  _opcode: Opcode, _inputs: Array[Field], fieldTypes: Array[FieldType])
        extends Hypernode[Operation](_inputs.asInstanceOf[Array[Hyperedge[Operation]]])
{
  /** The opcode for the operation. */
  def opcode: Opcode = _opcode

  /** Remove reference to the possible large Opcode instance (constant opcode functions may have data references). */
  def releaseResources() { _opcode = null.asInstanceOf[Opcode] }

  /** The inputs to this kernel, as Fields, not as the base class
    * Hyperedge[Operation]
    */
  override def inputs = super.inputs.asInstanceOf[Seq[Field]]

  /** The outputs to this kernel, as Fields, not as the base class
    * Hyperedge[Operation]
    */
  override def outputs = super.outputs.asInstanceOf[Seq[Field]]

  /** A description of the operation and the FieldTypes it operates on
    */
  override def toString = {
    val inputs = fieldTypes.toSeq.mkString("( ", ",", " )")
    s"$opcode $inputs"
  }

}

object Operation {
  def apply(opcode: Opcode, inputs: Array[Field], fieldTypes: Array[FieldType]) =
    new Operation(opcode, inputs, fieldTypes)

  def apply(opcode: Opcode, inputs: Array[Field], fieldType: FieldType) =
    new Operation(opcode, inputs, Array(fieldType))
}