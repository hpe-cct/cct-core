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

package cogx.runtime.debugger

import cogx.cogmath.circuit.Node
import cogx.platform.types.VirtualFieldRegister

/** A field in a ComputeGraph that is visible for debugging.
  *
  * @param inputs The fields which computationally drive the value of this
  *        field.
  * @param virtualFieldRegister The kernel which supplies the data in the field when
  *        executed. Note that a kernel can supply multiple fields.
  *
  * @author Greg Snider
  */

class ProbedField(inputs: Array[ProbedField],
                  private[runtime] val virtualFieldRegister: VirtualFieldRegister)
        extends Node[ProbedField](inputs)
{
  /** Get the type of the probed field. */
   val fieldType = virtualFieldRegister.fieldType

  override def toString = virtualFieldRegister.toString

  /** Get the hierarchical name for the field, root first.
    *
    * Hierarchical names are inferred automatically from the class structure
    * of the computation.
    *
    * @return Hierarchical name of the field, root fist.
    */
  def name: Seq[String] = virtualFieldRegister.name.split('.')

  /** Get the local name that the user gave the field in their application.
    *
    * @return Simple, one-component, name of the field
    */
  def simpleName: String = name(name.length - 1)

  /** Fields which determine the value of this field through operators.
    *
    * @return The fields which influence this field.
    */
  def dependsOn: Seq[ProbedField] = inputs

  def kernel = virtualFieldRegister.source

  /** If the field is a recurrence, returns the output field driving this.
    *
    * @return The output field, wrapped in Some(), driving this recurrence; if
    *         this is not a recurrence, returns None.
    */
  def feedbackFrom: Seq[ProbedField] = _feedbackFrom

  private var _feedbackFrom: Seq[ProbedField]  = Seq()

  def setFeedbackFrom(from: Seq[ProbedField]) {
    _feedbackFrom = from
  }
}