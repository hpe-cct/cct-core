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

package cogx.compiler.parser.op

import cogx.platform.types.Opcode
import cogx.cogmath.geometry.Shape

/** An operation taking three or more inputs, producing a Field.
  *
  * @author Dick Carter
  */
private[cogx]
sealed abstract class NaryOpcode(name: String = "") extends Opcode(name)

// Special ops

/** Dynamically select one field from an array of fields. */
private[cogx] case object FieldArraySelectOp                  extends NaryOpcode
/** Create a ColorField from 3 ScalarFields (red, green and blue). */
private[cogx] case object MergeColorPlanesOp                  extends NaryOpcode
private[cogx] case object StackOp                             extends NaryOpcode
private[cogx] case class TensorStackOp(newTensorShape: Shape) extends NaryOpcode
private[cogx] case object DomainFilterColumnsOp extends NaryOpcode
private[cogx] case object DomainFilterRowsOp extends NaryOpcode