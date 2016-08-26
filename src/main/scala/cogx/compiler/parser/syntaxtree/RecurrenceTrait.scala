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

import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.CompilerError
import cogx.platform.types.{Opcode, FieldType}

/** A mix-in to the Field class to support recurrences.
  *
  * This class used to be called SyntaxNode, but since Fields are not
  * nodes in the SyntaxTree, but rather subclass Hyperedge[Operation],
  * this new name seemed more appropriate.
  *
  * @author Greg Snider
  */
private[cogx]
trait RecurrenceTrait
        extends SemanticError
        with CompilerError
{
  /** Operation performed by this node. */
  def opcode: Opcode

  /** Type of the result produced by this node. */
  val fieldType: FieldType

  /** Optional recurrence input for this node. */
  private var _recurrence: Option[Field] = None

  /** Read the current recurrence input, if any. */
  def recurrence: Option[Field] =
    _recurrence

  /** Write the recurrent input, making sure it's legal. */
  def recurrence_=(that: Field) {
    if (_recurrence == None)
      _recurrence = Some(that)
    else
      multipleFeedbackError()
  }

  def hasRecurrence: Boolean = {
    recurrence match {
      case Some(x) => true
      case None => false
    }
  }

  /** Convert node to a string for debugging. */
  override def toString: String = {
    val string = opcode.toString + " => " + fieldType.toString
    recurrence match {
      case Some(field) =>
        if (field eq this)
          string + " <== self"
        else
          string + "  <==  " + field.toString
      case None =>
        string
    }
  }
}