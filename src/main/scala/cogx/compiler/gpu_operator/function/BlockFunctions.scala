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

package cogx.compiler.gpu_operator.function

import cogx.cogmath.geometry.Shape
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.statement._
import cogx.compiler.gpu_operator.statement.WhileBlock
import cogx.compiler.gpu_operator.statement.ElseIfBlock
import cogx.compiler.gpu_operator.statement.Assignment
import cogx.compiler.gpu_operator.statement.EndFor
import cogx.compiler.gpu_operator.statement.EndElseIf
import cogx.compiler.gpu_operator.statement.EndIf
import cogx.compiler.gpu_operator.statement.ForBlock
import cogx.compiler.gpu_operator.statement.ElseBlock
import cogx.compiler.gpu_operator.statement.EndWhile
import cogx.compiler.gpu_operator.statement.IfBlock
import cogx.compiler.gpu_operator.statement.EndElse


/** User level functions for creating blocks.
  *
  * @author Greg Snider
  */
trait BlockFunctions {

  /** Begin an "if" block. */
  def _if(condition: GPUExpression)(code: => Unit) {
    IfBlock(condition)
    code
    EndIf()
  }

  /** Begin an "else" block. */
  def _else(code: => Unit) {
    ElseBlock()
    code
    EndElse()
  }

  /** Begin an "elseif" block. */
  def _elseif(condition: GPUExpression)(code: => Unit) {
    ElseIfBlock(condition)
    code
    EndElseIf()
  }

  /** Begin a "while" block. */
  def _while(condition: GPUExpression)(code: => Unit) {
    WhileBlock(condition)
    code
    EndWhile()
  }

  /** Begin a "for" block. */
  def _for(pre: Assignment, condition: GPUExpression, post: Assignment)
          (code: => Unit)
  {
    ForBlock(pre, condition, post)
    code
    EndFor()
  }

  /** Begin a "forEachTensorElement" block. */
  def _forEachTensorElement(shape: Shape)(code: => Unit) {
    ForEachTensorElementBlock(shape)
    code
    EndForEachTensorElement()
  }

  /** Begin an anonymous block. */
  def _block(code: => Unit): Unit =
  {
    AnonymousBlock()
    code
    EndAnonymous()
  }

}
