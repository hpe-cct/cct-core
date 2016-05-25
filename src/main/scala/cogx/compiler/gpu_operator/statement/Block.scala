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

package cogx.compiler.gpu_operator.statement

import cogx.cogmath.geometry.Shape
import cogx.compiler.gpu_operator.expression.GPUExpression

/** A statement initiating the start of a block (for, if, ...).
  *
  * @author Greg Snider
  */
private[gpu_operator]
sealed abstract class Block extends Statement

/** Start of an if block.
  *
  * @param condition Condition expression for if.
  */
private[gpu_operator]
case class IfBlock(condition: GPUExpression) extends Block {
  override def toString: String = "if (" + condition.exprString + ") {"
  constructed()
}

/** Start of an else block.
  */
private[gpu_operator]
case class ElseBlock() extends Block {
  override def toString: String = "else {"
  constructed()
}

/** Start of an else-if block.
  *
  * @param cond Condition expression for else-if.
  */
private[gpu_operator]
case class ElseIfBlock(cond: GPUExpression) extends Block {
  override def toString: String = "else if (" + cond.exprString + "){"
  constructed()
}

/** Start of while block.
  *
  * @param cond Condition expression for while.
  */
private[gpu_operator]
case class WhileBlock(cond: GPUExpression) extends Block {
  override def toString: String = "while (" + cond.exprString + ") {"
  constructed()
}

/** Start of a for block.
  *
  * @param init Initial assignment to loop variable.
  * @param cond Condition expression.
  * @param post End-of-loop assignment to loop variable.
  */
private[gpu_operator]
case class ForBlock(init: Assignment,
                    cond: GPUExpression,
                    post: Assignment) extends Block
{
  Statement.unregister(init)
  Statement.unregister(post)
  override def toString: String =
    "for (" + init.toString + "; " + cond.exprString + "; " + post.toString + ") {"
  constructed()
}

/** Start of an anonymous block, used for creating variables in a new scope
  * without clashing with variables with the same name in an outer scope.
  *
  * Equivalent to this
  * {{{
  *    {          // Start anonymous block
  *       code
  *    }          // End anonymous block
  * }}}
  */
private[gpu_operator]
case class AnonymousBlock() extends Block {
  override def toString: String = "{"
  constructed()
}

/** Start of a function block.
  *
  * This translates to an empty string when integrated with HyperKernels, but
  * could be resurrected for a different implementation.
  */
private[gpu_operator]
case class FunctionBlock() extends Block {
  override val toString: String = ""
  constructed()
}

/** Start of a _forEachTensorElement block.
  *
  * @param shape The shape of the tensors to be iterated over.
  */
private[gpu_operator]
case class ForEachTensorElementBlock(shape: Shape) extends Block {
  override def toString: String = {
    "for (int _tensorElement = 0; _tensorElement < " +
      shape.points +
      "; _tensorElement++) {"
  }
  constructed()
}
