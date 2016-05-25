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

package cogx.compiler.gpu_operator.expression

import cogx.compiler.parser.semantics.SemanticError

/** Expression which takes a single, int or float argument. This assumes
  * that the type of the result equals the type of the argument.
  *
  * @param operator The operation performed to produce the expression.
  * @param arg The argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class UnaryPrefixExpression(operator: Operator, arg: GPUExpression)
        extends GPUExpression(operator, arg.gpuType, Array(arg))
        with SemanticError
{
  /** Generate string for the expression. */
  override def exprString: String = {
    val buffer = new StringBuffer
    buffer append operator.toString
    buffer append "("
    buffer append arg.exprString
    buffer append ")"
    buffer.toString
  }
}