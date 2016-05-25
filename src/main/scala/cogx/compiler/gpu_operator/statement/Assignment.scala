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

import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.declaration.LValue

/** An assignment statement.
  *
  * @param lvalue The lvalue that is being assigned to.
  * @param op Assigment operator, such as "=" or "+=" or "/=" etc.
  * @param expr The value being assigned to the lvalue.
  */
private[gpu_operator]
case class Assignment(lvalue: LValue, op: String, expr: GPUExpression)
        extends Statement
{
  constructed()

  /** Create a string for assignment statement. These are of the form:
    * {{{
    *    x = expr
    *    x[2] += expr
    *    x[i, 9] = expr
    * }}}
    */
  override def toString: String = {
    val statement = new StringBuffer
    statement append lvalue.name
    statement append " " append op append " "
    statement append expr.exprString
    statement.toString
  }
}

