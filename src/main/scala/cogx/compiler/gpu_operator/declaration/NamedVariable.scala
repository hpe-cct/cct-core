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

package cogx.compiler.gpu_operator.declaration

import cogx.compiler.gpu_operator.types.GPUType
import cogx.compiler.gpu_operator.expression.{Operator, GPUExpression}
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.CompilerError


/** A NAMED variable declaration. This is used internally, mainly for
  * creating temporary variables. Such variables may not be shared, and must
  * be of scalar (non-vector) type.
  *
  * @param baseType Fundamental scalar type of the variable (e.g. "int",
  *        "float", ...
  */
private[gpu_operator]
case class NamedVariable(baseType: GPUType, val name: String)
        extends GPUExpression(new Operator("named_variable"), baseType, Array())
        with Declaration
        with SemanticError
        with LValue
        with CompilerError
{
  if (baseType.elements > 1)
    internalError("named variables must be scalars")
  constructed()

  /** Create GPU code for the variable declaration. */
  override def toString: String = {
    val buffer = new StringBuffer
    buffer append baseType.toString
    buffer append " " append name
    buffer.toString
  }

  /** Create GPU code for a variable reference. */
  override def exprString: String = {
    name
  }
}