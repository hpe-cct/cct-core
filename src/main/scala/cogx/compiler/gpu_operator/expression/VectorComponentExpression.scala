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

import cogx.compiler.gpu_operator.types.GPUType
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.gpu_operator.Constraints

/** Expression to access one or more components of a vector expression.
  *
  * @param arg A vector expression.
  * @param components Characters denoting components to extract, e.g. 'x', 'y'
  */
private[gpu_operator]
class VectorComponentExpression(arg: GPUExpression, components: Array[Char])
        extends GPUExpression(
          new Operator("vectorComponent"),
          GPUType(arg.gpuType.elementType, components.length),
          Array(arg)
        )
        with SemanticError
        with Constraints
{
  // Semantic checking
  val inputVectorLength = arg.gpuType.elements
  if (components.length > MaxTensorSize)
    error("cannot create a vector longer than length " + MaxTensorSize)
  if (arg.gpuType.elements == 1)
    error("cannot take vector component of a scalar")
  for (component <- components) {
    component match {
      case 'x' =>
      case 'y' =>
      case 'z' =>
        if (inputVectorLength <= 2)
          error("cannot take component 'z' of a length 2 vector")
      case 'w' =>
        if (inputVectorLength <= 3)
          error("cannot take component 'w' of a length 2 or length 3 vector")
    }
  }

  /** Generate string for the expression.
    *
    * This has the form:
    * {{{
    *   (expression).xy
    * }}}
    */
  override def exprString: String = {
    val buffer = new StringBuffer
    buffer append "("
    buffer append arg.exprString
    buffer append ")"
    buffer append "."
    for (component <- components)
      buffer append component
    buffer.toString
  }
}