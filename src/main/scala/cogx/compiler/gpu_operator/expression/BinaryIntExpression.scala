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

import cogx.compiler.gpu_operator.types.AutomaticTypeConversions._
import cogx.compiler.parser.semantics.SemanticError

/** Expression which takes two integer arguments. This assumes
  * that the type of the result equals the type of the first argument.
  *
  * @param operator The operation performed to produce the expression.
  * @param arg1 The first argument to the operation.
  * @param arg2 The second argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class BinaryIntExpression(operator: Operator,
                          arg1: GPUExpression,
                          arg2: GPUExpression)
        extends GPUExpression(operator,
          convertType(arg1.gpuType, arg2.gpuType),
          Array(arg1, arg2))
        with SemanticError
{
  check(arg1.gpuType.isInt, "requires integer arguments")
  check(arg2.gpuType.isInt, "requires integer arguments")
}