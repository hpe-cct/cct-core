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

/** Expression which takes three floating point arguments. This assumes
  * that the type of the result equals the type of the first argument.
  *
  * @param operator The operation performed to produce the expression.
  * @param arg1 The first argument to the operation.
  * @param arg2 The second argument to the operation.
  * @param arg3 The third argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class TernaryFloatExpression(operator: Operator,
                             arg1: GPUExpression,
                             arg2: GPUExpression,
                             arg3: GPUExpression)
        extends GPUExpression(operator,
          convertType(convertType(arg1.gpuType, arg2.gpuType), arg3.gpuType),
          Array(arg1, arg2, arg3))
        with SemanticError
{
  check(arg1.gpuType.isFloat, "requires floating point arguments")
  check(arg2.gpuType.isFloat, "requires floating point arguments")
  check(arg3.gpuType.isFloat, "requires floating point arguments")
  check(arg1.gpuType == arg2.gpuType, "arguments are of different types")
  check(arg1.gpuType == arg3.gpuType, "arguments are of different types")
}