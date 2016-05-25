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
import cogx.platform.types.ElementTypes.{Float64, Int32, Int64}
import cogx.compiler.parser.semantics.SemanticError

/** Expression which takes two floating point arguments. The type of the
  * result is integral, but there are weird rules in play here:
  *
  * 1. If both arguments are float, the result is int.
  * 2. If both arguments are floatn, the result is intn.
  * 3. If both arguments are double, the result is int.
  * 4. If both arguments are doublen, the result is longn  !!!
  *
  * @param operator The operation performed to produce the expression.
  * @param arg1 The first argument to the operation.
  * @param arg2 The second argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class BinaryRelationalExpression(operator: Operator,
                                 arg1: GPUExpression,
                                 arg2: GPUExpression)
        extends GPUExpression(operator, {
          val isDouble = arg1.gpuType.elementType == Float64
          val isVector = arg1.gpuType.elements > 1
          if (isVector && isDouble)
            GPUType(Int64, arg1.gpuType.elements)
          else if (isVector && !isDouble)
            GPUType(Int32, arg1.gpuType.elements)
          else
            GPUType(Int32, 1)
        },
        Array(arg1, arg2)
        )
        with SemanticError
{
  check(arg1.gpuType.isFloat, "requires floating point arguments")
  check(arg2.gpuType.isFloat, "requires floating point arguments")
  check(arg1.gpuType == arg2.gpuType, "arguments are of different types")
}

