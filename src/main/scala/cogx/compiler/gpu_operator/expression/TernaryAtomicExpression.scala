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
import cogx.platform.types.ElementTypes.{Uint32, Int32}

/** An atomic expression which takes a pointer to a volatile var or
  * volatile array element, and 2 values, and returns a value.
  *
  * @param operator The operation performed to produce the expression.
  * @param pointer The pointer argument to the operation.
  * @param value1 The first value argument to the operation.
  * @param value2 The second value argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class TernaryAtomicExpression(operator: Operator,
                              pointer: PointerExpression,
                              value1: GPUExpression,
                              value2: GPUExpression)
        extends GPUExpression(
          operator,
          pointer.baseGpuType,
          Array(pointer.asInstanceOf[GPUExpression], value1, value2))
        with SemanticError
{
  val baseType = pointer.baseGpuType
  val elementType = baseType.elementType
  val elements = baseType.elements
  check(elements == 1,
    "atomic operators only work on scalars, not vectors")
  check(elementType == Int32 || elementType == Uint32,
    "atomic operations only on work on 'int' or 'uint'.")
  check(pointer.isVolatile,
    "atomic operations only work on volatile memory")

  check(value1.gpuType.elements == 1,
    "value argument to atomic function must be a scalar")
  val value1ElementType = value1.gpuType.elementType
  check(value1ElementType == Int32 || value1ElementType == Uint32,
    "value argument to atomic function must be int or uint")

  check(value2.gpuType.elements == 1,
    "value argument to atomic function must be a scalar")
  val value2ElementType = value1.gpuType.elementType
  check(value2ElementType == Int32 || value2ElementType == Uint32,
    "value argument to atomic function must be int or uint")
}
