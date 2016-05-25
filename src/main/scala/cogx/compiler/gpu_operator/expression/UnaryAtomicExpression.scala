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
  * volatile array element and returns a value.
  *
  * @param operator The operation performed to produce the expression.
  * @param arg The argument to the operation.
  * @return Result expression.
  */
private[gpu_operator]
class UnaryAtomicExpression(operator: Operator,
                            arg: PointerExpression)
        extends GPUExpression(operator,
          arg.baseGpuType, Array(arg.asInstanceOf[GPUExpression]))
        with SemanticError
{
  val baseType = arg.baseGpuType
  val elementType = baseType.elementType
  val elements = baseType.elements
  check(elements == 1,
    "atomic operators only work on scalars, not vectors")
  check(elementType == Int32 || elementType == Uint32,
    "atomic operations only on work on 'int' or 'uint'.")
  check(arg.isVolatile,
    "atomic operations only work on volatile memory")
}
