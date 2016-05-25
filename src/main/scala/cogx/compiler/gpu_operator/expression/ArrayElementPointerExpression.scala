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

import cogx.compiler.gpu_operator.declaration.ArrayLValue

/** A pointer to an element in an array.
  *
  * @author Greg snider
  */
case class ArrayElementPointerExpression(to: ArrayLValue)
        extends GPUExpression(new Operator("&"),
          to.gpuType,
          Array(to.asInstanceOf[GPUExpression]))
        with PointerExpression
{
  /** True if variable pointed to is volatile. */
  def isVolatile = to.isVolatile

  /** True if variable pointed to is shared (local). */
  def isShared = to.isShared

  /** Type of memory that pointer points to. */
  def baseGpuType = to.gpuType
}
