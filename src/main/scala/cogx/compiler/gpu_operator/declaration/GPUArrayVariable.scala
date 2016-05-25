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
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.gpu_operator.Constraints


/** A variable array declaration.
  *
  * Base types of variables are of the "float4" or "float" which means that
  * scalars have been merged into the vector formulation and that all base
  * types are vectors. It is possible to have arrays of base types, vectors,
  * as well.
  *
  * @param baseType Fundamental vector type of the variable. In OpenCL
  *        float, float2, float3, ... are all fundamental types. Vectors and
  *        scalars are not really distinguished.
  * @param size The dimensions of the array.
  */
class GPUArrayVariable(val baseType: GPUType,
                       val size: Array[GPUExpression])
        extends Declaration
        with SemanticError
        with Constraints
{
  /** True if variable is shared (local). */
  var isShared = false
  /** True if variable is volatile. */
  var isVolatile = false

  // Semantic checking.
  if (size.length > MaxArrayDimensions)
    error("array dimensions cannot exceed " + MaxArrayDimensions)
  for (s <- size) {
    val gpuType = s.gpuType
    if (!gpuType.isInt)
      error("array dimension must be an integer")
    if (gpuType.elements > 1)
      error("array dimension must be a scalar, not a vector")
  }

  /** Unique name for the variable. */
  val name = GPUVariable.createName()

  constructed()

  /** Array indexing, uses Scala convention of apply. */
  def apply(index: Array[GPUExpression]) =
    ArrayLValue(this, index)

  /** Array indexing, uses Scala convention of apply. */
  def apply(index: GPUExpression*): ArrayLValue =
    apply(index.toArray)

  /** Create GPU code for the variable declaration. */
  override def toString: String = {
    val buffer = new StringBuffer
    if (isVolatile)
      buffer append "volatile "
    if (isShared)
      buffer append "__local "
    buffer append baseType.toString
    buffer append " " append name
    if (size.length > 0) {
      buffer append "["
      for (i <- 0 until size.length) {
        buffer append size(i).toString
        if (i != size.length - 1)
          buffer append "]["
      }
      buffer append "]"
    }
    buffer.toString
  }
}