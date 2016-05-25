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

import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.gpu_operator.expression.{Operator, GPUExpression}
import cogx.compiler.gpu_operator.types.GPUType

/** A variable declaration.
  *
  * Base types of variables are of the "float4" or "float" which means that
  * scalars have been merged into the vector formulation and that all base
  * types are vectors. It is possible to have arrays of base types, vectors,
  * as well.
  *
  * @param baseType Fundamental vector type of the variable. In OpenCL
  *        float, float2, float3, ... are all fundamental types. Vectors and
  *        scalars are not really distinguished.
  */
private[gpu_operator]
case class GPUVariable(baseType: GPUType)
        extends GPUExpression(new Operator("variable"), baseType, Array())
        with Declaration
        with SemanticError
        with LValue
{
  /** Unique name for the variable. */
  val name = GPUVariable.createName()
  /** True if variable is shared (local). */
  var isShared = false
  /** True if variable is volatile. */
  var isVolatile = false

  constructed()

  /** Create GPU code for the variable declaration. */
  override def toString: String = {
    val buffer = new StringBuffer
    if (isVolatile)
      buffer append "volatile "
    if (isShared)
      buffer append "__local "
    buffer append baseType.toString
    buffer append " " append name

    // Add initializer if non-local. Always initialize to zero.
    if (!isShared) {
      buffer append " = "
      buffer append baseType.zero
    }
    buffer.toString
  }

  /** Create GPU code for a variable reference. */
  override def exprString: String = {
    name
  }
}

/** Companion object for Variable that generates unique names
  *
  * This object formerly had the field:
  *
  * private var count = 0
  *
  * We now use a thread-local version to enable simultaneous Cog compilation from multiple threads.
  */
object GPUVariable {
  /** Number of declarations made since program startup. Each thread gets its own instance starting with 0. */
  private val _count =  new ThreadLocal[Int] {
    override def initialValue() = 0
  }
  private def count = _count.get()
  private def count_=(newCount: Int) { _count.set(newCount) }

  /** Begin declaration numbering at 0.  Used to get identical GPUOperators to appear equal */
  def reset(): Unit = { count = 0 }

  /** Create a unique name for a variable. */
  def createName(): String = {
    count += 1
    "_var" + count
  }
}

