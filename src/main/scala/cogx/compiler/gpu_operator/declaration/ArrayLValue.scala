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

import cogx.compiler.gpu_operator.expression.{Operator, GPUExpression}
import cogx.compiler.parser.semantics.SemanticError

/** An array reference to a single vector element in an array
  * that is a legal lvalue.
  *
  * @param array The array variable being index.
  * @param index The index into the array variable producing an lvalue.
 */
private[gpu_operator]
case class ArrayLValue(array: GPUArrayVariable, index: Array[GPUExpression])
        extends GPUExpression(
          new Operator("arrayLValue"),
          array.baseType,
          Array()
        )
        with SemanticError
        with LValue
{
  require(index.length > 0)

  /** True if array pointed to is volatile. */
  def isVolatile = array.isVolatile

  /** True if array pointed to is shared (local). */
  def isShared = array.isShared

  // Semantic checking
  for (i <- index)
    check(i.gpuType.isInt, "non-integer index")
  check(index.length == array.size.length,
    "array indices do not match dimension of variable")

  /** Name of the lvalue. */
  val name: String = {
    val string = new StringBuffer
    string append array.name
    string append "["
    for (i <- 0 until index.length) {
      string append index(i).exprString
      if (i < index.length - 1)
        string append "]["
    }
    string append "]"
    string.toString
  }

  override def exprString: String = {
    name
  }
}
