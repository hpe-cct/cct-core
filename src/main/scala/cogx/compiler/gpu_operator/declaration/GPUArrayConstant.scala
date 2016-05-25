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

import cogx.cogmath.algebra.real.Matrix
import cogx.compiler.gpu_operator.FloatConstantsTrait
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.function.ConstantExpressionFunctions
import cogx.compiler.gpu_operator.types.GPUType
import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types.ElementTypes.Float32
object ConstantExpression extends ConstantExpressionFunctions
import ConstantExpression.ConstInt0D

/** A constant array declaration. This currently supports only arrays of floats.
  *
  * NOTE: These have been disabled since constant arrays may only be declared
  * at program scope and we don't yet have a mechanism for doing that.
  *
  * @param size The dimensions of the array.
  * @param values The constant values held in the array. This must be
  *               Array[Float] for 1D array, Array[Array[Float] ] for a 2D
  *               array, and Array[Array[Array[Float] ] ] for a 3D array.
  */
class GPUArrayConstant(size: Array[Int], values: Object)
  extends GPUArrayVariable(GPUType(Float32, 1),
    size.map(i => ConstInt0D(i).asInstanceOf[GPUExpression]))
  with SemanticError
  with FloatConstantsTrait
{
  values match {
    case v: Array[Float] =>
      if (size.length != 1)
        error("dimensionality of initializer doesn't match array dimensions")
    case v: Array[Array[Float]] =>
      if (size.length != 2)
        error("dimensionality of initializer doesn't match array dimensions")
    case v: Array[Array[Array[Float]]] =>
      if (size.length != 3)
        error("dimensionality of initializer doesn't match array dimensions")
    case x =>
      throw new Exception("illegal initializer for constant array: " + x)
  }

  def this(values: Float*) =
    this(Array(values.toArray.length), values.toArray)

  def this(matrix: Matrix) =
    this(Array(matrix.rows, matrix.columns), {
      val array = Array.tabulate[Float](matrix.rows, matrix.columns) {
        (r, c) => matrix(r, c)
      }
      array
    })

  /** Create GPU code for the constant array declaration. */
  override def toString: String = {
    values match {
      case v: Array[Float] =>
        toString1D(v)
      case v: Array[Array[Float]] =>
        toString2D(v)
      case v: Array[Array[Array[Float]]] =>
        toString3D(v)
      case x =>
        throw new Exception("internal error")
    }
  }

  /** Create GPU code for a 1D constant array. */
  private def toString1D(values: Array[Float]): String = {
    val buffer = new StringBuffer
    buffer append "__constant float "
    buffer append name
    buffer append "[" + values.length + "] = "
    for (i <- 0 until values.length) {
      buffer append toOpenCLString(values(i))
      if (i < values.length - 1)
        buffer append ", "
    }
    buffer.toString
  }

  /** Create GPU code for a 2D constant array. */
  private def toString2D(values: Array[Array[Float]]): String = {
    val buffer = new StringBuffer
    buffer append "__constant float "
    buffer append name
    buffer append "[" + values.length + "]"
    buffer append "[" + values(0).length + "] "
    buffer append "= {"
    for (i <- 0 until values.length) {
      buffer append "{"
      for (j <- 0 until values(i).length) {
        buffer append toOpenCLString(values(i)(j))
        if (j < values.length - 1)
          buffer append ", "
      }
      buffer append "}"
      if (i < values.length - 1)
        buffer append ", "
    }
    buffer append "}"
    buffer.toString
  }


  /** Create GPU code for a 3D constant array. */
  private def toString3D(values: Array[Array[Array[Float]]]): String = {
    error("3D constant arrays not yet supported")
    null
  }
}
