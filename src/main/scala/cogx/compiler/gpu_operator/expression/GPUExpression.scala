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

import cogx.parameters.Cog
import cogx.compiler.gpu_operator.types.GPUType

/** An expression.
  *
  * @param operator The operator that produces this expression.
  * @param gpuType The type of this expression.
  * @param args Arguments to the operator which produces this expression.
  */
class GPUExpression(val operator: Operator,
                    val gpuType: GPUType,
                    val args: Array[GPUExpression])
{
  // Debugging
  if (Cog.verboseUserGPUExpressions) {
    println("  expression > " + getClass.getSimpleName + " '" +
            operator.toString + "' " + gpuType.toString)
  }

  // Binary operators, all types, type preserving
  def +(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("+"), this, expr)
  def -(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("-"), this, expr)
  def *(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("*"), this, expr)
  def /(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("/"), this, expr)

  // Comparison operators, all types, boolean result
  @deprecated("use !== instead", "Cog 4.1")
  def !=(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("!="), this, expr)
  def !==(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("!="), this, expr)
  def >(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator(">"), this, expr)
  def <(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("<"), this, expr)
  def >=(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator(">="), this, expr)
  def <=(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("<="), this, expr)
  def &&(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("&&"), this, expr)
  def ||(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("||"), this, expr)
  /** Note that === replaces == to remove conflict with Scala. */
  def ===(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("=="), this, expr)


  // Binary, integer scalar and vector
  def %(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("%"), this, expr)

  // Binary operator, integral argument
  def >>(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator(">>"), this, expr)
  def <<(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("<<"), this, expr)

  // Binary operators, integer types
  def &(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("&"), this, expr)
  def ^(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("^"), this, expr)
  def |(expr: GPUExpression) =
    new BinaryInfixExpression(new Operator("|"), this, expr)

  // Prefix unary operators, type preserving
  def unary_!() =
    new UnaryPrefixExpression(new Operator("!"), this)

  def unary_+() =
    new UnaryPrefixExpression(new Operator("+"), this)

  def unary_-() = //(expr: Expression) =
    new UnaryPrefixExpression(new Operator("-"), this)

  def unary_~() =
    new UnaryPrefixExpression(new Operator("~"), this)


  /** Get vector component 0, a scalar, of the variable. */
  def x =
    new VectorComponentExpression(this, Array('x'))

  /** Get vector component 1, a scalar, of the variable. */
  def y =
    new VectorComponentExpression(this, Array('y'))

  /** Get vector component 2, a scalar, of the variable. */
  def z =
    new VectorComponentExpression(this, Array('z'))

  /** Get vector component 3, a scalar, of the variable. */
  def w =
    new VectorComponentExpression(this, Array('w'))

  // Two components:
  def xx = new VectorComponentExpression(this, Array('x', 'x'))
  def xy = new VectorComponentExpression(this, Array('x', 'y'))
  def xz = new VectorComponentExpression(this, Array('x', 'z'))
  def xw = new VectorComponentExpression(this, Array('x', 'w'))

  def yx = new VectorComponentExpression(this, Array('y', 'x'))
  def yy = new VectorComponentExpression(this, Array('y', 'y'))
  def yz = new VectorComponentExpression(this, Array('y', 'z'))
  def yw = new VectorComponentExpression(this, Array('y', 'w'))

  def zx = new VectorComponentExpression(this, Array('z', 'x'))
  def zy = new VectorComponentExpression(this, Array('z', 'y'))
  def zz = new VectorComponentExpression(this, Array('z', 'z'))
  def zw = new VectorComponentExpression(this, Array('z', 'w'))

  def wx = new VectorComponentExpression(this, Array('w', 'x'))
  def wy = new VectorComponentExpression(this, Array('w', 'y'))
  def wz = new VectorComponentExpression(this, Array('w', 'z'))
  def ww = new VectorComponentExpression(this, Array('w', 'w'))

  // Three components:
  def xxx = new VectorComponentExpression(this, Array('x', 'x', 'x'))
  def yxx = new VectorComponentExpression(this, Array('y', 'x', 'x'))
  def zxx = new VectorComponentExpression(this, Array('z', 'x', 'x'))
  def wxx = new VectorComponentExpression(this, Array('w', 'x', 'x'))

  def xyx = new VectorComponentExpression(this, Array('x', 'y', 'x'))
  def yyx = new VectorComponentExpression(this, Array('y', 'y', 'x'))
  def zyx = new VectorComponentExpression(this, Array('z', 'y', 'x'))
  def wyx = new VectorComponentExpression(this, Array('w', 'y', 'x'))

  def xzx = new VectorComponentExpression(this, Array('x', 'z', 'x'))
  def yzx = new VectorComponentExpression(this, Array('y', 'z', 'x'))
  def zzx = new VectorComponentExpression(this, Array('z', 'z', 'x'))
  def wzx = new VectorComponentExpression(this, Array('w', 'z', 'x'))

  def xwx = new VectorComponentExpression(this, Array('x', 'w', 'x'))
  def ywx = new VectorComponentExpression(this, Array('y', 'w', 'x'))
  def zwx = new VectorComponentExpression(this, Array('z', 'w', 'x'))
  def wwx = new VectorComponentExpression(this, Array('w', 'w', 'x'))



  def xxy = new VectorComponentExpression(this, Array('x', 'x', 'y'))
  def yxy = new VectorComponentExpression(this, Array('y', 'x', 'y'))
  def zxy = new VectorComponentExpression(this, Array('z', 'x', 'y'))
  def wxy = new VectorComponentExpression(this, Array('w', 'x', 'y'))

  def xyy = new VectorComponentExpression(this, Array('x', 'y', 'y'))
  def yyy = new VectorComponentExpression(this, Array('y', 'y', 'y'))
  def zyy = new VectorComponentExpression(this, Array('z', 'y', 'y'))
  def wyy = new VectorComponentExpression(this, Array('w', 'y', 'y'))

  def xzy = new VectorComponentExpression(this, Array('x', 'z', 'y'))
  def yzy = new VectorComponentExpression(this, Array('y', 'z', 'y'))
  def zzy = new VectorComponentExpression(this, Array('z', 'z', 'y'))
  def wzy = new VectorComponentExpression(this, Array('w', 'z', 'y'))

  def xwy = new VectorComponentExpression(this, Array('x', 'w', 'y'))
  def ywy = new VectorComponentExpression(this, Array('y', 'w', 'y'))
  def zwy = new VectorComponentExpression(this, Array('z', 'w', 'y'))
  def wwy = new VectorComponentExpression(this, Array('w', 'w', 'y'))


  def xxz = new VectorComponentExpression(this, Array('x', 'x', 'z'))
  def yxz = new VectorComponentExpression(this, Array('y', 'x', 'z'))
  def zxz = new VectorComponentExpression(this, Array('z', 'x', 'z'))
  def wxz = new VectorComponentExpression(this, Array('w', 'x', 'z'))

  def xyz = new VectorComponentExpression(this, Array('x', 'y', 'z'))
  def yyz = new VectorComponentExpression(this, Array('y', 'y', 'z'))
  def zyz = new VectorComponentExpression(this, Array('z', 'y', 'z'))
  def wyz = new VectorComponentExpression(this, Array('w', 'y', 'z'))

  def xzz = new VectorComponentExpression(this, Array('x', 'z', 'z'))
  def yzz = new VectorComponentExpression(this, Array('y', 'z', 'z'))
  def zzz = new VectorComponentExpression(this, Array('z', 'z', 'z'))
  def wzz = new VectorComponentExpression(this, Array('w', 'z', 'z'))

  def xwz = new VectorComponentExpression(this, Array('x', 'w', 'z'))
  def ywz = new VectorComponentExpression(this, Array('y', 'w', 'z'))
  def zwz = new VectorComponentExpression(this, Array('z', 'w', 'z'))
  def wwz = new VectorComponentExpression(this, Array('w', 'w', 'z'))


  def xxw = new VectorComponentExpression(this, Array('x', 'x', 'w'))
  def yxw = new VectorComponentExpression(this, Array('y', 'x', 'w'))
  def zxw = new VectorComponentExpression(this, Array('z', 'x', 'w'))
  def wxw = new VectorComponentExpression(this, Array('w', 'x', 'w'))

  def xyw = new VectorComponentExpression(this, Array('x', 'y', 'w'))
  def yyw = new VectorComponentExpression(this, Array('y', 'y', 'w'))
  def zyw = new VectorComponentExpression(this, Array('z', 'y', 'w'))
  def wyw = new VectorComponentExpression(this, Array('w', 'y', 'w'))

  def xzw = new VectorComponentExpression(this, Array('x', 'z', 'w'))
  def yzw = new VectorComponentExpression(this, Array('y', 'z', 'w'))
  def zzw = new VectorComponentExpression(this, Array('z', 'z', 'w'))
  def wzw = new VectorComponentExpression(this, Array('w', 'z', 'w'))

  def xww = new VectorComponentExpression(this, Array('x', 'w', 'w'))
  def yww = new VectorComponentExpression(this, Array('y', 'w', 'w'))
  def zww = new VectorComponentExpression(this, Array('z', 'w', 'w'))
  def www = new VectorComponentExpression(this, Array('w', 'w', 'w'))


  /** Generate string for the expression.
    *
    * This has the form:
    * {{{
    *   ( arg0, arg1, ... )
    * }}}
    */
  def exprString: String = {
    val buffer = new StringBuffer
    buffer append " "
    buffer append operator.toString
    buffer append "("
    for (i <- 0 until args.length) {
      buffer append args(i).exprString
      if (i < args.length - 1)
        buffer append ", "
    }
    buffer append ")"
    buffer.toString
  }

  /** Default string conversion. */
  override def toString: String = exprString
}
