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

import cogx.compiler.gpu_operator.statement.Assignment
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.types.GPUType

/** An lvalue is a memory location that can be assigned to. (The name is
  * historical: "lvalue" is meant to suggest "left hand side of an assignment
  * statement.") In OpenCL, this is all vector/scalar types, but not arrays.
  * Examples:
  * {{{
  *   double x;
  *   x;                 // lvalue
  *   double y[3][4];
  *   y;                 // not an lvalue
  *   y[1][0];           // lvalue
  *   double4 z;
  *   z;                 // lvalue
  *   double4 w[8];
  *   w;                 // not an lvalue
  *   w[3];              // lvalue
  * }}}
  *
  * @author Greg Snider
  */
private[gpu_operator]
trait LValue {
  /** Name of the lvalue (a variable name). */
  val name: String
  /** Type of the lvalue. */
  val gpuType: GPUType

  /** Assign a value to an lvalue.
    *
    * @param expr Value to be assigned to the lvalue.
    */
  def :=(expr: GPUExpression) = Assignment(this, "=", expr)
  def +=(expr: GPUExpression) = Assignment(this, "+=", expr)
  def -=(expr: GPUExpression) = Assignment(this, "-=", expr)
  def *=(expr: GPUExpression) = Assignment(this, "*=", expr)
  def %=(expr: GPUExpression) = Assignment(this, "%=", expr)
  def /=(expr: GPUExpression) = Assignment(this, "/=", expr)
  def &=(expr: GPUExpression) = Assignment(this, "&=", expr)
  def ^=(expr: GPUExpression) = Assignment(this, "^=", expr)
  def |=(expr: GPUExpression) = Assignment(this, "|=", expr)
  def <<=(expr: GPUExpression) = Assignment(this, "<<=", expr)
  def >>=(expr: GPUExpression) = Assignment(this, ">>=", expr)
}

