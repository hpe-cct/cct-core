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

package cogx.compiler.gpu_operator.function

import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.gpu_operator.expression._

/** Built-in integer functions.
  *
  * @author Greg Snider
  */
trait IntegerFunctions extends SemanticError {
  def _abs(expr: GPUExpression) =
    new UnaryIntExpression(new Operator("abs"), expr)

  def _abs_diff(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("abs"), expr1, expr2)

  def _add_sat(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("add_sat"), expr1, expr2)

  def _hadd(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("hadd"), expr1, expr2)

  def _rhadd(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("rhadd"), expr1, expr2)

  def _clz(expr: GPUExpression) =
    new UnaryIntExpression(new Operator("clz"), expr)

  // Clamp is both an integer function and a common (float) function
  def _clamp(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    if (expr1.gpuType.isInt)
      new TernaryIntExpression(new Operator("clamp"), expr1, expr2, expr3)
    else
      new TernaryFloatExpression(new Operator("clamp"), expr1, expr2, expr3)

  def _mad_hi(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryIntExpression(new Operator("mad_hi"), expr1, expr2, expr3)

  def _mad_sat(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryIntExpression(new Operator("mad_sat"), expr1, expr2, expr3)

  // Max is both an integer function and a common (float) function
  def _max(expr1: GPUExpression, expr2: GPUExpression) =
    if (expr1.gpuType.isInt)
      new BinaryIntExpression(new Operator("max"), expr1, expr2)
    else
      new BinaryFloatExpression(new Operator("max"), expr1, expr2)

  // Max is both an integer function and a common (float) function
  def _min(expr1: GPUExpression, expr2: GPUExpression) =
    if (expr1.gpuType.isInt)
      new BinaryIntExpression(new Operator("min"), expr1, expr2)
    else
      new BinaryFloatExpression(new Operator("min"), expr1, expr2)

  def _mul_hi(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("mul_hi"), expr1, expr2)

  def _rotate(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("rotate"), expr1, expr2)

  def _sub_sat(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("sub_sat"), expr1, expr2)

  def _mad24(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryIntExpression(new Operator("mad24"), expr1, expr2, expr3)

  def _mul24(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryIntExpression(new Operator("mul24"), expr1, expr2)
}