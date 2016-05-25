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
import cogx.compiler.gpu_operator.expression.{UnaryRelationalExpression, Operator, BinaryRelationalExpression, GPUExpression}

/** Built-in relational functions.
  *
  * @author Greg Snider
  */
trait RelationalFunctions extends SemanticError {
  def _isequal(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isequal"), expr1, expr2)

  def _isnotequal(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isnotequal"), expr1, expr2)

  def _isgreater(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isgreater"), expr1, expr2)

  def _isgreaterequal(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isgreaterequal"), expr1, expr2)

  def _isless(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isless"), expr1, expr2)

  def _islessequal(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("islessequal"), expr1, expr2)

  def _islessgreater(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("islessgreater"), expr1, expr2)

  def _isinfinite(expr: GPUExpression) =
    new UnaryRelationalExpression(new Operator("isinfinite"), expr)

  def _isinf(expr: GPUExpression) =
    new UnaryRelationalExpression(new Operator("isinf"), expr)

  def _isnan(expr: GPUExpression) =
    new UnaryRelationalExpression(new Operator("isnan"), expr)

  def _isnormal(expr: GPUExpression) =
    new UnaryRelationalExpression(new Operator("isnormal"), expr)

  def _isordered(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isordered"), expr1, expr2)

  def _isunordered(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryRelationalExpression(new Operator("isunordered"), expr1, expr2)
}
