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
import cogx.compiler.gpu_operator.expression.{UnaryFloatExpression, Operator, BinaryFloatExpression, GPUExpression}

/** Built-in geometric functions.
  *
  * @author Greg Snider
  */
trait GeometricFunctions extends SemanticError {
  def _dot(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("dot"), expr1, expr2)

  def _distance(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("distance"), expr1, expr2)

  def _length(expr1: GPUExpression) =
    new UnaryFloatExpression(new Operator("length"), expr1)

  def _normalize(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("normalize"), expr)

  def _fast_distance(expr1: GPUExpression, expr2: GPUExpression) =
    new BinaryFloatExpression(new Operator("fast_distance"), expr1, expr2)

  def _fast_length(expr1: GPUExpression) =
    new UnaryFloatExpression(new Operator("fast_length"), expr1)

  def _fast_normalize(expr: GPUExpression) =
    new UnaryFloatExpression(new Operator("fast_normalize"), expr)
}

