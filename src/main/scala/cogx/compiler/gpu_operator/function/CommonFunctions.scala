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

/** Built-in common functions.
  *
  * Note that thre following three functions
  * {{{
  *   _clamp
  *   _max
  *   _min
  * }}}
  * are also IntegerFunctions, so they are designed there.
  *
  * @author Greg Snider
  */
trait CommonFunctions extends SemanticError {

  // _clamp  --> see IntegerFunctions
  // _max    --> see IntegerFunctions
  // _min    --> see IntegerFunctions

  def _mix(expr1: GPUExpression, expr2: GPUExpression, expr3: GPUExpression) =
    new TernaryFloatExpression(new Operator("mix"), expr1, expr2, expr3)

  def _radians(expr1: GPUExpression) =
    new UnaryFloatExpression(new Operator("radians"), expr1)

  def _degrees(expr1: GPUExpression) =
    new UnaryFloatExpression(new Operator("degrees"), expr1)

  def _sign(expr1: GPUExpression) =
    new UnaryFloatExpression(new Operator("sign"), expr1)

}