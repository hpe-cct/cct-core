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
import cogx.compiler.gpu_operator.expression.{Operator, GPUExpression}
import cogx.compiler.gpu_operator.types.GPUType
import cogx.platform.types.ElementTypes.{Int32, Float32}

/** Vector literal construction functions. These follow the rules of OpenCL
  * which allow mixing of vector sizes to produce the result, plus allow
  * replication when the input is a single scalar.
  *
  * @author Greg Snider
  */
trait VectorLiteralFunctions extends SemanticError {
  def _float2(e1: GPUExpression) =
    vectorLiteral(GPUType(Float32, 2), Array(e1))

  def _float2(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Float32, 2), Array(e1, e2))

  def _float3(e1: GPUExpression) =
    vectorLiteral(GPUType(Float32, 3), Array(e1))

  def _float3(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Float32, 3), Array(e1, e2))

  def _float3(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression) =
    vectorLiteral(GPUType(Float32, 3), Array(e1, e2, e3))

  def _float4(e1: GPUExpression) =
    vectorLiteral(GPUType(Float32, 4), Array(e1))

  def _float4(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Float32, 4), Array(e1, e2))

  def _float4(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression) =
    vectorLiteral(GPUType(Float32, 4), Array(e1, e2, e3))

  def _float4(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression, e4: GPUExpression) =
    vectorLiteral(GPUType(Float32, 4), Array(e1, e2, e3, e4))

  def _int2(e1: GPUExpression) =
    vectorLiteral(GPUType(Int32, 2), Array(e1))

  def _int2(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Int32, 2), Array(e1, e2))

  def _int3(e1: GPUExpression) =
    vectorLiteral(GPUType(Int32, 3), Array(e1))

  def _int3(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Int32, 3), Array(e1, e2))

  def _int3(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression) =
    vectorLiteral(GPUType(Int32, 3), Array(e1, e2, e3))

  def _int4(e1: GPUExpression) =
    vectorLiteral(GPUType(Int32, 4), Array(e1))

  def _int4(e1: GPUExpression, e2: GPUExpression) =
    vectorLiteral(GPUType(Int32, 4), Array(e1, e2))

  def _int4(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression) =
    vectorLiteral(GPUType(Int32, 4), Array(e1, e2, e3))

  def _int4(e1: GPUExpression, e2: GPUExpression, e3: GPUExpression, e4: GPUExpression) =
    vectorLiteral(GPUType(Int32, 4), Array(e1, e2, e3, e4))

  /** OpenCL type vector literal construction.
    *
    * @param literalType Desired result type.
    * @param expr Expression to cast to desired result type.
    * @return The type cast expression.
    */
  private def vectorLiteral(literalType: GPUType,
                            expr: Array[GPUExpression]): GPUExpression =
  {
    val openclName = "(" + literalType.toString + ")"
    val outputElements = literalType.elements
    val inputElements = expr.map(_.gpuType.elements).foldLeft(0)(_ + _)
    if (outputElements != inputElements) {
      // If input elements is 1 (implying a scalar), then OpenCL requires
      // replication and this is legal.
      if (inputElements > 1)
        error("wrong number of vector literal components for desired result type")
    }
    new GPUExpression(new Operator(openclName), literalType, expr)
  }
}