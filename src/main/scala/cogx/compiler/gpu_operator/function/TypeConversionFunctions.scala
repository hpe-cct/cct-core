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
import cogx.platform.types.ElementTypes.{Uint32, Int32, Float32}

/** Explicit type conversion functions.
  *
  * @author Greg Snider
  */
trait TypeConversionFunctions extends SemanticError {
  def _convert_float(e: GPUExpression) = convert(GPUType(Float32, 1), e)
  def _convert_float2(e: GPUExpression) = convert(GPUType(Float32, 2), e)
  def _convert_float3(e: GPUExpression) = convert(GPUType(Float32, 3), e)
  def _convert_float4(e: GPUExpression) = convert(GPUType(Float32, 4), e)

  def _convert_int(e: GPUExpression) = convert(GPUType(Int32, 1), e)
  def _convert_int2(e: GPUExpression) = convert(GPUType(Int32, 2), e)
  def _convert_int3(e: GPUExpression) = convert(GPUType(Int32, 3), e)
  def _convert_int4(e: GPUExpression) = convert(GPUType(Int32, 4), e)

  def _convert_uint(e: GPUExpression) = convert(GPUType(Uint32, 1), e)
  def _convert_uint2(e: GPUExpression) = convert(GPUType(Uint32, 2), e)
  def _convert_uint3(e: GPUExpression) = convert(GPUType(Uint32, 3), e)
  def _convert_uint4(e: GPUExpression) = convert(GPUType(Uint32, 4), e)

  def _as_float(e: GPUExpression) = reinterpret(GPUType(Float32, 1), e)
  def _as_float2(e: GPUExpression) = reinterpret(GPUType(Float32, 2), e)
  def _as_float3(e: GPUExpression) = reinterpret(GPUType(Float32, 3), e)
  def _as_float4(e: GPUExpression) = reinterpret(GPUType(Float32, 4), e)

  def _as_int(e: GPUExpression) = reinterpret(GPUType(Int32, 1), e)
  def _as_int2(e: GPUExpression) = reinterpret(GPUType(Int32, 2), e)
  def _as_int3(e: GPUExpression) = reinterpret(GPUType(Int32, 3), e)
  def _as_int4(e: GPUExpression) = reinterpret(GPUType(Int32, 4), e)

  def _as_uint(e: GPUExpression) = reinterpret(GPUType(Uint32, 1), e)
  def _as_uint2(e: GPUExpression) = reinterpret(GPUType(Uint32, 2), e)
  def _as_uint3(e: GPUExpression) = reinterpret(GPUType(Uint32, 3), e)
  def _as_uint4(e: GPUExpression) = reinterpret(GPUType(Uint32, 4), e)


  /** OpenCL type conversion expression.
    *
    * Note that the long names of unsigned fundamental types, e.g.
    * "unsigned int" must be converted to the short names ("uint")
    *
    * @param toType Desired result type.
    * @param expr Expression to cast to desired result type.
    * @return The type cast expression.
    */
  private def convert(toType: GPUType, expr: GPUExpression): GPUExpression = {
    val openclName =
      if (toType.elements == 1)
        "(" + toType.toString + ")"
      else
        "convert_" + toType.toString.replaceAll("unsigned ", "u")
    new GPUExpression(new Operator(openclName), toType, Array(expr) )
  }

  /** OpenCL type reinterpretation expression.
    *
    * Note that the long names of unsigned fundamental types, e.g.
    * "unsigned int" must be converted to the short names ("uint")
    *
    * @param toType Desired result type.
    * @param expr Expression to reinterpret as desired result type.
    * @return The type reinterpratation expression.
    */
  private def reinterpret(toType: GPUType, expr: GPUExpression): GPUExpression = {
    val openclName =
      "as_" + toType.toString.replaceAll("unsigned ", "u")
    new GPUExpression(new Operator(openclName), toType, Array(expr) )
  }
}
