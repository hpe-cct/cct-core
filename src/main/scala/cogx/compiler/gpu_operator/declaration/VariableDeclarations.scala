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

import cogx.platform.types.ElementTypes._
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.types.GPUType

/** Functions for declaring variables in the GPU kernel.
  *
  * @author Greg Snider
  */
private[gpu_operator]
trait VariableDeclarations {

  // Vector / scalar declarations:

  def _charVar() = GPUVariable(GPUType(Int8, 1))
  def _char2Var() = GPUVariable(GPUType(Int8, 2))
  def _char3Var() = GPUVariable(GPUType(Int8, 3))
  def _char4Var() = GPUVariable(GPUType(Int8, 4))

  def _ucharVar() = GPUVariable(GPUType(Uint8, 1))
  def _uchar2Var() = GPUVariable(GPUType(Uint8, 2))
  def _uchar3Var() = GPUVariable(GPUType(Uint8, 3))
  def _uchar4Var() = GPUVariable(GPUType(Uint8, 4))

  def _shortVar() = GPUVariable(GPUType(Int16, 1))
  def _short2Var() = GPUVariable(GPUType(Int16, 2))
  def _short3Var() = GPUVariable(GPUType(Int16, 3))
  def _short4Var() = GPUVariable(GPUType(Int16, 4))

  def _ushortVar() = GPUVariable(GPUType(Uint16, 1))
  def _ushort2Var() = GPUVariable(GPUType(Uint16, 2))
  def _ushort3Var() = GPUVariable(GPUType(Uint16, 3))
  def _ushort4Var() = GPUVariable(GPUType(Uint16, 4))

  def _intVar() = GPUVariable(GPUType(Int32, 1))
  def _int2Var() = GPUVariable(GPUType(Int32, 2))
  def _int3Var() = GPUVariable(GPUType(Int32, 3))
  def _int4Var() = GPUVariable(GPUType(Int32, 4))

  def _uintVar() = GPUVariable(GPUType(Uint32, 1))
  def _uint2Var() = GPUVariable(GPUType(Uint32, 2))
  def _uint3Var() = GPUVariable(GPUType(Uint32, 3))
  def _uint4Var() = GPUVariable(GPUType(Uint32, 4))

  def _longVar() = GPUVariable(GPUType(Int64, 1))
  def _long2Var() = GPUVariable(GPUType(Int64, 2))
  def _long3Var() = GPUVariable(GPUType(Int64, 3))
  def _long4Var() = GPUVariable(GPUType(Int64, 4))

  def _ulongVar() = GPUVariable(GPUType(Uint64, 1))
  def _ulong2Var() = GPUVariable(GPUType(Uint64, 2))
  def _ulong3Var() = GPUVariable(GPUType(Uint64, 3))
  def _ulong4Var() = GPUVariable(GPUType(Uint64, 4))

  def _halfVar() = GPUVariable(GPUType(Float16, 1))
  def _half2Var() = GPUVariable(GPUType(Float16, 2))
  def _half3Var() = GPUVariable(GPUType(Float16, 3))
  def _half4Var() = GPUVariable(GPUType(Float16, 4))

  def _floatVar() = GPUVariable(GPUType(Float32, 1))
  def _float2Var() = GPUVariable(GPUType(Float32, 2))
  def _float3Var() = GPUVariable(GPUType(Float32, 3))
  def _float4Var() = GPUVariable(GPUType(Float32, 4))

  def _doubleVar() = GPUVariable(GPUType(Float64, 1))
  def _double2Var() = GPUVariable(GPUType(Float64, 2))
  def _double3Var() = GPUVariable(GPUType(Float64, 3))
  def _double4Var() = GPUVariable(GPUType(Float64, 4))

  // Array declarations:

  def _charArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int8, 1), size.toArray)
  def _char2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int8, 2), size.toArray)
  def _char3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int8, 3), size.toArray)
  def _char4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int8, 4), size.toArray)

  def _ucharArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint8, 1), size.toArray)
  def _uchar2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint8, 2), size.toArray)
  def _uchar3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint8, 3), size.toArray)
  def _uchar4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint8, 4), size.toArray)

  def _shortArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int16, 1), size.toArray)
  def _short2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int16, 2), size.toArray)
  def _short3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int16, 3), size.toArray)
  def _short4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int16, 4), size.toArray)

  def _ushortArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint16, 1), size.toArray)
  def _ushort2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint16, 2), size.toArray)
  def _ushort3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint16, 3), size.toArray)
  def _ushort4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint16, 4), size.toArray)

  def _intArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int32, 1), size.toArray)
  def _int2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int32, 2), size.toArray)
  def _int3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int32, 3), size.toArray)
  def _int4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int32, 4), size.toArray)

  def _uintArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint32, 1), size.toArray)
  def _uint2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint32, 2), size.toArray)
  def _uint3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint32, 3), size.toArray)
  def _uint4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint32, 4), size.toArray)

  def _longArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int64, 1), size.toArray)
  def _long2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int64, 2), size.toArray)
  def _long3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int64, 3), size.toArray)
  def _long4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Int64, 4), size.toArray)

  def _ulongArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint64, 1), size.toArray)
  def _ulong2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint64, 2), size.toArray)
  def _ulong3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint64, 3), size.toArray)
  def _ulong4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Uint64, 4), size.toArray)

  def _halfArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float16, 1), size.toArray)
  def _half2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float16, 2), size.toArray)
  def _half3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float16, 3), size.toArray)
  def _half4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float16, 4), size.toArray)

  def _floatArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float32, 1), size.toArray)
  def _float2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float32, 2), size.toArray)
  def _float3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float32, 3), size.toArray)
  def _float4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float32, 4), size.toArray)

  def _doubleArray(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float64, 1), size.toArray)
  def _double2Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float64, 2), size.toArray)
  def _double3Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float64, 3), size.toArray)
  def _double4Array(size: GPUExpression*) = new GPUArrayVariable(GPUType(Float64, 4), size.toArray)

  // Constant array declarations:
  // These have been disabled since constant arrays may only be declared
  // at program scope and we don't yet have a mechanism for doing that.
  /*
  def _constantFloatArray(values: Float*) =
    new GPUArrayConstant(Array(values.toArray.length), values.toArray)
  def _constantFloatArray(values: Array[Float]) =
    new GPUArrayConstant(Array(values.length), values)
  def _constantFloatArray(values: Array[Array[Float]]) =
    new GPUArrayConstant(Array(values.length, values(0).length), values)
  def _constantFloatArray(values: Matrix) =
    new GPUArrayConstant(values)
    */
}