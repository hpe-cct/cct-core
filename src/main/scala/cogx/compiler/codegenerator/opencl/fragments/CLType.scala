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

package cogx.compiler.codegenerator.opencl.fragments

/** OpenCL has support for various scalar and vector floating point types,
  * enumerated here.
  *
  * For a given CLType, it's often useful in HyperKernel code generation to
  * know a string representation of the type (i.e. 'name') and a string
  * representation of the zero of the type (i.e. 'zero').
  *
  * CLPixel is used to represent the Cog-standard representation of 2D and 3D
  * color images: RGBA with floats.  Note that the 'zero' of CLPixel has
  * the alpha channel set to 1.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class CLType(val name: String, val zero: String) {
  override def toString = name
}

private[cogx]
case object CLFloat extends CLType("float", "0.0f")

private[cogx]
case object CLFloat2 extends CLType("float2", "(float2) (0.0f, 0.0f)")

private[cogx]
case object CLFloat3 extends CLType("float3", "(float3) (0.0f, 0.0f, 0.0f)")

private[cogx]
case object CLFloat4 extends CLType("float4", "(float4) (0.0f, 0.0f, 0.0f, 0.0f)")

private[cogx]
case object CLFloat8 extends CLType("float8", "(float8) (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f)")

private[cogx]
case object CLFloat16 extends CLType("float16", "(float16) (0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f)")

private[cogx]
case class CLFloatN(n: Int) extends CLType("float" + n, "(float" + n + ")" +
        "(" + ("0.0f, " * n).dropRight(2) + ")")

// Old form of CLFloat2, CLFloat3 and CLFloat4 before CLFloatN(n:Int) existed

//object CLFloat2 extends CLType("float2", "(float2) (0.0f, 0.0f)")
//object CLFloat3 extends CLType("float3", "(float3) (0.0f, 0.0f, 0.0f)")
//object CLFloat4 extends CLType("float4", "(float4) (0.0f, 0.0f, 0.0f, 0.0f)")

private[cogx]
case object CLInt extends CLType("int", "0")

private[cogx]
case object CLInt2 extends CLType("int2", "(int2) (0,0)")

private[cogx]
case object CLInt3 extends CLType("int3", "(int3) (0,0,0)")

private[cogx]
case object CLInt4 extends CLType("int4", "(int4) (0,0,0,0)")

private[cogx]
case object CLPixel extends CLType("float4", "(float4) (0.0f, 0.0f, 0.0f, 1.0f)")

private[cogx]
case object CLComplex extends CLType("float2", "(float2) (0.0f, 0.0f)")

