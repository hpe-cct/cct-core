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

package cogx.compiler.codegenerator.opencl

/** OpenCL "defines", available to all Cog OpenCL kernels. These supplement
  * the built-in OpenCL functions. Note that we express everything as functions
  * (actually, macros), even arithmetic operations, for ease of compilation.
  *
  * Use of convert_float below did not conform to the OpenCL spec, which
  * requires convert_float4 (for example) to convert tensors of length 4.
  * The AMD compiler complained about this with error messages.
  * A workaround is to use 'select', but this requires creating a vector of
  * 1.0f's and 0.0f's of the appropriate length.  For this we use typeof().
  * Casting 0.0f to typeof(a) and 1.0f to typeof(b) has the desirable property
  * that either a or b can be a scalar and OpenCL will expand it to a vector.
  *
  * Oops.  typeof() is not supported by Intel's OpenCL implementation, so I'm
  * introducing an "#ifdef" to swap in the needed defines for AMD only.
  *
  *     -RJC
  *
  * @author Greg Snider
  */
private[cogx]
object CogCLFunctions {
  /** Functions combining real numbers to produce a real result.
    *
    * Nvidia incorrectly defines M_PI (that is only defined in the cl_khr_fp64
    * extension) while AMD does not. So we force its definition here if not
    * already defined so we don't need the cl_khr_fp64 extension.
    */
  val real =
    """|#define convert_float1(a) (convert_float(a))
       |#define signum(a, s) (fabs(convert_float##s((a) > 0.0f)) - fabs(convert_float##s((a) < 0.0f)))
       |#define greaterThan(a, b, s) fabs(convert_float##s(isgreater(a, b)))
       |#define greaterThanEqual(a, b, s) fabs(convert_float##s(isgreaterequal(a, b)))
       |#define lessThan(a, b, s) fabs(convert_float##s(isless(a, b)))
       |#define lessThanEqual(a, b, s) fabs(convert_float##s(islessequal(a, b)))
       |#define equals(a, b, s) fabs(convert_float##s(isequal(a, b)))
       |#define notEquals(a, b, s) fabs(convert_float##s(isnotequal(a, b)))
       |#define add(a, b) ((a) + (b))
       |#define subtract(a, b) ((a) - (b))
       |#define multiply(a, b) ((a) * (b))
       |#define divide(a, b) ((a) / (b))
       |#define reciprocal(a) (1.0f / (a))
       |#define sq(a) ((a) * (a))
       |#define rectify(a) (fmax((a), 0.0f))
       |#define negate(a) ((a) * -1.0f)
       |#define copy(a) (a)
       |#define threshold(a, thresh) ((float)((a) > (thresh)))
       |#define imageAdd(a, b) ((float4) ((a).x + b, (a).y + b, (a).z + b, 1.0f))
       |#define imageSubtract(a, b) ((float4) ((a).x - b, (a).y - b, (a).z - b, 1.0f))
       |#define imageMultiply(a, b) ((float4) ((a).x * b, (a).y * b, (a).z * b, 1.0f))
       |#define imageDivide(a, b) ((float4) ((a).x / b, (a).y / b, (a).z / b, 1.0f))
       |#define greaterThanFloat4(a, b) ((float4)((float)((a).x > (b).x), (float)((a).y > (b).y), (float)((a).z > (b).z), 1.0f))
       |#define greaterThanEqualFloat4(a, b) ((float4)((float)((a).x >= (b).x), (float)((a).y >= (b).y), (float)((a).z >= (b).z), 1.0f))
       |#define lessThanFloat4(a, b) ((float4)((float)((a).x < (b).x), (float)((a).y < (b).y), (float)((a).z < (b).z), 1.0f))
       |#define lessThanEqualFloat4(a, b) ((float4)((float)((a).x <= (b).x), (float)((a).y <= (b).y), (float)((a).z <= (b).z), 1.0f))
       |#define equalsFloat4(a, b) ((float4)((float)((a).x == (b).x), (float)((a).y == (b).y), (float)((a).z == (b).z), 1.0f))
       |#define notEqualsFloat4(a, b) ((float4)((float)((a).x != (b).x), (float)((a).y != (b).y), (float)((a).z != (b).z), 1.0f))
       |#define greaterThanConstFloat4(a, b) ((float4)((float)((a).x > (b)), (float)((a).y > (b)), (float)((a).z > (b)), 1.0f))
       |#define greaterThanEqualConstFloat4(a, b) ((float4)((float)((a).x >= (b)), (float)((a).y >= (b)), (float)((a).z >= (b)), 1.0f))
       |#define lessThanConstFloat4(a, b) ((float4)((float)((a).x < (b)), (float)((a).y < (b)), (float)((a).z < (b)), 1.0f))
       |#define lessThanEqualConstFloat4(a, b) ((float4)((float)((a).x <= (b)), (float)((a).y <= (b)), (float)((a).z <= (b)), 1.0f))
       |#define equalsConstFloat4(a, b) ((float4)((float)((a).x == (b)), (float)((a).y == (b)), (float)((a).z == (b)), 1.0f))
       |#define notEqualsConstFloat4(a, b) ((float4)((float)((a).x != (b)), (float)((a).y != (b)), (float)((a).z != (b)), 1.0f))
       |#ifndef M_PI
       |#define M_PI (3.14159265359)
       |#endif
       |""".stripMargin

  /** Functions combining complex numbers to produce a complex result. */
  val complex =
    """|#define complexAdd(a,b) ((float2) ((a).x + (b).x, (a).y + (b).y))
       |#define complexSubtract(a,b) ((float2) ((a).x - (b).x, (a).y - (b).y))
       |#define complexMultiply(a,b) ((float2)(mad(-(a).y, (b).y, (a).x * (b).x), mad((a).y, (b).x, (a).x * (b).y)))
       |#define complexDivide(a,b) ((float2) ((((a).x * (b).x + (a).y * (b).y) / norm(b)), (((a).y * (b).x - (a).x * (b).y) / norm(b))))
       |#define complexReciprocal(a) ((float2) ((((a).x) / norm(a)), ((-(a).y) / norm(a))) )
       |#define complexAddReal(a,b) ((float2) ((a).x + b, (a).y))
       |#define complexSubtractReal(a,b) ((float2) ((a).x - b, (a).y))
       |#define complexMultiplyReal(a,b) ((float2) ((a).x * b, (a).y * b))
       |#define complexDivideReal(a,b) ((float2) ((a).x / b, (a).y / b))
       |#define conjTransp(a) ((float2)(-(a).y, (a).x))
       |#define complexExp(a) ((float2) ( native_exp((a).x) * (float2) (native_cos((a).y),native_sin((a).y)) ))
       |#define conjugate(a) ((float2)((a).x, -(a).y))
       |""".stripMargin

  /**
   * Functions which convert complex numbers to real numbers.
   */
  val complexToReal =
    """|#define phase(a) (atan2((a).y, (a).x))
       |#define norm(a) ((a).x * (a).x + (a).y * (a).y)
       |#define magnitude(a) (native_sqrt(norm(a)))
       |#define realPart(a) ((a).x)
       |#define imaginaryPart(a) ((a).y)
       |""".stripMargin

  /** Functions which convert real numbers to complex numbers. */
  val realToComplex =
    """|#define realToComplex(a) ((float2) ((a), 0.0f))
       |""".stripMargin
}