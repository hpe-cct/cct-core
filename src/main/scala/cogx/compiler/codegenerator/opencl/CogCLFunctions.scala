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
      "#ifdef _AMD_OPENCL\n" +
        "  #define signum(a) (select((typeof(a)) 0.0f, 1.0f, (a) > 0.0f) - select((typeof(a)) 0.0f, 1.0f, (a) < 0.0f))\n" +
        "  #define greaterThan(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, isgreater(a, b))\n" +
        "  #define greaterThanEqual(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, isgreaterequal(a, b))\n" +
        "  #define lessThan(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, isless(a, b))\n" +
        "  #define lessThanEqual(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, islessequal(a, b))\n" +
        "  #define equals(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, isequal(a, b))\n" +
        "  #define notEquals(a, b) select((typeof(a)) 0.0f, (typeof(b)) 1.0f, isnotequal(a, b))\n" +
      "#else\n" +
        "  #define signum(a) (fabs(convert_float((a) > 0.0f)) - fabs(convert_float((a) < 0.0f)))\n" +
        "  #define greaterThan(a, b) fabs(convert_float(isgreater(a, b)))\n" +
        "  #define greaterThanEqual(a, b) fabs(convert_float(isgreaterequal(a, b)))\n" +
        "  #define lessThan(a, b) fabs(convert_float(isless(a, b)))\n" +
        "  #define lessThanEqual(a, b) fabs(convert_float(islessequal(a, b)))\n" +
        "  #define equals(a, b) fabs(convert_float(isequal(a, b)))\n" +
        "  #define notEquals(a, b) fabs(convert_float(isnotequal(a, b)))\n" +
      "#endif /* _AMD_OPENCL */\n" +
      "#define add(a, b) ((a) + (b))\n" +
      "#define subtract(a, b) ((a) - (b))\n" +
      "#define multiply(a, b) ((a) * (b))\n" +
      "#define divide(a, b) ((a) / (b))\n" +
      "#define reciprocal(a) (1.0f / (a))\n" +
      "#define sq(a) ((a) * (a))\n" +
      "#define rectify(a) (fmax((a), 0.0f))\n" +
      "#define negate(a) ((a) * -1.0f)\n" +
      "#define copy(a) (a)\n" +
      "#define threshold(a, thresh) ((float)((a) > (thresh)))\n" +
      "#define imageAdd(a, b) ((float4) ((a).x + b, (a).y + b, (a).z + b, 1.0f))\n" +
      "#define imageSubtract(a, b) ((float4) ((a).x - b, (a).y - b, (a).z - b, 1.0f))\n" +
      "#define imageMultiply(a, b) ((float4) ((a).x * b, (a).y * b, (a).z * b, 1.0f))\n" +
      "#define imageDivide(a, b) ((float4) ((a).x / b, (a).y / b, (a).z / b, 1.0f))\n" +
      "#define greaterThanFloat4(a, b) ((float4)((float)((a).x > (b).x), (float)((a).y > (b).y), (float)((a).z > (b).z), 1.0f))\n" +
      "#define greaterThanEqualFloat4(a, b) ((float4)((float)((a).x >= (b).x), (float)((a).y >= (b).y), (float)((a).z >= (b).z), 1.0f))\n" +
      "#define lessThanFloat4(a, b) ((float4)((float)((a).x < (b).x), (float)((a).y < (b).y), (float)((a).z < (b).z), 1.0f))\n" +
      "#define lessThanEqualFloat4(a, b) ((float4)((float)((a).x <= (b).x), (float)((a).y <= (b).y), (float)((a).z <= (b).z), 1.0f))\n" +
      "#define equalsFloat4(a, b) ((float4)((float)((a).x == (b).x), (float)((a).y == (b).y), (float)((a).z == (b).z), 1.0f))\n" +
      "#define notEqualsFloat4(a, b) ((float4)((float)((a).x != (b).x), (float)((a).y != (b).y), (float)((a).z != (b).z), 1.0f))\n" +
      "#define greaterThanConstFloat4(a, b) ((float4)((float)((a).x > (b)), (float)((a).y > (b)), (float)((a).z > (b)), 1.0f))\n" +
      "#define greaterThanEqualConstFloat4(a, b) ((float4)((float)((a).x >= (b)), (float)((a).y >= (b)), (float)((a).z >= (b)), 1.0f))\n" +
      "#define lessThanConstFloat4(a, b) ((float4)((float)((a).x < (b)), (float)((a).y < (b)), (float)((a).z < (b)), 1.0f))\n" +
      "#define lessThanEqualConstFloat4(a, b) ((float4)((float)((a).x <= (b)), (float)((a).y <= (b)), (float)((a).z <= (b)), 1.0f))\n" +
      "#define equalsConstFloat4(a, b) ((float4)((float)((a).x == (b)), (float)((a).y == (b)), (float)((a).z == (b)), 1.0f))\n" +
      "#define notEqualsConstFloat4(a, b) ((float4)((float)((a).x != (b)), (float)((a).y != (b)), (float)((a).z != (b)), 1.0f))\n" +
      "#ifndef M_PI\n" +
      "#define M_PI (3.14159265359)\n" +
      "#endif\n"


  /** Functions combining complex numbers to produce a complex result. */
  val complex =
      "#define complexAdd(a,b) ((float2) ((a).x + (b).x, (a).y + (b).y))\n" +
      "#define complexSubtract(a,b) ((float2) ((a).x - (b).x, (a).y - (b).y))\n" +
      "#define complexMultiply(a,b) ((float2)(mad(-(a).y, (b).y, (a).x * (b).x), mad((a).y, (b).x, (a).x * (b).y)))\n" +
      "#define complexDivide(a,b) ((float2) " +
        "((((a).x * (b).x + (a).y * (b).y) / norm(b)), (((a).y * (b).x - (a).x * (b).y) / norm(b))))\n" +
      "#define complexReciprocal(a) ((float2) ((((a).x) / norm(a)), ((-(a).y) / norm(a))) )\n" +
      "#define complexAddReal(a,b) ((float2) ((a).x + b, (a).y))\n" +
      "#define complexSubtractReal(a,b) ((float2) ((a).x - b, (a).y))\n" +
      "#define complexMultiplyReal(a,b) ((float2) ((a).x * b, (a).y * b))\n" +
      "#define complexDivideReal(a,b) ((float2) ((a).x / b, (a).y / b))\n" +
      "#define conjTransp(a) ((float2)(-(a).y, (a).x))\n" +
      "#define complexExp(a) ((float2) ( native_exp((a).x) * " +
                                        "(float2) (native_cos((a).y),native_sin((a).y)) ))\n" +
      "#define conjugate(a) ((float2)((a).x, -(a).y))\n"

  /**
   * Functions which convert complex numbers to real numbers.
   */
  val complexToReal =
      "#define phase(a) (atan2((a).y, (a).x))\n" +
      "#define norm(a) ((a).x * (a).x + (a).y * (a).y)\n" +
      "#define magnitude(a) (native_sqrt(norm(a)))\n" +
      "#define realPart(a) ((a).x)\n" +
      "#define imaginaryPart(a) ((a).y)\n"

  /** Functions which convert real numbers to complex numbers. */
  val realToComplex =
      "#define realToComplex(a) ((float2) ((a), 0.0f))\n"
}