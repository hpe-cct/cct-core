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

package cogx.compiler.gpu_operator.types

import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types.ElementTypes.{Float32, Float64}

/** Supplies type conversion primitives.
  *
  * OpenCL requires automatic type conversions in arithmetic expressions when
  * combining scalars and vectors. For example:
  * {{{
  *    float x;
  *    float4 y;
  *    float4 sum1 = x + y;   // legal, x automatically converted
  *    float4 sum2 = y + x;   // legal, x automatically converted
  *
  * }}}
  *
  */
private[gpu_operator]
object AutomaticTypeConversions extends SemanticError {

  /** Type cast an expression to match the common type, if any, of it and
    * a second expression.
    *
    * If both expressions have the same type, no typecasting occurs. If one
    * is a scalar and the other a vector, casting is done to the vector type.
    * If both expressions are vector types but of different lengths, an
    * error has occurred.
    *
    * If both expressions are scalars, then conversion is done using the
    * "usual arithmetic conversions" as defined in the C99 spec, section
    * 6.3.1.8, as required by OpenCL. The rules are ordered, with highest
    * priority first:
    *
    * Usage:
    * {{{
    *    x:  GPUType(Float32, 1)
    *    x4: GPUType(Float32, 4)
    *    y:  GPUType(Float32, 1)
    *    y4: GPUType(Float32, 4)
    *
    *    convertType(x, y)       // => GPUType(Float32, 1)
    *    convertType(x, x4)      // => GPUType(Float32, 4)
    *    convertType(x4, x)      // => GPUType(Float32, 4)
    *    convertType(x4, y4)     // => GPUType(Float32, 4)
    * }}}
    *
    * @param type1 The type of the first expression.
    * @param type2 The type of the second expression.
    * @return A promoted type of type1 and type2 meeting the type casting
    *        rules of OpenCL.
    */
  private[gpu_operator] def convertType(type1: GPUType, type2: GPUType): GPUType = {
    if (type1.elementType != type2.elementType) {
      // If both are scalars, then the "usual arithmetic conversions" apply.
      if (type1.isScalar && type2.isScalar)
        usualArithmeticConversions(type1, type2)
      else
        incompatibleTypes(type1, type2)
    }
    if (type1.elements != type2.elements) {
      val maxElements = type1.elements max type2.elements
      val minElements = type1.elements min type2.elements
      if (minElements != 1)
        incompatibleTypes(type1, type2)
      else
        GPUType(type1.elementType, maxElements)
    } else
      type1
  }

  /** Error processing for two incompatible types.
    *
    * @param type1 First type.
    * @param type2 Second type.
    * @return Dummy type to keep Scala parser happy.
    */
  private def incompatibleTypes(type1: GPUType, type2: GPUType): GPUType = {
    error("incompatible types for automatic type casting:\n" +
            "   " + type1.toString + "\n" +
            "   " + type2.toString + "\n")
    type1
  }

  /** Perform the "usual arithmetic conversions" as defined in the C99 spec,
    * section 6.3.1.8.
    *
    * @param type1 First type.
    * @param type2 Second type.
    * @return result type
    */
  private def usualArithmeticConversions(type1: GPUType, type2: GPUType): GPUType = {
    if (type1.elementType == Float64 || type2.elementType == Float64)
      GPUType(Float64, 1)
    else if (type1.elementType == Float32 || type2.elementType == Float32)
      GPUType(Float32, 1)
    else if (type1.elementType == type2.elementType)
      type1
    else if (type1.isSignedInt == type2.isSignedInt) {
      if (type1.bits > type2.bits)
        type1
      else
        type2
    } else if (type1.isUnsignedInt && type1.bits >= type2.bits)
      type1
    else if (type2.isUnsignedInt && type2.bits >= type1.bits)
      type2
    else if (type1.isSignedInt && type1.bits > type2.bits)
      type1
    else if (type2.isSignedInt && type2.bits > type1.bits)
      type2
    else if (type1.isSignedInt)
      type1.toUnsigned
    else
      type2.toUnsigned
  }
}
