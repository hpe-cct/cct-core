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

import cogx.compiler.CompilerError
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Fields may be addressed by sequencing through tensors in the field, or
  * through all numbers in all tensors in the field. This defines the
  * addressing policy.
  *
  * @author Greg Snider
  */
private[cogx]
sealed abstract class AddressingMode {
  /** Return the appropriate OpenCL type for the type of the field and
    * the addressing mode.  This is used to tell you the return type
    * of the read* operations.
    *
    * @param fieldType The type of the field.
    * @return OpenCL type for the tensor.
    */
  def clType(fieldType: FieldType): CLType
}

/** Address each point in the field, treating each element at each point as
  * a single object.
  *
  * This is an optimization that applies only if the tensors are sufficiently
  * small: 1, 2, 3, or 4 floats.
  *
  */
private[cogx]
case object SmallTensorAddressing extends AddressingMode with CompilerError {

  /** Return the appropriate OpenCL type for representing a tensor as an array.
    * For example, a geometrical vector in a 3D field would be represented as
    * "float3" in OpenCL.
    *
    * @param fieldType The type of the field.
    * @return OpenCL type for the tensor.
    */
  def clType(fieldType: FieldType): CLType = {
    fieldType.elementType match {
      case Float32 =>
        fieldType.tensorShape.points match {
          case 16 => if (isSmallTensorField(fieldType)) CLFloat16 else CLFloatN(16)
          case 8 => if (isSmallTensorField(fieldType)) CLFloat8 else CLFloatN(8)
          case 4 => CLFloat4
          case 3 => CLFloat3
          case 2 => CLFloat2
          case 1 => CLFloat
          // Following is OK, but kernel better not be writing a variable with
          // this type.
          case _ => CLFloat
        }
      case Complex32 =>
        fieldType.tensorShape.points match {
          case 1 => CLComplex
          // Following is OK, but kernel better not be writing a variable with
          // this type.
          case _ => CLComplex
        }
      case Uint8Pixel =>
        CLPixel
      case x =>
        internalError("Unsupported Element type: " + x)
        null
    }
  }
}


/** Address each point in the field, where the tensors in the field are too
  * large (more than 4 floats) for SmallTensorAddressing.
  */
private[cogx]
case object BigTensorAddressing extends AddressingMode with CompilerError {
  def clType(fieldType: FieldType): CLType = {
    fieldType.elementType match {
      case Float32 =>
        CLFloat
      case Complex32 =>
        CLComplex
      case Uint8Pixel =>
        CLPixel
      case x =>
        internalError("Unsupported Element type: " + x)
        null
    }
  }
}

/** Address each number within each tensor in the field. */
private[cogx]
case object TensorElementAddressing extends AddressingMode with CompilerError {
  def clType(fieldType: FieldType): CLType = {
    fieldType.elementType match {
      case Float32 =>
        CLFloat
      case Complex32 =>
        CLComplex
      case Uint8Pixel =>
        CLPixel
      case x =>
        internalError("Unsupported Element type: " + x)
        null
    }
  }
}

