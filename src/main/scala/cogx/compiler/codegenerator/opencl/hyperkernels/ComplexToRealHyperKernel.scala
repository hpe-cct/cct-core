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

package cogx.compiler.codegenerator.opencl.hyperkernels

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op._

/** Calculate a real property (e.g. realPart, imaginaryPart, magnitude)
  * of a ComplexField
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel, in a length-1 array.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ComplexToRealHyperKernel private (in: Array[VirtualFieldRegister],
                                operation: Opcode,
                                resultType: FieldType,
                                addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  val code = new StringBuffer
  code.append("    float2 input = read(@in0);\n")
  operation match {
    case RealPartOp =>
      code.append("        float  output = realPart(input);\n")
    case ImaginaryPartOp =>
      code.append("        float  output = imaginaryPart(input);\n")
    case OrientationOp =>
      code.append("        float  output = phase(input);\n")
      code.append("        int tooSmall = (output <= -(convert_float(M_PI) / 2.0f));\n")
      code.append("        int tooBig = (output > (convert_float(M_PI) / 2.0f));\n")
      code.append("        output += tooSmall * convert_float(M_PI) - tooBig * convert_float(M_PI);\n")
    case PhaseOp =>
      code.append("        float  output = phase(input);\n")
    case MagnitudeOp =>
      code.append("        float  output = magnitude(input);\n")
  }
  code.append("        @out0 = output;")
  addCode(code.toString)
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ComplexToRealHyperKernel extends HyperHelper {

  /** Calculate a real property of a ComplexField.
    *
    * @param in The input virtual field register driving this kernel, in a length-1 array.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(isComplexField(inType))
    require(toReal(inType) == resultType)

    val addressing = bestAddressMode(in, resultType)

    new ComplexToRealHyperKernel(in, operation, resultType, addressing)
  }
}

