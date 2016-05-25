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
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.CogCLFunctions
import cogx.compiler.parser.op.RealToComplexOp
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Kernel that converts a real field to a complex field by setting the
  * imaginary part to 0.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class RealToComplexHyperKernel private (in: VirtualFieldRegister,
                                 operation: Opcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode)
{

  val code = new StringBuilder
  code.append(CogCLFunctions.realToComplex)
  code append "    float input = read(@in0);\n"
  code append "    @out0 = realToComplex(input);\n"
  addCode(code.toString())

//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object RealToComplexHyperKernel extends HyperHelper {

  /**
   * Create a kernel that converts a real field to a complex field by setting the
   * imaginary part to 0.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): HyperKernel = {
    val inType = in.fieldType
    require(inType.elementType == Float32)
    val expectedResultType = toComplex(inType)
    require(resultType == expectedResultType)
    require(operation == RealToComplexOp)
    val addressing = bestAddressMode(Array(in), resultType)
    new RealToComplexHyperKernel(in, operation, resultType, addressing)
  }
}
