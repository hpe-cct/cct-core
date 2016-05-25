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
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ComplexBinaryRealConstOp

/** Combines both real and imaginary components of each complex element of a
  * complex field with a real constant.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param op The opcode for this op.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ComplexBinaryRealConstHyperKernel private (in: Array[VirtualFieldRegister],
                                 op: ComplexBinaryRealConstOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(op, in, resultType, addressMode)
{

  addCode("    @out0 = " + OpcodeToFunction(op) + "(read(@in0), " +
          op.const + "f);")
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ComplexBinaryRealConstHyperKernel extends HyperHelper {

  /**
   * Create a HyperKernel that combines both real and imaginary components of
   * each complex element of a complex field with a real constant.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ComplexBinaryRealConstOp, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val in0Type = in(0).fieldType
    require(isComplexField(in0Type))
    require(resultType == in0Type)
    val addressing = bestAddressMode(in, resultType)
    new ComplexBinaryRealConstHyperKernel(in, operation, resultType, addressing)
  }
}
