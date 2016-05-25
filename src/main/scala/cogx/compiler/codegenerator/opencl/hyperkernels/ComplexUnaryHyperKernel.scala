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
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.ComplexUnaryOp

/** Performs a unary (single-input) function on each element of a complex field.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ComplexUnaryHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: ComplexUnaryOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{

  addCode("    @out0 = " + OpcodeToFunction(operation) + "(read(@in0));")
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ComplexUnaryHyperKernel extends HyperHelper {

  /**
   * Create a HyperKernel that performs a unary (single-input) function on each
   * element of a complex field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ComplexUnaryOp, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val in0Type = in(0).fieldType
    require(isComplexField(in0Type))

    require(resultType == in0Type)
    val addressing = bestAddressMode(in, resultType)
    new ComplexUnaryHyperKernel(in, operation, resultType, addressing)
  }
}
