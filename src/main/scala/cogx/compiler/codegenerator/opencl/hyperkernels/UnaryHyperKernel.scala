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

import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.opencl.fragments.{HyperKernel, AddressingMode}
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.UnaryOpcode

/** Applies a unary operator on a field.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The UnaryOp for this operation, with its function.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class UnaryHyperKernel private (in: Array[VirtualFieldRegister],
                                operation: UnaryOpcode,
                                resultType: FieldType,
                                addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {
  addCode("    @out0 = " + OpcodeToFunction(operation) + "(read(@in0));")
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object UnaryHyperKernel extends HyperHelper {

  /**
   * Create a Hyperkernel that applies a unary operator to a field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: UnaryOpcode, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    require(in(0).fieldType == resultType)
    val addressing = bestAddressMode(in, resultType)
    new UnaryHyperKernel(in, operation, resultType, addressing)
  }
}
