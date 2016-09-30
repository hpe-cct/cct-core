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
import cogx.compiler.parser.op.ConstantScalarOp
import cogx.platform.types.{FieldType, VirtualFieldRegister}

/**
  * A hyperkernel that creates a field with a real constant.
  *
  * @author Dick Carter
  *
  * @param in The (empty) array of input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ConstantHyperKernel private (in: Array[VirtualFieldRegister],
                                               operation: ConstantScalarOp,
                                               resultType: FieldType,
                                               addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  addCode(s"    @out0 = ${operation.value}f;")
}

/** Factory object for creating kernels of this type. */
private[cogx]
object ConstantHyperKernel extends HyperHelper {

  /**
   * Create a hyperkernel that initializes a field with a constant.
   *
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return The synthesized hyperkernel.
   */
  def apply(operation: ConstantScalarOp,
            resultType: FieldType): HyperKernel = {
    val in = Array[VirtualFieldRegister]()
    val addressing = bestAddressMode(in, resultType)
    new ConstantHyperKernel(in, operation, resultType, addressing)
  }
}

