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
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.FilterNaNOp

/** Filters NaN values from the input.
  *
  * @author Ben Chandler
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for the operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressing The addressing mode of this kernel.
  */
private[cogx]
class NaNHyperKernel private (in: Array[VirtualFieldRegister],
                              operation: Opcode,
                              resultType: FieldType,
                              addressing: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressing) {

  val code = new StringBuffer
  code append "   float in = read(@in0);"
  code append "   @out0 = isnan(in) ? 0.0f : in;"
  addCode(code.toString)
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object NaNHyperKernel extends HyperHelper {
  /** Create a HyperKernel to Filter NaN values from a field.
    *
    * @param in The virtual field register field driving this kernel.
    * @param operation The opcode for the operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    require(in(0).fieldType == resultType)
    require(operation == FilterNaNOp)
    val addressing = bestAddressMode(in, resultType)
    new NaNHyperKernel(in, operation, resultType, addressing)
  }
}

