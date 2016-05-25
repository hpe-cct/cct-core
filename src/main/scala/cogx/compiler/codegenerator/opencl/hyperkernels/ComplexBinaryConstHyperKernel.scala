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
import cogx.cogmath.algebra.complex.Complex
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.ComplexBinaryComplexConstOpcode

/** Combines each element of a complex field with a complex constant.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param op The opcode for this op.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ComplexBinaryConstHyperKernel private (in: Array[VirtualFieldRegister],
                                 op: ComplexBinaryComplexConstOpcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(op, in, resultType, addressMode)
{

  def complexToString(c: Complex): String = {
    val s = "((float2) (" + c.real + "f, " + c.imaginary + "f))"
    s
  }

  addCode("    @out0 = " + OpcodeToFunction(op) + "(read(@in0), " +
          complexToString(op.const) + ");")
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ComplexBinaryConstHyperKernel extends HyperHelper {

  /**
   * Create a HyperKernel that combines each element of a complex field with a
   * complex constant.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ComplexBinaryComplexConstOpcode,
            resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val in0Type = in(0).fieldType
    require(isComplexField(in0Type))
    require(resultType == in0Type)
    val addressing = bestAddressMode(in, resultType)
    new ComplexBinaryConstHyperKernel(in, operation, resultType, addressing)
  }
}
