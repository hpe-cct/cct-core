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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, DontCare, AddressingMode, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import HyperKernel._
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.BinaryOpcode

/** An ImagePointKernel transforms one or two images of the same size,
  * producing another image also of the same size. All computation is done on a
  * point-by-point basis. But the second image is allowed to have a single pixel,
  * in which case the single point of the second image is combined with each
  * point of the first.</p>
  *
  * Since this kernel never references outside of any input field, the sampling
  * border handling policy is DontCare.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ColorBinaryHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: BinaryOpcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode, DontCare)
{
  val code = new StringBuilder
  val secondFieldIs0D = in(1).fieldType.fieldShape.points == 1

  // Do the math
  if (secondFieldIs0D) {
    setLayerRowColumn(in(1).fieldType, "0", "0", "0")
    code.append("    @out0 = " + OpcodeToFunction(operation) +
            "(read(@in0), readNonlocal(@in1));\n")
  }
  else
    code.append("    @out0 = " + OpcodeToFunction(operation) +
            "(read(@in0), read(@in1));\n")
  addCode(code.toString())
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ColorBinaryHyperKernel {

  /**
   * Create an image kernel that transforms one or two images of the same size,
   * producing another image also of the same size.
   *
   * @param in The two input virtual field registers driving this kernel.
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: BinaryOpcode,
            resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    val in0Type = in(0).fieldType
    val in1Type = in(1).fieldType
    val secondFieldIs0D = in1Type.fieldShape.points == 1
    if (!secondFieldIs0D)
      require(in0Type.fieldShape == in1Type.fieldShape)
    require(resultType == in0Type)
    val addressing = SmallTensorAddressing
    new ColorBinaryHyperKernel(in, operation, resultType, addressing)
  }
}
