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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.UpsampleOp

/** Upsamples a 2D tensor field by inserting zeroes between all rows and
  * columns.
  *
  * This will only work on tensor fields and color fields.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class Upsample2DHyperKernel private (in: Array[VirtualFieldRegister],
                                     operation: UpsampleOp,
                                     resultType: FieldType,
                                     addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val inType = in(0).fieldType
  val fs = operation.factor.toString
  val ps = operation.phase.toString

  val code = new StringBuffer
  code append setLayerRowColumn(inType, "_layer/"+fs, "_row/"+fs, "_column/"+fs)
  code append "const int tensorElement = _tensorElement;\n"

  code append "    @out0 = "
  if (inType.dimensions > 0){
    code append "(((_column -" +ps+ ") % " +fs+ " != 0)"
    if (inType.dimensions > 1)
      code append " || ((_row - " +ps+ ") % " +fs+ " != 0)"
    if (inType.dimensions > 2)
      code append " || ((_layer - " +ps+ ")% " +fs+ " != 0)"
  }
  code append ") ? "
  code append addressing.clType(fieldType).zero + " : "
  code append "readNonlocal(@in0);\n"
  addCode(code.toString)
  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Upsample2DHyperKernel {

  /** Create a hyperkernel that upsamples a 2D tensor field.
    *
    * @param in The input virtual field register to be upsampled.
    * @param operation The UpsampleOp for this operation, with factor and
    *                  phase parameters.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: UpsampleOp, resultType: FieldType):
    HyperKernel =
  {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(inType.dimensions >= 1 && inType.dimensions <= 3)
    val expectedOutputType = inType.upsample(operation.factor)
    require(expectedOutputType == resultType)

    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new Upsample2DHyperKernel(in, operation, resultType, addressing)
  }
}
