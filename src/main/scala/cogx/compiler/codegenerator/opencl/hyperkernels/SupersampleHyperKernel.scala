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
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.SupersampleOp

/** 2X supersampling (each pixel is replicated 2 times in each dimension
  * in the output image).  The approach taken here seems on the surface
  * to be wasteful: multiple threads load the same value.  But, with
  * GPU L1/L2 cacheing, the performance impact should be be minimal.
  *
  * @author Dick Carter
  *
  * @param in The virtual field register of the input to be supersampled..
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class SupersampleHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: Opcode,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode){

  val inType = in(0).fieldType
  val code = new StringBuffer
  code append setLayerRowColumn(inType, "_layer/2", "_row/2", "_column/2")
  code append "@out0 = readNonlocal(@in0);"
  addCode(code.toString)
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object SupersampleHyperKernel {

  /** Create a hyperkernel that supersamples a 2D tensor field.
    *
    * @param in The virtual field register of the input to be supersampled.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType):
    HyperKernel =
  {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(inType.dimensions >= 1 && inType.dimensions <= 3)
    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    val outputShape = inType.fieldShape.supersample
    val expectedOutputType = new FieldType(outputShape, inType.tensorShape, inType.elementType)
    require(expectedOutputType == resultType)
    require(operation == SupersampleOp)
    new SupersampleHyperKernel(in, operation, resultType, addressing)
  }
}
