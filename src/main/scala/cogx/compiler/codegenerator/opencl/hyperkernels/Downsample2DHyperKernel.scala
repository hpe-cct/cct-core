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
import cogx.compiler.parser.op.DownsampleOp

/** Downsamples a tensor field by extracting the field tensors at even rows and
  * even columns.
  *
  * This will only work on tensor fields and color fields.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field register to be downsampled as a length-1 Array.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class Downsample2DHyperKernel private (in: Array[VirtualFieldRegister],
                                       operation: DownsampleOp,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{

  val inType = in(0).fieldType
  val fs = operation.factor.toString
  val ps = operation.phase.toString

  val code = new StringBuffer
  code append setLayerRowColumn(inType, "_layer*" +fs+ " + " +ps , "_row*" +fs+ " + " +ps, "_column*" +fs+ " + " +ps)
  code append "const int tensorElement = _tensorElement;"
  code append "@out0 = readNonlocal(@in0);"
  addCode(code.toString)
  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Downsample2DHyperKernel {

  /** Create a hyperkernel that downsamples a 2D tensor field.
    *
    * @param in The input virtual field register to be downsampled as a length-1 Array.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: DownsampleOp, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val inType = in(0).fieldType

    require(inType.dimensions >= 1 && inType.dimensions <= 3)

    require(operation.factor > 1, "downsample factor must be > 1")
    require(operation.factor > operation.phase && operation.phase >= 0,
      "phase must be >=0 and < factor" )

    //Ensure that the phase shift does not spill over the edge of the border of the field
    val inSizes = in(0).fieldType.fieldShape.toArray
    val remainders = inSizes.map(x=>  (x-1) % operation.factor)
    require(remainders.map(_ >= operation.phase).reduce(_&&_),
      "phase overlaps the boundary of the field")

    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new Downsample2DHyperKernel(in, operation, resultType, addressing)
  }
}

