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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, TensorElementAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.SliceOp

/** The SliceKernel takes an N-dimensional field and a constant integer slicing
  * index and returns an N-1 dimensional field.  The result field is the subset
  * of the input elements that have the first coordinate equal to the slice
  * index.  Works on any type of input field (scalar, vector, matrix, etc.) of
  * dimensions >= 1.</p>
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The SliceOp opcode
  * @param resultType The type of the resulting vector field.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class SliceHyperKernel private (in: VirtualFieldRegister,
                                       operation: SliceOp,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode) {

  val code = new StringBuilder

  val inType = in.fieldType
  val inDim = inType.dimensions
  val firstDimensionSize = inType.fieldShape(0)

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  code append "    const int index = " + operation.index + ";\n"

  inDim match {
    case 1 =>
      code append setLayerRowColumn(inType, "0", "0", "index")
    case 2 =>
      code append setLayerRowColumn(inType, "0", "index", "_column")
    case 3 =>
      code append setLayerRowColumn(inType, "index", "_row", "_column")
    case _ =>
      throw new RuntimeException("Unsupported input field dimension: " + inDim)
  }
  code append "    @out0 = readNonlocal(@in0);\n"
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object SliceHyperKernel {

  /**
   * Create a hyperkernel that slices an N-dimensional field to form an N-1
   * dimensional field, based on an index provided by a 0D scalar field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The SliceOp opcode
   * @param resultType The type of the resulting vector field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: VirtualFieldRegister, operation: SliceOp, resultType: FieldType): HyperKernel = {
    val inType = in.fieldType
    val expectedResultType =  new FieldType(inType.fieldShape.drop(1), inType.tensorShape, inType.elementType)
    require(resultType == expectedResultType)

    val index = operation.index
    val maxIndex = inType.fieldShape(0) - 1
    require(index >= 0 && index <= maxIndex,
      "Slice index " + index + " out of range: [0," + maxIndex + "].")

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new SliceHyperKernel(in, operation, resultType, addressing)
  }
}

