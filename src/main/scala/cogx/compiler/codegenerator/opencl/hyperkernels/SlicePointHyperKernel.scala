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
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.compiler.parser.op.SlicePointOp

/** A SlicePointKernel takes an N-dimensional field (first child) and
  * a 0-dimensional scalar field (second child) and uses the single value in
  * the second field as a slicing index into the first field. If necessary, the
  * slicing index value is converted to an integer and clipped by the range
  * of the first dimension of the first field.  Works on any type of input
  * field (scalar, vector, matrix, etc.) of dimensions >= 1.</p>
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The inputs virtual field register driving this kernel.
  * @param operation The SlicePointOp opcode
  * @param resultType The type of the resulting vector field.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class SlicePointHyperKernel private (in: Array[VirtualFieldRegister],
                                       operation: Opcode,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val code = new StringBuilder

  val inType = in(0).fieldType
  val inDim = inType.dimensions
  val firstDimensionSize = inType.fieldShape(0)

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  // Get index and clip it to the bounds of the first dimension.
  code append "    float floatIndex = readScalar(@in1);\n"
  code append "    int index = convert_int_rtz(floatIndex);\n"
  code append "    index = max(index, 0);\n"
  code append "    index = min(index, " + (firstDimensionSize - 1) + ");\n"

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
object SlicePointHyperKernel {

  /**
   * Create a hyperkernel that slices an N-dimensional field to form an N-1
   * dimensional field, based on an index provided by a 0D scalar field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The SlicePointOp opcode
   * @param resultType The type of the resulting vector field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    val inType = in(0).fieldType
    val expectedResultType =  new FieldType(inType.fieldShape.drop(1), inType.tensorShape, inType.elementType)
    require(resultType == expectedResultType)

    val indexType = in(1).fieldType
    require(indexType == new FieldType(Shape(), Shape(), Float32))

    require(operation == SlicePointOp)

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new SlicePointHyperKernel(in, operation, resultType, addressing)
  }
}

