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
import cogx.compiler.parser.op.TensorSliceOp

/** This kernel 'slices' a matrix field by taking the same row of elements from
  * each matrix, thereby producing a VectorField of the same fieldShape as the
  * input.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The TensorSliceOp opcode, with its index of the matrix
  *                  row to be extracted to the vector field result.
  * @param resultType The type of the resulting vector field.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class SliceMatricesHyperKernel private (in: VirtualFieldRegister,
                                       operation: TensorSliceOp,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode) {

  def toHexChar(i: Int) = {
    require(i >= 0 && i < 16)
    "0123456789ABCDEF"(i)
  }

  val code = new StringBuilder
  val inType = in.fieldType
  val tensorRowIndex = operation.index
  val tensorColumns = inType.tensorColumns
  val startIndex = tensorRowIndex * tensorColumns
  val untilIndex = (tensorRowIndex + 1) * tensorColumns
  val outType = addressing.clType(resultType).name

  addressMode match {
    case SmallTensorAddressing =>
      if (isBigTensorField(inType)) {
        for (index <- startIndex until untilIndex) {
          code append "    tensorElement = " + index + ";\n"
          code append "    float elem_" + index + " = readElement(@in0);\n"
        }
        code append "   @out0 = (" + outType + ") ("
        code append (startIndex until untilIndex).map(i => "elem_" + i).mkString(", ")
        code append ");\n"
      }
      else if (inType.tensorShape.points == 1)
        code append "        @out0 = read(@in0);\n"
      else {
        val indices = (startIndex until untilIndex).map(toHexChar).mkString
        code append "        @out0 = (read(@in0)).s" + indices + ";\n"
      }
    case TensorElementAddressing =>
      code append setLayerRowColumn(resultType, "_layer", "_row", "_column")
      code append "    const int tensorElement = _tensorElement + " + tensorRowIndex * tensorColumns + ";\n"
      code append "        @out0 = readNonlocal(@in0);\n"
    case _ => throw new RuntimeException("Addressing mode not supported by this kernel: " + addressMode)
  }
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object SliceMatricesHyperKernel {

  /**
   * Create a hyperkernel that slices a matrix field, yielding a vector field..
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The TensorSliceOp opcode, with its index of the matrix
   *                  row to be extracted to the vector field result.
   * @param resultType The type of the resulting vector field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: VirtualFieldRegister, operation: TensorSliceOp, resultType: FieldType): HyperKernel = {
    val inType = in.fieldType
    require(inType.tensorShape.dimensions == 2)
    require(resultType.tensorShape.dimensions == 1)
    require(inType.fieldShape == resultType.fieldShape)
    require(inType.tensorColumns == resultType.tensorColumns)

    val tensorRowIndex = operation.index
    require(tensorRowIndex >= 0 && tensorRowIndex < inType.tensorRows)

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new SliceMatricesHyperKernel(in, operation, resultType, addressing)
  }
}

