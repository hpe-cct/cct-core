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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, AddressingMode, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.SubspaceOp

/** Extract a portion of an input field.  Unlike the Subfield operation, this
  * kernel expects a constant origin for the extracted region (not a dynamic
  * guide vector).  Also, the extracted region must be within the bounds of the
  * input field, so no border policy is brought into play.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The SubspaceOp for this operation, with its ranges.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class SubspaceHyperKernel private (in: Array[VirtualFieldRegister],
                                operation: SubspaceOp,
                                resultType: FieldType,
                                addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {
  val code = new StringBuffer
  val inDim = in(0).fieldType.dimensions
  code append "    column = _column + " + operation.indices(inDim - 1).start + ";\n"
  if (inDim >= 2)
    code append "    row = _row + " + operation.indices(inDim - 2).start + ";\n"
  if (inDim >= 3)
    code append "    layer = _layer + " + operation.indices(inDim - 3).start + ";\n"

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  code append "    @out0 = readNonlocal(@in0);\n"
  addCode(code.toString)
  //  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object SubspaceHyperKernel extends HyperHelper {

  /**
   * Create a Hyperkernel that extracts a portion of an input field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The SubspaceOp for this operation, with its ranges.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: SubspaceOp, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val inType = in(0).fieldType
    val indices = operation.indices
    require(inType.dimensions == indices.length,
      "The number of Range parameters for the subspace operator must match " +
              "the dimensionality of the space.")
    for (dim <- 0 until indices.length) {
      val range = indices(dim)
      require(range.step == 1, "Subspace operator requires a stride of 1")
      require(range.start >= 0 && range.end <= inType.fieldShape(dim),
        "\nRange in subspace operator exceeds size of field.\n" +
                "  range: " + range.start + " until " + range.end + "\n" +
                "  field: " + inType.fieldShape + "\n")
    }
    val newFieldShape = new Shape(operation.indices.map(_.length).toArray)
    val expectedResultType = new FieldType(newFieldShape, inType.tensorShape, inType.elementType)
    require(expectedResultType == resultType)
    val addressing = bestAddressMode(in, resultType)
    new SubspaceHyperKernel(in, operation, resultType, addressing)
  }
}