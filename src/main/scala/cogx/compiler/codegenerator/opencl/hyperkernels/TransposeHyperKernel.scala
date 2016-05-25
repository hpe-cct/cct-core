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

import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types._
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.Transpose2DOp

/** Computes the transpose of a 2D tensor field.
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode Addressing mode for this kernel (must be small tensor).
  *
  * @author Greg Snider
  */
private[cogx]
class TransposeHyperKernel private (in: VirtualFieldRegister,
                                    operation: Opcode,
                                    resultType: FieldType,
                                    addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode, ClampToEdge)
{
  require(in.fieldType.fieldShape.dimensions == 2,
    "Transpose only works on 2D fields")
  require(in.fieldType.rows == resultType.columns)
  require(in.fieldType.columns == resultType.rows)
  val tensorType: String = addressMode.clType(in.fieldType).name
  private val rows = in.fieldType.rows
  private val columns = in.fieldType.columns
  //require(rows >= 16 && columns >= 16, "field too small to transpose")

  // This requires a 16 x 16 work group to avoid bank conflicts.
  val WorkGroupRows = 16
  val WorkGroupColumns = WorkGroupRows
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(resultType, addressing,
      WorkGroupRows, WorkGroupColumns)

  val code =
    """
      |     // Local memory holds a blockSize x blockSize tile. It is padded
      |     // by 1 in each row to prevent local memory bank conflict.
      |     __local %tensorType% tile[%columnSize% * (%rowSize% + 1)];
      |
      |     // Read the input pixel nonlocally. We need to swap the group row
      |     // and column because addressing is organized around the output
      |     // field and we need to read the input field.
      |     int groupColumn = get_group_id(0);
      |     int groupRow = get_group_id(1);
      |     row = groupColumn * get_local_size(0) + _localRow;
      |     column = groupRow * get_local_size(1) + _localColumn;
      |     %tensorType% pixel = readNonlocal(@in0);
      |
      |     // Read *rows* of global memory into *columns* of local memory.
      |     // Because we have padded each row with an extra value, there are no
      |     // bank conflicts.
      |     int tileRow = _localColumn;
      |     int tileColumn = _localRow;
      |     int tileIndex = tileRow * (%rowSize% + 1) + tileColumn;
      |     tile[tileIndex] = pixel;
      |     barrier(CLK_LOCAL_MEM_FENCE);
      |
      |
      |     // Write *rows* of tile memory to *rows* of global memory.
      |     tileIndex = _localRow * (%rowSize% + 1) + _localColumn;
      |     pixel = tile[tileIndex];
      |
      |     barrier(CLK_LOCAL_MEM_FENCE);
      |
      |     @out0 = pixel;
      |
    """
  val codeString = code.toString.stripMargin.
          replaceAll("%rowSize%", WorkGroupRows.toString).
          replaceAll("%columnSize%", WorkGroupColumns.toString).
          replaceAll("%tensorType%", tensorType)
  addCode(codeString)
  //debugCompile
}

/** Factory for TransposeHyperKernel. */
private[cogx]
object TransposeHyperKernel {

  /** Create a hyperkernel that implements a 2D field transpose.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister],
            operation: Opcode,
            resultType: FieldType): HyperKernel =
  {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(inType.dimensions == 2)
    require(operation == Transpose2DOp)

    // The result must have number of rows and columns switched
    val outShape = Shape(inType.fieldShape(1), inType.fieldShape(0))
    val expectedResultType =
      new FieldType(outShape, inType.tensorShape, inType.elementType)
    require(resultType == expectedResultType)
    val addressing = SmallTensorAddressing
    new TransposeHyperKernel(in(0), operation, resultType, addressing)
  }
}