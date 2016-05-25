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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.cogmath.algebra.real.Logarithm
import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.compiler.parser.op.UnaryOpcode
import cogx.platform.types.{FieldType, VirtualFieldRegister}

/** Transposes an M x N image where M and N are powers of 2 and multiples of 16.
  *
  * @param in The virtual field register of the input field to be interleaved.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @author Greg Snider
  */
private[cogx]
class DCTTransposeHyperKernel private(in: VirtualFieldRegister,
                                      operation: UnaryOpcode,
                                      resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, BigTensorAddressing)
        with Logarithm
{
  require(in.fieldType.fieldShape.dimensions == 2,
    "Transpose only works on 2D fields")
  require(in.fieldType.rows == resultType.columns)
  require(in.fieldType.columns == resultType.rows)
  private val rows = in.fieldType.rows
  private val columns = in.fieldType.columns
  require(isPowerOf2(rows) && isPowerOf2(columns),
    "Transpose rows and columns must be powers of 2")
  require(rows >= 16 && columns >= 16,
    "Tranpose rows and columns must be multiples of 16")

  // This requires a 16 x 16 work group to avoid bank conflicts.
  val WorkGroupRows = 16
  val WorkGroupColumns = WorkGroupRows
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(resultType, addressing,
      WorkGroupRows, WorkGroupColumns)

  val code =
    """
      |   // Local memory holds a blockSize x blockSize tile. It is padded
      |   // by 1 in each row to prevent local memory bank conflict.
      |
      |   __local float tile[%columnSize% * (%rowSize% + 1)];
      |
      |   for (int tensorElement = 0; tensorElement < _tensorElements; tensorElement++) {
      |
      |     // Read *rows* of global memory into *columns* of local memory.
      |     // Because we have padded each row with an extra value, there are no
      |     // bank conflicts.
      |     int tileRow = _localColumn;
      |     int tileColumn = _localRow;
      |     int tileIndex = tileRow * (%rowSize% + 1) + tileColumn;
      |     float pixel = readElement(@in0);
      |     tile[tileIndex] = pixel;
      |     barrier(CLK_LOCAL_MEM_FENCE);
      |
      |     // Write *rows* of tile memory to *rows* of global memory, but swap
      |     // the group row and column
      |     int groupColumn = get_group_id(0);
      |     int groupRow = get_group_id(1);
      |     row = groupColumn * get_local_size(0) + _localRow;
      |     column = groupRow * get_local_size(1) + _localColumn;
      |     tileIndex = _localRow * (%rowSize% + 1) + _localColumn;
      |     pixel = tile[tileIndex];
      |
      |     barrier(CLK_LOCAL_MEM_FENCE);
      |
      |     @outElementNonlocal0 = pixel;
      |   }
    """
  val codeString = code.toString.stripMargin.
          replaceAll("%rowSize%", WorkGroupRows.toString).
          replaceAll("%columnSize%", WorkGroupColumns.toString)
  addCode(codeString)
}

/** Factory for creating DCTransposeHyperKernels.
  *
  */
private[cogx]
object DCTTransposeHyperKernel {

  /** Create a DCTTransposeHyperKernel
    *
    * @param in The virtual field register of the input field to be interleaved.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The hyperkernel
    */
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType): HyperKernel = {
    new DCTTransposeHyperKernel(in, operation, resultType)
  }
}