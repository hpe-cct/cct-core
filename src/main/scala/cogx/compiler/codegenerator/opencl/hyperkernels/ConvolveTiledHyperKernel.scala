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

import cogx.cogmath.geometry.Shape
import cogx.platform.types.{CrossCorrelationOrientation, ConvolutionOrientation, VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.opencl.fragments.{HyperKernel, AddressingMode}
import cogx.compiler.parser.op.ConvolveTiledOp

/** Performs convolution on a field with each thread responsible for multiple outputs (a "mini-tile").
  * This kernel works best on large (256x256 or greater) inputs convolved with small
  * (9x9 or smaller) filters.  In this domain, this kernel is 2-3X faster than conventional approaches
  * that employ the GPU local memory.
  *
  * When run on NVidia's 319 driver, this kernel showed promising results on a GTX 680 compared with the paper:
  *
  * Communication-Minimizing 2D Convolution in GPU Registers, by Iandola, Sheffield, Anderson, et.al.
  *
  * This kernel timed out at 7.4usec on a 5x5 convolution of a 9216x9216 input, which looked to about what was
  * reported in the paper (and just 50% above the "copy limit").  The kernel also timed out at 12usec for a 7x7
  * convolution (2.4X the copy limit), compared to 25usec as shown in the paper.  Note that a complex multiply run
  * as part of an FFT approach could run at best 3X slower than the copy limit (3 I/Os x 2 floats/element compared
  * with 2 I/Os x 1 float/element).
  *
  * The performance of this kernel was hurt by 40% when moving past the 319 kernel to the 331 or later driver
  * (for reasons yet to be determined).
  *
  * @author Dick carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The UnaryOp for this operation, with its function.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ConvolveTiledHyperKernel private (in: Array[VirtualFieldRegister],
                                operation: ConvolveTiledOp,
                                resultType: FieldType,
                                addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  // For a small input like 5x5, this doesn't seem to matter.
  makeInputConstant(1)

  val filterOrientation = operation.filterOrientation
  val Rows = resultType.fieldShape(0)
  val Columns = resultType.fieldShape(1)
  val FieldPoints = Rows*Columns

  val LocalRows = operation.localRows
  val LocalColumns = operation.localColumns

  val FilterRows = in(1).fieldType.rows
  val FilterColumns = in(1).fieldType.columns

  require(FilterRows % 2 == 1)
  require(FilterColumns % 2 == 1)

  val OutRowsPerThread = operation.rowsPerThread
  val OutColsPerThread = operation.colsPerThread

  val workFieldRows = (Rows + OutRowsPerThread - 1)/OutRowsPerThread
  val workFieldColumns = (Columns + OutColsPerThread - 1)/OutColsPerThread

  override lazy val workFieldType = resultType.resize(Shape(workFieldRows, workFieldColumns))
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressMode, LocalRows, LocalColumns)

  /** The number of rows of the image input a thread must read in */
  val threadInRows = OutRowsPerThread + FilterRows - 1
  /** The number of columns of the image input a thread must read in */
  val threadInColumns = OutColsPerThread + FilterColumns - 1

  val code = new StringBuilder

  for(outRow <- 0 until OutRowsPerThread)
    for(outCol <- 0 until OutColsPerThread)
      code append s"    float sum_${outRow}_${outCol} = 0.0f;\n"

  // Rather than put in tests for boundary conditions to prevent a thread from reading or writing
  // where it shouldn't if (Rows,Columns) is not a multiple of (OutRowsPerThread,OutColsPerThread), we
  // scale the "origin" of the thread's output tile by an amount that may not be an integer.  Neighboring threads
  // may thus be working (and writing) overlapping elements, although with the same value.  Wierd, but efficient,
  // since the NVidia compiler seems to produce short multiply/shift sequences for integer division. This was
  // motivated to avoid boundary checks for BorderValid convolution (which with a single-output-per-thread approach
  // has no boundary checks when reading the input).
  if (Rows % workFieldRows == 0)
    code append s"    int rowBaseIndex = _row * $OutRowsPerThread;\n"
  else
    code append s"    int rowBaseIndex = (_row * $Rows) / $workFieldRows;\n"
  if (Columns % workFieldColumns == 0)
    code append s"    int colBaseIndex = _column * $OutColsPerThread;\n"
  else
    code append s"    int colBaseIndex = (_column * $Columns) / $workFieldColumns;\n"
  code append s"    float filterVal, tileVal;\n"

  def contributesTo(inRow: Int, inCol: Int, outRow: Int, outCol: Int) = {
    val rowHalo = FilterRows / 2
    val colHalo = FilterColumns / 2

    val shiftedOutRow = outRow + rowHalo
    val shiftedOutCol = outCol + colHalo

    (inRow >= shiftedOutRow - rowHalo) && (inRow <= shiftedOutRow + rowHalo) &&
    (inCol >= shiftedOutCol - colHalo) && (inCol <= shiftedOutCol + colHalo)
  }

  for (inRow <- 0 until threadInRows) {
    for (inCol <- 0 until threadInColumns) {
    // read in an input tile element for this thread

      code append s"    // Reading in tileVal($inRow,$inCol)\n"
      code append s"    row = rowBaseIndex + $inRow;  "
      code append s"    column = colBaseIndex + $inCol;  "
      code append s"    tileVal = readNonlocal(@in0);\n"

      // Go over the output elements to see if this input tile element should be summed in

      // The ordering of traversing the filter seems to matter a little, although not in a consistent way
      // for different filter sizes.  By flipping the outer loop, the filter values are read in a consistent
      // forward direction. The following code is left in for further research:

      val flipOrder = true
      val (rowStart, rowEnd, colStart, colEnd, step) = filterOrientation match {
        case ConvolutionOrientation =>
          if (flipOrder)
            (OutRowsPerThread-1, 0, OutColsPerThread-1, 0, -1)
          else
            (0, OutRowsPerThread-1, 0, OutColsPerThread-1, 1)
        case CrossCorrelationOrientation =>
          (0, OutRowsPerThread-1, 0, OutColsPerThread-1, 1)
      }

      for (outRow <- rowStart to rowEnd by step) {
        for (outCol <- colStart to colEnd by step) {
          if (contributesTo(inRow, inCol, outRow, outCol)) {
            // read in filter value.  Convolution requires a flipping of the filter.
            val (filterRow, filterColumn) = filterOrientation match {
              case ConvolutionOrientation => (FilterRows - 1 - (inRow - outRow), FilterColumns - 1 - (inCol - outCol))
              case CrossCorrelationOrientation => (inRow - outRow, inCol - outCol)
            }
            code append s"    // Reading in filterVal($filterRow,$filterColumn)\n"
            code append s"    row = $filterRow; "
            code append s"    column = $filterColumn; "
            code append s"    filterVal = readNonlocal(@in1);\n"

            // sum in product of tile and filter

            code append s"    sum_${outRow}_${outCol} += tileVal * filterVal;\n"

          }
        }
      }
    }
  }

  // Note: moving the output result process up into the multiply-add was slower

  code append "// Outputting results\n"
  for (outRow <- 0 until OutRowsPerThread) {
    for (outCol <- 0 until OutColsPerThread) {

      code append s"    row = rowBaseIndex + ${outRow};\n"
      code append s"    column = colBaseIndex + ${outCol};\n"
      code append s"    @outNonlocal0 = sum_${outRow}_${outCol};\n"

    }
  }

  addCode(code.toString)
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ConvolveTiledHyperKernel extends HyperHelper {

  /**
   * Create a Hyperkernel that applies a unary operator to a field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ConvolveTiledOp, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    require(resultType == outFieldType(in(0).fieldType, in(1).fieldType))
    val addressing = bestAddressMode(in, resultType)
    new ConvolveTiledHyperKernel(in, operation, resultType, addressing)
  }

  def outFieldType(in0Type: FieldType, filterType: FieldType) =
    in0Type.resize(Shape(in0Type.fieldShape(0) - filterType.fieldShape(0) + 1,
                         in0Type.fieldShape(1) - filterType.fieldShape(1) + 1))
}
