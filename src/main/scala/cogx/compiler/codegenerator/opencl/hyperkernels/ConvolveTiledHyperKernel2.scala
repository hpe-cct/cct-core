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
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, HyperKernel, AddressingMode}
import cogx.compiler.parser.op._

/** Performs convolution on a field with each thread responsible for multiple outputs (a "mini-tile").
  * This kernel works best on large (256x256 or greater) inputs convolved with small
  * (9x9 or smaller) filters.  In this domain, this kernel is 2-3X faster than conventional approaches
  * that employ the GPU local memory.
  *
  * This kernel variant was created from the ConvolveTiledHyperKernel, with the added features of frame-project
  * plane mixing and tensor reduction added.  This turned out to be slower than the equivalent operation performed
  * by the non-mini-tiled ConvolveHyperKernel.  The reason seems to be excessive register pressure which kills GPU
  * occupancy.  The mini-tiling approach requires lots of registers, so adding the registers for the reduction
  * accumulators hurt performance.
  *
  * One experiment would be to use shared memory to store the reduction accumulators, since they are accessed rarely
  * compared to the other register values.
  *
  * The above claim of 2X - 3X needs qualification.  It seems the performance of this approach has been uniquely hurt
  * by the move to NVidia's 331 (or 334, 337, 340 or 343) drivers, compared to the 319 driver.  The 319 driver is
  * listed as providing CUDA 4.2, while the others are listed as CUDA 6.0.  So much for progress.
  *
  * Perhaps CUDA 6.0 has a slightly different approach to register allocation and this kernel's 46 registers was at the
  * edge of some occupancy threshhold.
  *
  * This kernel still needs to adapt to the case where the resultType.tensorShape.points % filtersPerWorkGroup != 0
  *
  * @author Dick carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The UnaryOp for this operation, with its function.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ConvolveTiledHyperKernel2 private (in: Array[VirtualFieldRegister],
                                operation: ConvolveTiledOp,
                                resultType: FieldType,
                                addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  // For a small input like 5x5, this doesn't seem to matter.
  makeInputConstant(1)

  val filterOrientation = operation.filterOrientation
  val Rows = resultType.fieldShape(0)
  val Columns = resultType.fieldShape(1)
  val FieldPoints = Rows * Columns

  val LocalRows = operation.localRows
  val LocalColumns = operation.localColumns

  val FilterRows = in(1).fieldType.rows
  val FilterColumns = in(1).fieldType.columns

  require(FilterRows % 2 == 1)
  require(FilterColumns % 2 == 1)

  val OutRowsPerThread = operation.rowsPerThread
  val OutColsPerThread = operation.colsPerThread

  val filtersPerWorkGroup = 4

  val workFieldRows = (Rows + OutRowsPerThread - 1) / OutRowsPerThread
  val workFieldColumns = (Columns + OutColsPerThread - 1) / OutColsPerThread

  val workFieldTensorShape = Shape((resultType.tensorShape.points + filtersPerWorkGroup - 1)/ filtersPerWorkGroup)
  override lazy val workFieldType =
    resultType.resize(Shape(workFieldRows, workFieldColumns)).resizeTensor(workFieldTensorShape)
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressMode, LocalRows, LocalColumns)

  /** The number of rows of the image input a thread must read in */
  val threadInRows = OutRowsPerThread + FilterRows - 1
  /** The number of columns of the image input a thread must read in */
  val threadInColumns = OutColsPerThread + FilterColumns - 1

  val code = new StringBuilder

  var indentSpaces = 8

  def indent(s: String, numSpaces: Int) = {
    // split isn't the best here as "\n\n" is equivalent to "\n", plus existence of a trailing "\n" is not visible
    val appendChar = if (s.last == '\n') "\n" else ""
    s.replaceAll("\n", " \n").split('\n').mkString(" " * numSpaces, "\n" + " " * numSpaces, "").replaceAll(" \n", "\n") + appendChar
  }

  def append(s: String) {
    code append indent(s, indentSpaces)
  }

  // First line has 4 extra spaces, get around this by generating a blank line to start
  append("\n")

  for (i <- 0 until filtersPerWorkGroup)
    for (outRow <- 0 until OutRowsPerThread)
      for (outCol <- 0 until OutColsPerThread)
        append(s"float sum${i}_${outRow}_${outCol} = 0.0f;\n")

  // Rather than put in tests for boundary conditions to prevent a thread from reading or writing
  // where it shouldn't if (Rows,Columns) is not a multiple of (OutRowsPerThread,OutColsPerThread), we
  // scale the "origin" of the thread's output tile by an amount that may not be an integer.  Neighboring threads
  // may thus be working (and writing) overlapping elements, although with the same value.  Wierd, but efficient,
  // since the NVidia compiler seems to produce short multiply/shift sequences for integer division. This was
  // motivated to avoid boundary checks for BorderValid convolution (which with a single-output-per-thread approach
  // has no boundary checks when reading the input).
  if (Rows % workFieldRows == 0)
    append(s"int rowBaseIndex = _row * $OutRowsPerThread;\n")
  else
    append(s"int rowBaseIndex = (_row * $Rows) / $workFieldRows;\n")
  if (Columns % workFieldColumns == 0)
    append(s"int colBaseIndex = _column * $OutColsPerThread;\n")
  else
    append(s"int colBaseIndex = (_column * $Columns) / $workFieldColumns;\n")

  append(s"float tileVal;\n")

  val imageTensorPoints = in(0).fieldType.tensorShape.points
  val filterTensorPoints = in(1).fieldType.tensorShape.points

  val blockReduceTensorElementCode = {
    val snippet = new StringBuilder
    for (i <- 0 until filtersPerWorkGroup)
      for (outRow <- 0 until OutRowsPerThread)
        for (outCol <- 0 until OutColsPerThread)
          snippet append s"float reductionAccumulator${i}_r${outRow}_c$outCol = 0.0f;\n"

    snippet append s"for (int reductionIndex = 0; reductionIndex < $imageTensorPoints; reductionIndex++) {\n"
    snippet append s"    imageTensorElement = reductionIndex;\n"
    snippet.toString
  }

  // Adjust setting of tensorElement for image read in
  if (addressMode == TensorElementAddressing) {
    append("float imageTensorElement;\n")
    operation.vectorMode match {
      case ProjectFrame =>
        append(s"imageTensorElement = _tensorElement % $imageTensorPoints;\n")
      case BackProjectFrame =>
        s"imageTensorElement = _tensorElement % $imageTensorPoints;\n"
      case ProjectFrameBlockReduceSum =>
        append(blockReduceTensorElementCode)
        indentSpaces += 4
      case BackProjectFrameBlockReduceSum =>
        append(blockReduceTensorElementCode)
        indentSpaces += 4
      case FilterAdjoint => append(s"imageTensorElement = _tensorElement % $imageTensorPoints;\n")
      case PlaneByPlane => append("imageTensorElement = _tensorElement;\n")
    }
  }

  // Adjust setting of tensorElement for filter read in, as needed
  if (addressMode == TensorElementAddressing) {
    append("float filterTensorElement = " +
      (operation.vectorMode match {
      case ProjectFrame => " _tensorElement;\n"
      case ProjectFrameBlockReduceSum =>
        s"reductionIndex + _tensorElement * ${imageTensorPoints * filtersPerWorkGroup};\n"
      case BackProjectFrame =>
        s"imageTensorElement * ${filterTensorPoints / imageTensorPoints} + _tensorElement / $imageTensorPoints;\n"
      case BackProjectFrameBlockReduceSum =>
        s"reductionIndex * ${filterTensorPoints / imageTensorPoints} + _tensorElement * $filtersPerWorkGroup;\n"
      case FilterAdjoint => s"_tensorElement / $imageTensorPoints;\n"
      case PlaneByPlane => "_tensorElement;\n"
    })
    )
  }

  append(s"float filterVal;\n")

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

      append(s"// Reading in tileVal($inRow,$inCol)\n")
      if (addressMode == TensorElementAddressing)
        append("tensorElement = imageTensorElement;\n")
      append(s"row = rowBaseIndex + $inRow;  column = colBaseIndex + $inCol;\n")
      append(s"tileVal = readElementNonlocal(@in0);\n")

      // output index from , just 0 to start
      for (outIdx <- 0 until filtersPerWorkGroup) {
        // Go over the output elements to see if this input tile element should be summed in

        // The ordering of traversing the filter seems to matter a little, although not in a consistent way
        // for different filter sizes.  By flipping the outer loop, the filter values are read in a consistent
        // forward direction. The following code is left in for further research:

        val flipOrder = true
        val (rowStart, rowEnd, colStart, colEnd, step) = filterOrientation match {
          case ConvolutionOrientation =>
            if (flipOrder)
              (OutRowsPerThread - 1, 0, OutColsPerThread - 1, 0, -1)
            else
              (0, OutRowsPerThread - 1, 0, OutColsPerThread - 1, 1)
          case CrossCorrelationOrientation =>
            (0, OutRowsPerThread - 1, 0, OutColsPerThread - 1, 1)
        }

        for (outRow <- rowStart to rowEnd by step) {
          for (outCol <- colStart to colEnd by step) {
            if (contributesTo(inRow, inCol, outRow, outCol)) {
              // read in filter value.  Convolution requires a flipping of the filter.
              val (filterRow, filterColumn) = filterOrientation match {
                case ConvolutionOrientation => (FilterRows - 1 - (inRow - outRow), FilterColumns - 1 - (inCol - outCol))
                case CrossCorrelationOrientation => (inRow - outRow, inCol - outCol)
              }
              append(s"// Reading in filterVal($filterRow,$filterColumn)\n")

              val multipleFilterIndexDelta = if (operation.vectorMode == ProjectFrameBlockReduceSum) imageTensorPoints else 1

              if (addressMode == TensorElementAddressing)
                append(s"tensorElement = filterTensorElement + ${outIdx * multipleFilterIndexDelta};\n")

              append(s"row = $filterRow; column = $filterColumn;\n")
              append(s"filterVal = readElementNonlocal(@in1);\n")

              // sum in product of tile and filter

              append(s"sum${outIdx}_${outRow}_${outCol} += tileVal * filterVal;\n")

            }
          }
        }
      }
    }
  }

  if (operation.vectorMode != ProjectFrameBlockReduceSum && operation.vectorMode != BackProjectFrameBlockReduceSum) {
    append("// Outputting results\n")
    append(s"tensorElement = _tensorElement;\n")
    for (outRow <- 0 until OutRowsPerThread) {
      for (outCol <- 0 until OutColsPerThread) {
        append(s"row = rowBaseIndex + ${outRow};\n")
        append(s"column = colBaseIndex + ${outCol};\n")
        append(s"@outElementNonlocal0 = sum0_${outRow}_$outCol;\n")
      }
    }
  }
  else {
    require(addressMode == TensorElementAddressing)
    val snippet = new StringBuilder
    for (i <- 0 until filtersPerWorkGroup) {
      for (outRow <- 0 until OutRowsPerThread) {
        for (outCol <- 0 until OutColsPerThread) {
          append(s"reductionAccumulator${i}_r${outRow}_c$outCol += sum${i}_${outRow}_${outCol}; sum${i}_${outRow}_${outCol} = 0.0f;\n\n")
        }
      }
    }

    // close for(int reductionIndex = 0; reductionIndex < $imageTensorPoints; reductionIndex++) {
    indentSpaces -= 4
    append("}\n")
    append("// Outputting results\n")
    for (i <- 0 until filtersPerWorkGroup) {
      append(s"tensorElement = _tensorElement * $filtersPerWorkGroup + $i;\n")
      for (outRow <- 0 until OutRowsPerThread) {
        for (outCol <- 0 until OutColsPerThread) {
          append(s"row = rowBaseIndex + ${outRow};\n")
          append(s"column = colBaseIndex + ${outCol};\n")
          append(s"@outElementNonlocal0 = reductionAccumulator${i}_r${outRow}_c$outCol;\n")
        }
      }
    }
  }

  addCode(code.toString)
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ConvolveTiledHyperKernel2 extends HyperHelper {

  val DefaultLocalRows = 1
  val DefaultLocalColumns = 256
  val DefaultRowsPerThread = 8
  val DefaultColsPerThread = 1
  /**
   * Create a Hyperkernel that applies a unary operator to a field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: AbstractConvolveOp, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    // The kernel may be invoked by an optimizer that doesn't care about the detailed parameters of the convolution
    // and instead just passes in a generic ConvolveOp.  Map this ConvolveOp to the best ConvolveTiledOp.
    val convolveTiledOp = operation match {
      case op: ConvolveOp =>
        ConvolveTiledOp(DefaultLocalRows, DefaultLocalColumns, DefaultRowsPerThread, DefaultColsPerThread,
          op.borderPolicy, op.filterOrientation, op.samplingPolicy, op.vectorMode, op.batchSize)
      case op: ConvolveTiledOp => op
      case op: ConvolveToSmallFieldTiledOp =>
        ConvolveTiledOp(DefaultLocalRows, DefaultLocalColumns, DefaultRowsPerThread, DefaultColsPerThread,
          op.borderPolicy, op.filterOrientation, op.samplingPolicy, op.vectorMode, op.batchSize)
    }
    require(convolveTiledOp.borderPolicy == BorderValid,
      "ConvolveTiledHyperKernel requires BorderValid borderPolicy, found: " + convolveTiledOp.borderPolicy)
    require(convolveTiledOp.samplingPolicy == NoSamplingConvolution,
      "ConvolveTiledHyperKernel requires NoSamplingConvolution samplingPolicy, found: " + convolveTiledOp.samplingPolicy)
    require(resultType == outputFieldType(in(0).fieldType, in(1).fieldType, convolveTiledOp.vectorMode))
    val addressing = TensorElementAddressing
    new ConvolveTiledHyperKernel2(in, convolveTiledOp, resultType, addressing)
  }

  def outputFieldType(input: FieldType, filter: FieldType, vectorMode: VectorMode) = {
    val outputFieldShape = Shape(input.fieldShape(0) - filter.fieldShape(0) + 1,
      input.fieldShape(1) - filter.fieldShape(1) + 1)
    input.resize(Shape(input.fieldShape(0) - filter.fieldShape(0) + 1,
      input.fieldShape(1) - filter.fieldShape(1) + 1))

    val outputTensorShape = vectorMode match {
      case ProjectFrame =>
        filter.tensorShape
      case ProjectFrameBlockReduceSum =>
        val outputTensorPoints = filter.tensorShape.points / input.tensorShape.points
          Shape(outputTensorPoints)
      case BackProjectFrame =>
        filter.tensorShape
      case BackProjectFrameBlockReduceSum =>
        val outputTensorPoints = filter.tensorShape.points / input.tensorShape.points
          Shape(outputTensorPoints)
      case FilterAdjoint =>
        val outputTensorPoints = input.tensorShape.points * filter.tensorShape.points
        if (outputTensorPoints > 1)
          Shape(outputTensorPoints)
        else
          Shape()
      case PlaneByPlane =>
        if (input.tensorOrder == 0)
          filter.tensorShape
        else
          input.tensorShape
    }
    /** the FieldType of the output field */
    val fieldType = new FieldType(outputFieldShape, outputTensorShape, Float32)
    fieldType
  }
}
