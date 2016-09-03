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
import cogx.platform.types.ElementTypes._
import cogx.cogmath.algebra.real.Logarithm
import cogx.compiler.parser.op._
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ConvolveOp
import cogx.parameters.Cog
import ConvolveHyperKernel._


/** Convolve a field with a dynamic filter field.
  *
  * This kernel is fairly complex, but then again it performs convolution
  * or correlation with integrated upsampling or downsampling, with 5 different
  * boundary handling options.
  *
  * If downsampling is specified, the filter field (the 2nd input) is
  * conceptually applied to the input before the downsampling.
  * A performance benefit is realized because output values that would be
  * discarded by the subsequent downsampling are never calculated.
  *
  * If upsampling is specified, the filter field (the 2nd input) is
  * conceptually applied to the input after the upsampling.
  * A performance benefit is realized because the dot product of the filter
  * and the upsampled input is sparsely calculated, taking advantage of the
  * known 0's produced by the upsampling.
  *
  * Here's a curious result from a regression test on this kernel.  Times are in
  * uSec/step and column heading is (imageTensorDepth, filterTensorDepth).  Note
  * how use of float3's is slower than float4's in SmallTensorAlways mode, even
  * though the Shared memory footprint (in bytes) is the same for float3's and
  * float4's.  Possibly some vector instructions are missing for float3's?
  *
  * image = 512 x 512, filter = 15 x 15
  *
  *                         		(1,1)		(1,2)		  (1,3)		  (1,4)		  (2,1)		  (2,2)		  (3,1)		  (3,3)		  (4,1)		  (4,4)
  *  UseSmallTensorNever	     544	     859	    1238	    1619	     837	     848	    1202	    1233	    1583	    1622
  *  UseSmallTensorAlways	     448	     421	     428	     437	     520	     530	    1316	    1317	    1075	    1072
  *  UseSmallTensorWhenBest	   442	     447	     452	     451	     531	     532	    1211	    1243	    1081	    1083
  *
  * @author Matthew D. Pickett and Dick Carter
  * @param inputs The input and filter virtual field registers driving this kernel.
  * @param operation the opcode
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @param params A bundle of platform parameters that affect kernel code generation and optimization.
  */
private[cogx]
class ConvolveHyperKernel private (inputs: Array[VirtualFieldRegister],
                                          val operation: ConvolveOp,
                                          resultType: FieldType,
                                          addressMode: AddressingMode,
                                          params: ConvolutionParams)
    extends HyperKernel(operation,
          inputs, resultType,
          addressMode)
    with Logarithm
{
  /** The extensive workgroup parameter calculation has been moved off to a
    * helper class so that it is also available to the factory method.  We
    * don't have to repeat the calculation, since we can import the params
    * class fields.
    */
  import params._

  require(groupSize > 1,
    "Could not fit inputTile and filter (" + localMemBytesNeeded + " bytes)" +
            " in available local memory (" + localMemBytesAvailable + " bytes)")

  /** in ProjectFrameBlockReduceSum mode, the result tensor length might not be a multiple of the filtersPerWorkGroup.
    * This routine helps identify for which iterations thread-pruning code needs to be inserted.
    *
    * @param i The iteration (from 0 until filtersPerWorkGroup)
    * @return  Should thread-pruning code be inserted.
    */
  def iterationNeedsPruning(i: Int) = {
    val lastValidPlaneModulus = (resultType.tensorShape.points/batchSize - 1) % filtersPerWorkGroup
    i > lastValidPlaneModulus
  }

  override lazy val workFieldType = calcWorkFieldType(resultType, filtersPerWorkGroup, batchSize)
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressMode, unsampledRows, unsampledColumns)

  val code = new StringBuffer

  //define and allocate the local tile and its offset relative to the origin of
  //the input field
  code append s"    __local $imageTensorType localTile[$paddedLocalVolume];\n"
  if (putFilterInConstantMemory)
    makeInputConstant(1)
  else if (filtersPerWorkGroup == 1)
    code append s"        __local $filterTensorType localFilter[$filterVolume];\n"
  else {
    code append s"        // filtersPerWorkGroup = $filtersPerWorkGroup\n"
    for (i <- 0 until filtersPerWorkGroup)
      code append s"        __local $filterTensorType localFilter$i[$filterVolume];\n"
  }

  // This kernel should work on 1-dimensional fields because:
  //   1. get_group_id(1) will return 0 per the OpenCl Spec
  //   2. fieldType.rows == 1 for a 1D field

  inputDims match {
    case 2 =>
    case 1 =>  code append "        int _localRow = 0;\n"
    case x => throw new RuntimeException("Kernel " + this + " does not support " + x + "D input fields.")
  }
  code append "        int threadIndex = _localColumn + _localRow * _localColumns;\n"

  // Adjustment for filtersPerWorkGroup?
  val workTensorElementsPerInput = workFieldType.tensorShape.points / batchSize
  if (batchSize > 1) {
    // The image within the batch that this thread is working on
    code append s"        int imageIndex = _tensorElement / $workTensorElementsPerInput;\n"
    // The thread offset of this thread within the group of threads working on this same image
    code append s"        int withinImageThreadElement = _tensorElement % $workTensorElementsPerInput;\n"
  }
  else {
    // The image within the batch that this thread is working on
    code append s"        const int imageIndex = 0;\n"
    // The thread offset of this thread within the group of threads working on this same image
    code append s"        int withinImageThreadElement = _tensorElement;\n"
  }

  // Set up tensorElement variable for input image tile reading

  // Amount of indent the multi-line strings of the body of the kernel code will get
  private var indentSpaces = 4

  val blockReduceTensorElementCode = {
    val snippet = new StringBuilder
    snippet append "\n// Block reduce loop \n"
    if (filtersPerWorkGroup == 1)
      snippet append s"$convolveTensorType reductionAccumulator = $convolveTensorZero;\n"
    else
      for (i <- 0 until filtersPerWorkGroup)
        snippet append s"$convolveTensorType reductionAccumulator$i = $convolveTensorZero;\n"
    snippet append s"for (int reductionIndex = 0; reductionIndex < $planesPerImage; reductionIndex++) {\n"
    if (batchSize == 1)
      snippet append s"    tensorElement = reductionIndex;\n"
    else
      snippet append s"    tensorElement = reductionIndex + imageIndex * $planesPerImage;\n"
    snippet.toString
  }

  // Adjust setting of tensorElement for image read in, as needed
  val tensorElementCode =
    if (addressMode == TensorElementAddressing)
      operation.vectorMode match {
        case ProjectFrame =>
          s"""
             |  tensorElement = _tensorElement % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
        case BackProjectFrame =>
          s"""
             |  tensorElement = _tensorElement % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
        case ProjectFrameBlockReduceSum =>
          indentSpaces += 4
          blockReduceTensorElementCode
        case BackProjectFrameBlockReduceSum =>
          indentSpaces += 4
          blockReduceTensorElementCode
        case FilterAdjoint =>
          s"""
             |  tensorElement = _tensorElement % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
        case PlaneByPlane =>
          """
            |  tensorElement = _tensorElement;
          """.stripMargin
        case FilterAdjointBlockReduceSum =>
          throw new RuntimeException(s"Internal compiler error: invalid vector mode ${operation.vectorMode} seen.")
      }
    else
      ""

  // Generate the convolution kernel code as the sum of separate code blocks
  // val convCode = setupCode + readLocalCode + borderCode + readFilterCode + multiplyAddCode

  /** Set up some variables helpful to the rest of the calculation */
  val setupCode = samplingPolicy match {
    case UpsampleInputConvolution(step) =>
      s"""
        |int postSamplingGroupStartRow = get_group_id(1) * $unsampledRows - $lowHaloRows;    // Fix per all border policies
        |int postSamplingGroupStartColumn = get_group_id(0) * $unsampledColumns - $lowHaloColumns;    // Fix per all border policies
        |int preSamplingGroupStartRow = ceil(postSamplingGroupStartRow / $upsampleStep.0f);
        |int preSamplingGroupStartColumn = ceil(postSamplingGroupStartColumn / $upsampleStep.0f);
        |""".stripMargin
    case DownsampleOutputConvolution(step) =>
      s"""
        |int preSamplingGroupStartRow = get_group_id(1) * $unsampledRows * $downsampleStep - $lowHaloRows;
        |int preSamplingGroupStartColumn = get_group_id(0) * $unsampledColumns * $downsampleStep - $lowHaloColumns;
        |""".stripMargin
    case NoSamplingConvolution =>
      s"""
        |int preSamplingGroupStartRow = get_group_id(1) * $unsampledRows - $lowHaloRows;
        |int preSamplingGroupStartColumn = get_group_id(0) * $unsampledColumns - $lowHaloColumns;
        |""".stripMargin
  }
  /** First part of the loop to read in the input image tile */
  val readLocalCode =
    s"""
      |  // Input image tile read-in
      |  for (int localTileIndex = threadIndex; localTileIndex < $paddedLocalVolume; localTileIndex += $groupSize) {
      |      int tileRow = localTileIndex / $paddedLocalColumns;
      |      int tileColumn = localTileIndex - tileRow * $paddedLocalColumns;
      |      row = preSamplingGroupStartRow + tileRow;
      |      column = preSamplingGroupStartColumn + tileColumn;
    """.stripMargin
  /** Closing part of the loop to read in the input image tile, handles various border policies */
  val borderCode = borderPolicy match {
    case BorderZero =>
      s"""
        |      // BorderZero edge handling
        |      if (column >=0 && column < $inputColumns && row >= 0 && row < $inputRows)
        |          localTile[localTileIndex] = readNonlocal(@in0);
        |      else
        |          localTile[localTileIndex] = $imageTensorZero;
        |  }
      """.stripMargin
    case BorderClamp =>
      s"""
        |      // BorderClamp edge handling
        |      if (row < 0)
        |          row = 0;
        |      if (row >= $inputRows)
        |          row = $inputRows - 1;
        |      if (column < 0)
        |          column = 0;
        |      if (column >= $inputColumns)
        |          column = $inputColumns - 1;
        |      localTile[localTileIndex] = readNonlocal(@in0);
        |  }
      """.stripMargin
    case BorderCyclic =>
      // The following code seemingly hung on NVIDIA Maxwell bricks for the "Scalar convolve Scalar (1D)"
      // sub-test within the StaticConvolutionSpec test.  Reversing the order of 3rd and 4th lines
      // (the two 'while' statements adjusting the column value), corrected the problem.
      //
      // Given we had no explanation for this behavior, we switched to a different approach using the '%' operator.
      // Unfortunately, we discovered that the operator was not guaranteed to return a positive value for negative
      // inputs, so an additional check and add was put in place.
//      s"""
//        |      while (row < 0) row += $inputRows;
//        |      while (row >= $inputRows) row -= $inputRows;
//        |      while (column < 0) column += $inputColumns;
//        |      while (column >= $inputColumns) column -= $inputColumns;
//        |      localTile[localTileIndex] = readNonlocal(@in0);
//        |  }
//      """.stripMargin
      s"""
        |      // BorderCyclic edge handling
        |      row %= $inputRows;
        |      if (row < 0) row += $inputRows;
        |      column %= $inputColumns;
        |      if (column < 0) column += $inputColumns;
        |      localTile[localTileIndex] = readNonlocal(@in0);
        |  }
      """.stripMargin
    case BorderValid =>
      // We need to protect the kernel from reading outside of the valid image input (on the high side).
      // If the workgroup dims divide the output dims evenly then this is not necessary.  The need for this
      // was only seen after the padding was reduced to 1, and then only in rare instances, such as when
      // the input was equal to the TLB size of 1MByte.
      val protectRow =
        if (resultType.rows % workGroup.localRows != 0)
          s"""|      if (row >= $inputRows)
              |          row = $inputRows - 1;
              |""".stripMargin
        else ""

      val protectColumn =
        if (resultType.columns % workGroup.localColumns != 0)
          s"""|      if (column >= $inputColumns)
              |          column = $inputColumns - 1;
              |""".stripMargin
        else ""

      "  // BorderValid edge handling\n" +
        protectRow + protectColumn +
        """|      localTile[localTileIndex] = readNonlocal(@in0);
           |  }
           |""".stripMargin
    case BorderFull =>
      s"""
        |      // BorderFull edge handling
        |      if (column >=0 && column < $inputColumns && row >= 0 && row <$inputRows)
        |          localTile[localTileIndex] = readNonlocal(@in0);
        |      else
        |          localTile[localTileIndex] = $imageTensorZero;
        |  }
      """.stripMargin
  }
  // Adjust setting of tensorElement for filter read in, as needed
  val postReadLocalCode =
    if (addressMode == TensorElementAddressing)
      operation.vectorMode match {
        case ProjectFrame =>
          if (batchSize == 1)
            "  tensorElement = _tensorElement;\n"
          else
            s"  tensorElement = _tensorElement % $filterTensorPoints;\n"
        case ProjectFrameBlockReduceSum =>
            s"  tensorElement = reductionIndex + withinImageThreadElement * " + (planesPerImage * filtersPerWorkGroup) + ";\n"
        case BackProjectFrame =>
          s"  tensorElement = $numLogicalFilters * (withinImageThreadElement % $planesPerImage) + withinImageThreadElement / $planesPerImage;\n"
        case BackProjectFrameBlockReduceSum =>
          s"  tensorElement = reductionIndex * $numLogicalFilters + withinImageThreadElement * $filtersPerWorkGroup;\n"
        case FilterAdjoint =>
          s"  tensorElement = withinImageThreadElement / $planesPerImage + imageIndex * $numLogicalFilters;\n"
        case FilterAdjointBlockReduceSum =>
          throw new RuntimeException(s"Internal compiler error: invalid vector mode ${operation.vectorMode} seen.")
        case PlaneByPlane => "" // tensorElement already set up properly above
      }
    else
      ""
  // Replace for-loop with simple if-test when possible
  val EnableFilterSimplification = true

  /** Read in the entire filter (if it's not placed in constant memory) */
  def readSingleFilterCode(filterMemSuffix: String): String = if (putFilterInConstantMemory) "" else filterOrientation match {
    case CrossCorrelationOrientation =>
      if (EnableFilterSimplification && filterVolume <= groupSize)
        s"""|  // read-in of filter $filterMemSuffix
            |  if (threadIndex < $filterVolume) {
            |      row = threadIndex / $filterColumns;
            |      column = threadIndex - row * $filterColumns;
            |      localFilter$filterMemSuffix[threadIndex] = readNonlocal(@in1);
            |  }
            |""".stripMargin
      else
        s"""|  // read-in of filter $filterMemSuffix
            |  for (int filterIndex = threadIndex; filterIndex < $filterVolume; filterIndex += $groupSize) {
            |      row = filterIndex / $filterColumns;
            |      column = filterIndex - row * $filterColumns;
            |      localFilter$filterMemSuffix[filterIndex] = readNonlocal(@in1);
            |  }
            |""".stripMargin
    case ConvolutionOrientation =>
      if (EnableFilterSimplification && filterVolume <= groupSize)
        s"""|  // read-in of filter $filterMemSuffix
            |  if (threadIndex < $filterVolume) {
            |      row = threadIndex / $filterColumns;
            |      column = threadIndex - row * $filterColumns;
            |      row = $filterRows - 1 - row;
            |      column = $filterColumns - 1 - column;
            |      localFilter$filterMemSuffix[threadIndex] = readNonlocal(@in1);
            |  }
            |""".stripMargin
      else
        s"""|  // read-in of filter $filterMemSuffix
            |  for (int filterIndex = threadIndex; filterIndex < $filterVolume; filterIndex += $groupSize) {
            |      row = filterIndex / $filterColumns;
            |      column = filterIndex - row * $filterColumns;
            |      row = $filterRows - 1 - row;
            |      column = $filterColumns - 1 - column;
            |      localFilter$filterMemSuffix[filterIndex] = readNonlocal(@in1);
            |  }
            |""".stripMargin
  }

  val multipleFilterIndexDelta = if (operation.vectorMode == ProjectFrameBlockReduceSum) planesPerImage else 1

  val readFilterCode: String =
    if (filtersPerWorkGroup == 1)
      readSingleFilterCode("")
    else {
      val snippet = new StringBuilder
      snippet append readSingleFilterCode("0")
      for (i <- 1 until filtersPerWorkGroup) {
        // Make sure tensorElement never exceeds filterTensorPoints.  If dummy data is needed, doubly-read in a previous filter.
        if (iterationNeedsPruning(i))
          snippet append s"    if (tensorElement < $filterTensorPoints - $multipleFilterIndexDelta)\n    "
        snippet append s"  tensorElement += $multipleFilterIndexDelta;\n\n"
        snippet append readSingleFilterCode(i.toString)
      }
      snippet.toString
    }
  /** Perform the actual convolution/correlation */
  val multiplyAddCode = samplingPolicy match {
    case UpsampleInputConvolution(step) =>
      val commonHeader =
        s"""
           |  // Multiply-add code
           |  barrier(CLK_LOCAL_MEM_FENCE);
           |  int postSamplingThreadStartRow = postSamplingGroupStartRow + _localRow;
           |  int postSamplingThreadStartColumn = postSamplingGroupStartColumn + _localColumn;
           |  int preSamplingThreadStartRow = ceil(postSamplingThreadStartRow / $upsampleStep.0f);
           |  int preSamplingThreadStartColumn = ceil(postSamplingThreadStartColumn / $upsampleStep.0f);
           |  int tileRow =  preSamplingThreadStartRow - preSamplingGroupStartRow;
           |  int firstRow =  preSamplingThreadStartRow * $upsampleStep - postSamplingThreadStartRow;
           |  int firstColumn =  preSamplingThreadStartColumn * $upsampleStep - postSamplingThreadStartColumn;
           |""".stripMargin
      val retVal = new StringBuffer
      retVal append commonHeader

      if (filtersPerWorkGroup == 1) {
        retVal append
          s"""
             |  $convolveTensorType rowSum = $convolveTensorZero;
             |  $convolveTensorType columnSum = $convolveTensorZero;
             |  for (int i = firstRow; i < $filterRows; i += $upsampleStep, tileRow++) {
             |      columnSum = $convolveTensorZero;
             |      int tileColumn =  preSamplingThreadStartColumn - preSamplingGroupStartColumn;
             |      for( int j = firstColumn; j < $filterColumns; j += $upsampleStep, tileColumn++){
             |          int tilePos = tileRow * $paddedLocalColumns + tileColumn;
             |          int filterPos = i*$filterColumns + j;
             |          columnSum += localTile[tilePos]*localFilter[filterPos];
             |      }
             |      rowSum += columnSum;
             |  }
             |""".stripMargin
      }
      else {
        for (i <- 0 until filtersPerWorkGroup)
          retVal append s"  $convolveTensorType rowSum$i = $convolveTensorZero;\n"
        retVal append
          s"""|  for (int i = firstRow; i < $filterRows; i += $upsampleStep, tileRow++) {
              |""".stripMargin
        for (i <- 0 until filtersPerWorkGroup)
          retVal append s"      $convolveTensorType columnSum$i = $convolveTensorZero;\n"
        retVal append
          s"""|      int tileColumn =  preSamplingThreadStartColumn - preSamplingGroupStartColumn;
              |      for( int j = firstColumn; j < $filterColumns; j += $upsampleStep, tileColumn++){
              |          int tilePos = tileRow * $paddedLocalColumns + tileColumn;
              |          int filterPos = i*$filterColumns + j;
              |""".stripMargin
        for (i <- 0 until filtersPerWorkGroup)
          retVal append "          columnSum" + i + " += localTile[tilePos] * localFilter" + i + "[filterPos];\n"
          retVal append "      }\n"
        for (i <- 0 until filtersPerWorkGroup)
          retVal append "      rowSum" + i + " += columnSum" + i + ";\n"
        retVal append "  }\n"
      }
      retVal.toString()
    case _ =>
      val filterRead = if (addressMode == TensorElementAddressing) "readElementNonlocal" else "readNonlocal"
      if (putFilterInConstantMemory && filterOrientation == CrossCorrelationOrientation)
        s"""
          |  // Multiply-add code
          |  barrier(CLK_LOCAL_MEM_FENCE);
          |  $convolveTensorType rowSum = $convolveTensorZero;
          |  $convolveTensorType columnSum = $convolveTensorZero;
          |  for (int row = 0; row < $filterRows; row++) {
          |      columnSum = $convolveTensorZero;
          |      for(int column = 0; column < $filterColumns; column++){
          |          int tilePos = (_localRow * $downsampleStep + row) * $paddedLocalColumns + (_localColumn * $downsampleStep + column);
          |          columnSum += localTile[tilePos] * $filterRead(@in1);
          |      }
          |      rowSum += columnSum;
          |  }
        """.stripMargin
      else if (putFilterInConstantMemory && filterOrientation == ConvolutionOrientation)
        s"""
          |  // Multiply-add code
          |  barrier(CLK_LOCAL_MEM_FENCE);
          |  $convolveTensorType rowSum = $convolveTensorZero;
          |  $convolveTensorType columnSum = $convolveTensorZero;
          |  for (int i = 0; i < $filterRows; i++) {
          |      columnSum = $convolveTensorZero;
          |      for(int j = 0; j < $filterColumns; j++){
          |          int tilePos = (_localRow * $downsampleStep + i) * $paddedLocalColumns + (_localColumn * $downsampleStep + j);
          |          row = $filterRows - 1 - i;
          |          column = $filterColumns - 1 - j;
          |          columnSum += localTile[tilePos] * $filterRead(@in1);
          |      }
          |      rowSum += columnSum;
          |  }
        """.stripMargin
      else {
        val MaxUnrollableColumns = 100  // Rough guess at what constitutes too much code for the NVidia compiler
        if (filterColumns < MaxUnrollableColumns) {
          // Unroll only the inner loop.  Let the NVIDIA compiler unroll the outer loop, which it generally does if it
          // wouldn't result in too large a kernel.
          val commonHeader =
            s"""
              |  // Multiply-add code
              |  barrier(CLK_LOCAL_MEM_FENCE);
              |  int tilePos = _localRow * $downsampleStep * $paddedLocalColumns + _localColumn * $downsampleStep;
              |  int filterPos = 0;
              |""".stripMargin
          if (filtersPerWorkGroup == 1) {
            commonHeader +
            s"""
              |  $convolveTensorType rowSum = $convolveTensorZero;
              |  $convolveTensorType columnSum = $convolveTensorZero;
              |  $convolveTensorType tileValue;
              |  for (int i = 0; i < $filterRows; i++) {
              |      columnSum = $convolveTensorZero;
              |      tilePos = (_localRow * $downsampleStep + i) * $paddedLocalColumns + (_localColumn * $downsampleStep);
            """.stripMargin +
            """      tileValue = localTile[tilePos++];
              |      columnSum += tileValue * localFilter[filterPos++];
            """.stripMargin * filterColumns +            // Block repeated 'filterColumns' times
            s"""
              |     rowSum += columnSum;
              |  }
            """.stripMargin
          }
          else {
            // We are unrolling the inner loop here too, but with multiple filters processed for each input tile read
            val unrolled = new StringBuffer
            unrolled append commonHeader
            for (i <- 0 until filtersPerWorkGroup) {
              unrolled append s"  $convolveTensorType rowSum$i = $convolveTensorZero;\n"
              unrolled append s"  $convolveTensorType columnSum$i = $convolveTensorZero;\n"
            }
            unrolled append s"  $convolveTensorType tileValue;\n"
            unrolled append s"  for (int i = 0; i < $filterRows; i++) {\n"
            for (i <- 0 until filtersPerWorkGroup) {
              unrolled append s"      columnSum$i = $convolveTensorZero;\n"
            }
            unrolled append s"      tilePos = (_localRow * $downsampleStep + i) * $paddedLocalColumns + (_localColumn * $downsampleStep);\n"
            for (j <- 0 until filterColumns) {
              unrolled append "      tileValue = localTile[tilePos++];\n"
              for (i <- 0 until filtersPerWorkGroup)
                unrolled append "      columnSum" + i + " += tileValue * localFilter" + i + "[filterPos];\n"
              unrolled append "      filterPos++;\n"
            }
            for (i <- 0 until filtersPerWorkGroup)
              unrolled append "      rowSum" + i + " += columnSum" + i + ";\n"
            unrolled append "  }\n"
            unrolled.toString()
          }
        }
        else {
          // legacy approach that does not unroll inner loop (~10% slower)
          s"""
            |  // Multiply-add code
            |  barrier(CLK_LOCAL_MEM_FENCE);
            |  $convolveTensorType rowSum = $convolveTensorZero;
            |  $convolveTensorType columnSum = $convolveTensorZero;
            |  for (int i = 0; i < $filterRows; i++) {
            |      columnSum = $convolveTensorZero;
            |      for(int j = 0; j < $filterColumns; j++){
            |          int tilePos = (_localRow * $downsampleStep + i) * $paddedLocalColumns + (_localColumn * $downsampleStep + j);
            |          int filterPos = i * $filterColumns + j;
            |          columnSum += localTile[tilePos] * localFilter[filterPos];
            |      }
            |      rowSum += columnSum;
            |  }
          """.stripMargin

        }

      }
  }

  /** Perform output */
  val outputCode =
    if (operation.vectorMode == FilterAdjoint && batchSize > 1) {
      // Batched FilterAdjoint mode is unique in that the outputs for a given image of the batch are interleaved
      // with the outputs of other images, rather than being stacked.  The reason is that one typically wants to
      // do a reduction by the batchSize on the result and we want blockReduceSum() to do the trick rather than
      // introduce a new operation such as interleavedReduceSum().  Another motivation is that if the filterAdjoint
      // operation is performed by the ConvolveToSmallField HyperKernel, a given logical output will already be
      // a stack of results that needs to be blockReduceSummed.  By adopting the following output convention, these
      // two sums can be collapsed.
      s"""|  // Output code
          |  tensorElement = $batchSize * withinImageThreadElement + imageIndex;
          |  @outElementNonlocal0 = rowSum;
          |""".stripMargin
    }
    else if (operation.vectorMode != ProjectFrameBlockReduceSum && operation.vectorMode != BackProjectFrameBlockReduceSum) {
      s"""|  // Output code
          |  @out0 = rowSum;
          |""".stripMargin
    }
    else {
      addressMode match {
        case TensorElementAddressing =>
          if (filtersPerWorkGroup == 1) {
            """
              |      reductionAccumulator += rowSum;
              |      barrier(CLK_LOCAL_MEM_FENCE);    // Make sure all threads are done with multiply-add loop before returning to local mem write
              |  }
              |  // Output code
              |  @out0 = reductionAccumulator;
            """.stripMargin
          }
          else {
            val snippet = new StringBuilder
            for (i <- 0 until filtersPerWorkGroup)
              snippet append "  reductionAccumulator" + i + " += rowSum" + i + ";\n"
            snippet append "  barrier(CLK_LOCAL_MEM_FENCE);    // Make sure all threads are done with multiply-add loop before returning to local mem write\n"
            snippet append "}\n"
            snippet append "// Output code\n"
            snippet append "if (_row < _rows && _column < _columns) {\n"
            snippet append "    row = _row;\n"
            snippet append "    column = _column;\n"
            if (batchSize == 1) {
              for (i <- 0 until filtersPerWorkGroup) {
                snippet append s"    tensorElement = _tensorElement * $filtersPerWorkGroup + $i;\n"
                // If numLogicalFilters is not a multiple of filtersPerWorkGroup, we need to guard against writing bogus values
                if (iterationNeedsPruning(i))
                  snippet append s"    if (tensorElement < $resultTensorPoints)\n    "
                snippet append s"    @outElementNonlocal0 = reductionAccumulator$i;\n"
              }
            }
            else {
              for (i <- 0 until filtersPerWorkGroup) {
                snippet append s"    tensorElement = imageIndex * $numLogicalFilters + withinImageThreadElement * $filtersPerWorkGroup + $i;\n"
                // If numLogicalFilters is not a multiple of filtersPerWorkGroup, we need to guard against writing bogus values
                if (iterationNeedsPruning(i))
                  snippet append s"    if (withinImageThreadElement * $filtersPerWorkGroup + $i < $numLogicalFilters)\n    "
                snippet append s"    @outElementNonlocal0 = reductionAccumulator$i;\n"
              }
            }
            snippet append "}\n"
            snippet.toString
          }
        case SmallTensorAddressing =>
          require(imageTensorPoints == filterTensorPoints, "Expecting equal tensor size for image and filter")

          if (imageTensorPoints == 1)
            "// Output code\n" + "@out0 = rowSum;\n"
          else
            "// Output code\n" + "@out0 = " + TensorReduceHyperKernel.reduce("rowSum", "add", 0, imageTensorPoints) + ";\n"
        case _ =>
      }
    }

  /** Combine the various kernel code snippets into the enire kernel program */
  val convCode = setupCode + tensorElementCode + readLocalCode + borderCode + postReadLocalCode +
                 readFilterCode + multiplyAddCode + outputCode

  def indent(s:String, numSpaces: Int) = s.split('\n').mkString(" "*numSpaces, "\n" + " "*numSpaces, "")

  code append indent(convCode, indentSpaces)

  addCode(code.toString)

  val Debug = false
  if (Debug) {
    println(params.toString)
    debugCompile()
  }
}

/** Helper class to estimate the throughput of the convolution.
  *
  * We need to estimate the throughput of the convolution in both
  * SmallTensorAddressing and TensorElementAddressing modes, without actually
  * creating two separate kernels (which wire themselves into the circuit upon
  * construction).
  *
  * This helper class performs the front-end analysis of the ConvolutionHyperKernel,
  * and comes up with the workgroup parameters that can be used to estimate the
  * kernel's throughput.  This is used by the factory method to decide which
  * is the best addressing mode to use.
  *
  * @param inputs The input and filter virtual field registers driving this kernel.
  * @param operation the opcode
  * @param resultType The FieldType of the result of this kernel.
  * @param addressing The addressing mode of this kernel.
  */
private[cogx]
class ConvolutionParams (inputs: Array[VirtualFieldRegister],
                         operation: ConvolveOp,
                         resultType: FieldType,
                         val addressing: AddressingMode,
                         codeGenParams: OpenCLKernelCodeGenParams)
        extends  Logarithm
{
  private val EnableFilterInConstMemory = true
  val borderPolicy = operation.borderPolicy
  val filterOrientation = operation.filterOrientation
  val samplingPolicy = operation.samplingPolicy
  val vectorMode = operation.vectorMode
  val batchSize = operation.batchSize

  private val imageType = inputs(0).fieldType
  val imageCLType = addressing.clType(imageType)
  val imageTensorType = imageCLType.name
  val imageTensorZero = imageCLType.zero
  val imageTensorPoints = imageType.tensorShape.points
  val planesPerImage = imageTensorPoints / batchSize

  private val filterType = inputs(1).fieldType
  val filterCLType = addressing.clType(filterType)
  val filterTensorType = filterCLType.name
  val filterTensorPoints = filterType.tensorShape.points
  val numLogicalFilters =
    operation.vectorMode match {
      case FilterAdjoint => filterTensorPoints / batchSize
      case _ => filterTensorPoints / planesPerImage
    }

  val resultCLType = addressing.clType(resultType)

  // The convolveCLType is the type of the element involved in the summing loop of the convolution.  This is often the
  // resultCLType, but not for the strange case of the ProjectFrameBlockReduceSum where the result is a ScalarField, yet
  // the summing loop is performed potentially on vectors.
  val convolveCLType =
    operation.vectorMode match {
      case ProjectFrameBlockReduceSum => addressing match {
        case TensorElementAddressing => resultCLType
        case SmallTensorAddressing =>
          require(imageTensorPoints == filterTensorPoints, "Expecting equal tensor size for image and filter")
          imageCLType
        case _ => throw new RuntimeException("Unexpected addressing mode")
      }
      case BackProjectFrameBlockReduceSum => addressing match {
        case TensorElementAddressing => resultCLType
        case SmallTensorAddressing =>
          require(imageTensorPoints == filterTensorPoints, "Expecting equal tensor size for image and filter")
          imageCLType
        case _ => throw new RuntimeException("Unexpected addressing mode")
      }
      case _ => resultCLType
    }

  val convolveTensorType = convolveCLType.name
  val convolveTensorZero = convolveCLType.zero


  val expectedResultType = outputFieldType(inputs(0).fieldType,
    inputs(1).fieldType, borderPolicy, samplingPolicy, vectorMode, batchSize)

  require(expectedResultType == resultType)

  val resultTensorPoints = resultType.tensorShape.points

  // OpenCL stores float3's in a 16-byte container, i.e. effective vector
  // length = 4

  val paddedImageTensorPoints = roundUpPowerOf2(imageTensorPoints)

  val paddedFilterTensorPoints = roundUpPowerOf2(filterTensorPoints)

  val downsampleStep = samplingPolicy match {
    case DownsampleOutputConvolution(step) => step
    case _ => 1
  }

  val upsampleStep = samplingPolicy match {
    case UpsampleInputConvolution(step) => step
    case _ => 1
  }

  //define field shapes
  val inputDims = imageType.dimensions
  val inputLayers = imageType.layers
  val inputRows = imageType.rows
  val inputColumns = imageType.columns

  val filterDims = filterType.dimensions
  val filterLayers = filterType.layers
  val filterRows = filterType.rows
  val filterColumns = filterType.columns

  private val filterVolumeOneTensorPlane = filterLayers*filterRows*filterColumns
  private val globalMemoryFilterVolumeBytes =
    4 * filterTensorPoints * new FieldMemoryLayoutImpl(filterType).pageSize

  private val imagePoints = imageType.fieldShape.points

  // I found that convolution on small input fields is so fast, that other
  // factors must come into play to make constant memory use slower (additional
  // kernel launch overhead?).  Const memory use was measured to be a win for
  // all filter sizes for a 256 x 256 input and above.  -RJC
  private val constMemFilterIsAWin =
    (imagePoints >= 256 * 256) ||
            (imagePoints >= 128 * 128 && filterVolumeOneTensorPlane >= 17*17)

  // The new ProjectFrameBlockReduceSum mode is not yet compatible with the use of constant memory.
  val putFilterInConstantMemory = EnableFilterInConstMemory &&
    (operation.vectorMode != ProjectFrameBlockReduceSum) &&
    (operation.vectorMode != BackProjectFrameBlockReduceSum) &&
          constMemFilterIsAWin &&
          (samplingPolicy match {
            case UpsampleInputConvolution(step) => false
            case _ => globalMemoryFilterVolumeBytes <= codeGenParams.maxConstantBufferSize
          })

  val filterVolume =
    if (putFilterInConstantMemory)
      0
    else
      filterLayers*filterRows*filterColumns

  //                 Everything you wanted to know about "halos"

  // An output tile generally needs an expanded region of the input to complete
  // the convolution.  For a square, odd-sized filter with, say, BorderClamp
  // convolution, the "halo" region will be symmetrical and equal to
  // (filterSize-1)/2.  For some of the more exotic borderpolicies like BorderValid,
  // an output tile has no "top" and "left" halos, but has a "bottom" and "right"
  // halo equal to filterSize - 1.  One concept that remains true of all border
  // policies is that the sum of both halos from opposite sides of the
  // input tile is filterSize - 1.  Thus, we coin the term "fullHalo" to
  // represent this concept.  Further, the halo that has lower indices (e.g.
  // the "left" or "top" halos) will be called the "lowHalo".  The lowHalo and
  // and fullHalo are the two concepts needed by the kernel code generation,
  // and seem to handle even-sized filters used in BorderValid and BorderFull
  // convolution simply.  A prior version of this kernel had only the
  // half-sized halo along with a "border multiplier" that was based on the
  // border policy, but now these somewhat confusing concepts are gone.

  def lowHalo(filterSize: Int) = {
    borderPolicy match {
      case BorderFull => filterSize - 1
      case BorderValid => 0
      case _ => (filterSize-1)/2
    }
  }
  val lowHaloLayers:Int = lowHalo(filterLayers)
  val lowHaloRows:Int = lowHalo(filterRows)
  val lowHaloColumns:Int = lowHalo(filterColumns)

  val fullHaloLayers:Int = filterLayers-1
  val fullHaloRows:Int = filterRows-1
  val fullHaloColumns:Int = filterColumns-1

  //Determine local tile size
  //Assume group size is always = 256. Is there a way to detect this?
  var groupSize = 256

  val localMemBytesAvailable = codeGenParams.localMemSize

  /** Given a dimension of the workgroup in the post-sampled (i.e. output)
   * space, what is the maximum input dimension that the workgroup needs to access.
   * Note that for upsampled outputs with a step that does not divide the workgroup
   * size evenly, the calculated return value is the maximum number of elements
   * needed in the input tile.
   */
  def unsampledToPadded(unsampledEdge: Int, samplingPolicy: ConvolutionSamplingPolicy, fullHalo: Int) = {
    samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        // Round up any fractional input element
        math.ceil((unsampledEdge + fullHalo) / step.toFloat).toInt
      case DownsampleOutputConvolution(step) =>
        unsampledEdge * step + fullHalo
      case NoSamplingConvolution =>
        unsampledEdge + fullHalo
    }
  }

  // Loop over parameter-determining calculation, lowering the groupSize if
  // necessary until the local memory fits within the available size.
  // Note init to 1 is best default for some vars that are not reassigned below.

  var unsampledColumns, unsampledRows, unsampledLayers,
  paddedLocalColumns, paddedLocalRows, paddedLocalLayers,
  paddedLocalVolume, filtersPerWorkGroup = 1

  def localMemBytesNeeded = {
    val rawBytesNeeded = addressing match {
      case TensorElementAddressing => 4 * (paddedLocalVolume + filterVolume * filtersPerWorkGroup)
      case SmallTensorAddressing => 4 * (paddedLocalVolume * paddedImageTensorPoints +
        filterVolume * paddedFilterTensorPoints)
      case x => throw new RuntimeException("Internal compiler error: " +
        "unexpected addressing mode " + x)
    }
    /** Chunk size for shared memory allocations, 256 = NVidia Kepler value */
    val SharedMemoryAllocationUnitSize = 256
    HyperKernel.roundUp(SharedMemoryAllocationUnitSize, rawBytesNeeded).toInt
  }

  def desiredLocalMemoryOK = localMemBytesNeeded <= localMemBytesAvailable
  
  do {
    val perfectColumnEdge = math.pow(groupSize.toFloat, 1f/inputDims.toFloat).toFloat
    unsampledColumns = roundDownPowerOf2(perfectColumnEdge)
    paddedLocalColumns = unsampledToPadded(unsampledColumns, samplingPolicy, fullHaloColumns)

    if (inputDims > 1) {
      val perfectRowEdge = math.pow(groupSize.toFloat/unsampledColumns, 1f/(inputDims-1).toFloat).toFloat
      unsampledRows = roundDownPowerOf2(perfectRowEdge)
      paddedLocalRows = unsampledToPadded(unsampledRows, samplingPolicy, fullHaloRows)
    }

    if (inputDims > 2) {
      val perfectLayerEdge = groupSize.toFloat/(unsampledColumns * unsampledRows)
      unsampledLayers = roundDownPowerOf2(perfectLayerEdge)
      paddedLocalLayers = unsampledToPadded(unsampledLayers, samplingPolicy, fullHaloLayers)
    }

    paddedLocalVolume = paddedLocalLayers*paddedLocalRows*paddedLocalColumns

    if (!desiredLocalMemoryOK)
      groupSize /= 2

  } while (!desiredLocalMemoryOK && groupSize > 0)
  
  // The above while-loop picked a groupSize assuming filtersPerWorkGroup == 1
  // Now let's see if more filters will fit in shared memory, assuming we
  // are in a vectorMode that desires that:

  if (Cog.fastProjectFrame &&
    (operation.vectorMode == ProjectFrameBlockReduceSum || operation.vectorMode == BackProjectFrameBlockReduceSum) &&
      addressing == TensorElementAddressing) {
    filtersPerWorkGroup = maxOutputPlanesPerWorkGroup(filterVolumeOneTensorPlane)

    def paramsOK = {
      val EnoughWorkGroups = 30 // = 15 SM's * 2 WorkGroups per SM, for reasonable occupancy of a NVidia Titan Black
      val trialWorkFieldType = calcWorkFieldType(resultType, filtersPerWorkGroup, batchSize)
      val trialWorkGroup = HyperKernel.computeWorkGroupParameters(trialWorkFieldType, addressing,
        unsampledRows, unsampledColumns)
      // The number of filters per workgroup is also the number of output planes (i.e. resultType tensor elements)
      // We don't need to process 8 output planes per workgroup if there are only 4, for example.
      val ok = resultType.tensorShape.points >= filtersPerWorkGroup &&
        trialWorkGroup.workGroups >= EnoughWorkGroups && desiredLocalMemoryOK
      ok
    }
    while (filtersPerWorkGroup > 1 && !paramsOK)
      filtersPerWorkGroup -= 1
  }

  // We estimate the kernel's throughput by considering GPU occupancy: the
  // number of concurrent workgroups that can run on a "compute unit" (known
  // in NVidia parlance as a "streaming multiprocessor" or "SM".  This may
  // be limited by shared memory or register use.  The NVidia Kepler GPU's have
  // a limit of 16, the Fermi's 8.  We don't have a good way of knowing the
  // register-use-driven limit, so we just set up a constant to reflect both
  // hardware and register use limits:

  val MaxConcurrentWorkGroups = 12   // Don't change without rerunning regression
  val concurrentWorkGroups =
    math.min(MaxConcurrentWorkGroups,
      localMemBytesAvailable / localMemBytesNeeded).toInt

  // Throughput estimated from a heuristic based on timed kernels.  In general,
  // the SmallTensorAddressing kernel was a win, but not always.  The problem
  // is that the SmallTensorAddressing has higher SharedMemory requirements, and
  // this can limit GPU occupancy.  Also, for large filters, the groupSize may
  // have to be lowered to get the input tile to fit.
  val throughput = addressing match {
    case TensorElementAddressing =>
      (concurrentWorkGroups * groupSize * (1.0f / resultType.tensorShape.points)).toInt
    case SmallTensorAddressing => concurrentWorkGroups * groupSize
    case x => throw new RuntimeException("Internal compiler error: " +
            "unexpected addressing mode " + x)
  }

  /** Some important parameters of the class, for debugging. */
  override def toString: String =
  s"""
     |  groupSize: $groupSize
     |  unsampledLayers: $unsampledLayers
     |  unsampledRows: $unsampledRows
     |  unsampledColumns: $unsampledColumns
     |  inputLayers: $inputLayers
     |  inputRows: $inputRows
     |  inputCols: $inputColumns
     |  filterDims: $filterDims
     |  filterLayers: $filterLayers
     |  filterRows: $filterRows
     |  filterCols: $filterColumns
     |  filterVolume: $filterVolume
     |  lowHaloLayers: $lowHaloLayers
     |  lowHaloRows: $lowHaloRows
     |  lowHaloCols: $lowHaloColumns
     |  paddedLocalLayers: $paddedLocalLayers
     |  paddedLocalRows: $paddedLocalRows
     |  paddedLocalColumns: $paddedLocalColumns
     |  paddedLocalVolume: $paddedLocalVolume
     |  downsampleStep: $downsampleStep
     |  upsampleStep: $upsampleStep
   """.stripMargin
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ConvolveHyperKernel {

  /** Create a kernel that convolves a field with a dynamic filter field.
    *
    * @param inputs The input field and the filter field driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @param smallTensorUse Policy on use of small tensors by the kernel.
    * @param codeGenParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @return The synthesized hyperkernel.
    *
    */
  def apply(inputs: Array[VirtualFieldRegister], operation: ConvolveOp, resultType: FieldType,
            smallTensorUse: ConvolutionSmallTensorUsePolicy,
            codeGenParams: OpenCLKernelCodeGenParams): HyperKernel = {

    import operation._
    // BorderValid convolutions or crossCorrelations in filter adjoint mode
    // are handled by a different kernel. Technically, this other kernel could
    // handle ScalarField convolve ScalarField in Bordervalid mode to small outputs
    // better, but we haven't special-cased that situation here yet.

    def stdConvolve = {
      val bestParameters = bestParams(inputs, operation, resultType, smallTensorUse, codeGenParams)
      val bestAddressMode = bestParameters.addressing
      new ConvolveHyperKernel(inputs, operation, resultType, bestAddressMode, bestParameters)
    }

    // FilterAdjoint instances with upsampling (due to a strided forward cross-correlation) are more efficiently
    // performed via a matrix multiply of the backpropped gradient field by a "toeplitz matrix" that is a
    // function of the image input.
    def crossCorrelateViaMatrixMultiply = {
      val gradientField = inputs(1)
      val gradientFieldType = gradientField.fieldType
      val gradientPoints = gradientFieldType.fieldShape.points
      val gradientTensorPoints = gradientFieldType.tensorShape.points
      require(gradientTensorPoints % batchSize == 0,
        s"Expecting gradient field vector depth $gradientTensorPoints to be a multiple of $batchSize.")
      val logicalFilters = gradientFieldType.tensorShape.points / batchSize
      val inputFeatures = inputs(0).fieldType.tensorShape.points / batchSize
      val resultPoints = resultType.fieldShape.points
      require(samplingPolicy.isInstanceOf[UpsampleInputConvolution] && operation.filterOrientation == CrossCorrelationOrientation)
      // Toeplitz matrix input can be quite large- do matrix multiply in pieces and sum the result
      // Swag value 4 below based on AlexNet.
      val chunksToMinimizeFootprint = 4
      val toeplitzMatrixSize = toeplitzMatrixSizeBytes(inputs, operation, resultType.fieldShape).toInt
      val chunksToEnsureSuccessfulAllocation = math.ceil(toeplitzMatrixSize.toDouble / codeGenParams.maxMemAllocSize).toInt
      // chunks cannot be more than batchSize, then should ensure successful allocation, finally small footprint
      val chunks = math.min(batchSize, math.max(chunksToMinimizeFootprint, chunksToEnsureSuccessfulAllocation))
      val partialResults = Array.tabulate(chunks) { chunk => {
        val fromImage = batchSize * chunk/ chunks
        val untilImage = math.min(batchSize * (chunk+1) / chunks, batchSize)
        val numImages = untilImage - fromImage
        // Shuffle planes of the "dY" input to group together same-feature planes rather than same-image planes.
        val shuffled =
          if (batchSize > 1)
            FilterAdjointShuffleHyperKernel(Array(gradientField),FilterAdjointShuffleOp(batchSize, fromImage, untilImage),
              gradientFieldType.resizeTensor(Shape(numImages*logicalFilters))).outputs(0)
          else
            gradientField
        // Reshape the "dY" input to a 0D MatrixField as needed for the Matrix multiply
        val dYreshaped = ReshapeHyperKernel(shuffled,ReshapeOp(Shape(),Shape(logicalFilters, gradientPoints * numImages),false)).outputs(0)
        val toeplitzResultType =
          new FieldType(Shape(),Shape(gradientPoints * numImages, inputFeatures * resultPoints), inputs(0).fieldType.elementType)
        val toeplitzOp =
          FilterAdjointToeplitzOp(borderPolicy, filterOrientation, samplingPolicy, batchSize, fromImage, untilImage, resultType.fieldShape)
        val toeplitzMatrix = FilterAdjointToeplitzHyperKernel(inputs(0), toeplitzOp, toeplitzResultType).outputs(0)
        val transformInputs = Array(dYreshaped,toeplitzMatrix)
        val transformResultType =
          new FieldType(Shape(),Shape(logicalFilters,inputFeatures*resultPoints), inputs(0).fieldType.elementType)
        // Perform the filter adjoint operation as a matrix multiply that includes a reduction by the batchSize
        val unShapedResult = MatrixMatrixTransformHyperKernel(transformInputs,
          MatrixTransformMatrixOp(false, false), transformResultType, codeGenParams).outputs(0)
        ReshapeHyperKernel(unShapedResult, ReshapeOp(resultType.fieldShape, resultType.tensorShape, false))
      }}

      def sumPartialResults(fromIndex: Int, untilIndex: Int): HyperKernel = {
        if (fromIndex + 1 > untilIndex)
          throw new RuntimeException("Internal error: sumPartialResults reduction wierdness.")
        else if (fromIndex + 1 == untilIndex)
          partialResults(fromIndex)
        else {
          val midPoint = (fromIndex + untilIndex)/2
          val firstOperand = sumPartialResults(fromIndex, midPoint).outputs(0)
          val secondOperand = sumPartialResults(midPoint, untilIndex).outputs(0)
          BinaryHyperKernel(Array(firstOperand, secondOperand), AddOp, resultType)
        }
      }
      sumPartialResults(0, chunks)
    }

    borderPolicy match {
      case BorderValid =>
        vectorMode match {
          case FilterAdjoint =>
            val gradientField = inputs(1)
            val gradientFieldType = gradientField.fieldType
            val gradientPoints = gradientFieldType.fieldShape.points
            val gradientTensorPoints = gradientFieldType.tensorShape.points
            require(gradientTensorPoints % batchSize == 0,
              s"Expecting gradient field vector depth $gradientTensorPoints to be a multiple of $batchSize.")
            val resultPoints = resultType.fieldShape.points
            // The ConvolveToSmallFieldPipelinedHyperKernel was designed for large filters that
            // don't fit in local memory.  If the filter fits in local memory and particularly if
            // the result field is bigger than the filter, the std convolve kernel should be used.
            // Upsampling in filteradjoint mode is defined differently than the way it's
            // supported by the standard convolution kernel.  Avoid std convolve kernel in this case.
            if (samplingPolicy == NoSamplingConvolution &&
              (gradientPoints <= 7*7 ||
               gradientPoints <= 17*17 && gradientPoints < resultPoints))
              stdConvolve
            // The pipelined convolution kernels were seen to fail the regression tests for an AMD 6470m.
            // This is how we disabled use of those kernels until a fix was instituted.  The fix is not
            // completely satisfying: it involves putting some extra padding around 2 of the local memories
            // in the affected kernels.
//            else if (!platformParams.isNVidia)
//              ConvolveToSmallFieldHyperKernel(inputs, operation, resultType, platformParams)
            // The ConvolveTosSmallField kernels are not that efficient for upsampled inputs.  We found
            // that performing the convolution as a matrix-multiply by the Toeplitz matrix is faster.
            else if (batchSize == 1 && canUseFilterAdjointBlockReduceSum(inputs, operation, resultType.fieldShape, codeGenParams))
              crossCorrelateViaMatrixMultiply
            else if (ConvolveToSmallFieldPipelinedTiledHyperKernel.isRecommended(inputs))
              ConvolveToSmallFieldPipelinedTiledHyperKernel(inputs, operation, resultType, codeGenParams)
            else
              ConvolveToSmallFieldPipelinedHyperKernel(inputs, operation, resultType, codeGenParams)
            // Older version of kernel.  This can be removed after we have
            // "field tested" (pun intended) the tricky pipelined version above.
//            else
//              ConvolveToSmallFieldHyperKernel(inputs, operation, resultType, codeGenParams)

          case FilterAdjointBlockReduceSum =>
            // The optimizer that creates this vector mode should already have performed this check.
            require(canUseFilterAdjointBlockReduceSum(inputs, operation, resultType.fieldShape, codeGenParams),
              s"Internal error: filterAdjoint via matrix multiply not possible to generate $resultType")
            crossCorrelateViaMatrixMultiply
          case _ =>
            // Certain convolutions with big filters may not be possible with the stdConvolve kernel if their
            // minimum local memory needs exceed the amount of local memory of the GPU.  The
            // ConvolveToSmallField* kernels may help here, but they apply upsampling to the second input, not
            // the first as needed here.  So, if there is no upsampling/downsampling and the std convolve is
            // having trouble, try the ConvolveToSmallField* kernels.
            try {
              stdConvolve
            }
            catch {
              case e: RuntimeException =>
                if (samplingPolicy == NoSamplingConvolution && ConvolveToSmallFieldPipelinedTiledHyperKernel.isRecommended(inputs))
                  ConvolveToSmallFieldPipelinedTiledHyperKernel(inputs, operation, resultType, codeGenParams)
                else if (samplingPolicy == NoSamplingConvolution)
                  ConvolveToSmallFieldPipelinedHyperKernel(inputs, operation, resultType, codeGenParams)
                else
                  throw e
            }
        }
      case _ => stdConvolve
    }
  }

  /** Create a kernel that convolves a field with a dynamic filter field.
    *
    * @param inputs The input field and the filter field driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultFieldShape The FieldShape of the result of this kernel.
    * @param codeGenParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @return The synthesized hyperkernel.
    *
    */
  def canUseFilterAdjointBlockReduceSum(inputs: Array[VirtualFieldRegister], operation: AbstractConvolveOp, resultFieldShape: Shape,
            codeGenParams: OpenCLKernelCodeGenParams): Boolean = {

    import operation._

    val resultPoints = resultFieldShape.points

    // A flag that should be hand-set false during characterization of this approach
    val enablePruningBasedOnStride = true

    if (borderPolicy == BorderValid && (vectorMode == FilterAdjoint || vectorMode == FilterAdjointBlockReduceSum)) {
      samplingPolicy match {
        case UpsampleInputConvolution(step) =>
          filterOrientation == CrossCorrelationOrientation &&
            (!enablePruningBasedOnStride || step > 2 || step == 2 && resultPoints > 5*5)  // Tuning swag based on TitanX and 1080
        case NoSamplingConvolution => false
        case x: DownsampleOutputConvolution => false
      }
    }
    else
      false
  }
  /** The size of the Toeplitz matrix in bytes to perform convolution via matrix multiply.
    *
    * @param inputs The input field and the filter field driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultFieldShape The FieldShape of the result of this kernel.
    * @return The size of the Toeplitz matrix in bytes.
    *
    */
  def toeplitzMatrixSizeBytes(inputs: Array[VirtualFieldRegister], operation: AbstractConvolveOp,
                              resultFieldShape: Shape): Long = {

    import operation._

    val resultPoints = resultFieldShape.longPoints

    // We often call the 2nd field argument of a convolution the "filter", but in FilterAdjoint mode
    // this can be confusing- the 2nd argument is the gradient field whose FieldType matches the output
    // Fieldtype of the corresponding forward ProjectFrame operation.  The output field of the
    // FilterAdjoint convolution is the back-propped gradient field whose FieldType matches the filter
    // field of the ProjectFrame operation
    val gradientFieldType = inputs(1).fieldType
    val gradientPoints = gradientFieldType.fieldShape.points
    val gradientTensorPoints = gradientFieldType.tensorShape.points
    require(gradientTensorPoints % batchSize == 0,
      s"Expecting gradient field vector depth $gradientTensorPoints to be a multiple of $batchSize.")
    val inputFeatures = inputs(0).fieldType.tensorShape.points / batchSize
    // Before using the matrix-multiply approach, make sure the toeplitz matrix is not too big
    val toeplitzMatrixRows = gradientPoints * batchSize
    val toeplitzMatrixColumns = inputFeatures * resultPoints
    val toeplitzResultBytes = toeplitzMatrixRows * toeplitzMatrixColumns * 4
    toeplitzResultBytes
  }

  /** Certain vectorModes, like ProjectFrameBlockReduceSum, process a number of output planes per workgroup.  This
    * parameter sets the maximum planes a workgroup will attempt to process in an attempt to avoid kernels with
    * too many registers and hence poor GPU occupancy.  The numbers 6 and 16 were arrived at after a regression
    * against both Fermi and Kepler bricks for filter sizes 1, 3, 5, 7 and 9.
    */
  def maxOutputPlanesPerWorkGroup(filterPoints: Int) =
    if (filterPoints == 1) 16 else 6

  /** Given a kernel that processes `blockingFactor` output planes per workGroup, what is the workField? */
  def calcWorkFieldType(resultType: FieldType, blockingFactor: Int, batchSize: Int) = {
    val resultPointsPerInput = resultType.tensorShape.points / batchSize
    val workTensorPointsPerInput = (resultPointsPerInput + blockingFactor - 1)/ blockingFactor
    val workTensorPoints = workTensorPointsPerInput * batchSize
    resultType.resizeTensor(Shape(workTensorPoints))
  }

  /** Return the ConvolutionParams for the predicted best (fastest) addressing mode
    *
    * @param inputs The input and filter virtual field registers driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @param smallTensorUse Policy on use of small tensors by the kernel.
    * @return The ConvolutionParams for the predicted best.
    *
    */
  def bestParams(inputs: Array[VirtualFieldRegister], operation: ConvolveOp, resultType: FieldType,
                 smallTensorUse: ConvolutionSmallTensorUsePolicy, codeGenParams: OpenCLKernelCodeGenParams) = {
    lazy val tensorElementAddressingParams =
      new ConvolutionParams(inputs, operation, resultType, TensorElementAddressing, codeGenParams)
    lazy val smallTensorAddressingParams =
      new ConvolutionParams(inputs, operation, resultType, SmallTensorAddressing, codeGenParams)
    if (operation.batchSize > 1)
      tensorElementAddressingParams
    else
      operation.vectorMode match {
        case ProjectFrame =>
          tensorElementAddressingParams
        case ProjectFrameBlockReduceSum =>
          if (isTensor0Field(resultType)) smallTensorAddressingParams else tensorElementAddressingParams
        case BackProjectFrame =>
          tensorElementAddressingParams
        case BackProjectFrameBlockReduceSum =>
          if (isTensor0Field(resultType)) smallTensorAddressingParams else tensorElementAddressingParams
        case FilterAdjoint =>
          tensorElementAddressingParams
        case PlaneByPlane =>
          if (!isSmallTensorField(resultType))
            tensorElementAddressingParams       // Can't use SmallTensorAddressing when result field is not a SmallTensor
          else if (isTensor0Field(resultType))
            smallTensorAddressingParams         // Can't use TensorElementAddressing when result field is a ScalarField
          else {
            smallTensorUse match {
              case UseSmallTensorNever => tensorElementAddressingParams
              case UseSmallTensorAlways => smallTensorAddressingParams
              case UseSmallTensorWhenBest =>
                if (smallTensorAddressingParams.throughput >
                  tensorElementAddressingParams.throughput)
                  smallTensorAddressingParams
                else if (smallTensorAddressingParams.throughput ==
                  tensorElementAddressingParams.throughput) {
                  smallTensorAddressingParams
                }
                else
                  tensorElementAddressingParams
            }
          }
        case FilterAdjointBlockReduceSum =>
          throw new RuntimeException(s"Internal compiler error: invalid vector mode ${operation.vectorMode} seen.")
      }
  }

  /**
   * A method for determining the FieldType of the output of convolution
   *
   * @param input the input field
   * @param filter the filter field
   * @param borderPolicy the BorderPolicy used for the convolution operation
   * @param samplingPolicy the ConvolutionSamplingPolicy used for the convolution
   * @param vectorMode how to mix the planes of a VectorField-VectorField convolution
   * @param batchSize number of logical multi-plane "images" in the input field
   * @author Matthew Pickett
   *
   */
  def outputFieldType(input:FieldType, filter:FieldType,
          borderPolicy:BorderPolicy, samplingPolicy:ConvolutionSamplingPolicy,
          vectorMode: VectorMode, batchSize: Int) = {

    val inputShape = input.fieldShape
    val filterShape = filter.fieldShape
    val dims = inputShape.dimensions
    val inputSizes = inputShape.toArray
    val filterSizes = filterShape.toArray
    val inputTensorPoints = input.tensorShape.points
    val filterTensorPoints = filter.tensorShape.points
    val planesPerImage = inputTensorPoints / batchSize

    val upsampleInputSizes = samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        vectorMode match {
          case FilterAdjoint => inputSizes
          case FilterAdjointBlockReduceSum => inputSizes
          case _ => inputSizes.map(_ * step)
        }
      case _ => inputSizes
    }

    val upsampleFilterSizes = samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        vectorMode match {
          case FilterAdjoint => filterSizes.map(_ * step)
          case FilterAdjointBlockReduceSum => filterSizes.map(_ * step)
          case _ => filterSizes
        }
      case _ => filterSizes
    }

    val borderSizes = borderPolicy match {
      case BorderValid => Array.tabulate(dims){
        (i) => upsampleInputSizes(i) - upsampleFilterSizes(i) + 1 }
      case BorderFull => Array.tabulate(dims){
        (i) => upsampleInputSizes(i) + upsampleFilterSizes(i) - 1 }
      case _ => upsampleInputSizes
    }

    // The DynamicConvolutionGenerator rejects pre-downsampled field sizes
    // that are not a multiple of the step.  The following treatment gives
    // the same answer for those cases, but rounds up otherwise.

    val downsampleSizes = samplingPolicy match {
      case DownsampleOutputConvolution(step) =>
        borderSizes.map(size => math.ceil(size.toDouble / step).toInt)
      case _ => borderSizes
    }

    /** the FieldShape of the output field */
    val shape = Shape(downsampleSizes)

    def frameInputChecks {
      require(inputTensorPoints % batchSize == 0,
        s"Input tensorshape points $inputTensorPoints must be a multiple of the batchSize $batchSize")
      require(input.elementType == Float32 && filter.elementType == Float32)
      require(input.tensorOrder < 2 && filter.tensorOrder < 2, s"Frame convolve operations not supported on MatrixFields")
      require(filter.tensorShape.points % planesPerImage == 0,
        s"filter tensor length $filterTensorPoints not a multiple of per-image tensor length $planesPerImage")
    }

    /** input field checks, a bit different for each VectorMode */
    vectorMode match {
      case ProjectFrame => frameInputChecks
      case ProjectFrameBlockReduceSum => frameInputChecks
      case BackProjectFrame => frameInputChecks
      case BackProjectFrameBlockReduceSum =>  frameInputChecks
      case FilterAdjoint =>
        require(filterTensorPoints % batchSize == 0,
          s"Filter tensorshape points $filterTensorPoints must be a multiple of the batchSize $batchSize")
        require(input.elementType == Float32 &&
                filter.elementType == Float32)
        require(input.tensorOrder < 2 && filter.tensorOrder < 2)
      case FilterAdjointBlockReduceSum =>
        require(filterTensorPoints % batchSize == 0,
          s"Filter tensorshape points $filterTensorPoints must be a multiple of the batchSize $batchSize")
        require(input.elementType == Float32 &&
                filter.elementType == Float32)
        require(input.tensorOrder < 2 && filter.tensorOrder < 2)
      case PlaneByPlane =>
        // Before batching was added, Cog's standard convolve operator could handle
        // a VectorField convolve ScalarField, and a ScalarField convolve VectorField.
        // This behavior is the same as ProjectFrame in some cases.  We ignore batchSize
        // for PlaneByPlane mode so as not to disturb the previous behavior and blur
        // the distinction between further between PlaneByPlane and ProjectFrame.
        require(input.tensorOrder == 0 ||
          filter.tensorOrder == 0 ||
          input.tensorShape == filter.tensorShape)
    }

    val outputTensorShape = vectorMode match {
      case ProjectFrame =>
        Shape(filter.tensorShape.points * batchSize)
      case ProjectFrameBlockReduceSum =>
        val outputTensorPoints = batchSize * (filter.tensorShape.points / planesPerImage)
        // Somewhat awkwardly, this kernel returns a VectorField of length 1 if the
        // filter and input are of equal tensor lengths, but not small tensors.
        // It was too disruptive to the ConvolveHyperKernel code to handle this corner case
        // more elegantly (by generating a ScalarField, as it does for SmallTensor inputs).
        if (outputTensorPoints == 1 && isSmallTensorField(input))
          Shape()
        else
          Shape(outputTensorPoints)
      case BackProjectFrame =>
        Shape(filter.tensorShape.points * batchSize)
      case BackProjectFrameBlockReduceSum =>
        val outputTensorPoints = batchSize * (filter.tensorShape.points / planesPerImage)
        // Somewhat awkwardly, this kernel returns a VectorField of length 1 if the
        // filter and input are of equal tensor lengths, but not small tensors.
        // It was too disruptive to the ConvolveHyperKernel code to handle this corner case
        // more elegantly (by generating a ScalarField, as it does for SmallTensor inputs).
        if (outputTensorPoints == 1 && isSmallTensorField(input))
          Shape()
        else
          Shape(outputTensorPoints)
      case FilterAdjoint =>
        // Both image and 2nd "filter" inputs are expanded by the batchSize.  Output
        // has a single `batchSize` factor in its length.
        val planesPerFilter = filterTensorPoints / batchSize
        val outputTensorPoints = planesPerImage * planesPerFilter * batchSize
        if (outputTensorPoints > 1)
          Shape(outputTensorPoints)
        else
          Shape()
      case FilterAdjointBlockReduceSum =>
        // Both image and 2nd "filter" inputs are expanded by the batchSize.  Output
        // has no `batchSize` factor in its length.
        val planesPerFilter = filterTensorPoints / batchSize
        val outputTensorPoints = planesPerImage * planesPerFilter
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
    /** If either image or filter is complex, then the output will be too,
      * although the FFT kernels will be used instead of this kernel. */
    val outputElementType =
      if (input.elementType == Complex32 ||
              filter.elementType == Complex32)
        Complex32
      else
        Float32

    /** the FieldType of the output field */
    val fieldType = new FieldType(shape, outputTensorShape, outputElementType)
    fieldType
  }

}
