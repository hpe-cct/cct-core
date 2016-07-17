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
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types._
import cogx.cogmath.algebra.real.Logarithm
import cogx.compiler.parser.op._
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.ConvolveOp


/** Convolve a field with a dynamic filter field.
  *
  * This kernel performs convolution and correlation similar to the
  * ConvolveHyperKernel, but performs the calculation with a different
  * arrangement of threads.  This kernel is used when the convolution produces
  * a small output field in relation to the size of the input, which can only
  * happen if BorderValid convolution is performed with closely-sized inputs.
  *
  * The rationale behind this alternate approach is that when the output is small,
  * the standard practice of assigning a thread to each output element results
  * in too-few threads to keep the GPU's ALUs busy.  Instead, the second input
  * is tiled into 16x16 workgroups of threads that perform a multiplication with a
  * corresponding input tile, followed by a summing reduction within the workgroup
  * (much like that performed by the ScalarReduceHyperKernel).  This reduction
  * does not fully cover the needed dot-product of the operands, so a further
  * block reduce kernel is invoked as a final stage.
  *
  * If Upsampling is specified, it is assumed to apply to the 2nd field before
  * the convolution is performed.
  *
  * @author Dick Carter
  * @param inputs The input field and the filter field driving this kernel.
  * @param operation the opcode
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @param params Workgroup size and other parameters of the convolution.
  */
private[cogx]
class ConvolveToSmallFieldPipelinedHyperKernel private (inputs: Array[VirtualFieldRegister],
                                          operation: ConvolveOp,
                                          resultType: FieldType,
                                          addressMode: AddressingMode,
                                          params: ConvolutionToSmallFieldParams)
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

  require(borderPolicy == BorderValid, s"Expecting BorderValid border policy, found $borderPolicy")

  require(groupSize > 1,
    s"Could not fit inputTile and filter ($localMemBytesNeeded bytes)" +
            s" in available local memory ($localMemBytesAvailable bytes)")

  // We base the "workfield" FieldShape on the 2nd input's FieldShape. We might then have a workfield
  // tensorShape dictated by the tensor length of the first input (call this 'N'), or the length of the second
  // length of the second input (call this 'K'), or the length of the output (here N*K).


  // The choice between N, K and N*K for the number of layers is not obvious.
  // The issue is whether we cache the 1st input, the 2nd input, or neither.
  // The choice affects the amount of data read in and the total number of
  // threads executing.  Let 'I' be the size of the input image, which we'll
  // say is roughly the size of the 2nd input.  We'll also ignore the halo
  // expansion and assume the sampling step is 1.  Then, the following table
  // summarizes the choice:

  //                      Data read in             Number of threads

  //  cache 1st input     (N + K*N) * I                  N * I
  //
  //  cache 2st input     (N*K + K) * I                  K * I
  //
  //  cache neither input (K*N + K*N) * I                K * N * I
  //
  // As a first cut, we'll assume that the kernel is compute-bound and that
  // having the largest number of workgroups, and then perhaps GPU occupancy,
  // is best.  Other issues to think about here are the scaling to small
  // inputs, where the number of workgroups is small and doesn't fill all the
  // GPU's SMs, and the effect of the GPU's L2 cache.

  // A side-experiment confirms that cacheing the first input (by having a workgroup
  // process all uses of the first input) is not worth the extra code
  // complexity, as it improves the performance a negligible 2%.

  // As a side note, assume a 16x16 workgroup that outputs a 5x5 filter bank.
  // The input tile footprint in local memory will be 20*20*4 = 1.6 KB and the
  // reduction local memory footprint will be 256*4 = 1 KB.  Thus the total
  // local memory needs of 2.6 KB will permit the maximum of 8 workgroups to
  // run on an NVIDIA SM concurrently, and live under the 48 KB maximum.  Having
  // an upsample step of 2 will only slightly limit the number of workgroups on
  // an SM to 7.

  override lazy val workFieldType = filterType.resizeTensor(Shape(planesPerImage * numLogicalFilters * batchSize))
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressing, localRows, localColumns)

  private val localWorkSize = workGroup.localWorkSize


  // Pipelined reduction occurs in a memory twice the size as the number of threads participating
  require(workGroupTileVolume == 2 * localWorkSize)

  require(isPowerOf2(localWorkSize))
  /** Number of initial iterations with no output yet ready.  For a
    * localWorkSize of 256, this is 7. */
  val pipeDepth = log2(localWorkSize).toInt - 1
  val resultPoints = resultType.fieldShape.points
  val resultColumns = resultType.columns

  val code = new StringBuffer

  //define and allocate the local tile and its offset relative to the origin of
  //the input field
  code append s"    __local float localTile[$paddedLocalVolume];\n"
  code append s"        __local volatile float mem0[$workGroupTileVolume+$AMDbugPipelinedReductionMemPadding];\n"
  code append s"        __local volatile float mem1[$workGroupTileVolume+$AMDbugPipelinedReductionMemPadding];\n"
  // In order for thread 1 to execute the same code lines as the rest of the
  // workgroup, we set up pointers to the memories that can be manipulated
  // differently for thread 1.  Here is the default (non-thread-1) setup:
  code append s"        __local volatile float *reduceMem0 = mem0;\n"
  code append s"        __local volatile float *reduceMem1 = mem1;\n"

  val resultMemSize = resultType.fieldShape.points
  code append s"        __local volatile float results[$resultMemSize];\n"


  code append "    int logicalOutputPlane = _tensorElement;\n"

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
  val setTensorElement: String =
    operation.vectorMode match {
      case ProjectFrame =>
        s"""
           |        tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
           |""".stripMargin
      case ProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case BackProjectFrame =>
        s"""
           |        tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
           |""".stripMargin
      case BackProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case FilterAdjoint =>
        s"""
           |        tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
           |""".stripMargin
      case PlaneByPlane =>
        s"          tensorElement = logicalOutputPlane;\n"
    }
  code append setTensorElement

  inputDims match {
    case 2 =>
    case x => throw new RuntimeException("Kernel " + this + " does not support " + x + "D input fields.")
  }

  code append s"        int groupStartRow = get_group_id(1) * ${localRows * upsampleStep};\n"
  code append s"        int groupStartColumn = get_group_id(0) * ${localColumns * upsampleStep};\n"

  /** First part of the loop to read in the input image tile */

  // Most workgroups do not need the boundary checks, which are part of the inner
  // loop.  Best then is to see which workgroups need the checks and invoke the
  // slower boundary-check read-in code only for those workgroups.

  code append s"    // Input image tile read-in\n"
  code append s"    int threadIndex = _localColumn + _localRow * _localColumns;\n"
  code append s"    if (groupStartColumn + $paddedLocalColumns < $inputColumns &&\n"
  code append s"        groupStartRow + $paddedLocalRows < $inputRows) {\n"
                       //  no-boundary-check code
  code append s"        for (int localTileIndex = threadIndex; localTileIndex < $paddedLocalVolume; localTileIndex += $groupSize) {\n"
  code append s"            int tileRow = localTileIndex / $paddedLocalColumns;\n"
  code append s"            int tileColumn = localTileIndex - tileRow * $paddedLocalColumns;\n"
  code append s"            row = groupStartRow + tileRow;\n"
  code append s"            column = groupStartColumn + tileColumn;\n"
  code append s"            localTile[localTileIndex] = readNonlocal(@in0);\n"
  code append s"        }\n"
  code append s"    } else {\n"
                       // with-boundary-check code
  code append s"        for (int localTileIndex = threadIndex; localTileIndex < $paddedLocalVolume; localTileIndex += $groupSize) {\n"
  code append s"            int tileRow = localTileIndex / $paddedLocalColumns;\n"
  code append s"            int tileColumn = localTileIndex - tileRow * $paddedLocalColumns;\n"
  code append s"            row = groupStartRow + tileRow;\n"
  code append s"            column = groupStartColumn + tileColumn;\n"
  code append s"            if (row < $inputRows && column < $inputColumns)\n"
  code append s"                localTile[localTileIndex] = readNonlocal(@in0);\n"
  code append s"            else\n"
  code append s"                localTile[localTileIndex] = 0.0f;\n"
  code append s"        }\n"
  code append s"    }\n"

  // Adjust setting of tensorElement for filter read in, as needed
  val postReadLocalCode =
    operation.vectorMode match {
      case ProjectFrame =>
        if (batchSize == 1)
          "  tensorElement = logicalOutputPlane;\n"
        else
          s"  tensorElement = logicalOutputPlane % $filterTensorPoints;\n"
      case ProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case BackProjectFrame =>
        s"  tensorElement = $numLogicalFilters * (withinImageThreadElement % $planesPerImage) + withinImageThreadElement / $planesPerImage;\n"
      case BackProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case FilterAdjoint =>
        s"  tensorElement = withinImageThreadElement / $planesPerImage + imageIndex * $numLogicalFilters;\n"
      case PlaneByPlane => "" // tensorElement already set up properly above
    }
  code append postReadLocalCode

  /** Read in the filter, each thread gets its own value */
  code append "    // Filter tile read-in to thread-private `filterVal` variable\n"
  filterOrientation match {
    case CrossCorrelationOrientation =>
      code append "    float filterVal = 0.0f;\n"
      code append s"    if (_row < $filterRows && _column < $filterColumns) {\n"
      code append "        row = _row;\n"
      code append "        column = _column;\n"
      code append "        filterVal = readNonlocal(@in1);\n"
      code append "    }\n"
    case ConvolutionOrientation =>
      code append "    float filterVal = 0.0f;\n"
      code append s"    if (_row < $filterRows && _column < $filterColumns) {\n"
      code append s"        row = ${filterRows - 1} - _row;\n"
      code append s"        column = ${filterColumns - 1} - _column;\n"
      code append "        filterVal = readNonlocal(@in1);\n"
      code append "    }\n"
  }
  // Adjust setting of tensorElement for output
  val preMultiplyAddCode =
    operation.vectorMode match {
      case FilterAdjoint =>
        s"tensorElement = ($batchSize * withinImageThreadElement + imageIndex) * $numFilterTiles + get_group_id(1) * get_num_groups(0) + get_group_id(0);\n"
      case _ =>
        s"tensorElement = logicalOutputPlane * $numFilterTiles + get_group_id(1) * get_num_groups(0) + get_group_id(0);\n"
    }
  code append preMultiplyAddCode

  /** Perform the actual convolution/correlation */

  // The following is a merged form of the routine that works for all sampling
  // modes.  Remember that at most one of %upsampleStep% and %downsampleStep%
  // will be > 1 for any given sampling mode.
  // The first barrier(CLK_LOCAL_MEM_FENCE) is doing double-duty: it synchronizes
  // between the initialization of localTile and the first read, but also between
  // the final read of reductionMem[0] at the bottom of the loop, and the start
  // of the next reduction iteration, which re-initializes reductionMem[0]

  filterOrientation match {
    case CrossCorrelationOrientation =>
      code append "    int firstProductiveElementIndex = 0;\n"
    case ConvolutionOrientation =>
      code append "    int firstProductiveElementIndex = " +
      ((upsampleStep - 1) * paddedLocalColumns + upsampleStep - 1).toString + ";\n"
  }

  val loopIterations = resultPoints + pipeDepth

  // The 256-float tile to be reduced is put in the high-half of a 512-float
  // memory and reduced toward location 1.  By thread index 'tIdx' the operations
  // are:
  //
  // tIdx 128-> 255:    mem[tIdx] = mem[128+tIdx] + mem[256 + tIdx];
  // tIdx  64-> 127:    mem[tIdx] = mem[ 64+tIdx] + mem[128 + tIdx];
  // tIdx  32->  63:    mem[tIdx] = mem[ 32+tIdx] + mem[ 64 + tIdx];
  // tIdx  16->  31:    mem[tIdx] = mem[ 16+tIdx] + mem[ 32 + tIdx];
  // tIdx   8->  15:    mem[tIdx] = mem[  8+tIdx] + mem[ 16 + tIdx];
  // tIdx   4->   7:    mem[tIdx] = mem[  4+tIdx] + mem[  8 + tIdx];
  // tIdx   2->   3:    mem[tIdx] = mem[  2+tIdx] + mem[  4 + tIdx];
  // tIdx   1      :    mem[tIdx] = mem[  1+tIdx] + mem[  2 + tIdx];
  // tIdx   0      :    mem[tIdx] = mem[  0+tIdx] + mem[  1 + tIdx];
  //
  // The operation of tIdx 0 is not used, since the result is produced by
  // tid 1. Groups of threads use the same "base index" for the operands
  // they read.  For example, tIdx's 128->256 use a base index 128 for the
  // 1st operand read (i.e. op1BaseIndex = 128), and 256 for op2BaseIndex.
  // These base indices are calculated by formula below:

  // The following code implements the above-described base-address calculation.
  // It is commented out because it was replaced by code that eliminated a
  // 2-way bank conflict on the memory reads that occurs between threads 0->15
  // and 16->31.  The fix is to alter the accesses for threads 16->31 as follows:

  // tIdx   24-> 31:    mem[tIdx] = mem[16+16+tIdx] + mem[16+8+tIdx];
  // tIdx   20-> 23:    mem[tIdx] = mem[16+ 8+tIdx] + mem[16+4+tIdx];
  // tIdx   18-> 19:    mem[tIdx] = mem[16+ 4+tIdx] + mem[16+2+tIdx];
  // tIdx   17     :    mem[tIdx] = mem[16+ 2+tIdx] + mem[16+1+tIdx];
  // tIdx   16     :    mem[tIdx] = mem[16+ 1+tIdx] + mem[16+0+tIdx];

  // One way to think about this: a thread with tid 16->31 calculates
  // which bank the tIdx-16 thread would access on the second read, and reads
  // that bank first.  Likewise, it calculates which bank the tid-16 thread
  // would access on the first read, and reads that bank second.

  // Original, slightly slower approach with 2-way read bank conflict

//  code append "    int inputIndex;\n"
//  code append "    unsigned int op2BaseIndex = threadIndex;\n"
//  code append "    op2BaseIndex |= op2BaseIndex >> 1;\n"
//  code append "    op2BaseIndex |= op2BaseIndex >> 2;\n"
//  code append "    op2BaseIndex |= op2BaseIndex >> 4;\n"
//  code append "    op2BaseIndex |= op2BaseIndex >> 8;\n"
//  code append "    op2BaseIndex += 1;\n"
//  code append "    unsigned int op1BaseIndex = op2BaseIndex / 2;\n"
//  code append "    row = 0;\n"

  // Previously, this kernel wrote the result values to global memory from thread 1
  // as they were generated.  The "if (threadIndex == 1) @out0 = ..." statement
  // was identified as being a performance-killer because of the thread divergence
  // and the additional memory accesses.  To remove this bottleneck, all threads
  // write their results through pointers, and thread 1 has its pointers moved
  // to point to a special 'results' memory.  Further, as results are generated,
  // the thread 1 pointers are bumped so that results are not overwritten.  The
  // only special-case code is now of the form "if (threadIndex == 1) memPtr++"
  // and this adjustment of a thread-local variable does not impact performance.

  code append "    int inputIndex;\n"
  code append "    unsigned int op2BaseIndex = threadIndex;\n"
  code append "    if (threadIndex >= 16 && threadIndex < 32) {\n"
  code append "        op2BaseIndex -= 16;\n"
  code append "    }\n"
  code append "    op2BaseIndex |= op2BaseIndex >> 1;\n"
  code append "    op2BaseIndex |= op2BaseIndex >> 2;\n"
  code append "    op2BaseIndex |= op2BaseIndex >> 4;\n"
  code append "    op2BaseIndex |= op2BaseIndex >> 8;\n"
  code append "    op2BaseIndex += 1;\n"
  code append "    unsigned int op1BaseIndex = op2BaseIndex / 2;\n"
  code append "    if (threadIndex >= 16 && threadIndex < 32) {\n"
  code append "        op1BaseIndex = 16 + op2BaseIndex;\n"
  code append "        op2BaseIndex = 16 + op2BaseIndex / 2;\n"
  code append "    }\n"

  code append "    int outIndex = threadIndex;\n"
  code append "    if (threadIndex == 1) {\n"
  code append "        reduceMem0 = &results[0];\n"
  code append "        reduceMem1 = &results[0];\n"
  code append "        outIndex = 0;\n"
  code append "    }\n"

  // Ensure localTile[] is read in before accessing it for the reductions
  code append "    barrier(CLK_LOCAL_MEM_FENCE);\n"

  def iterationCode(i: Int, indentSize: Int = 4) {
    val indent = " " * indentSize
    code append "\n" + indent + "// unrolled loop iteration " + i + "\n\n"
    val readMem = "mem" + (i % 2)
    val writeMem = "reduceMem" + ((i + 1) % 2)

    // first 'resultPoints' iterations should input a tile to be reduced
    if (i < resultPoints ) {
      val inRow = i / resultColumns
      val inColumn = i - inRow * resultColumns
      if (inColumn == 0)
        code append indent + "inputIndex = baseInputIndex;\n"
      code append indent + readMem + s"[$localWorkSize + threadIndex] = localTile[inputIndex] * filterVal;\n"
      if (inColumn != resultColumns - 1)
        code append indent + s"inputIndex += $downsampleStep;\n"
      else
        code append indent + s"baseInputIndex += ${downsampleStep * paddedLocalColumns};\n"
    }

    code append indent + "barrier(CLK_LOCAL_MEM_FENCE);\n"
    code append indent + writeMem + "[outIndex] = " + readMem + "[op1BaseIndex + threadIndex] + " + readMem + "[op2BaseIndex + threadIndex];\n"

    if (i >= pipeDepth) {
      // Thread 1 is writing to the 'results' memory.  We want to bump
      // up both pointers so we don't have to worry about which flavor of
      // write statement above is used.

      code append indent + "if (threadIndex == 1) {\n"
      code append indent + "    reduceMem0++;\n"
      code append indent + "    reduceMem1++;\n"
      code append indent + "}\n"
    }

  }

  // multiply-add code

  code append s"    int baseInputIndex = firstProductiveElementIndex + _localRow * ${paddedLocalColumns * upsampleStep} + _localColumn * $upsampleStep;\n"


  // The use of pipelining and double buffering makes unrolling a bit tricky:
  // There's an initial period when the pipeline is filling where we don't output
  // any values.  There's also a period at the end when we are letting the pipe
  // drain: outputting results but no longer adding new inputs.  With a small number
  // of results, these two regions may overlap: i.e. we may no longer be stuffing
  // the pipeline, yet no output is ready yet.  Completely unrolling the iterations
  // is one fairly simple approach to handle this.  Problem is, with many output
  // points, the kernel gets too big.  Thus, we try an approach where a large
  // central region that we are both inputting and outputting values is handled
  // by a "input row processing loop" with the per-column-element processing unrolled.
  // Since consequetive rows will have a different "buffer polarity" if there are
  // an odd number of columns per row, we handle rows in pairs.

  def loopedIterationCode(startIteration: Int, untilIteration: Int) {
    require (startIteration % resultColumns == 0)
    require (untilIteration % resultColumns == 0)
    val startRow = startIteration / resultColumns
    val untilRow = untilIteration / resultColumns
    code append "\n    // partially unrolled loop iterations " + startIteration +
            " through " + (untilIteration - 1) + "\n\n"
    code append s"    for (int inRow = $startRow; inRow < $untilRow; inRow++) {\n"
    for (i <- startIteration until startIteration + resultColumns)
      iterationCode(i, 8)
    if (resultColumns % 2 == 1 && startIteration + resultColumns < untilIteration) {
      code append "\n        inRow++;\n"
      code append s"        if (inRow < $untilRow) {\n"
      for (i <- startIteration + resultColumns until startIteration + 2 * resultColumns)
        iterationCode(i, 12)
      code append "        }\n"
    }
    code append "    }\n"
  }

  // We limit the number of unrolled iterations, since huge kernels can
  // seemingly hang the compilation.
  val MaxUnrolledLoopIterations = 50

  val firstNormalRowStart = HyperKernel.roundUp(resultColumns, pipeDepth).toInt
  if (loopIterations <= MaxUnrolledLoopIterations ||
          firstNormalRowStart >= resultPoints) {
    // All iterations unrolled
    for (i <- 0 until loopIterations)
      iterationCode(i)
  } else {
    for (i <- 0 until firstNormalRowStart)
      iterationCode(i)
    // A central core of iterations expressed as a loop
    loopedIterationCode(firstNormalRowStart, resultPoints)
    for (i <- resultPoints until loopIterations)
      iterationCode(i)
  }

  // Final outputting of values put in the results memory by thread 1.

  code append s"    barrier(CLK_LOCAL_MEM_FENCE);\n"
  code append s"    // Output code\n"
  code append s"    for (outIndex = threadIndex; outIndex < ${resultRows * resultColumns}; outIndex += $localWorkSize) {\n"
  code append s"        row = outIndex / $resultColumns;\n"
  code append s"        column = outIndex - row * $resultColumns;\n"
  code append s"        @outElementNonlocal0 = results[outIndex];\n"
  code append s"    }"

  addCode(code.toString)

  val Debug = false
  if (Debug) {
    println(params.toString)
    debugCompile()
  }
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ConvolveToSmallFieldPipelinedHyperKernel {

  /** Create a kernel that convolves a field with a dynamic filter field.
    *
    * @param inputs The input and filter virtual field registers driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @return The synthesized hyperkernel.
    *
    */
  def apply(inputs: Array[VirtualFieldRegister], operation: ConvolveOp,
            resultType: FieldType, codeGenParams: OpenCLKernelCodeGenParams): HyperKernel = {

    // This kernel performs the convolution as separate convolutions of tiles
    // of the 2nd "filter" input.  As such, it does not produce a fully reduced
    // output, but instead produces a "too tall" output that must be further
    // summed by the TensorReduceHyperKernel.

    val params =
      new ConvolutionToSmallFieldParams(inputs, operation, resultType, codeGenParams,
        filterVolumeMultiplier = 2,
        numFilterMemories = 2,
        numResultMemories = 1)
    import params._
    if (numFilterTiles == 1)
      new ConvolveToSmallFieldPipelinedHyperKernel(inputs, operation, resultType, TensorElementAddressing, params)
    else {
      val convolveTensorPoints = planesPerImage * numLogicalFilters * numFilterTiles * batchSize
      val convolveResultType = resultType.resizeTensor(Shape(convolveTensorPoints))
      val convolveKernel =
        new ConvolveToSmallFieldPipelinedHyperKernel(inputs, operation,
          convolveResultType, TensorElementAddressing, params)
      TensorReduceHyperKernel(convolveKernel.outputs(0), TensorReduceSumOp(numFilterTiles), resultType)
    }
  }

}
