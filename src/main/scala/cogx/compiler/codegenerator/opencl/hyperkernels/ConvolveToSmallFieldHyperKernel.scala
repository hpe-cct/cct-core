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
import cogx.cogmath.algebra.real.Logarithm
import cogx.compiler.parser.op._
import cogx.platform.opencl.OpenCLKernelCodeGenParams
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
  * @param inputs The input and filter virtual field registers driving this kernel.
  * @param operation the opcode
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @param params Workgroup size and other parameters of the convolution.
  */
private[cogx]
class ConvolveToSmallFieldHyperKernel private (inputs: Array[VirtualFieldRegister],
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
    "Could not fit inputTile and filter (" + localMemBytesNeeded + " bytes)" +
            " in available local memory (" + localMemBytesAvailable + " bytes)")

  // This kernel operates in BigTensorAddressing mode, because currently that's
  // the only way to write multiple output elements from a single thread, as
  // thread0 must do for each workgroup.  Now ideally we would base the
  // "workfield" on the 2nd input (at least as far as the field shape) and then
  // have a tensorShape dictated by the tensor length of the first input (call
  // this 'N'), or the length of the second input (call this 'K'), or the length
  // of the output (here N*K).  We'd prefer to make the workfield a VectorField
  // with one of these vector lengths, but then we wouldn't get the desired
  // number of workgroups in BigTensorAddressing mode.  The trick we use is to
  // expand the workfield to 3D with the number of layers equal to N, K or N*K.
  // We then use the workgroup '_layer' value to set 'tensorElement' when
  // accessing the first or second input tensor elements.

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

  // As a side note, assume a 16x16 workgroup that outputs a 5x5 filter bank.
  // The input tile footprint in local memory will be 20*20*4 = 1.6 KB and the
  // reduction local memory footprint will be 256*4 = 1 KB.  Thus the total
  // local memory needs of 2.6 KB will permit the maximum of 8 workgroups to
  // run on an NVIDIA SM concurrently, and live under the 48 KB maximum.  Having
  // an upsample step of 2 will only slightly limit the number of workgroups on
  // an SM to 7.

  val workGroupLayers = planesPerImage * numLogicalFilters * batchSize

  override lazy val workFieldType =
    filterType.incrementDimensions(workGroupLayers).resizeTensor(Shape())
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressing, localRows, localColumns)

  private val localWorkSize = workGroup.localWorkSize

  val code = new StringBuffer

  //define and allocate the local tile and its offset relative to the origin of
  //the input field
  code append s"     __local float localTile[$paddedLocalVolume];\n"
  code append s"        __local volatile float reductionMem[$workGroupTileVolume];\n"
  code append s"        int logicalOutputPlane = _layer;\n"

  val workTensorElementsPerInput = workFieldType.layers / batchSize
  if (batchSize > 1) {
    // The image within the batch that this thread is working on
    code append s"        int imageIndex = _layer / $workTensorElementsPerInput;\n"
    // The thread offset of this thread within the group of threads working on this same image
    code append s"        int withinImageThreadElement = _layer % $workTensorElementsPerInput;\n"
  }
  else {
    // The image within the batch that this thread is working on
    code append s"        const int imageIndex = 0;\n"
    // The thread offset of this thread within the group of threads working on this same image
    code append s"        int withinImageThreadElement = _layer;\n"
  }

  // Set up tensorElement variable for input image tile reading
  val setTensorElement: String =
    operation.vectorMode match {
      case ProjectFrame =>
        s"""
           |  tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
      case ProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case BackProjectFrame =>
        s"""
           |  tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
      case BackProjectFrameBlockReduceSum =>
        throw new RuntimeException("Mode not supported for this operation")
      case FilterAdjoint =>
        s"""
           |  tensorElement = logicalOutputPlane % $planesPerImage + imageIndex * $planesPerImage;
          """.stripMargin
      case PlaneByPlane =>
        s"    tensorElement = logicalOutputPlane;\n"
    }
  code append setTensorElement

  inputDims match {
    case 2 =>
    case x => throw new RuntimeException("Kernel " + this + " does not support " + x + "D input fields.")
  }

  // Generate the convolution kernel code as the sum of separate code blocks
  // val convCode = setupCode + readLocalCode + borderCode + readFilterCode + multiplyAddCode

  /** Set up some variables helpful to the rest of the calculation */
  val setupCode =
    s"""
      |    int groupStartRow = get_group_id(1) * $localRows * $upsampleStep;
      |    int groupStartColumn = get_group_id(0) * $localColumns * $upsampleStep;
    """.stripMargin

  /** First part of the loop to read in the input image tile */
  val readLocalCodeNoBoundaryChecks =
    s"""
      |    for (int localTileIndex = threadIndex; localTileIndex < $paddedLocalVolume; localTileIndex += $groupSize) {
      |        int tileRow = localTileIndex / $paddedLocalColumns;
      |        int tileColumn = localTileIndex - tileRow * $paddedLocalColumns;
      |        row = groupStartRow + tileRow;
      |        column = groupStartColumn + tileColumn;
      |        localTile[localTileIndex] = readNonlocal(@in0);
      |    }
    """.stripMargin
  val readLocalCodeWithBoundaryChecks =
    s"""
      |    for (int localTileIndex = threadIndex; localTileIndex < $paddedLocalVolume; localTileIndex += $groupSize) {
      |        int tileRow = localTileIndex / $paddedLocalColumns;
      |        int tileColumn = localTileIndex - tileRow * $paddedLocalColumns;
      |        row = groupStartRow + tileRow;
      |        column = groupStartColumn + tileColumn;
      |        if (row < $inputRows && column < $inputColumns)
      |            localTile[localTileIndex] = readNonlocal(@in0);
      |        else
      |            localTile[localTileIndex] = 0.0f;
      |    }
    """.stripMargin
  // Most workgroups do not need the boundary checks, which are part of the inner
  // loop.  Best then is to see which workgroups need the checks and invoke the
  // slower boundary-check read-in code only for those workgroups.
  val readLocalCode =
    s"    int threadIndex = _localColumn + _localRow * _localColumns;\n" +
    s"    if (groupStartColumn + $paddedLocalColumns < $inputColumns &&\n" +
    s"        groupStartRow + $paddedLocalRows < $inputRows) {\n" +
            readLocalCodeNoBoundaryChecks +
    s"    } else {\n" +
            readLocalCodeWithBoundaryChecks +
    s"    }\n"

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

  /** Read in the filter, each thread gets its own value */
  val readFilterCode = filterOrientation match {
    case CrossCorrelationOrientation =>
      s"""
        |    float filterVal = 0.0f;
        |    if (_row < $filterRows && _column < $filterColumns) {
        |        row = _row;
        |        column = _column;
        |        filterVal = readNonlocal(@in1);
        |    }
      """.stripMargin
    case ConvolutionOrientation =>
      s"""
        |    float filterVal = 0.0f;
        |    if (_row < $filterRows && _column < $filterColumns) {
        |        row = ${filterRows - 1} - _row;
        |        column = ${filterColumns - 1} - _column;
        |        filterVal = readNonlocal(@in1);
        |    }
      """.stripMargin
  }
  // Adjust setting of tensorElement for output
  val preMultiplyAddCode =
    operation.vectorMode match {
      case FilterAdjoint =>
        s"tensorElement = ($batchSize * withinImageThreadElement + imageIndex) * $numFilterTiles + get_group_id(1) * get_num_groups(0) + get_group_id(0);\n"
      case _ =>
        s"tensorElement = logicalOutputPlane * $numFilterTiles + get_group_id(1) * get_num_groups(0) + get_group_id(0);\n"
    }

  val reductionCode = {
    val code = new StringBuffer
    // Revamped reduction code that adapts to the OpenCL platform warp size.
    var threadCount = 512
    while (threadCount >= 2) {
      val midPoint = threadCount / 2
      if (localWorkSize >= threadCount) {
        if (midPoint >= params.codeGenParams.warpSize)
          code.append(s"            if (threadIndex < $midPoint) {\n")
        code.append(s"                reductionMem[threadIndex] = reductionMem[threadIndex] + reductionMem[threadIndex + $midPoint];\n")
        if (midPoint > codeGenParams.warpSize || midPoint == 1)
          code.append("            }\n")
        if (midPoint > codeGenParams.warpSize)
          code.append("            barrier(CLK_LOCAL_MEM_FENCE);\n")
      }
      threadCount /= 2
    }
    code.toString
  }

  /** Perform the actual convolution/correlation */
  // The following is a merged form of the routine that works for all sampling
  // modes.  Remember that at most one of %upsampleStep% and %downsampleStep%
  // will be > 1 for any given sampling mode.
  // The first barrie(CLK_LOCAL_MEM_FENCE) is doing double-duty: it synchronizes
  // between the initialization of localTile and the first read, but also between
  // the final read of reductionMem[0] at the bottom of the loop, and the start
  // of the next reduction iteration, which re-initializes reductionMem[0]

  val firstProductiveElementIndexInit = filterOrientation match {
    case CrossCorrelationOrientation =>
      "    int firstProductiveElementIndex = 0;\n"
    case ConvolutionOrientation =>
      "    int firstProductiveElementIndex = " +
      ((upsampleStep - 1) * paddedLocalColumns + upsampleStep - 1).toString + ";\n"
  }
  val multiplyAddCode =
    firstProductiveElementIndexInit +
    s"""
      |    int baseInputIndex = firstProductiveElementIndex + _localRow * $paddedLocalColumns * $upsampleStep + _localColumn * $upsampleStep;
      |    for (row = 0; row < $resultRows; row++, baseInputIndex += ${downsampleStep * paddedLocalColumns}) {
      |        int inputIndex = baseInputIndex;
      |        for (column = 0; column < $resultColumns; column++, inputIndex += $downsampleStep) {
      |            barrier(CLK_LOCAL_MEM_FENCE);
      |            reductionMem[threadIndex] = localTile[inputIndex] * filterVal;
      |            barrier(CLK_LOCAL_MEM_FENCE);
    """.stripMargin +
            reductionCode +
    """
      |            if (threadIndex == 0)
      |                @outElementNonlocal0 = reductionMem[0];
      |        }
      |    }
    """.stripMargin
  /** Combine the various kernel code snippets into the enire kernel program */
  val convCode = setupCode + readLocalCode + postReadLocalCode +
          readFilterCode + preMultiplyAddCode + multiplyAddCode

  code append convCode
  addCode(code.toString)

  val Debug = false
  if (Debug) {
    println(params.toString)
    debugCompile()
  }
}

/** Helper class to estimate the throughput of the convolution.
  *
  * This helper class performs the front-end analysis of the ConvolutionHyperKernel,
  * and comes up with the workgroup parameters that can be used to estimate the
  * kernel's throughput.
  *
  * When the ConvolveToSmallFieldPipelinedHyperKernel was created, a number
  * of additional arguments were added to this routine so that it could support
  * the needs of this new kernel and the legacy slower ConvolveToSmallFieldHyperKernel.
  *
  * @param inputs The input and filter virtual field registers driving this kernel.
  * @param operation the opcode
  * @param resultType The FieldType of the result of this kernel.
  * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
  */
private[cogx]
class ConvolutionToSmallFieldParams (inputs: Array[VirtualFieldRegister],
                                     operation: ConvolveOp,
                                     resultType: FieldType,
                                     val codeGenParams: OpenCLKernelCodeGenParams,
                                     filterVolumeMultiplier: Int = 1,
                                     numFilterMemories: Int = 1,
                                     numResultMemories: Int = 0,
                                     miniTileRows: Int = 1,
                                     _localColumns: Int = HyperKernel.DefaultLocalColumns,
                                     _localRows: Int = HyperKernel.DefaultLocalRows)
        extends  Logarithm
{
  import ConvolveHyperKernel._
  val borderPolicy = operation.borderPolicy
  val filterOrientation = operation.filterOrientation
  val samplingPolicy = operation.samplingPolicy
  val vectorMode = operation.vectorMode
  val batchSize = operation.batchSize

  private val imageType = inputs(0).fieldType

  val filterType = inputs(1).fieldType

  val expectedResultType = outputFieldType(inputs(0).fieldType,
    inputs(1).fieldType, borderPolicy, samplingPolicy, vectorMode, batchSize)

  require(expectedResultType == resultType)

  val imageTensorPoints = imageType.tensorShape.points
  val planesPerImage = imageTensorPoints / batchSize

  val filterTensorPoints = filterType.tensorShape.points
  val numLogicalFilters =
    operation.vectorMode match {
      case FilterAdjoint => filterTensorPoints / batchSize
      case _ => filterTensorPoints / planesPerImage
    }

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

  val resultRows = resultType.rows
  val resultColumns = resultType.columns
  // halo is applied on one side of input tile
  val haloLayers:Int = inputLayers - filterLayers * upsampleStep
  val haloRows:Int = inputRows - filterRows * upsampleStep
  val haloColumns:Int = inputColumns - filterColumns * upsampleStep

  //Determine local tile size
  var localColumns = _localColumns
  var localRows = if (inputDims <= 1) 1 else _localRows
  var localLayers = 1
  var groupSize = localColumns * localRows * localLayers

  val localMemBytesAvailable = codeGenParams.localMemSize

  /** Given a dimension of the workgroup in the pre-sampled filter (2nd input)
   * space, what is the maximum input dimension that the workgroup needs to access.
   */
  def unsampledToPadded(unsampledEdge: Int, samplingPolicy: ConvolutionSamplingPolicy, halo: Int) = {
    samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        unsampledEdge * step + halo
      case DownsampleOutputConvolution(step) =>
        unsampledEdge + halo
      case NoSamplingConvolution =>
        unsampledEdge + halo
    }
  }

  // Loop over parameter-determining calculation, lowering the groupSize if
  // necessary until the local memory fits within the available size.
  // Note init to 1 is best default for some vars that are not reassigned below.

  var workGroupTileVolume,
  paddedLocalColumns, paddedLocalRows, paddedLocalLayers,
  paddedLocalVolume, localMemBytesNeeded = 1

  // A non-deterministic misbehavior of the ConvolveToSmallFieldPipelinedHyperkernel
  // and the ConvolveToSmallFieldPipelinedTiledHyperkernel is present only on AMD
  // GPUs.  It was "fixed" by adding some padding to the "mem0" and "mem1" memories.
  // The bug is evoked by the "Vector convolveFilterAdjoint Vector" subtest of the
  // StaticConvolutionSpec test (among other subtests).  The symptom is that the 2nd
  // output value is sporadically getting 0 for the inputTile value that thread 1 accesses.

  // The value 16 was empirically determined to fix the bug both on a laptop "Caicos" AMD GPU
  // and a workstation "Tahiti" AMD GPU.

  // Unfortunately, we have to always add the padding, because we could compile with an NVidia
  // brick, save the ComputeGraph to an HDF5 file, then restore and run on AMD hardware.
  val AMDbugPipelinedReductionMemPadding = 16

  do {
    paddedLocalColumns = unsampledToPadded(localColumns, samplingPolicy, haloColumns)

    if (inputDims > 1) {
      val perfectRowEdge = math.pow(groupSize.toFloat/localColumns, 1f/(inputDims-1).toFloat).toFloat
      localRows = roundDownPowerOf2(perfectRowEdge)
      paddedLocalRows = unsampledToPadded(localRows * miniTileRows, samplingPolicy, haloRows)
    }

    if (inputDims > 2) {
      val perfectLayerEdge = groupSize.toFloat/(localColumns * localRows)
      localLayers = roundDownPowerOf2(perfectLayerEdge)
      paddedLocalLayers = unsampledToPadded(localLayers, samplingPolicy, haloLayers)
    }
    
    // We don't actually need the filter data in shared memory, but we do
    // need a filter-volume-sized shared memory for the dot-product reduction.
    // BTW, could this be reduced in half with a clever first reduction pass?
    workGroupTileVolume = localLayers * localRows * localColumns * filterVolumeMultiplier
    
    paddedLocalVolume = paddedLocalLayers*paddedLocalRows*paddedLocalColumns

    val resultVolume = resultType.fieldShape.points

    localMemBytesNeeded = 4 * (paddedLocalVolume +
                               workGroupTileVolume * numFilterMemories +
                               AMDbugPipelinedReductionMemPadding * numFilterMemories +
                               resultVolume * numResultMemories)
    /** Chunk size for shared memory allocations, 256 = NVidia Kepler value */
    val SharedMemoryAllocationUnitSize = 256
    localMemBytesNeeded = HyperKernel.roundUp(SharedMemoryAllocationUnitSize,
                                              localMemBytesNeeded).toInt

    if (localMemBytesNeeded > localMemBytesAvailable) {
      // If the memories don't fit in the available GPU local memory, reduce the threads in
      // the workgroup by a factor of 2 and recalculate the shape (as being roughly square).
      groupSize /= 2
      val perfectColumnEdge = math.pow(groupSize.toFloat, 1f/inputDims.toFloat).toFloat
      localColumns = roundDownPowerOf2(perfectColumnEdge)
    }

  } while (localMemBytesNeeded > localMemBytesAvailable && groupSize > 0)

  val numFilterTiles =
    math.ceil(filterLayers.toFloat/localLayers).toInt *
    math.ceil(filterRows.toFloat/(localRows*miniTileRows)).toInt *
    math.ceil(filterColumns.toFloat/localColumns).toInt

  /** Some important parameters of the class, for debugging. */
  override def toString: String =
    s"""
     |  groupSize: $groupSize
     |  localLayers: $localLayers
     |  localRows: $localRows
     |  localColumns: $localColumns
     |  inputDims: $inputDims
     |  inputLayers: $inputLayers
     |  inputRows: $inputRows
     |  inputCols: $inputColumns
     |  filterDims: $filterDims
     |  filterLayers: $filterLayers
     |  filterRows: $filterRows
     |  filterCols: $filterColumns
     |  filterTileVolume: $workGroupTileVolume
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
object ConvolveToSmallFieldHyperKernel {

  /** Create a kernel that convolves a field with a dynamic filter field.
    *
    * @param inputs The input and filter virtual field registers driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @return The synthesized hyperkernel.
    *
    */
  def apply(inputs: Array[VirtualFieldRegister], operation: ConvolveOp,
            resultType: FieldType,
            platformParams: OpenCLKernelCodeGenParams): HyperKernel = {

    // This kernel performs the convolution as separate convolutions of tiles
    // of the 2nd "filter" input.  As such, it does not produce a fully reduced
    // output, but instead produces a "too tall" output that must be further
    // summed by the TensorReduceHyperKernel.

    val params =
      new ConvolutionToSmallFieldParams(inputs, operation, resultType, platformParams)
    import params._
    if (numFilterTiles == 1)
      new ConvolveToSmallFieldHyperKernel(inputs, operation, resultType, BigTensorAddressing, params)
    else {
      val convolveTensorPoints = planesPerImage * numLogicalFilters * numFilterTiles * batchSize
      val convolveResultType = resultType.resizeTensor(Shape(convolveTensorPoints))
      val convolveKernel =
        new ConvolveToSmallFieldHyperKernel(inputs, operation,
          convolveResultType, BigTensorAddressing, params)
      TensorReduceHyperKernel(convolveKernel.outputs(0), TensorReduceSumOp(numFilterTiles), resultType)
    }
  }

}
