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

package cogx.compiler.codegenerator.opencl.generator

import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.api.ImplicitConversions
import cogx.compiler.parser.op.ConvolveOp
import cogx.platform.types.DownsampleOutputConvolution
import cogx.platform.types.UpsampleInputConvolution
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Logarithm
import cogx.cogmath.algebra.complex.{Complex, ComplexMatrix}
import cogx.cogmath.fft.FFT2D
import cogx.compiler.codegenerator.opencl.cpukernels.{ConstantFieldKernel, FixedComplexMatrixKernel}
import cogx.compiler.codegenerator.opencl.generator.FFTConvolutionUtils._
import cogx.runtime.execution.Profiler

/** Code generation for convolution or cross-correllation with a kernel that may
  * be dynamically altered at runtime.  There are many cases to cover here,
  * based on:
  *
  * 1. What type of field, e.g. Scalar, Vector, Color or Complex, the inputs are.
  * 2. Whether the filter field (or image field for that matter) is truly
  *    dynamic, or is sourced from a static field (with no recurrence).
  * 3. Whether the input is large, and may be more efficiently handled by
  *    an FFT-based approach.
  * 4. The dimensionality of the convolution, 1D, 2D or 3D.
  * 5. Whether the filter needs to be flipped, as needed for cross-correlation.
  *
  * @author Dick Carter and Matthew Pickett
  */
private [cogx]
object DynamicConvolutionGenerator
        extends ImplicitConversions
        with Logarithm
{
  /** Enables convolution on VectorFields and MatrixFields (plane-by-plane)
    * with a single kernel for each of the various steps: expand, fft, multiply,
    * inverse, and trim.  This utilizes a "double-deep" VectorField or MatrixField
    * for any space-domain representation, rather than a (non-existent)
    * ComplexVectorField or ComplexMatrixField.
    */
  val EnableFFTVectorConvolution = true

  /** does an operation ask for upsampling or downsampling */
  def usesIntegratedSampling(operation: ConvolveOp) =
    operation.samplingPolicy match {
      case NoSamplingConvolution => false
      case DownsampleOutputConvolution(step) => true
      case UpsampleInputConvolution(step) => true
    }

  /* Top level triage of Convolution operation: handle ColorField by decomposition
   * into color planes, and decide whether the other cases are to be handled
   * directly by the Convolution kernel, or by and FFT approach.
   */
  def apply(inputs: Array[VirtualFieldRegister], operation: ConvolveOp,
            resultType: FieldType, fftUse: ConvolutionFFTUsePolicy,
            smallTensorUse: ConvolutionSmallTensorUsePolicy,
            codeGenParams: OpenCLKernelCodeGenParams,
            profiler: Profiler): AbstractKernel =
  {
    val inputImage = inputs(0)
    val imageType = inputImage.fieldType
    val inputFilter = inputs(1)
    val filterType = inputFilter.fieldType

    val planeType = toReal(resultType)

    if (isColorField(imageType)) {
      val red = ColorPlaneHyperKernel(Array(inputImage), ColorPlaneRedOp, planeType).outputs(0)
      val green = ColorPlaneHyperKernel(Array(inputImage), ColorPlaneGreenOp, planeType).outputs(0)
      val blue = ColorPlaneHyperKernel(Array(inputImage), ColorPlaneBlueOp, planeType).outputs(0)

      val redConvolved =
        apply(Array(red, inputFilter), operation, planeType,
          fftUse, smallTensorUse, codeGenParams, profiler).outputs(0)
      val greenConvolved =
        apply(Array(green, inputFilter), operation, planeType,
          fftUse, smallTensorUse, codeGenParams, profiler).outputs(0)
      val blueConvolved =
        apply(Array(blue, inputFilter), operation, planeType,
          fftUse, smallTensorUse, codeGenParams, profiler).outputs(0)

      CreateColorFieldHyperKernel(Array(redConvolved, greenConvolved, blueConvolved),
        MergeColorPlanesOp, resultType)
    }
    else if (isRealField(imageType) &&
             isRealField(filterType)) {
      def chooseFFTifBigAndSupported() = {
        val fftOKToUse = fftUse.fftOK
        val stdConvolveOKToUse = fftUse.stdConvolveOK

        val specialPlaneMixing = operation.vectorMode match {
          case ProjectFrame => true
          case ProjectFrameBlockReduceSum => true
          case BackProjectFrame => true
          case BackProjectFrameBlockReduceSum => true
          case FilterAdjoint => true
          case FilterAdjointBlockReduceSum => true
          case PlaneByPlane => false
        }

        // Use std convolve if fft is prohibited, if convolve uses a feature
        // not supported by the fft (e.g. integrated upsampling or matrix fields),
        // or finally if it's the best among both enabled options.
        val dynamicFilter = filterIsDynamic(inputFilter.source)
        if (!fftOKToUse ||
            usesIntegratedSampling(operation) ||
            specialPlaneMixing ||
            imageType.tensorOrder > 1 ||
            filterType.tensorOrder > 1 ||
            stdConvolveOKToUse &&
            !fftConvolutionIsFaster(imageType, filterType, operation, dynamicFilter))
          convolveDirectly(inputs, operation, resultType, smallTensorUse, codeGenParams, profiler)
        else if (fftOKToUse && EnableFFTVectorConvolution &&
                (imageType.tensorOrder > 0 || filterType.tensorOrder > 0))
          FFTVectorConvolutionGenerator(inputs, operation, resultType)
        else if (imageType.tensorOrder == 2 &&
                imageType.tensorShape == filterType.tensorShape)
         matrixConvolveMatrix(inputs, operation, resultType, fftUse, smallTensorUse, codeGenParams, profiler)
        else if (imageType.tensorOrder == 1 &&
                imageType.tensorShape == filterType.tensorShape)
          vectorConvolveVector(inputs, operation, resultType, fftUse, smallTensorUse, codeGenParams, profiler)
        else if (isTensor0Field(imageType) && filterType.tensorOrder == 1)
          scalarConvolveVector(inputs, operation, resultType, fftUse, smallTensorUse, codeGenParams, profiler)
        else
          convolveBy2DFFT(inputs, operation, resultType)
      }

      inputImage.fieldType.dimensions match {
        case 3 =>
          // Non-fft convolve does not handle 3D convolution yet
          convolveBy3DFFT(inputs, operation, resultType)
        case 2 =>
          operation.borderPolicy match {
            case BorderClamp =>  chooseFFTifBigAndSupported()
            case BorderZero =>   chooseFFTifBigAndSupported()
            case BorderCyclic => chooseFFTifBigAndSupported()
            case _ => convolveDirectly(inputs, operation, resultType, smallTensorUse, codeGenParams, profiler)
          }
        case 1 =>
          convolveDirectly(inputs, operation, resultType, smallTensorUse, codeGenParams, profiler)
        case x =>
          throw new RuntimeException(
            "Convolution not supported on field of dimension " + x)
      }
    }
    else if (isTensor0Field(inputImage.fieldType) &&
            isRealField(inputImage.fieldType) &&
            isComplexField(inputFilter.fieldType) &&
            inputFilter.fieldType.rows <= 13)
    {
      // Convolving a scalar field with a complex vector field is done
      // directly if the filter size is small enough.
      convolveDirectly(inputs, operation, resultType, smallTensorUse, codeGenParams, profiler)
    }
    else if (isComplexField(inputImage.fieldType) ||
            isComplexField(inputFilter.fieldType)) {
      require(!usesIntegratedSampling(operation),
        "Integrated upsampling or downsampling not supported for complex field convolution.")
      if (imageType.tensorOrder > 0 || filterType.tensorOrder > 0)
        FFTVectorConvolutionGenerator(inputs, operation, resultType)
      else
        inputImage.fieldType.dimensions match {
        case 3 =>
          convolveBy3DFFT(inputs, operation, resultType)
        case 2 =>
          convolveBy2DFFT(inputs, operation, resultType)
        case x =>
          throw new RuntimeException(
            "Complex Convolution not supported on field of dimension " + x)
        }
    }
    else {
        throw new RuntimeException("Convolution not supported on field types " +
                inputImage.fieldType + " and " + inputFilter.fieldType)
    }
  }

  /** Will FFT-based convolution need to do a forward FFT of the filter? */
  private def filterIsDynamic(filter: AbstractKernel) : Boolean = {
    filter match {
      case k: ConstantFieldKernel =>
        k.opcode match {
          case ConstantScalar2DOp(f) => false
          case ConstantComplex2DOp(f) => false
          case ConstantVector2DOp(f) => false
          case ConstantMatrix2DOp(f) => false
          case x => throw new RuntimeException("Internal compiler error: " +
                  "unexpected constant opcode: " + x)
        }
      case x => true
    }
  }

  /** Perform a one-plane-at-a-time convolution of a VectorField input and a
    * VectorField filter bank using separate kernels, stacking the resulting
    * ScalarFields into a VectorField output.
    */
  def vectorConvolveVector(inputs: Array[VirtualFieldRegister], op: ConvolveOp,
       resultType: FieldType, fftUse: ConvolutionFFTUsePolicy,
       smallTensorUse: ConvolutionSmallTensorUsePolicy,
       platformParams: OpenCLKernelCodeGenParams, profiler: Profiler): AbstractKernel =
  {
    val image = inputs(0)
    val imageType = image.fieldType
    val imagePlaneType = imageType.resizeTensor(Shape())

    val filter = inputs(1)
    val filterType = filter.fieldType
    val filterPlaneType = filterType.resizeTensor(Shape())

    require(imageType.tensorShape == filterType.tensorShape)
    require(imageType.tensorOrder == 1)

    val N = imageType.tensorColumns

    val inputPlanes = Array.tabulate(N) { i =>
      SliceVectorsHyperKernel(image, TensorSliceOp(i), imagePlaneType)
    }
    val filterPlanes = Array.tabulate(N) { i =>
      SliceVectorsHyperKernel(filter, TensorSliceOp(i), filterPlaneType)
    }
    val convolvedPlanes =  Array.tabulate(N) { i =>
      apply(Array(inputPlanes(i).outputs(0), filterPlanes(i).outputs(0)), op, imagePlaneType,
        fftUse, smallTensorUse, platformParams, profiler).outputs(0)
    }
    StackTensorsHyperKernel(convolvedPlanes, TensorStackOp(Shape(N)), imageType)
  }

  /** Perform a one-vector-plane-at-a-time convolution of a MatrixField input and a
    * MatrixField filter bank using separate kernels, stacking the resulting
    * VectorFields into a MatrixField output.
    */
  def matrixConvolveMatrix(inputs: Array[VirtualFieldRegister], op: ConvolveOp,
       resultType: FieldType, fftUse: ConvolutionFFTUsePolicy,
       smallTensorUse: ConvolutionSmallTensorUsePolicy,
       platformParams: OpenCLKernelCodeGenParams,
                           profiler: Profiler): AbstractKernel =
  {
    val image = inputs(0)
    val imageType = image.fieldType
    val imagePlaneType = imageType.resizeTensor(Shape(imageType.tensorColumns))

    val filter = inputs(1)
    val filterType = filter.fieldType
    val filterPlaneType = filterType.resizeTensor(Shape(filterType.tensorColumns))

    require(imageType.tensorShape == filterType.tensorShape)
    require(imageType.tensorOrder == 2)

    val N = imageType.tensorRows

    val inputPlanes = Array.tabulate(N) { i =>
      SliceMatricesHyperKernel(image, TensorSliceOp(i), imagePlaneType)
    }
    val filterPlanes = Array.tabulate(N) { i =>
      SliceMatricesHyperKernel(filter, TensorSliceOp(i), filterPlaneType)
    }
    val convolvedPlanes =  Array.tabulate(N) { i =>
      vectorConvolveVector(Array(inputPlanes(i).outputs(0), filterPlanes(i).outputs(0)), op,
        imagePlaneType, fftUse, smallTensorUse, platformParams, profiler).outputs(0)
    }
    StackTensorsHyperKernel(convolvedPlanes, TensorStackOp(Shape(N)), imageType)
  }

  /** Perform a convolution of a ScalarField input and a VectorField filter bank
    * using separate kernels, stacking the resulting ScalarFields into a
    * VectorField output.
    */
  def scalarConvolveVector(inputs: Array[VirtualFieldRegister], op: ConvolveOp,
       resultType: FieldType, fftUse: ConvolutionFFTUsePolicy,
       smallTensorUse: ConvolutionSmallTensorUsePolicy,
       platformParams: OpenCLKernelCodeGenParams, profiler: Profiler): AbstractKernel =
  {
    val image = inputs(0)
    val imageType = image.fieldType

    val filter = inputs(1)
    val filterType = filter.fieldType
    val filterPlaneType = filterType.resizeTensor(Shape())

    require(imageType.tensorOrder == 0)
    require(filterType.tensorOrder == 1)
    val N = filterType.tensorColumns
    val resultType = imageType.resizeTensor(Shape(N))

    val filterPlanes = Array.tabulate(N) { i =>
      SliceVectorsHyperKernel(filter, TensorSliceOp(i), filterPlaneType)
    }
    val convolvedPlanes =  Array.tabulate(N) { i =>
      apply(Array(image, filterPlanes(i).outputs(0)), op, imageType,
        fftUse, smallTensorUse, platformParams, profiler).outputs(0)
    }
    StackTensorsHyperKernel(convolvedPlanes, TensorStackOp(Shape(N)), resultType)
  }

  /** Perform a convolution of a ScalarField, VectorField or MatrixField input
    * and a ScalarField filter via the FFT.  This uses the slow technique of
    * slicing and stacking to handle the VectorField and MatrixField inputs.
    */
  def convolveBy2DFFT(inputs: Array[VirtualFieldRegister],
            op: ConvolveOp, resultType: FieldType): AbstractKernel =
  {
    val image = inputs(0)
    val filter = inputs(1)

    val filterRows = filter.fieldType.rows
    val filterColumns = filter.fieldType.columns

    val rowApronSize = filterRows / 2
    val columnApronSize = filterColumns / 2

    val expandedShape =
      FFTConvolutionUtils.fftShape2D(image.fieldType, filter.fieldType, op)
    val fftRows = expandedShape(0)
    val fftColumns = expandedShape(1)
    val realResultType = new FieldType(expandedShape, Shape(), Float32)
    val complexResultType = toComplex(realResultType)
    val scaling = 1f / (fftRows * fftColumns)

    def complexMatrixToKernel(kernelMatrix: ComplexMatrix): AbstractKernel = {
      val bigKernel =
        kernelMatrix.expand(fftRows, fftColumns, borderFill = false).
          shiftCyclic(-rowApronSize, -columnApronSize)
      val freqKernel = FFT2D.transform(bigKernel) * scaling
      FixedComplexMatrixKernel(freqKernel)
    }

    // Prep a dynamic filter: flip, expand, shift, fft, scale
    def dynamicFilterPrep: AbstractKernel = {
      val flippedAsNeeded =
        op.filterOrientation match {
          case ConvolutionOrientation =>
            filter
          case CrossCorrelationOrientation =>
            FlipHyperKernel(Array(filter), FlipOp, filter.fieldType).outputs(0)
        }
      val expandedType = filter.fieldType.resize(expandedShape)
      val expanded = ExpandBorderHyperKernel(flippedAsNeeded,
        ExpandBorderOp(expandedShape, BorderZero), expandedType)
      val shiftOp = ShiftOp(Array(-rowApronSize, -columnApronSize), BorderCyclic)
      val bigKernel = ShiftHyperKernel(Array(expanded.outputs(0)), shiftOp, expandedType)
      val freqKernel = FFT2DHyperKernel(bigKernel.outputs.toArray, FFT2DOp(scaling), Array(complexResultType))
      freqKernel

      // Using the integrated scaling above is probably not a huge win: the
      // multiply-const kernel that is saved would be merged into the downstream
      // spacial-domain complex multiply.  Here's the old code:

//      ComplexBinaryRealConstHyperKernel(Array(freqKernel),
//        ComplexMultiplyRealConstOp(scaling), complexResultType)
    }

    // Still some work to do to make purely functional and avoid instantiating
    // the entire kernel matrix
    val kernelSource: AbstractKernel = filter.source match {
      case k: ConstantFieldKernel =>
        k.opcode match {
          case ConstantScalar2DOp(f) =>
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrix =
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(filterRows, filterColumns,
                    (r,c) => Complex(f(r,c), 0f))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(filterRows, filterColumns,
                    (r,c) => Complex(f(filterRows - 1 - r, filterColumns - 1 - c), 0f))
              }
            complexMatrixToKernel(kernelMatrix)
          case ConstantComplex2DOp(f) =>
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrix =
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(filterRows, filterColumns,
                    (r,c) => f(r,c))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(filterRows, filterColumns,
                    (r,c) => f(filterRows - 1 - r, filterColumns - 1 - c))
              }
            complexMatrixToKernel(kernelMatrix)
          case x => throw new RuntimeException("Internal compiler error: " +
                  "unexpected constant opcode: " + x)
        }
      case x => dynamicFilterPrep
    }

    // Do a FFT - Multiply - InverseFFT on a single expande ScalarField image plane

    def processScalarImage(scalarImage: VirtualFieldRegister): AbstractKernel = {
      val expandedImageType = scalarImage.fieldType
      val freqImagePlane =
        FFT2DHyperKernel(Array(scalarImage), FFT2DOp(), Array(toComplex(expandedImageType)))

      // Do frequency domain convolution
      val freqConvolvePlane =
        ComplexBinaryHyperKernel(Array(freqImagePlane.outputs(0), kernelSource.outputs(0)),
          ComplexMultiplyOp, freqImagePlane.fieldType)

      // Inverse FFT and trimming.
      val spaceDomainPlaneType =
        if (isRealField(scalarImage.fieldType) && isRealField(filter.fieldType))
          toReal(expandedImageType)
        else
          toComplex(expandedImageType)

      val spaceDomainPlane =
        FFT2DHyperKernel(freqConvolvePlane.outputs.toArray, InverseFFT2DOp(), Array(spaceDomainPlaneType))
      spaceDomainPlane
    }

    // Do a FFT - Multiply - InverseFFT on a single VectorField image
    def processVectorImage(vectorImage: VirtualFieldRegister): AbstractKernel = {
      val N = vectorImage.fieldType.tensorShape.points
      val imagePlanes = Array.tabulate(N) { i =>
        SliceVectorsHyperKernel(vectorImage, TensorSliceOp(i), vectorImage.fieldType.resizeTensor(Shape())).outputs(0)
      }
      val trimmedPlanes = imagePlanes.map(processScalarImage(_).outputs(0)).toArray
      StackTensorsHyperKernel(trimmedPlanes, TensorStackOp(Shape(N)), vectorImage.fieldType)
    }

    // Do a FFT - Multiply - InverseFFT on a single MatrixField image
    def processMatrixImage(matrixImage: VirtualFieldRegister): AbstractKernel = {
      val matrixShape = matrixImage.fieldType.tensorShape
      val matrixRows = matrixShape(0)
      val matrixCols = matrixShape(1)
      val vectorPlanes = Array.tabulate(matrixRows) { i =>
        SliceMatricesHyperKernel(matrixImage, TensorSliceOp(i), matrixImage.fieldType.resizeTensor(Shape(matrixCols))).outputs(0)
      }
      val trimmedVectorPlanes = vectorPlanes.map(processVectorImage(_).outputs(0)).toArray
      StackTensorsHyperKernel(trimmedVectorPlanes, TensorStackOp(Shape(matrixRows)), matrixImage.fieldType)
    }

    // Expand the input, best done as a single kernel before slicing
    val expandedPlaneType = image.fieldType.resize(expandedShape)
    val expandImage =
      ExpandBorderHyperKernel(image, ExpandBorderOp(expandedShape, op.borderPolicy), expandedPlaneType)

    // Slice (as needed) - FFT - Multiply - FFT-Inverse - Stack (as needed)
    val unTrimmedResult = expandImage.fieldType.tensorOrder match {
      case 0 => processScalarImage(expandImage.outputs(0))
      case 1 => processVectorImage(expandImage.outputs(0))
      case 2 => processMatrixImage(expandImage.outputs(0))
      case x => throw new RuntimeException("Unexpected tensor dim: " + x)
    }

    // Trim the result back to the original image size, best done as a single
    // kernel after stacking
    val trimmedResult =
      TrimHyperKernel(Array(unTrimmedResult.outputs(0)),
        TrimOp(image.fieldType.fieldShape),
        unTrimmedResult.fieldTypes(0).resize(image.fieldType.fieldShape))
    trimmedResult
  }

  def convolveBy3DFFT(inputs: Array[VirtualFieldRegister],
            operation: ConvolveOp, resultType: FieldType): AbstractKernel =
  {
    throw new RuntimeException("3D Convolution not yet supported")
  }

  def convolveDirectly(inputs: Array[VirtualFieldRegister],
            operation: ConvolveOp, resultType: FieldType,
            smallTensorUse: ConvolutionSmallTensorUsePolicy,
            platformParams: OpenCLKernelCodeGenParams,
            profiler: Profiler): AbstractKernel =
  {
    // Direct 1D or 2D real field convolution via the ConvolveHyperKernel
    // The are many options to cover here, based on:
    //
    val borderPolicy = operation.borderPolicy
    val samplingPolicy = operation.samplingPolicy
    val filterOrientation = operation.filterOrientation

    // Ensure that filters have field shapes that are odd if the filter needs
    // to be centered on each input point
    val filterShape = inputs(1).fieldType.fieldShape.toArray
    borderPolicy match {
      case BorderValid =>
      case BorderFull =>
      case other =>
        require(filterShape.map((x) => (x % 2) == 1 ).reduce(_&&_),
          "filter must be of odd shape for border policy " + other)
    }

    //Ensure that the operation has a well-defined adjoint by requiring that
    // the input shape is an integer multiple of the subsampling step after border
    // expansion/contraction. The user must deal with shaping the input if it
    // fails.
    val inputShape = inputs(0).fieldType.fieldShape.toArray
    val borderContraction = borderPolicy match {
      case BorderValid => filterShape.map(_-1)
      case BorderFull => filterShape.map(1 - _)
      case _ => filterShape.map(_*0)
    }
    val inputSizeValid = samplingPolicy match {
      case DownsampleOutputConvolution(step) =>
        Array.tabulate(inputShape.length){
          (i) => (inputShape(i)-borderContraction(i)) % step  == 0
        }.reduce(_&&_)
      case _ => true
    }
    require(inputSizeValid, "The size of the input for convolution must be " +
            "divisible by the downsampling step size after border contraction.")

    // Integrated upsampling with the BorderClamp policy is not a wise thing
    // to: it has no adjoint and produces terrible edge effects.  Help the
    // user out by throwing an exception so he/she find a better approach.
    samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        if (step > 1 && borderPolicy == BorderClamp)
          throw new RuntimeException("Integrated upsampling with BorderClamp convolution not supported")
      case _ => false
    }

    ConvolveHyperKernel(inputs, operation, resultType, smallTensorUse, platformParams, profiler)
  }
}
