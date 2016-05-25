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

import cogx.platform.types._
import cogx.platform.types.ElementTypes.Complex32
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.api.ImplicitConversions
import cogx.compiler.parser.op.ConvolveOp
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Logarithm
import cogx.cogmath.algebra.complex.{ComplexVector, Complex, ComplexMatrix}
import cogx.cogmath.fft.FFT2D
import cogx.compiler.codegenerator.opencl.cpukernels.ConstantFieldKernel

/** Code generation for convolution with a kernel that is dynamically altered at
  * runtime.  There are many cases to cover here, based on:
  *
  * 1. What type of field, e.g. Scalar, Vector, Color or Complex, the inputs are.
  * 2. Whether the filter field (or image field for that matter) is truly
  *    dynamic, or is sourced from a static field (with no recurrence).
  * 3. Whether the input is large, and may be more efficiently handled by
  *    an FFT-based approach.
  * 4. The dimensionality of the convolution, 1D, 2D or 3D.
  *
  * This was a clone of the DynamicConvolutionGenerator designed to process
  * just vector and matrix convolutions via the FFT.  This is done with a
  * "private" double-deep real vector or matrix field to hold the intermediate
  * complex fields.  This class will likely soon go away once HyperFields are
  * introduced along with multi-output kernels.
  *
  * @author Dick Carter
  */
private [cogx]
object FFTVectorConvolutionGenerator
        extends ImplicitConversions
        with Logarithm
{
  /** Handle FFT-based convolution where either image or filter (or both) are a
    * VectorField or MatrixField.  The inputs and outputs are necessarily
    * real (since Cog doesn't support complex vector fields natively).  However,
    * a VectorField with 2X the number of tensor elements is used privately by
    * this generator and the kernels it invokes: the FFT2DVectorHyperKernel and
    * the FFTMultiplyHyperKernel (see further comments there for the data layout).
    */
  def apply(inputs: Array[VirtualFieldRegister],
            op: ConvolveOp, resultType: FieldType): AbstractKernel =
  {
    val image = inputs(0)
    val kernel = inputs(1)

    val imageType = image.fieldType
    val kernelType = kernel.fieldType

    val kernelRows = kernelType.rows
    val kernelColumns = kernelType.columns

    require(imageType.tensorShape == kernelType.tensorShape ||
            isTensor0Field(imageType) || isTensor0Field(kernelType),
    "incompatible field types for vector convolution: " + imageType + ", " +
    kernelType)

    // The tensor shape of the result (after the inverse FFT) */
    val resultTensorShape =
      if (isTensor0Field(imageType))
        kernelType.tensorShape
      else
        imageType.tensorShape

    // Compute size of FFT image. Note that we use 2 * apronSize
    // to accomodate border fill. For zero fill, 1 * apronSize is
    // sufficient.  For cyclic fill, if the image were a power of 2, technically
    // no border would be required; however, if any expansion is performed, it
    // had better be at least 2 * apronSize.  For now, since we have a further
    // complication with the alignment padding, safest is to always add 2 * apronSize.

    val rowApronSize = kernelRows / 2
    val columnApronSize = kernelColumns / 2

    val expandedShape =
      FFTConvolutionUtils.fftShape2D(image.fieldType, kernel.fieldType, op)
    val fftRows = expandedShape(0)
    val fftColumns = expandedShape(1)

    val scaling = 1f / (fftRows * fftColumns)

    def complexMatrixToKernel(kernelMatrix: ComplexMatrix): AbstractKernel = {
      val bigKernel =
        kernelMatrix.expand(fftRows, fftColumns, borderFill = false).
          shiftCyclic(-rowApronSize, -columnApronSize)
      val freqKernel = FFT2D.transform(bigKernel) * scaling
      val resultType = new FieldType(Shape(freqKernel.rows, freqKernel.columns), Shape(), Complex32)
      val opcode = ConstantComplex2DOp((r,c) => freqKernel(r,c))
      new ConstantFieldKernel(resultType, opcode)
    }

    /** Converts an array of constant complex filter matrices into the form needed
      * for a vector FFT.
      */
    def complexMatricesToConstantVectorFieldKernel(kernelMatrices: Array[ComplexMatrix]): AbstractKernel = {
      val N = kernelMatrices.length
      val bigKernels = kernelMatrices.map(
        _.expand(fftRows, fftColumns, borderFill = false).
          shiftCyclic(-rowApronSize, -columnApronSize))
      val freqKernels = bigKernels.map(FFT2D.transform(_) * scaling)
      val freqKernelShape = Shape(freqKernels(0).rows, freqKernels(0).columns)
      val resultType =
        new FieldType(freqKernelShape, Shape(N), Complex32)
      val opcode = ConstantComplexVector2DOp((r,c) =>
        ComplexVector(N, (i) => freqKernels(i)(r,c))
      )
      new ConstantFieldKernel(resultType, opcode)
    }

    // Prep a dynamic filter: flip, expand, shift, fft, scale
    def dynamicFilterPrep(scaleFactor: Float) = {
      val flippedAsNeeded =
        op.filterOrientation match {
          case ConvolutionOrientation =>
            kernel
          case CrossCorrelationOrientation =>
            FlipHyperKernel(Array(kernel), FlipOp, kernel.fieldType).outputs(0)
        }
      val expandedType = kernel.fieldType.resize(expandedShape)
      val expanded = ExpandBorderHyperKernel(flippedAsNeeded,
        ExpandBorderOp(expandedShape, BorderZero), expandedType)
      val shiftOp = ShiftOp(Array(-rowApronSize, -columnApronSize), BorderCyclic)
      val bigKernel = ShiftHyperKernel(Array(expanded.outputs(0)), shiftOp, expandedType)
      val freqKernelType = toComplex(bigKernel.fieldType)

      val freqKernel = FFT2DHyperKernel(bigKernel.outputs.toArray, FFT2DOp(scaleFactor), Array(freqKernelType))
      freqKernel
    }

    // The scale factor can be applied to either the image or the filter.
    // Since the filter is constant sometimes, we fold the scaling into the filter always.
    // I experimented with applying the scaling to the image for the
    // ScalarField image convolve VectorField filter case, but saw no discernable
    // speed-up over always applying the scaling to the filter, so I left it

    // Still some work to do to make purely functional and avoid instantiating
    // the entire kernel matrix
    val kernelSource = kernel.source match {
      case k: ConstantFieldKernel =>
        k.opcode match {
          case ConstantScalar2DOp(f) =>
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrix =
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => Complex(f(r,c), 0f))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => Complex(f(kernelRows - 1 - r, kernelColumns - 1 - c), 0f))
              }
            complexMatrixToKernel(kernelMatrix)
          case ConstantComplex2DOp(f) =>
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrix =
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => f(r,c))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => f(kernelRows - 1 - r, kernelColumns - 1 - c))
              }
            complexMatrixToKernel(kernelMatrix)
          case ConstantVector2DOp(f) =>
            val vectorLength = f(0,0).length
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrices = Array.tabulate(vectorLength) { i =>
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => Complex(f(r,c)(i), 0f))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => Complex(f(kernelRows - 1 - r, kernelColumns - 1 - c)(i), 0f))
              }
            }
            complexMatricesToConstantVectorFieldKernel(kernelMatrices)
          case ConstantComplexVector2DOp(f) =>
            val vectorLength = f(0,0).length
            // Perform kernel flip if needed for cross-correlation
            val kernelMatrices = Array.tabulate(vectorLength) { i =>
              op.filterOrientation match {
                case ConvolutionOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => f(r,c)(i))
                case CrossCorrelationOrientation =>
                  ComplexMatrix(kernelRows, kernelColumns,
                    (r,c) => f(kernelRows - 1 - r, kernelColumns - 1 - c)(i))
              }
            }
            complexMatricesToConstantVectorFieldKernel(kernelMatrices)
          case ConstantMatrix2DOp(f) =>
            throw new RuntimeException("Convolution of images or filters " +
            "organized as MatrixFields not supported- use VectorFields.")
          case x => throw new RuntimeException("Internal compiler error: " +
                  "unexpected constant opcode: " + x)
        }
      case x => dynamicFilterPrep(scaling)
    }

    // Convert the image to complex if necessary and expand the borders.
    val expandedImageType = imageType.resize(expandedShape)
    val expandImage =
      ExpandBorderHyperKernel(image, ExpandBorderOp(expandedShape, op.borderPolicy), expandedImageType)

    val freqImageType = toComplex(expandedImageType)
    val freqImage = FFT2DHyperKernel(expandImage.outputs.toArray, FFT2DOp(), Array(freqImageType))

    // Do frequency domain convolution
    val freqConvolvedType = freqImage.fieldType.resizeTensor(resultTensorShape)
    val freqConvolved = ComplexBinaryHyperKernel(Array(freqImage.outputs(0), kernelSource.outputs(0)),
      ComplexMultiplyOp, freqConvolvedType)

    // Inverse FFT and trimming.
    val spaceDomainType =
      if (isComplexField(imageType) || isComplexField(kernelType))
        freqConvolvedType
      else
        toReal(freqConvolvedType)

    val spaceDomain =
      FFT2DHyperKernel(freqConvolved.outputs.toArray, InverseFFT2DOp(), Array(spaceDomainType))

    val trimmed =
      TrimHyperKernel(Array(spaceDomain.outputs(0)),
        TrimOp(image.fieldType.fieldShape),
        spaceDomainType.resize(image.fieldType.fieldShape))

    trimmed
  }
}
