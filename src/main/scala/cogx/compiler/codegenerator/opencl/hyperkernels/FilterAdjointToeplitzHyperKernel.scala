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
import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types._
import cogx.compiler.parser.op.FilterAdjointToeplitzOp

/** Create a Toeplitz matrix from a field typically used as the input of "project frame" operator.  The matrix
  * can then be used in a matrix-matrix multiply that performs the "filter adjoint" operation (the back
  * propagation of output gradients to a bank of filters).
  *
  * Some context for this operator (currently explained assuming an unbatched input):
  *
  * The forward "project frame" operation performs a convolution of a multi-feature-plane input with a bank of
  * independent filters.  The convolution can be strided to effectively produce a downsampled output and also
  * specifies a "border policy."  This kernel currently supports only the "BorderValid" policy that stipulates
  * the filters never extend beyond the boundaries of the input.  The shapes of the inputs and outputs are:
  *
  *                Field type         Field shape                          Tensor shape
  *
  * Image Input:  VectorField        (InputRows, InputColumns)             (InputFeatures)
  *
  * Filter Input: VectorField        (FilterRows, FilterColumns)           (InputFeatures * NumFilters)
  *
  * Image Output: VectorField        (OutputRows, OutputColumns)           (NumFilters)
  *
  * The various sizes given are related (for downsample stride 1) by the relationships:
  *
  * InputRows = FilterRows + OutputRows - 1
  * InputColumns = FilterColumns + OutputColumns - 1
  *
  * Things are a bit trickier with a strides > 1.  This platform takes the approach that the filter is first conceptually
  * convolved with a stride of 1, then the resulting intermediate is downsampled by the stride.  Further, the downsampled
  * field must be a multiple of the stride.  The relationships for stride > 1 then are:
  *
  * InputRows = FilterRows + stride * OutputRows - 1
  * InputColumns = FilterColumns + stride * OutputColumns - 1
  *
  * We turn now to the "filter adjoint" operation used to backprop the "project frame" operation described above.
  *
  * The field sizes involved when the filter adjoint operation is viewed as a cross-correlation, using the terminology
  * set forth above:
  *
  *                    Field type         Field shape                             Tensor shape
  *
  * Gradient "Input":  VectorField        (OutputRows, OutputColumns)             (NumFilters)
  *
  * Image Input:       VectorField        (InputRows, InputColumns)               (InputFeatures)
  *
  * Filter Output:     VectorField        (FilterRows, FilterColumns)             (InputFeatures * NumFilters)
  *
  * Now, to cast the filter adjoint operation as a single matrix multiply, we use 0D MatrixFields of the following sizes:
  *
  *                       Field type   Field shape   Tensor shape
  *
  * Gradient "Input":     MatrixField     ()         (NumFilters, OutputRows * OutputColumns)
  *
  * Image Input Toeplitz: MatrixField     ()         (OutputRows * OutputColumns, InputFeatures * FilterRows * FilterCoumns)
  *
  * Filter Output:        MatrixField     ()         (NumFilters, InputFeatures * FilterRows * FilterColumns)
  *
  * Now, the punchline: this kernel transforms the "Image Input" of the project frame operation (described first above)
  * into the "Image Input Toeplitz" matrix, which can then be used to perform the back-project convolution as a matrix
  * multiply.  This generally involves a duplication and shuffling of values.  The sizes of the transformation are:
  *
  *                Field type         Field shape          Tensor shape
  *
  * Image Input:  VectorField  (InputRows, InputColumns)   (InputFeatures)
  *
  * Image Input
  * Toeplitz:     MatrixField             ()    (OutputRows * OutputColumns, InputFeatures * FilterRows * FilterColumns)
  *
  * We finally explain why this kernel only supports a filter orientation of CrossCorrelateOrientation.  The reason
  * gets back to the adjointness rules for convolution/crosscorrelation:
  *
  * For stride = 1 (i.e. no downsampling) for a forward crosscorrelation:
  *
  * Y = X crosscorrelate_border_valid W
  * dX = dY convolve_border_full W              // X adjoint
  * dW = X crosscorrelate_border_valid dY       // W adjoint
  *
  * For stride = N (i.e. a downsampling by N) for a forward crosscorrelation:
  *
  * Y = downsample(X crosscorrelate_border_valid W, N)
  * dX = upsample(dY,N) convolve_border_full W
  * dW = X crosscorrelate_border_valid upsample(dY, N)
  *
  *
  * For stride = 1 (i.e. no downsampling) for a forward convolution:
  *
  * Y = X convolve_border_valid W
  * dX = dY crosscorrelate_border_full W
  * dW = flip(X) convolve_border_valid dY    or dW = flipped(X crosscorrelate_border_valid dY)
  *
  * For stride = N (i.e. a downsampling by N) for a forward convolution:
  *
  * Y = downsample(X convolve_border_valid W, N)
  * dX = upsample(dY,N) crosscorrelate_border_full W
  * dW = flip(X) convolve_border_valid upsample(dY, N)    or dW = flipped(X crosscorrelate_border_valid upsample(dY, N))
  *
  * Our ConvolveHyperKernel, while it has a flag for specifying the flipping of the second input
  * (filterOrientation=ConvolveOrientation), it does not flip the first input.  Thus, it's probably just as easy
  * to only use the filter adjoint form:
  *
  * dW = flipped(X crosscorrelate_border_valid upsample(dY, N))
  *
  * Thus, we only anticipate seeing the FilterAdjoint vectorMode coupled with a filterOrientation
  * of CrossCorrelationOrientation.  We need not support the ConvolutionOrientation in this alternate approach
  * to the FilterAdjoint calculation.
  *
  * Finally, some thoughts on future work:
  *
  * For AlexNet layer1, batchSize=512, the Toeplitz matrix size is roughly 2.1 GB.
  * This large size has multiple downsides:
  *   - it requires Java reflection to create such a buffer through the JOCL interface (a fragile technique).
  *   - it cannot be easily inspected from Java due to the 2GB limit for DirectBuffers.
  *   - it might exceed the single-allocation limit of the GPU (as it would for GPU's like the NVIDIA 1080 with 8GB)
  *   - it would bulk up the global memory footprint of the model, perhaps exceeding the total allocation limit.
  *
  * An approach to improve this situation would be to generate the Toeplitz matrix in chunks.  The chunks would be
  * vertical slices through the monolithic matrix.  For AlexNet Layer1 for example, one natural division would be
  * into 3 matrices, each of 121 columns, corresponding to the red, green, blue layers of the input.  After multiplying
  * the gradient matrix by the 3 matrices separately, the 3 result matrices could be consolidated into a single
  * interleaved matrix:
  *
  * val redFilterGradients = gradientIn.transform(redToeplitz)
  * val greenFilterGradients = gradientIn.transform(greenToeplitz)
  * val blueFilterGradients = gradientIn.transform(blueToeplitz)
  *
  * val redReshaped = redFilterGradients.reshape(Shape(11*11), Shape(96))
  * val greenReshaped = greenFilterGradients.reshape(Shape(11*11), Shape(96))
  * val blueReshaped = blueFilterGradients.reshape(Shape(11*11), Shape(96))
  *
  * val stacked = stack(redReshaped, greenReshaped, blueReshaped)
  *
  * val filterGradient = stacked.reshape(Shape(11,11), Shape(3*96))
  *
  * While this approach has the advantage of reducing the Toeplitz matrix footprint (since the chunks would likely
  * share buffers), the small transform outputs might be less efficiently generated. For example, the highest
  * efficiency matrix multiply currently has a "mini-tile" size of 8, so that kernel would run with only
  * 11*11*96/8 = 1400 threads,  Some possible fixes could be:
  *
  *   - smaller workgroup size to boost the number of workgroups
  *   - smaller mini-tile size, but not 1x1
  *   - field.transform(fields: Array[Field])                  (might throw away footprint reduction benefit)
  *   - get concurrent kernel execution working
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register, an image of multiple feature planes.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel, a 0D MatrixField to be matrix-multiply ready.
  */
private[cogx]
class FilterAdjointToeplitzHyperKernel private (in: VirtualFieldRegister,
                                       operation: FilterAdjointToeplitzOp,
                                       resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, SmallTensorAddressing)
{
  // inputFeatures = K
  // filterPoints = F^2
  // gradientRows * gradientCols = N^2 / S^2

  import operation._
  val inType = in.fieldType
  require(inType.dimensions == 2, s"Expecting 2D input field, found $inType")
  val inputRows = inType.rows
  val inputColumns = inType.columns
  val inputFeatures = inType.tensorShape.points / batchSize

  require(borderPolicy == BorderValid, s"kernel $this only supports BorderValid convolution.")
  require(filterOrientation == CrossCorrelationOrientation,
    s"kernel $this only supports CrossCorrelationOrientation filterOrientation.")
  require(filterShape.dimensions == 2, s"kernel $this only supports 2D filters.")
  val (filterRows, filterColumns) = (filterShape(0), filterShape(1))
  val filterPoints = filterRows * filterColumns
  require(resultType.tensorColumns % filterPoints == 0, s"Internal error: unexpected size of result type $resultType.")
  require(resultType.tensorColumns / filterPoints == inputFeatures,
    s"Internal error: unexpected size of result type $resultType.")

  val stride = samplingPolicy match {
    case NoSamplingConvolution => 1
    case UpsampleInputConvolution(stride) => stride
    case x: DownsampleOutputConvolution => throw new RuntimeException(s"Unexpected sampling policy $x")
  }

  require((inputRows - filterRows + 1) % stride == 0,
    s"Improper sized input for BorderValid convolution- inputRows=$inputRows, filterRows=$filterRows, stride=$stride")
  require((inputColumns - filterColumns + 1) % stride == 0,
    s"Improper sized input for BorderValid convolution- inputCols=$inputColumns, filterCols=$filterColumns, stride=$stride")

  val gradientRows = (inputRows - filterRows + 1) / stride
  val gradientColumns = (inputColumns - filterColumns + 1) / stride
  val gradientPoints = gradientRows * gradientColumns

  val numImages = untilImage - fromImage
  val expectedResultRows = numImages * gradientPoints
  require(resultType.tensorRows == expectedResultRows, s"Expecting $expectedResultRows result field tensor rows, found ${resultType.tensorRows}.")
  // 0D fields are run by default by 1D kernels with 256-long 1D local work groups.
  // We want this kernel to run more like a 2D kernel, so we set the fictional
  // workfieldtype to be a ScalarField whose fieldshape is the kernel's result tensorshape.

  override lazy val workFieldType = new FieldType(resultType.tensorShape, Shape(), resultType.elementType)

  val filterElements = filterPoints * inputFeatures

  // Remember that the threads are assigned based on a virtual workField that has rows and columns equal
  // to the tensorRows and tensorColumns of the output 0D MatrixField.  We make the adjustments to
  // tensorElement to make a "pseudo-local" write of the output.

  val code =
    s"""| int filterElementIndex = _column % $filterPoints;
        | int filterRow = filterElementIndex / $filterColumns;
        | int filterColumn = filterElementIndex % $filterColumns;
        | int imageBatchIndex = $fromImage + _row / $gradientPoints;
        | int gradientOffset = _row % $gradientPoints;
        | int gradientRow = gradientOffset / $gradientColumns;
        | int gradientColumn = gradientOffset % $gradientColumns;
        |
        | // read input
        | int row = gradientRow * $stride + filterRow;
        | int column = gradientColumn * $stride + filterColumn;
        | int filterPlane = _column / $filterPoints;
        | int tensorElement = imageBatchIndex * $inputFeatures + filterPlane;
        | float inputVal = readElementNonlocal(@in0);
        |
        | // write output
        | row = 0;
        | column = 0;
        | tensorElement = _row * $filterElements + _column;
        | @outElementNonlocal0 = inputVal;
        |""".stripMargin

  addCode(code)
  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object FilterAdjointToeplitzHyperKernel {

  /** Create a hyperkernel that reshapes a field.
    *
    * @param in The input virtual field register
    * @param op The unary opcode for this operation.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, op: FilterAdjointToeplitzOp, resultType: FieldType): HyperKernel = {

    new FilterAdjointToeplitzHyperKernel(in, op, resultType)
  }
}