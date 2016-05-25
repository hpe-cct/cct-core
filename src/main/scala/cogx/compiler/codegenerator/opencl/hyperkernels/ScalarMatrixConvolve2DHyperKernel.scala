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
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op.ScalarMatrixConvolve2DOp
import cogx.cogmath.geometry.Shape

/** StaticConvolution of a 2D scalar field with a 2D matrix field, producing a 2D
  * vector field as output.
  *
  * The matrices in the input matrix field serve as convolution kernels for
  * the input scalar field. The output vector field has the same shape as the
  * input scalar field, but the size of each vector is equal to the number of
  * matrices in the input matrix field. Each "tensor element slice" of the
  * output vector field represents the convolution of the input scalar field
  * with the corresponding matrix in the matrix field.
  *
  * @param inScalar Virtual field register for the input 2D scalar field.
  * @param inMatrix Virtual field register for the input 2D matrix field.
  * @param operation Opcode for hyperkernel.
  * @param outVectorType Type of output vector field.
  *
  * @author Greg Snider
  */
private[cogx]
class ScalarMatrixConvolve2DHyperKernel private (inScalar: VirtualFieldRegister,
                                                 inMatrix: VirtualFieldRegister,
                                                 operation: ScalarMatrixConvolve2DOp,
                                                 outVectorType: FieldType)
        extends HyperKernel(operation, Array(inScalar, inMatrix), outVectorType,
          BigTensorAddressing)
{
  val filterSize = inMatrix.fieldType.tensorShape(0)
  val matrixRows = inMatrix.fieldType.rows
  val matrixColumns = inMatrix.fieldType.columns
  val haloSize = filterSize / 2
  val inType = CLFloat
  val code = new StringBuffer

  // Read in a tile of the input scalar field to local memory.
  code append LocalTensorMemory2D(inScalar.fieldType.fieldShape, inType,
    haloSize, haloSize, haloSize, haloSize, operation.borderPolicy)

  // Image tile is cached. Step through each matrix in the matrix field,
  // using it as a filter, and convolve it with the input tile. Each work-item
  // will filter around its start location, starting from the filter radius
  // left and up.
  code append """
    // Loop through every filter in matrix field. The "row" and "column"
    // variables are used for nonlocal addressing in the input matrix field.
    int outVectorElement = 0;
    for (row = 0; row < %matrixFieldRows%; row++) {
      for (column = 0; column < %matrixFieldColumns%; column++) {

        // Loop through every element of every filter.
        // NOTE: this is coded for cross-correlation only right now.

        float sum = 0.0f;  // convolutional sum at one point
        int inTensorElement = 0;  // nonlocal index into input filter
        for (int r = _localRow; r < _localRow + %filterSize%; r++) {
          for (int c = _localColumn; c < _localColumn + %filterSize%; c++) {
            // Input image element
            float inputPixel = localImage[r][c];
            // Filter element
            tensorElement = inTensorElement++;
            float filterElement = readNonlocal(@in1);
            sum += inputPixel * filterElement;
          }
        }

        // Write convolutional sum for this point.
        tensorElement = outVectorElement++;
        @outElement0 = sum;
      }
    }
  """

  val codeString = code.toString.
          replaceAll("%matrixFieldRows%", matrixRows.toString).
          replaceAll("%matrixFieldColumns%", matrixColumns.toString).
          replaceAll("%filterSize%", filterSize.toString)
  addCode(codeString)

  val Debug = false
  if (Debug)
    debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ScalarMatrixConvolve2DHyperKernel {
  /** Create a HyperKernel that convolves a scalar field with a
    * matrix field, producing a matrix field.
    *
    * @param inScalar Virtual field register for the input 2D scalar field.
    * @param inMatrix Virtual field register for the input 2D matrix field.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(inScalar: VirtualFieldRegister, inMatrix: VirtualFieldRegister,
            operation: ScalarMatrixConvolve2DOp, resultType: FieldType): HyperKernel =
  {
    operation.borderPolicy match {
      case BorderClamp =>
      case BorderCyclic =>
      case BorderZero =>
      case BorderFull =>
        throw new RuntimeException("unsupported: " + operation.borderPolicy)
      case BorderValid =>
        throw new RuntimeException("unsupported: " + operation.borderPolicy)
    }
    require(operation.samplingPolicy == NoSamplingConvolution,
      "sampling not supported yet")
    require(operation.filterOrientation == CrossCorrelationOrientation,
      "convolution not supported yet")
    val scalarType = inScalar.fieldType
    val matrixType = inMatrix.fieldType
    require(matrixType.fieldShape.dimensions == 2, "matrix field dimensions")
    require(matrixType.tensorShape.dimensions == 2, "Bad matrix shape")
    require(matrixType.tensorShape(0) == matrixType.tensorShape(1),
      "matrix in field must be square")
    require(matrixType.tensorShape(0) % 2 == 1,
      "matrix in field must have odd size")
    val resultFieldShape = scalarType.fieldShape
    val resultTensorShape = Shape(matrixType.fieldShape.points)
    val expectedResultType =
      new FieldType(resultFieldShape, resultTensorShape, Float32)
    require(expectedResultType == resultType)
    new ScalarMatrixConvolve2DHyperKernel(inScalar, inMatrix, operation, resultType)
  }
}

