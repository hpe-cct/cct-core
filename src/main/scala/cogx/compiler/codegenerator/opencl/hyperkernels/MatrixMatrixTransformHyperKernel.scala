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

import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.MatrixTransformMatrixOp

/** Transform all matrices in a matrix field (second input) with corresponding
  * matrices in a matrix field (first input).  The field Shapes must match, or
  * at least one must be 0D.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param op The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class MatrixMatrixTransformHyperKernel private (in: Array[VirtualFieldRegister],
                                                val op: MatrixTransformMatrixOp,
                                                val resultType: FieldType)
        extends HyperKernel(op, in, resultType, BigTensorAddressing)
{
  val matrix1Type = in(0).fieldType
  val matrix1Shape = matrix1Type.tensorShape
  val matrix1Columns = matrix1Shape(1)
  val matrix1Rows = matrix1Shape(0)

  val matrix2Type = in(1).fieldType
  val matrix2Shape = matrix2Type.tensorShape
  val matrix2Columns = matrix2Shape(1)
  val matrix2Rows = matrix2Shape(0)

  val resultShape = resultType.tensorShape
  val resultColumns = resultShape(1)
  val resultRows = resultShape(0)

  val dotProductSize =
    if (op.transposeIn1) matrix1Type.tensorRows else matrix1Type.tensorColumns

  val code = new StringBuffer
  code append "\n"

  // loop through all tensor elements
  // Coded as one outer loop to aide unrolling (unconfirmed hypothesis)
  code append s"int outRow = 0, outColumn = 0;\n"
  code append s"for (int outIndex = 0; outIndex < _tensorElements; outIndex++) {\n"
  code append s"    float sum = 0.0f;\n"
  code append s"    for (int i = 0; i < $dotProductSize; i++) {\n"
  // Read matrix1 element
  if (op.transposeIn1)
    code append s"        tensorElement = $matrix1Columns * i + outRow;\n"
  else
    code append s"        tensorElement = $matrix1Columns * outRow + i;\n"

  // FYI, readPoint generates the same code as readElement, but because _inX_rows
  // and _inX_layers are not defined for 0D fields, and since _inX_columns == 1,
  // readElement does the right thing (it ignores _row and _column).
  // Nevertheless, I feel the following coding is safer:
  if (matrix1Type.dimensions == 0)
    code append s"        float matrix1Element = readPoint(@in0);\n"
  else
    code append s"        float matrix1Element = readElement(@in0);\n"
  // Read matrix2 element
  if (op.transposeIn2)
    code append s"        tensorElement = $matrix2Columns * outColumn + i;\n"
  else
    code append s"        tensorElement = $matrix2Columns * i + outColumn;\n"

  if (matrix2Type.dimensions == 0)
    code append s"        float matrix2Element = readPoint(@in1);\n"
  else
    code append s"        float matrix2Element = readElement(@in1);\n"
  code append s"        sum += matrix1Element * matrix2Element;\n"
  code append s"    }\n"
  code append s"    if (outColumn < " + (resultColumns - 1) + ") {\n"
  code append s"        outColumn++;\n"
  code append s"    }\n"
  code append s"    else {\n"
  code append s"        outColumn = 0;\n"
  code append s"        outRow++;\n"
  code append s"    }\n"

  code append s"    tensorElement = outIndex;\n"
  code append s"    @outElement0 = sum;\n"
  code append s"}\n"

  addCode(code.toString)
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object MatrixMatrixTransformHyperKernel {
  /**
   * Creates a kernel that performs the matrix field transform.
   *
   * @param in The two input virtual field registers driving this kernel.
   * @param op The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister],
            op: MatrixTransformMatrixOp,
            resultType: FieldType): HyperKernel =
  {
    require(in.length == 2, "Expecting two inputs, found " + in.length)
    val matrix1Type = in(0).fieldType
    val matrix1Shape = matrix1Type.tensorShape
    val matrix1Columns = matrix1Shape(1)
    val matrix1Rows = matrix1Shape(0)

    val matrix2Type = in(1).fieldType
    val matrix2Shape = matrix2Type.tensorShape
    val matrix2Columns = matrix2Shape(1)
    val matrix2Rows = matrix2Shape(0)

    val dotProductSizePerIn1 =
      if (op.transposeIn1) matrix1Type.tensorRows else matrix1Type.tensorColumns
    val dotProductSizePerOp2 =
      if (op.transposeIn2) matrix2Type.tensorColumns else matrix2Type.tensorRows

    require(dotProductSizePerIn1 == dotProductSizePerOp2,
      "Incompatible matrix shapes: " + matrix1Type.tensorShape +
      " * " + matrix2Type.tensorShape + ", op(transposeIn1,transposeIn2) = " + op)

    require(matrix1Type.dimensions == 0 ||
            matrix2Type.dimensions == 0 ||
            matrix1Type.fieldShape == matrix2Type.fieldShape,
      "Matrix1 and Matrix2 field shapes must match or be 0D: " +
              matrix1Type.fieldShape + " " + matrix2Type.fieldShape)
    require(matrix1Shape.dimensions == 2, "Bad matrix1 shape: " + matrix1Shape)
    require(matrix2Shape.dimensions == 2, "Bad matrix2 shape: " + matrix2Shape)

    val expectedResultFieldShape =
      if (matrix1Type.dimensions == 0)
        matrix2Type.fieldShape
      else
        matrix1Type.fieldShape
    require(resultType.fieldShape == expectedResultFieldShape,
      "Result field shape mismatch: expected " + expectedResultFieldShape + ", actual " + resultType.fieldShape)
    val resultRows =
      if (op.transposeIn1) matrix1Type.tensorColumns else matrix1Type.tensorRows
    val resultColumns =
      if (op.transposeIn2) matrix2Type.tensorRows else matrix2Type.tensorColumns
    val expectedResultTensorShape = Shape(resultRows, resultColumns)
    require(resultType.tensorShape == expectedResultTensorShape,
      "Result tensor shape mismatch: expected " + expectedResultTensorShape + ", actual " + resultType.tensorShape)

    val expectedResultType =
      new FieldType(expectedResultFieldShape, expectedResultTensorShape, matrix1Type.elementType)
    require(resultType == expectedResultType,
      "Unexpected result FieldType: expected " + expectedResultType + ", actual " + resultType)

    val approxTiledKernelThreads = resultType.tensorShape.points / (MatrixMatrixTransform0DFieldTiledHyperKernel.tileSize)

    // An estimate, should be benchmarked.
    val threadThreshold = 8000

    if (matrix1Type.dimensions == 0 && matrix2Type.dimensions == 0 && approxTiledKernelThreads > threadThreshold)
      new MatrixMatrixTransform0DFieldTiledHyperKernel(in, op, resultType)
    else if (matrix1Type.dimensions == 0 && matrix2Type.dimensions == 0)
      new MatrixMatrixTransform0DFieldHyperKernel(in, op, resultType)
    else
      new MatrixMatrixTransformHyperKernel(in, op, resultType)
  }
}

