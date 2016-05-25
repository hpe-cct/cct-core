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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.algebra.real.Matrix
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.{VectorTransposeOp, MatrixTransposeOp}

/** Transposes all tensors within a field.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class TensorTransposeHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: Opcode,
                                      val resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{

  def toHexChar(i: Int) = {
    require(i >= 0 && i < 16)
    "0123456789ABCDEF"(i)
  }

  val code = new StringBuffer

  val inType = in(0).fieldType
  val inTensorColumns = inType.tensorColumns
  val inTensorRows = inType.tensorRows
  val inTensorPoints = inTensorRows * inTensorColumns
  val outTensorColumns = resultType.tensorColumns
  val outTensorRows = resultType.tensorRows

  if (inTensorPoints == 1 || inTensorRows == 1)
    code append "        @out0 = read(@in0);\n"
  else
    addressMode match {
      case SmallTensorAddressing =>
        val outType = addressing.clType(resultType).name
        code append "    " + outType + " inTensor = read(@in0);\n"
        // Matrix with the elements numbered in row-major order
        val straightSeq= Matrix(inTensorRows, inTensorColumns, (r,c) => r * inTensorColumns + c)
        // Transpose matrix to get the transposed sequence
        val transposedSeq = straightSeq.transpose.asArray
        code append "   @out0 = (" + outType + ") ("
        code append (0 until inTensorPoints).map(i =>
          "inTensor.s" + toHexChar(transposedSeq(i).toInt)
        ).mkString(", ")
        code append ");\n"
      case TensorElementAddressing =>
        val inType = in(0).fieldType
        code append setLayerRowColumn(inType, "_layer", "_row", "_column")
        code append "    int outTensorRow = _tensorElement / " + outTensorColumns + ";\n"
        code append "    int outTensorColumn = _tensorElement - outTensorRow * " + outTensorColumns + ";\n"
        code append "    int inTensorRow = outTensorColumn;\n"
        code append "    int inTensorColumn = outTensorRow;\n"
        code append "    tensorElement = inTensorColumn + inTensorRow * " + inTensorColumns + ";\n"
        code append "    @out0 = readNonlocal(@in0);\n"
      case _ => throw new RuntimeException("Addressing mode not supported by this kernel: " + addressMode)
    }
  addCode(code.toString)
//        debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object TensorTransposeHyperKernel {

  /** Create a HyperKernel that transposes all tensors within a field..
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {

    require(operation == MatrixTransposeOp || operation == VectorTransposeOp)
    val inType = in(0).fieldType
    val expectedResultTensorShape =
      if (inType.tensorShape.dimensions == 1)
        Shape(1,inType.tensorShape.points)
      else
        Shape(inType.tensorColumns, inType.tensorRows)
    val expectedResultType = new FieldType(inType.fieldShape, expectedResultTensorShape, Float32)
    require(resultType == expectedResultType)
    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new TensorTransposeHyperKernel(in, operation, resultType, addressing)
  }
}

