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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, TensorElementAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.JoinOp

/** Joins exactly two fields with the same dimensionality and tensor shape.
  * Further, the input field shapes must match except for the first dimension.
  * The "stack" operation is similar, but it takes any number of inputs of
  * equal field shape.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class JoinHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: Opcode,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val code = new StringBuffer

  code append "    const int boundaryIndex = " + in(0).fieldType.fieldShape(0) + ";\n"
  code append setLayerRowColumn(resultType, "_layer", "_row", "_column")
  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  // Determine which input to read based on whether the first index falls
  // above or below the boundary index

  // The read of in0 could be a local read, but that would permit merging that
  // might not be well thought out.  We eliminate this possibility by doing
  // a non-local read.

  code append "    if (_%firstIndex% < boundaryIndex)\n"
  code append "        @out0 = readNonlocal(@in0);\n"
  code append "    else {\n"
  code append "        %firstIndex% = _%firstIndex% - boundaryIndex;\n"
  code append "        @out0 = readNonlocal(@in1);\n"
  code append "    }\n"

  val outDim = resultType.dimensions
  val codeString =
  outDim match {
    case 3 => code.toString.replaceAll("%firstIndex%", "layer")
    case 2 => code.toString.replaceAll("%firstIndex%", "row")
    case 1 => code.toString.replaceAll("%firstIndex%", "column")
    case _ => throw new RuntimeException("Illegal output dimension: " + outDim)
  }
  addCode(codeString)
//        debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object JoinHyperKernel extends HyperHelper {

  /** Create a HyperKernel that joins two fields.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {

    require(operation == JoinOp)
    require(in.length == 2)
    val in0Type = in(0).fieldType
    val in1Type = in(1).fieldType
    require(in0Type.fieldShape.drop(1) == in1Type.fieldShape.drop(1))
    require(in0Type.tensorShape == in1Type.tensorShape)
    require(in0Type.elementType == in1Type.elementType)

    val expectedResultFieldShape = in0Type.fieldShape join in1Type.fieldShape

    val expectedResultType = new FieldType(expectedResultFieldShape,
      in0Type.tensorShape, in0Type.elementType)
    require(expectedResultType == resultType)

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new JoinHyperKernel(in, operation, resultType, addressing)
  }
}

