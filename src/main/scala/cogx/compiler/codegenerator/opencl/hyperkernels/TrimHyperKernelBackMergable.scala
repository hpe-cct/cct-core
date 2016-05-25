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
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.TrimOp

/** Trim a field to a smaller size.  This kernel uses a number of threads
  * equal to the input size, so is better suited to merging backwards, although
  * the Hyperkernel merger does not yet handle this case.  This was created
  * to test proper indexing code generation for the case where the work group
  * size was not determined by the output field.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register to be trimmed.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @return The synthesized hyperkernel.
  */
private[cogx]
class TrimHyperKernelBackMergable private (in: Array[VirtualFieldRegister],
                                           operation: TrimOp,
                                           resultType: FieldType,
                                           addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  // Work item and group structure determined from input field
  // (as a test mostly of this new facility).
  val inType = in(0).fieldType
  override lazy val workFieldType = inType

  val code = new StringBuffer
  // Now prune threads that are not supposed to drive their output
  code append returnIfOutOfOutputFieldBounds(0)
  code append "@out0 = read(@in0);"
  addCode(code.toString)
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object TrimHyperKernelBackMergable {

  /** Trim a field to a smaller size.
    *
    * @param in The input virtual field register to be trimmed.
    * @param operation The TrimOp opcode, with its newShape parameter.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: TrimOp, resultType: FieldType):
    HyperKernel =
  {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(inType.dimensions == resultType.dimensions)
    for (i <- 0 until resultType.dimensions)
      require(resultType.fieldShape(i) <= inType.fieldShape(i),"Trimming to a larger fieldShape!")

    val expectedResultType = new FieldType(operation.resultShape, inType.tensorShape, inType.elementType)
    require(expectedResultType == resultType)

    val addressing =
      if (isSmallTensorField(inType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new TrimHyperKernelBackMergable(in, operation, resultType, addressing)
  }
}
