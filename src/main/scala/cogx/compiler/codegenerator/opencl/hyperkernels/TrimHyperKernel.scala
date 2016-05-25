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

import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, HyperKernel, AddressingMode}
import cogx.compiler.parser.op.TrimOp
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Trim a field to a smaller size.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register to be trimmed.
  * @param operation The TrimOp opcode, with its resultShape parameter.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @return The synthesized hyperkernel.
  */
private[cogx]
class TrimHyperKernel private (in: Array[VirtualFieldRegister],
                               operation: TrimOp,
                               resultType: FieldType,
                               addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  val code = new StringBuffer
  code append "@out0 = read(@in0);"
  addCode(code.toString)
  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object TrimHyperKernel {

  /** Trim a field to a smaller size.
    *
    * @param in The input virtual field register to be trimmed.
    * @param operation The TrimOp opcode, with its resultShape parameter.
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

    new TrimHyperKernel(in, operation, resultType, addressing)
  }
}
