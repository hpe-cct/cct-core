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
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.FlipOp

/** Hyperkernel that flips a tensor field in every dimension.
  *
  * @author Dick Carter
  *
  * @param in The virtual field register of the input field to be flipped.
  * @param operation The FlipOp Opcode.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class FlipHyperKernel private (in: Array[VirtualFieldRegister],
                                        operation: Opcode,
                                        resultType: FieldType,
                                        addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {
  val inType = in(0).fieldType

  val code = new StringBuffer

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  // Note: I don't like using _layers, _rows, and _columns since I believe
  // these global variables will be going away as we improve the merger. -RJC

  code append setLayerRowColumn(inType,
    (inType.layers - 1).toString + " - _layer",
    (inType.rows - 1).toString + " - _row",
    (inType.columns - 1).toString + " - _column")
  code append "    @out0 = readNonlocal(@in0);\n"

  addCode(code.toString)
  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object FlipHyperKernel {
  /** Create a hyperkernel that flips a tensor field.
    *
    * @param in The virtual field register of the input field to be flipped.
    * @param operation The FlipOp opcode.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType):
  HyperKernel =
  {
    require(in.length == 1)
    require(operation == FlipOp)
    val inType = in(0).fieldType
    require(inType == resultType)
    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new FlipHyperKernel(in, operation, resultType, addressing)
  }
}

