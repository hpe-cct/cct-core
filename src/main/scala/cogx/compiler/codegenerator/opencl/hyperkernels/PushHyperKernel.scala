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
import cogx.compiler.parser.op.PushOp

/** Implements a fixed-size stack of N-dimensional fields that are represented as
  * an (N+1) dimensional field. Each time a field is "pushed" onto the stack,
  * existing slices in the stack get pushed down one (slice 0 goes to slice 1,
  * slice 1 goes to slice 2, etc.) and the new N-D field becomes slice 0.
  * The bottom-most slice of the stack is discarded by the push operation.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The opcode describing the operation (should be PushOp).
  * @param resultType The type of the resulting vector field.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class PushHyperKernel private (in: Array[VirtualFieldRegister],
                                       operation: Opcode,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val code = new StringBuilder
  val stackType = in(0).fieldType
  val stackDim = stackType.dimensions
  val sliceType = in(1).fieldType

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  stackDim match {
    case 1 =>
      code append "    if (_column == 0) {\n"
    case 2 =>
      code append "    if (_row == 0) {\n"
    case 3 =>
      code append "    if (_layer == 0) {\n"
    case _ =>
      throw new RuntimeException("Unsupported input field dimension: " + stackDim)
  }
  // Currently dangerous to make this a plain read(): might enable improper
  // merging.  Improve the merger, then update if appropriate. -RJC
  code append setLayerRowColumn(sliceType, "_layer", "_row", "_column")
  code append "        @out0 = readNonlocal(@in1);\n"
  code append "    }\n"
  code append "    else {\n"
  stackDim match {
    case 1 =>
      code append setLayerRowColumn(stackType, "0", "0", "_column - 1")
    case 2 =>
      code append setLayerRowColumn(stackType, "0", "_row - 1", "_column")
    case 3 =>
      code append setLayerRowColumn(stackType, "_layer - 1", "_row", "_column")
    case _ =>
      throw new RuntimeException("Unsupported input field dimension: " + stackDim)
  }
  code append "        @out0 = readNonlocal(@in0);\n"
  code append "    }\n"
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object PushHyperKernel {

  /**
   * Create a hyperkernel that implements a fixed-size stack of N-dimensional fields.
   *
   * @param in The input virtual field registers driving this kernel.
   * @param operation The opcode describing the operation (should be PushOp).
   * @param resultType The type of the resulting vector field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {

    require(in.length == 2)
    val stackType = in(0).fieldType
    val sliceType = in(1).fieldType

    val expectedSliceType = new FieldType(stackType.fieldShape.drop(1), stackType.tensorShape, stackType.elementType)
    require(expectedSliceType == sliceType)
    require(stackType == resultType)
    require(operation == PushOp)

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new PushHyperKernel(in, operation, resultType, addressing)
  }
}

