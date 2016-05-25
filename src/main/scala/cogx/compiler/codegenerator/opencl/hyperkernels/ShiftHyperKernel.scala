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
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ShiftOp

/** Hyperkernel that shifts a tensor field.
  *
  * This kernel was quickly added to Cog's repertoire of kernels during the
  * port to Cog 4.0, to avoid having to port the CPUScalarNonlocal kernel.
  * Truth is, there are a number of kernels addressing similar functionality,
  * including the SubSpace, Subfield and Warp2D HyperKernels.  They differ on
  * whether the output is the same size as the input, whether a border policy
  * is supported, whether interpolation of values is supported, and whether
  * certain parameters of the operation are static vs. dynamic.
  *
  * These kernels should be consolodated to just 1 or 2 kernels.   XXX
  *
  * Pre-processing the shift amounts would allow the code below to be optimized.
  *
  * @author Ben Chandler and Dick Carter
  *
  * @param in The input virtual field register to be shifted.
  * @param operation An instance of ShiftOp with shift parameters and border
  *                  policy.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ShiftHyperKernel private (in: Array[VirtualFieldRegister],
                                        operation: ShiftOp,
                                        resultType: FieldType,
                                        addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {
  val inType = in(0).fieldType
  val shifts = operation.shifts
  val borderPolicy = operation.borderPolicy
  val dim = resultType.dimensions
  val readZero = addressMode.clType(resultType).zero

  val code = new StringBuffer

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"
  dim match {
    case 3 =>
      code append "    layer = _layer - (" + shifts(0) + ".0f);\n"
      code append "    row =   _row - (" + shifts(1) + ".0f);\n"
      code append "    column = _column - (" + shifts(2) + ".0f);\n"
    case 2 =>
      code append "    row =   _row - (" + shifts(0) + ".0f);\n"
      code append "    column = _column - (" + shifts(1) + ".0f);\n"
    case 1 =>
      code append "    column = _column - (" + shifts(0) + ".0f);\n"
    case x => throw new RuntimeException("Unsupported result field dimension " + x)
  }

  // If we're outside the bounds of the input, implement the configured border
  // policy.
  borderPolicy match {
    case BorderCyclic =>
      if (dim > 2) {
        code append "        while (layer < 0) layer += _layers;\n"
        code append "        while (layer >= _layers) layer -= _layers;\n"
      }
      if (dim > 1) {
        code append "        while (row < 0) row += _rows;\n"
        code append "        while (row >= _rows) row -= _rows;\n"
      }
      code append "        while (column < 0) column += _columns;\n"
      code append "        while (column >= _columns) column -= _columns;\n"
    case BorderZero =>
      // One gets a zero output if any of the 4 locations referenced is
      // outside the input field.  We'll improve on this if needed.
      code append "        int outOfBounds = 0;\n"
      if (dim > 2) {
        code append "        outOfBounds += layer < 0;\n"
        code append "        outOfBounds += layer >= _layers;\n"
      }
      if (dim > 1) {
        code append "        outOfBounds += row < 0;\n"
        code append "        outOfBounds += row >= _rows;\n"
      }
      code append "        outOfBounds += column < 0;\n"
      code append "        outOfBounds += column >= _columns;\n"
    case BorderClamp =>
      if (dim > 2)
        code append "        layer = max(0, min(layer, _layers - 1));\n"
      if (dim > 1)
        code append "        row = max(0, min(row, _rows - 1));\n"
      code append "        column = max(0, min(column, _columns - 1));\n"
    case x =>
      throw new RuntimeException("Unsupported translate border policy: " + x)
  }
  code append "\n"
  if (borderPolicy == BorderZero)
    code append "    if (outOfBounds == 0) {\n"
  code append "    @out0 = readNonlocal(@in0);\n"
  if (borderPolicy == BorderZero) {
    code append "\n    } else\n"
    code append "        @out0 = " + readZero + ";"
  }
  addCode(code.toString)
  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ShiftHyperKernel {
  /** Create a hyperkernel that shifts a tensor field.
    *
    * @param in The input virtual field register to be shifted.
    * @param operation An instance of ShiftOp with shift parameters and border
    *                  policy.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: ShiftOp, resultType: FieldType):
  HyperKernel =
  {
    require(in.length == 1)
    val inType = in(0).fieldType
    require(inType == resultType)
    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new ShiftHyperKernel(in, operation, resultType, addressing)
  }
}

