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

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, SmallTensorAddressing, TensorElementAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.FieldArraySelectOp

/** Multiplexes an array of real fields (scalar, vector, matrix).
  *
  * Select a field from an array of fields using a 0D scalar field as an
  * index. If the index is in bounds, it streams the selected field as
  * output, otherwise it streams NaNs.
  *
  * @author Greg Snider
  *
  * @param in Virtual field registers for the inputs: the zeroth element is the
  *        index, the remaining elements are the array of input fields.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class MultiplexerHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: Opcode,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val select = in(0)
  val fieldInputs = in.drop(1)
  // Read the index of the desired field.
  val code = new StringBuffer
  code append "   int select = (int) readScalar(@in0);\n"

  // Generate a switch statement to perform the read on the desired field.
  // If the index is out of bounds, write NaN.
  code append "   float result;\n"
  code append "   switch (select) {\n"
  for (i <- 0 until fieldInputs.length) {
    code append "     case " + i + ":\n"
    code append "         result = read(@in" + (i + 1) + ");\n"
    code append "         break;\n"
  }
  code append "       default:\n"
  code append "           result = NAN;\n"
  code append "           break;\n"
  code append "   }\n"
  code append "   @out0 = result;\n"
  addCode(code.toString)
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object MultiplexerHyperKernel {

  /** Create a HyperKernel that multiplexes an array of real fields (scalar, vector, matrix).
    *
  * @param in Virtual field registers for the inputs: the zeroth element is the
  *        index, the remaining elements are the array of input fields.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    val select = in(0)
    val fieldInputs = in.drop(1)
    val fieldType = fieldInputs(0).fieldType
    require(fieldType == resultType)
    require(operation == FieldArraySelectOp)
    val addressMode =
      if (resultType.tensorShape.dimensions > 0)
        TensorElementAddressing
      else
        SmallTensorAddressing
    new MultiplexerHyperKernel(in, operation, resultType, addressMode)
  }
}

