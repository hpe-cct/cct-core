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

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.StackOp

/** Stack two or more real fields (each having the exact same field and tensor
  * shape) into a field of one-higher dimension.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class StackHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: Opcode,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  val code = new StringBuffer

  val readType = addressing.clType(resultType).name
  val readZero = addressing.clType(resultType).zero
  code append "   " + readType + " result = " + readZero + ";\n"

  // Create a select index pointing to the input field needed by this thread
  val outDim = resultType.dimensions
  outDim match {
    case 3 => code append "    int select = _layer;\n"
    case 2 => code append "    int select = _row;\n"
    case 1 => code append "    int select = _column;\n"
    case _ => throw new RuntimeException("Illegal output dimension: " + outDim)
  }

  // Generate a switch statement to perform the read on the desired field.
  // Note that highest dimension index of the output field (from amongst
  // _layer, _row, and _column) will be silently ignored by the input read().
  code append "   switch (select) {\n"
  for (i <- 0 until in.length) {
    code append "     case " + i + ":\n"
    code append "         result = read(@in" + i + ");\n"
    code append "         break;\n"
  }
  code append "   }\n"
  code append "   @out0 = result;\n"
  addCode(code.toString)

//        debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object StackHyperKernel extends HyperHelper {

  /** Create a HyperKernel that stacks two or more real fields.
    *
    * The output field shape is different from the input field shapes, so
    * this kernel is not currently mergeable with its input kernels.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {

    require(operation == StackOp)
    require(in.length > 0)

    val expectedResultType = in(0).fieldType.incrementDimensions(in.length)
    require(expectedResultType == resultType)

    val addressing = bestAddressMode(in, resultType)
    new StackHyperKernel(in, operation, resultType, addressing)
  }
}

