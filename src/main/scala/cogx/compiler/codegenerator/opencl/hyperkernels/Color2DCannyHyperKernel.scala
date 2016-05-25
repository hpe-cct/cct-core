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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, ClampToEdge, AddressingMode, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.CannyOp

/** Canny filter for color images.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The CannyOp opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class Color2DCannyHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: Opcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode, ClampToEdge)
{
  val code = new StringBuilder

  def readInput(variable: String, xCoord: String, yCoord: String) {
    code append "column = " + xCoord + ";\n"
    code append "row = " + yCoord + ";\n"
    code append "float4 " + variable + " = readNonlocal(@in0);\n"
  }

  // Assume a 3 x 3 kernel for now
  readInput("p00", "_column - 1", "_row - 1")
  readInput("p10", "_column    ", "_row - 1")
  readInput("p20", "_column + 1", "_row - 1")

  readInput("p01", "_column - 1", "_row")
  readInput("p21", "_column + 1", "_row")

  readInput("p02", "_column - 1", "_row + 1")
  readInput("p12", "_column    ", "_row + 1")
  readInput("p22", "_column + 1", "_row + 1")

  code append "    float4 gx = -p00.xyzw + p20.xyzw +\n"
  code append "                2.0f * (p21.xyzw - p01.xyzw)\n"
  code append "                -p02.xyzw + p22.xyzw;\n"

  code append "    float4 gy = -p00.xyzw - p20.xyzw +\n"
  code append "                2.0f * (p12.xyzw - p10.xyzw)\n"
  code append "                + p02.xyzw + p22.xyzw;\n"

  code append "    @out0 =  native_sqrt(gx * gx + gy * gy);\n"

  addCode(code.toString())
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Color2DCannyHyperKernel {

  /**
   * Create an image kernel that implements a Canny filter.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val in0Type = in(0).fieldType
    require(isColorField(in0Type))
    require(in0Type.dimensions == 2)
    require(resultType == in0Type)
    require(operation == CannyOp)
    val addressing = SmallTensorAddressing
    new Color2DCannyHyperKernel(in, operation, resultType, addressing)
  }
}
