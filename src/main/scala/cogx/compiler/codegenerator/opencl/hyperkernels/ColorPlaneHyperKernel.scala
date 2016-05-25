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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, DontCare, AddressingMode, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op._

/** Extracts the red, green, blue, or luminance plane from a color field.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The ColorPlaneOp opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ColorPlaneHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: ColorPlaneOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode, DontCare)
{
  val code = new StringBuilder

  code.append("    float4 pixel = read(@in0);\n")
  operation match {
    // red
    case ColorPlaneRedOp => code.append("    float component = pixel.x;\n")
    // green
    case ColorPlaneGreenOp => code.append("    float component = pixel.y;\n")
    // blue
    case ColorPlaneBlueOp => code.append("    float component = pixel.z;\n")
    // luminance
    case ColorPlaneLuminanceOp => code.append("    float component = " +
      "0.3811f * pixel.x + 0.5783f * pixel.y + 0.0402f * pixel.z;\n")
  }

  code append "    @out0 = component;\n"

  addCode(code.toString())
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ColorPlaneHyperKernel {

  /**
   * Create an image kernel that extracts the red, green, blue, or luminance
   * plane from a color field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The ColorPlaneOp opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ColorPlaneOp,
            resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val in0Type = in(0).fieldType
    require(isColorField(in0Type))
    val expectedResultType = new FieldType(in0Type.fieldShape, Shape(), Float32)
    require(resultType == expectedResultType)
    val addressing = SmallTensorAddressing
    new ColorPlaneHyperKernel(in, operation, resultType, addressing)
  }
}
