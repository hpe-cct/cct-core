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
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.MergeColorPlanesOp

/** Combines three real fields, representing the RGB parts, into a
  * color field.
 *
  * @author Dick Carter
  *
  * @param in The 3 input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class CreateColorFieldHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: Opcode,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode, DontCare)
{
  val code = new StringBuilder
  code append "    float red   = read(@in0);\n"
  code append "    float green = read(@in1);\n"
  code append "    float blue  = read(@in2);\n"
  code append "    float4 pixel = (float4) (red, green, blue, 1.0f);\n"
  code append "    @out0 = pixel;\n"
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object CreateColorFieldHyperKernel {

  /**
   * Create an image kernel that combines three real fields, representing the
   * RGB parts, into a color field.
   *
   * @param in The 3 input virtual field registers driving this kernel.
   * @param operation The opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType):
      HyperKernel =
  {
    require(in.length == 3)
    val expectedInputType =
      new FieldType(in(0).fieldType.fieldShape, Shape(), Float32)
    in.foreach(input => require(input.fieldType == expectedInputType))

    val expectedResultType =
      new FieldType(in(0).fieldType.fieldShape, Shape(3), Uint8Pixel)
    require(resultType == expectedResultType)

    require(operation == MergeColorPlanesOp)

    val addressing = SmallTensorAddressing
    new CreateColorFieldHyperKernel(in, operation, resultType, addressing)
  }
}
