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
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, DontCare, HyperKernel, AddressingMode}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape


/** Converts a color field to a vector field.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The ColorPlaneOp opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ColorFieldToVectorFieldHyperKernel private (in: VirtualFieldRegister,
                                                  operation: UnaryOpcode,
                                                  resultType: FieldType,
                                                  addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode, DontCare)
{
  val code =
    """
      |    float4 pixel = read(@in0);
      |    float3 vector = (float3) (pixel.x, pixel.y, pixel.z);
      |    @out0 = vector;
    """.stripMargin
  addCode(code)
  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ColorFieldToVectorFieldHyperKernel {

  /** Create an image kernel that converts a color field to a vector field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType):
     HyperKernel =
  {
    val inType = in.fieldType
    require(isColorField(inType))
    require(operation == ColorFieldToVectorFieldOp)
    val expectedResultType = new FieldType(inType.fieldShape, Shape(3), Float32)
    require(resultType == expectedResultType)
    val addressing = SmallTensorAddressing
    new ColorFieldToVectorFieldHyperKernel(in, operation, resultType, addressing)
  }
}
