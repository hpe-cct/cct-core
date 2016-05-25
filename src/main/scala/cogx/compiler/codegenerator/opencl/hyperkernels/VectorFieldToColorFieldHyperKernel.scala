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

import cogx.platform.types._
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.opencl.fragments._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape


/** Converts a vector field to a color field.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The ColorPlaneOp opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class VectorFieldToColorFieldHyperKernel private (in: VirtualFieldRegister,
                                                  operation: UnaryOpcode,
                                                  resultType: FieldType,
                                                  addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode, DontCare)
{
  // OpenCL has very strange behaviors using vectors.
  // The following code does NOT work:
  //
  //    float3 vector = read(@in0);
  //    float red = vector.x;
  //    float green = vector.y;
  //    float blue = vector.z;
  //    float4 pixel = (float4) (red, green, blue, 1.0f);
  //    @out0 = pixel;
  //
  // This looks like a serious bug in Nvidia's OpenCL compiler. The work-around
  // appears to be to never extract components from a vector into variables and
  // then reassemble them into a vector. Ugly.
  //
  val code =
    """
      |    float3 vector = read(@in0);
      |    float4 pixel = (float4) (vector.x, vector.y, vector.z, 1.0f);
      |    @out0 = pixel;
    """.stripMargin
  addCode(code)
  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object VectorFieldToColorFieldHyperKernel {

  /** Create an image kernel that converts a vector field to a color field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType):
  HyperKernel =
  {
    require(isColorField(resultType))
    val inType = in.fieldType
    require(inType.tensorShape == Shape(3))
    require(inType.fieldShape == resultType.fieldShape)
    require(operation == VectorFieldToColorFieldOp)
    val expectedResultType =
      new FieldType(inType.fieldShape, Shape(3), Uint8Pixel)
    require(resultType == expectedResultType)
    val addressing = SmallTensorAddressing
    new VectorFieldToColorFieldHyperKernel(in, operation, resultType, addressing)
  }
}
