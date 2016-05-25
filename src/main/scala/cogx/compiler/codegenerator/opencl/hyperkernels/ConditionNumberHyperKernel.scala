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
import cogx.compiler.codegenerator.opencl.fragments._
import cogx.cogmath.geometry.Shape

/** A hyperkernel that computes the condition number for every 2 x 2 matrix
  * in a matrix field.
  *
  * @author Greg Snider
  *
  * @param in Input matrix virtual field register.
  * @param operation Opcode
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class ConditionNumberHyperKernel private (in: VirtualFieldRegister,
                                          operation: Opcode,
                                          resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType,
          SmallTensorAddressing)
{
  val code =
    """
      | // Read in matrix.
      | float4 m = read(@in0);
      | float m00 = m.x;
      | float m01 = m.y;
      | float m10 = m.z;
      | float m11 = m.w;
      |
      | // Eigen analysis.
      | float trace = m00 + m11;
      | float determinant = m00 * m11 - m01 * m10;
      | float arg = sqrt(trace * trace + 4 * determinant);
      | float lambda1 = (trace + arg) / 2.0f;
      | float lambda2 = (trace - arg) / 2.0f;
      | float conditionNumber = fabs(lambda1 / (lambda2 + 0.0000001f));
      | @out0 = conditionNumber;
    """.stripMargin
  addCode(code)
  //  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ConditionNumberHyperKernel {

  /** Compute the condition number for every matrix in a matrix field.
    *
    * @param in The matrix virtual field register.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode,
            resultType: FieldType): AbstractKernel =
  {
    require(in.fieldType.tensorShape == Shape(2, 2))
    require(resultType.tensorShape.dimensions == 0)
    require(in.fieldType.fieldShape == resultType.fieldShape)
    new ConditionNumberHyperKernel(in, operation, resultType)
  }
}

