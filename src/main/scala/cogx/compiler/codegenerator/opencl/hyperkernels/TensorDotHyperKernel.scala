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

import cogx.compiler.codegenerator.opencl.fragments.{CLFloat8, CLFloat16, SmallTensorAddressing, HyperKernel}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.TensorDotOp

/** Computes the scalar dot product for corresponding tensors in two real
  * fields with the same shape and tensor type.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class TensorDotHyperKernel private (in: Array[VirtualFieldRegister],
                                            operation: Opcode,
                                            resultType: FieldType)
        extends HyperKernel(operation, in, resultType, SmallTensorAddressing) {

  val code = new StringBuffer

  val inType = in(0).fieldType

  // readElement() and read() know to ignore _row and _column for 0D 2nd operand

  if (isBigTensorField(inType)) {
    // At each field point, sum the product of corresponding matrix elements
    code append "    float result = 0.0f;\n"
    code append "    for (tensorElement = 0; tensorElement < " + inType.tensorShape.points + "; tensorElement++) {\n"
    code append "        result += readElement(@in0) * readElement(@in1);\n"
    code append "    }\n"
    code append "    @out0 = result;\n"
  }
  else {
    val inCLType = SmallTensorAddressing.clType(in(0).fieldType)
    code append "    " + inCLType.name + " a = read(@in0);\n"
    code append "    " + inCLType.name + " b = read(@in1);\n"

    // Note that the 'dot' function is only valid up to float4, not float8 or float16
    inCLType match {
      case CLFloat8 =>
        code append "    @out0 = dot(a.s0123, b.s0123) + dot(a.s4567, b.s4567);\n"
      case CLFloat16 =>
        code append "    @out0 = dot(a.s0123, b.s0123) + dot(a.s4567, b.s4567) + dot(a.s89AB, b.s89AB) + dot(a.sCDEF, b.sCDEF);\n"
      case _ =>
        code append "    @out0 = dot(a, b);\n"
    }
  }
  addCode(code.toString)
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object TensorDotHyperKernel extends HyperHelper {

  /** Create a hyperkernel that computes the dot product of corresponding
    * vectors (rank 1 tensors) in two vector fields.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    require(in(0).fieldType.fieldShape == in(1).fieldType.fieldShape ||
      in(1).fieldType.fieldShape.dimensions == 0)
    require(in(0).fieldType.tensorShape == in(1).fieldType.tensorShape)
    val inType = in(0).fieldType
    require(inType.tensorShape.dimensions >= 1)
    val expectedResultType =
      new FieldType(inType.fieldShape, Shape(), Float32)
    require(expectedResultType == resultType)
    require(operation == TensorDotOp)
    new TensorDotHyperKernel(in, operation, resultType)
  }
}
