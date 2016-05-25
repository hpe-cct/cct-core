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
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.cogmath.algebra.complex.ComplexArray
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.compiler.parser.op.ComplexBinaryOp

/** Combines two fields. The first field may be of any type, but the second field
  * may fall into one of two categories:
  *
  * 1. Same type as the first field. In this case, corresponding complex numbers in the
  *    two fields are combined.
  *
  * 2. A 0-D complex scalar field. In this case the complex in the second field is added
  *    to each complex in the first field.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ComplexBinaryHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: ComplexBinaryOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  require(in.length == 2)
  val in0Type = in(0).fieldType
  val in1Type = in(1).fieldType
  val firstFieldIsScalar = isTensor0Field(in0Type)
  val firstFieldIs0D = is0DField(in0Type)

  val secondFieldIsScalar = isTensor0Field(in1Type)
  val secondFieldIs0D = is0DField(in1Type)

  // generate expected result type and check against actual resultType

  val expectedFieldShape =
    if (firstFieldIs0D)
      in1Type.fieldShape
    else
      in0Type.fieldShape

  val expectedTensorShape =
    if (firstFieldIsScalar)
      in1Type.tensorShape
    else
      in0Type.tensorShape

  require(in0Type.elementType == in1Type.elementType,
    "Operand element types must match")

  val expectedResultType =
    new FieldType(expectedFieldShape, expectedTensorShape, in0Type.elementType)

  require(expectedResultType == resultType, "Result type mismatch: expected " +
          expectedResultType + ", found " + resultType)

  // If both fields are 0D, we should use the local read() function to aide
  // mergeability of 0D op circuits.

  val readFirstField: String =
    if (firstFieldIs0D && !secondFieldIs0D)
      if (firstFieldIsScalar)
        "readScalar(@in0)"
      else
        "readPoint(@in0)"
    else
      "read(@in0)"

  val readSecondField: String =
    if (secondFieldIs0D & !firstFieldIs0D)
      if (secondFieldIsScalar)
        "readScalar(@in1)"
      else
        "readPoint(@in1)"
    else
      "read(@in1)"

  addCode("    @out0 = " + OpcodeToFunction(operation) +
          "(" + readFirstField + ", " + readSecondField + ");")
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ComplexBinaryHyperKernel extends HyperHelper {

  /**
   * Combine two fields with the same dimensions by combining corresponding
   * tensor points with a binary operator. Either both fields must be the
   * same type, or the second one must be a scalar field.
   *
   * @param in The two input virtual field registers driving this kernel.
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ComplexBinaryOp, resultType: FieldType): HyperKernel = {
    val addressing = bestAddressMode(in, resultType)
    new ComplexBinaryHyperKernel(in, operation, resultType, addressing)
  }
}
