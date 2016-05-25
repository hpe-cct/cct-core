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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.TensorSliceOp

/** This kernel 'slices' a vector field by taking the same vector element from
  * each vector, thereby producing a ScalarField of the same fieldShape as the
  * input.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The TensorSliceOp opcode, with its index of the element
  *                  to be extracted to the scalar field result.
  * @param resultType The type of the resulting scalar field.
  */
private[cogx]
class SliceVectorsHyperKernel private (in: VirtualFieldRegister,
                                       operation: TensorSliceOp,
                                       resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, SmallTensorAddressing) {

  def toHexChar(i: Int) = {
    require(i >= 0 && i < 16)
    "0123456789ABCDEF"(i)
  }

  val code = new StringBuilder
  val inType = in.fieldType
  val vectorIndex = operation.index
  if (isBigTensorField(inType)) {
    code append "    const int tensorElement = " + vectorIndex + ";\n"
    code append "        @out0 = readElement(@in0);\n"
  }
  else if (inType.tensorColumns == 1) {
    // probably a rare case, but the .sN syntax is not valid on a scalar
    code append "        @out0 = read(@in0);\n"
  }
  else
    code append "        @out0 = (read(@in0)).s" + toHexChar(vectorIndex) + ";\n"
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object SliceVectorsHyperKernel {

  /**
   * Create a hyperkernel that slices a vector field, yielding a scalar field.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The TensorSliceOp opcode, with its index of the element
   *                  to be extracted to the scalar field result.
   * @param resultType The type of the resulting scalar field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: VirtualFieldRegister, operation: TensorSliceOp, resultType: FieldType): HyperKernel = {
    val inType = in.fieldType
    require(inType.tensorShape.dimensions == 1)
    require(resultType.tensorShape.dimensions == 0)
    require(inType.fieldShape == resultType.fieldShape)
    val vectorIndex = operation.index
    require(vectorIndex >= 0 && vectorIndex < inType.tensorColumns)

    new SliceVectorsHyperKernel(in, operation, resultType)
  }
}

