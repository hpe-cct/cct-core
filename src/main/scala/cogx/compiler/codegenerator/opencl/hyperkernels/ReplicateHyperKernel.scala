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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.ReplicateOp

/** Replicate the contents of an input scalar field as each tensor
  * of an output matrix or vector field.
  *
  * A ReplicateHyperKernel takes a 2D scalar field and replicates
  * the scalar field to create a matrix field with each matrix holding the
  * contents of the original scalar field. Alternatively, the ReplicateHyperKernel
  * takes a 1D scalar field and replicates the scalar field to create a vector
  * field with each vector holding the contents of the original scalar field.
  * See Cog programming manual for a description of this operation.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The virtual field register of the ScalarField to be replicated to
  *           each tensor of the output.
  * @param operation The ReplicateOp with its result fieldShape parameter.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class ReplicateHyperKernel private (in: Array[VirtualFieldRegister],
                                    operation: Opcode,
                                    resultType: FieldType)
        extends HyperKernel(operation, in, resultType, TensorElementAddressing) {

  val scalarFieldType = in(0).fieldType
  val scalarFieldDim = scalarFieldType.dimensions

  val code = new StringBuilder

  scalarFieldDim match {
    case 2 =>
      code.append("row = _tensorElement / " + scalarFieldType.columns + ";\n")
      code.append("column = _tensorElement - row * " + scalarFieldType.columns + ";\n")
    case 1 =>
      code.append("column = _tensorElement;\n")
    case _ =>
      throw new RuntimeException("Illegal scalar field input dimension: " + scalarFieldDim)
  }
  code.append("    @out0 = readNonlocal(@in0);\n")
  addCode(code.toString())
//            debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ReplicateHyperKernel {

  /**
    * Create a hyperkernel that performs the Replicate function.
    *
    * @param in The virtual field register of the ScalarField to be replicated
    *           to each tensor of the output.
    * @param operation The ReplicateOp with its result fieldShape parameter.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */

  def apply(in: Array[VirtualFieldRegister], operation: ReplicateOp, resultType: FieldType): HyperKernel = {
    require(in.length == 1)
    val expectedResultFieldShape = operation.resultShape
    val scalarFieldType = in(0).fieldType
    require(scalarFieldType.tensorOrder == 0)
    require(scalarFieldType.dimensions <= 2)
    val expectedResultType = new FieldType(expectedResultFieldShape,
      scalarFieldType.fieldShape, scalarFieldType.elementType)
    require(expectedResultType == resultType)

    new ReplicateHyperKernel(in, operation, resultType)
  }
}

