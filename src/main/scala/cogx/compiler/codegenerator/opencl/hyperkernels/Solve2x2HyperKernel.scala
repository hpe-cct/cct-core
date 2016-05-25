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
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.SolveOp

/** Solves the matrix equation Ax = b for x in the special case where A is a
  * matrix field with 2x2 matrices and b is a vector field with length 2 vectors.
  * This will work for 1D, 2D, and 3D fields.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The DyadUnaryOp specifying the dyad property desired.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class Solve2x2HyperKernel private (in: Array[VirtualFieldRegister],
                                            operation: Opcode,
                                            resultType: FieldType)
        extends HyperKernel(operation, in, resultType, SmallTensorAddressing) {

  val code = new StringBuffer
  // Load in the matrix and vector components
  code.append("    float4 matrix = read(@in0);\n")
  code.append("    float2 vector = read(@in1);\n")

  code.append("    float A00 = matrix.s0;\n")
  code.append("    float A01 = matrix.s1;\n")
  code.append("    float A10 = matrix.s2;\n")
  code.append("    float A11 = matrix.s3;\n")

  code.append("    float b0 = vector.s0;\n")
  code.append("    float b1 = vector.s1;\n")

  // Approximate inverse of A. We have to be careful since A might be singular
  // so we add a small fudge factor to prevent division by 0. This value is
  // actually a bit of a hack, since we want to return very small vectors if
  // the rank of the matrix is 0 or 1. This means that FUDGE must be much
  // larger than the machine EPS (which is probably about e -19), but much
  // smaller than the values we expect to find in the matrix. We compromise
  // here and pick a small value that meets both objections.
  //
  // If you care about the case where A has rank one, you can do better than
  // what we've done here by computing the pseudoinverse of A. This requires
  // that A be decomposed into the outer product of two vectors. From then,
  // you can compute the pseudo inverse. See the wikipedia article on Moore-
  // Penrose pseudoinverse in the section Construction/Rank decomposition.
  code.append("    const float FUDGE = 0.00000001f;\n")
  code.append("    float invDeterminant = 1.0f / (A00 * A11 - A01 * A10 + FUDGE);\n")
  code.append("    float invA00 = A11 * invDeterminant;\n")
  code.append("    float invA11 = A00 * invDeterminant;\n")
  code.append("    float invA01 = -A01 * invDeterminant;\n")
  code.append("    float invA10 = -A10 * invDeterminant;\n")

  // Solve: x = invA * b
  code.append("    float x0 = invA00 * b0 + invA01 * b1;\n")
  code.append("    float x1 = invA10 * b0 + invA11 * b1;\n")

  // Write out x to the output vector field
  code.append("    @out0 = (float2) (x0, x1);\n")
  addCode(code.toString)
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Solve2x2HyperKernel {

  /** Create a hyperkernel that solves the matrix equation Ax = b.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The DyadUnaryOp specifying the dyad property desired.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(operation == SolveOp)
    val matrixField = in(0)
    val vectorField = in(1)
    require(resultType == vectorField.fieldType)
    require(matrixField.fieldType.fieldShape == vectorField.fieldType.fieldShape)
    require(matrixField.fieldType.tensorShape.dimensions == 2)
    require(vectorField.fieldType.tensorShape.dimensions == 1)
    require(matrixField.fieldType.tensorShape(0) == 2)
    require(matrixField.fieldType.tensorShape(1) == 2)
    require(vectorField.fieldType.tensorShape(0) == 2)

    new Solve2x2HyperKernel(in, operation, resultType)
  }
}
