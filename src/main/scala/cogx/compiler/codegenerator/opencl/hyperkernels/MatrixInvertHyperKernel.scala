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

import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.MatrixInvertOp

/** Inverts every matrix in a matrix field using Gauss-Jordan elimination.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode of the kernel.
  * @param resultType The type of the resulting field.
  */
private[cogx]
class MatrixInvertHyperKernel private (in: VirtualFieldRegister,
                                       operation: Opcode,
                                       resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, BigTensorAddressing)
{
  // Two flags put in to address the numeric stability of the result:
  /** Force intermediate results to be 64-bit doubles. */
  val DoublePrecision = false
  /** Swap rows to avoid small (and NaN-creating 0) pivot values */
  val PartialPivoting = true

  // Kernel launch parameters. Since this kernel is memory intensive, allocating
  // too many threads per work group slows it down since that causes lots of
  // register spilling. Experiments on my laptop for matrix sizes ranging from
  // 4x4 to 8x8 suggests 64 threads is the best overall workgroup size. This
  // should be revisted, though, and the actual size should be a function of
  // the matrix size and the target GPU.

  /** Scheduling parameters for this kernel driver. */
  override lazy val workGroup = HyperKernel.computeWorkGroupParameters(workFieldType, addressing, 8, 8)

  val code = new StringBuilder

  // Each thread inverts one matrix from the field. Allocate local memory for
  // the matrix assigned to this thread plus memory to hold the inverse. If
  // the matrices are small (the most likely use case), these can actually be
  // allocated in registers.

  val matrixSize = in.fieldType.tensorColumns
  val ValType = if (DoublePrecision) "double" else "float"
  val One = if (DoublePrecision) "1.0" else "1.0f"
  val Zero = if (DoublePrecision) "0.0" else "0.0f"
  if (DoublePrecision)
    code.append("#pragma OPENCL EXTENSION cl_khr_fp64: enable\n")
  code.append("#define dimension " + matrixSize + "\n")
  code.append("    " + ValType + " matrix[dimension][dimension];\n")
  code.append("    " + ValType + " inverse[dimension][dimension];\n")

  // Read in the matrix from global memory.
  code append "    tensorElement = 0;\n"
  code.append("    for (int row = 0; row < dimension; row++)\n")
  code.append("        for (int col = 0; col < dimension; col++) {\n")
  code.append("            matrix[row][col] = readElement(@in0);\n")
  code.append("            tensorElement += 1;\n")
  code.append("        }\n")

  // Initialize the inverse to the identity matrix.
  code.append("    for (int row = 0; row < dimension; row++)\n")
  code.append("        for (int col = 0; col < dimension; col++)\n")
  code.append("            inverse[row][col] = (row == col) ? " + One + " : " + Zero + ";\n")

  // Compute the inverse using Gauss-Jordan elimination. For a description,
  // see the Wikipedia article on this topic (which was the source of the
  // following code).

  // I modified the code to perform less divisions, which create
  // divide-by-zero NaNs, as well as being slower.  Also, elements that are
  // theoretically 0.0 or 1.0 are never assigned-to or referenced.  The
  // previous routine performed calculations with these 0.0's and 1.0's,
  // but this magnified the numerical instability of the result.  -RJC

  // Step over the columns of the matrix, making a 1.0 diagonal element.
  code.append("    for (int k = 0; k < dimension; k++) {\n")

  if (PartialPivoting) {
    // determine which row (below the k-th row) has the
    // largest magnitude element in the k-th column position
    code.append("        unsigned int maxRowIndex = k;\n")
    code.append("        " + ValType + " maxRowVal = fabs(matrix[k][k]);\n")
    code.append("        for (int j = k + 1; j < dimension; j++) {\n")
    code.append("        " + ValType + " jthRowVal = fabs(matrix[j][k]);\n")
    code.append("            if (jthRowVal > maxRowVal) {\n")
    code.append("                maxRowIndex = j;\n")
    code.append("                maxRowVal = jthRowVal;\n")
    code.append("            }\n")
    code.append("        }\n")
    // swap row 'maxRowIndex' with row 'k'
    code.append("        if (maxRowIndex != k) {\n")
    code.append("            for (int j = k; j < dimension; j++) {\n")
    code.append("                " + ValType + " temp = matrix[maxRowIndex][j];\n")
    code.append("                matrix[maxRowIndex][j] = matrix[k][j];\n")
    code.append("                matrix[k][j] = temp;\n")
    code.append("            }\n")
    code.append("            for (int j = 0; j < dimension; j++) {\n")
    code.append("                " + ValType + " temp = inverse[maxRowIndex][j];\n")
    code.append("                inverse[maxRowIndex][j] = inverse[k][j];\n")
    code.append("                inverse[k][j] = temp;\n")
    code.append("            }\n")
    code.append("        }\n")
  }
  // scale rest of k-th row as if to make diagonal element 1.0
  code.append("        " + ValType + " valInv = " + One + "/ matrix[k][k];\n")
  code.append("        for (int j = k + 1; j < dimension; j++)\n")
  code.append("            matrix[k][j] *= valInv;\n")
  code.append("        for (int j = 0; j < dimension; j++)\n")
  code.append("            inverse[k][j] *= valInv;\n")
  // Now subtract the right amount of the k-th row from
  // each lower row
  code.append("        for (int i = k + 1; i < dimension; i++) {\n")
  code.append("            " + ValType + " scaleFactor = matrix[i][k];\n")

  code.append("            for (int j = k + 1; j < dimension; j++)\n")
  code.append("                matrix[i][j] -= matrix[k][j] * scaleFactor;\n")
  code.append("            for (int j = 0; j < dimension; j++)\n")
  code.append("               inverse[i][j] -= inverse[k][j] * scaleFactor;\n")
  code.append("        }\n")
  code.append("    }\n")

  // Original code for reference: does n*(n+1)/2 divisions, which is likely
  // to be slower and have an increased risk of producing NaNs:

  //    code.append("    for (int k = 0; k < dimension; k++) {\n")
  //    code.append("        for (int i = k; i < dimension; i++) {\n")
  //    code.append("            float valInv = 1.0f / matrix[i][k];\n")
  //    code.append("            for (int j = k + 1; j < dimension; j++)\n")
  ////    code.append("            for (int j = k; j < dimension; j++)\n")
  //    code.append("                matrix[i][j] *= valInv;\n")
  //    code.append("            for (int j = 0; j < dimension; j++)\n")
  //    code.append("                inverse[i][j] *= valInv;\n")
  //    code.append("        }\n")
  //    code.append("        for (int i = k + 1; i < dimension; i++) {\n")
  //    code.append("            for (int j = k + 1; j < dimension; j++)\n")
  ////    code.append("            for (int j = k; j < dimension; j++)\n")
  //    code.append("                matrix[i][j] -= matrix[k][j];\n")
  //    code.append("            for (int j = 0; j < dimension; j++)\n")
  //    code.append("               inverse[i][j] -= inverse[k][j];\n")
  //    code.append("        }\n")
  //    code.append("    }\n")

  // Matrix now in row echelon form- 1's on diagonal and 0's below
  // Now make it reduced row echelon by zeroing the upper triangle of
  // elements.

  code.append("    for (int i = dimension - 2; i >= 0; i--) {\n")
  code.append("        for (int j = dimension - 1; j > i; j--) {\n")
  code.append("            for (int k = 0; k < dimension; k++)\n")
  code.append("                inverse[i][k] -= matrix[i][j] * inverse[j][k];\n")
  // The following lines are unnecessary and increased the numerical error.  -RJC
  //    code.append("            for (int k = 0; k < dimension; k++)\n")
  //    code.append("                matrix[i][k] -= matrix[i][j] * matrix[j][k];\n")
  code.append("        }\n")
  code.append("    }\n")
  // Output computed inverse matrix
  code append "    tensorElement = 0;\n"
  code.append("    for (int row = 0; row < " + matrixSize + "; row++) {\n")
  code.append("        for (int col = 0; col < " + matrixSize + "; col++) {\n")
  code.append("            @outElement0 = (float) inverse[row][col];\n")
  code.append("            tensorElement += 1;\n")
  code.append("        }\n")
  code.append("    }\n")
  addCode(code.toString())
//      debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object MatrixInvertHyperKernel {

  /**
   * Create a hyperkernel that inverts every matrix in a matrix field using
   * Gauss-Jordan elimination.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The opcode of the kernel, checked against MatrixInvertOp.
   * @param resultType The type of the resulting field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): HyperKernel = {
    require(operation == MatrixInvertOp)
    val matrixShape = in.fieldType.tensorShape
    require(matrixShape.dimensions == 2)
    require(matrixShape(0) == matrixShape(1))
    require(in.fieldType == resultType)

    new MatrixInvertHyperKernel(in, operation, resultType)
  }
}

