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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.compiler.parser.op.UnaryOpcode
import cogx.platform.types.{FieldType, VirtualFieldRegister}

/** Interleaves rows and columns of the 2D input scalar field, used as a
  * preprocessing step before entering the DCT.
  *
  * The algorithm:
  *
  * For each row (column scrambling):
  *
  * 1. Extract the even columns and use those as the left half of the output.
  *
  * 2. Extract the odd columns, reflect them, and use that as the right half
  * of the output.
  *
  * Then scramble the rows using the same algorithm (row scrambling):
  *
  * 1. Extract the even rows and use those as the top half of the output.
  *
  * 2. Extract the odd rows, reflect them, and use that as the bottom half
  * of the output.
  *
  * The first step, column scrambling, is most difficult because of the need
  * coalesce read operations to global memory and the need to avoid local
  * memory bank conflicts.
  *
  * A detailed description can be found in "Image compression using the discrete
  * cosine transform," Watson, Mathematica Journal, 4(1), 1994
  *
  * @param in The virtual field register of the input field to be interleaved.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @author Greg Snider
  */
private[cogx]
class DCTInterleaveHyperKernel private (in: VirtualFieldRegister,
                                        operation: UnaryOpcode,
                                        resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, BigTensorAddressing)
{
  // Our work group is just a single warp. We're I/O bound.
  val ThreadsPerWorkGroup = 32
  // Rows of the input image (same size as output).
  val rows = in.fieldType.rows
  // Columns in each line of the input image.
  val columns = in.fieldType.columns
  val MinLocalMemorySize = 32768  // in bytes: from OpenCL 1.1 spec
  val BytesPerFloat = 4
  val LocalMemoryBanks = 2   // needed by this kernel
  val MaxColumns = (MinLocalMemorySize / BytesPerFloat) / LocalMemoryBanks
  require(columns <= MaxColumns,
    "image too wide for DCT, maximum width: " + MaxColumns)
  require(columns % ThreadsPerWorkGroup == 0, "image size must be a multiple of 32")
  require(rows % ThreadsPerWorkGroup == 0, "image size must be a multiple of 32")

  // We pretend we're working on an input field of rows x ThreadsPerWorkGroup,
  // since that's how we want OpenCL to schedule it.
  override lazy val workFieldType =
    in.fieldType.resize(Shape(in.fieldType.rows, ThreadsPerWorkGroup))
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressing,
      1, ThreadsPerWorkGroup)

  // We buffer a single line of the image.
  val code = new StringBuffer
  code append  """
    // Buffer for one line of the input image.
    __local float inputLine[%columns%];
    __local float outputLine[%columns%];

    for (int tensorElement = 0; tensorElement < _tensorElements; tensorElement++) {

      // Use the threads in this work group to read in the line, one chunk
      // at a time (a "chunk" is the number of threads in this work group).
      int chunks = %columns% / %threadsPerWorkGroup%;
      int layer = 0;
      int row = _row;
      for (int chunk = 0; chunk < chunks; chunk++) {
        int column = _column + chunk * %threadsPerWorkGroup%;
        inputLine[column] = readElementNonlocal(@in0);
      }
      barrier(CLK_LOCAL_MEM_FENCE);

      // Read the even columns of the input line and write them to the first
      // half of the output line and the odd columns and write them to the second
      // half of the input line. Note that we assume that the number of local
      // memory banks is 32 (true for all newer GPUs). We will get warp
      // divergence here, but no local memory bank conflicts.
      for (int chunk = 0; chunk < chunks; chunk++) {
        int column = _column + chunk * %threadsPerWorkGroup%;
        int evenDestColumn = column / 2;
        int oddDestColumn = %columns% - (column / 2) - 1;
        if (_column % 2 == 0)
          outputLine[evenDestColumn] = inputLine[column];
        else
          outputLine[oddDestColumn] = inputLine[column];
        barrier(CLK_LOCAL_MEM_FENCE);
      }

      // Calculate the output row for writing.
      if (_row % 2 == 0) {
        row = _row / 2;
      } else {
        row = %rows% - (_row / 2) - 1;
      }
      for (int chunk = 0; chunk < chunks; chunk++) {
        int column = _column + chunk * %threadsPerWorkGroup%;
        @outElementNonlocal0 = outputLine[column];
      }
    }
               """
  val codeString = code.toString.
          replaceAll("%rows%", rows.toString).
          replaceAll("%columns%", columns.toString).
          replaceAll("%threadsPerWorkGroup%", ThreadsPerWorkGroup.toString)
  addCode(codeString)
}

private[cogx]
object DCTInterleaveHyperKernel {
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType): HyperKernel = {
    new DCTInterleaveHyperKernel(in, operation, resultType)
  }
}