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

/** Deinterleaves columns of the 2D input scalar field, used as a postprocessing
  * step before leaving the FFT.
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
class DCTDeinterleaveHyperKernel private (in: VirtualFieldRegister,
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

      // Now for the descrambling:
      //   1. Threads in the left half of the image:
      //       a. even threads write first, doubling their index
      //       b. then odd threads write, double their index
      //   2. Threads in the right half of the image:
      //       a. even threads write first, mangling their index (see code)
      //       b. then odd threads write, mangling their index.
      //
      // Note that we assume that the number of local memory banks is 32
      // (true for all newer GPUs). We will get warp divergence here, but no
      // local memory bank conflicts.
      //
      for (int chunk = 0; chunk < chunks; chunk++) {
        int column = _column + chunk * %threadsPerWorkGroup%;
        int leftDestColumn = column * 2;
        int rightDestColumn = 2 * (%columns% - column) - 1;
        if (column < %columns% / 2) {
          // Left half
          if (_column % 2 == 0)
            outputLine[leftDestColumn] = inputLine[column];
          barrier(CLK_LOCAL_MEM_FENCE);
          if (_column % 2 == 1)
            outputLine[leftDestColumn] = inputLine[column];
          barrier(CLK_LOCAL_MEM_FENCE);
        } else {
          // Right half
          if (_column % 2 == 0)
            outputLine[rightDestColumn] = inputLine[column];
          barrier(CLK_LOCAL_MEM_FENCE);
          if (_column % 2 == 1)
            outputLine[rightDestColumn] = inputLine[column];
          barrier(CLK_LOCAL_MEM_FENCE);
        }
      }

      // Calculate the output row for writing.
      if (_row < %rows% / 2) {
        row = _row * 2;
      } else {
        row = 2 * (%rows% - _row) - 1;
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
object DCTDeinterleaveHyperKernel {
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType): HyperKernel = {
    new DCTDeinterleaveHyperKernel(in, operation, resultType)
  }
}