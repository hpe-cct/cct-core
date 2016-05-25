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

package cogx.compiler.codegenerator.opencl.fragments

import cogx.platform.types._
import cogx.cogmath.geometry.Shape

/** Synthesises code to allocate and fill a local (shared) memory tile from
  * an OpenCL input buffer.
  *
  * @author Greg Snider
  */
private[cogx]
object LocalTensorMemory2D {

  /** Synthesize code that allocates and fills a tile from a 2D field,
    * including "halos" on all sides needed for nonlocal processing.
    *
    * First step is to read in a tile of the input image into local memory
    * that all work-items in the work group can use to do the convolution.
    * There is one work-item (thread) per output pixel. This tile must be
    * padded all around to pull in parts of the image needed by the
    * convolution, which means that each thread will generally have to read
    * in more than one pixel, depending on the kernel size. Much of the code
    * for this kernel is modelled after the example in the book "Heterogeneous
    * Computing with OpenCL."
    *
    * Note there is a subtle issue here regarding threads. If the input field
    * rows and columns are not multiples of the work-group size, you must
    * still launch all of the threads in the work group to fill the local
    * memory. That is why the final return statement (which typically appears
    * at the beginning of kernels) is deferred until after the local memory
    * is loaded. This means that the HyperKernel code generator must be
    * aware that a kernel is using local memory in this way, which is flagged
    * by inheriting the NonlocalOperator trait. A better way would be to
    * have including of a local memory code segment force this (TODO).
    *
    * @param inShape The fieldShape of the field being read into local memory.
    * @param inType The type of field being read into local memory.
    * @param topHalo Size of top halo, in pixels.
    * @param rightHalo Size of right halo, in pixels.
    * @param bottomHalo Size of bottom halo, in pixels.
    * @param leftHalo Size of left halo, in pixels.
    * @param borderProcessing Algorithm for filling tile when the tile extends
    *        beyond the border (because of halos) of the input field.
    * @return A string that implements the loading of the local memory.
    */
  def apply(inShape: Shape,
            inType: CLType,
            topHalo: Int,
            rightHalo: Int,
            bottomHalo: Int,
            leftHalo: Int,
            borderProcessing: BorderPolicy): String =
  {
    require(inShape.dimensions == 2)
    val inRows = inShape(0)
    val inColumns = inShape(1)
    val code = new StringBuffer
    code.append("""
    // Declare local memory for caching a tile of the input field.
#define localWidth (_localColumns + %leftHalo% + %rightHalo%)
#define localHeight (_localRows + %topHalo% + %bottomHalo%)
    __local %tensorType% localImage[localHeight][localWidth];

    // Determine where each work group starts writing.
    const int groupStartCol = get_group_id(0) * _localColumns;
    const int groupStartRow = get_group_id(1) * _localRows;

    // Cache the data to local memory. If the needed memory location falls
    // outside the input image, use proper border policy. Note
    // that each work-item is only reading a small number of entries into
    // the local memory (check the 'for' loops).
    // Step across rows. Note the offset to read the halo.
    for (int r = _localRow; r < localHeight; r += _localRows) {
      int readRow = groupStartRow + r - %topHalo%;
      // Step across columns. Note the offset read the halo.
      for (int c = _localColumn; c < localWidth; c += _localColumns) {
        int readColumn = groupStartCol + c - %leftHalo%;
        // Read if it is in bounds, otherwise use correct border policy.
        if ((readRow < %inRows%) && (readColumn < %inColumns%) && (readRow >= 0) && (readColumn >= 0)) {
            int row = readRow;
            int column = readColumn;
            localImage[r][c] = readNonlocal(@in0); // (row, column)
        } else {
              """)

    val borderCode = borderProcessing match {
      case BorderZero =>
        """
          localImage[r][c] = %tensorZero%;
        """
      case BorderFull =>
        """
          localImage[r][c] = %tensorZero%;
        """
      case BorderValid =>
        """
          // This should never go out of bounds, so (in principle) this code
          // will never be executed
          localImage[r][c] = %tensorZero%;
        """
      case BorderClamp =>
        """
          int row = min(max(readRow, 0), _rows - 1);
          int column = min(max(readColumn, 0), _columns - 1);
          localImage[r][c] = readNonlocal(@in0); // (row, column)
        """
      case BorderCyclic =>
        """
          int row = 0;
          if (readRow < 0) {
            row = readRow + _rows;
          }
          if (readRow < _rows && readRow >=0) {
            row = readRow;
          }
          if (readRow >= _rows) {
            row = readRow - _rows;
          }

          int column = 0;
          if (readColumn < 0) {
            column = readColumn + _columns;
          }
          if (readColumn < _columns && readColumn >= 0) {
            column = readColumn;
          }
          if (readColumn >= _columns) {
            column = readColumn - _columns;
          }

          localImage[r][c] = readNonlocal(@in0); // (row, column)
        """
      case _ => throw new RuntimeException("border policy not implemented: " +
              borderProcessing)
    }
    code append borderCode
    code append """
          }
        }
      }
      barrier(CLK_LOCAL_MEM_FENCE);
      if (_row >= _rows || _column >= _columns)
        return;
#undef localWidth
#undef localHeight
    """
    code.toString.
            replaceAll("%tensorType%", inType.name).
            replaceAll("%tensorZero%", inType.zero).
            replaceAll("%topHalo%", topHalo.toString).
            replaceAll("%rightHalo%", rightHalo.toString).
            replaceAll("%bottomHalo%", bottomHalo.toString).
            replaceAll("%leftHalo%", leftHalo.toString).
            replaceAll("%inRows%", inRows.toString).
            replaceAll("%inColumns%", inColumns.toString)
  }
}