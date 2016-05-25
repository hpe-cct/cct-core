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
import cogx.cogmath.algebra.real.Matrix
import cogx.compiler.codegenerator.opencl.cpukernels.FixedVectorKernel

/** A hyperkernel that does bilateral filtering of a 2D tensor field, using
  * border clamping to handle the field boundary.
  *
  * A clear description of bilateral filtering can be found in "Real-time
  * edge-aware image processing with the bilateral grid," Chen, Paris and
  * Durand, 2007. Note that the algorithm described in that paper is *not*
  * used here, we instead do a brute force approach. (The approach in that
  * paper was perhaps motivated by the limitations of GPU shaders available
  * at the time).
  *
  * @author Greg Snider

  * @param in Input tensor virtual field register .
  * @param spatialFilter Spatial filter for smoothing input.
  * @param rangeSigma Sigma for Gaussian filter to smooth range.
  * @param operation Opcode
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class BilateralFilterHyperKernel private (in: VirtualFieldRegister,
                                 spatialFilter: Matrix,
                                 rangeSigma: Float,
                                 operation: Opcode,
                                 resultType: FieldType)
        extends HyperKernel(operation,
          Array(in, FixedVectorKernel(spatialFilter.toVector).outputs(0)),
          resultType, BigTensorAddressing)
{
  val filterSize = spatialFilter.rows
  require((spatialFilter.columns == filterSize) && (filterSize % 2 == 1))
  val halo = filterSize / 2
  val inType = in.fieldType

  // Field input 1 is the spatial filter. By marking this as constant, the GPU
  // can access it more efficiently in constant memory rather than global
  // memory.
  makeInputConstant(1)

  val code = new StringBuffer
  code append """
    // Declare local memory for caching a tile of the input field.
#define localWidth (_localColumns + %halo% + %halo%)
#define localHeight (_localRows + %halo% + %halo%)
    __local float localImage[localHeight][localWidth];

    // Determine where each work group starts writing.
    const int groupStartCol = get_group_id(0) * _localColumns;
    const int groupStartRow = get_group_id(1) * _localRows;

    // loop through all tensor elements
    for (int tensorElement = 0; tensorElement < _tensorElements; tensorElement++) {
      if (tensorElement != 0)
        barrier(CLK_LOCAL_MEM_FENCE);
      // Cache the data to local memory. If the needed memory location falls
      // outside the input image, use proper border policy. Note
      // that each work-item is only reading a small number of entries into
      // the local memory (check the 'for' loops).
      // Step across rows. Note the offset to read the halo.
      for (int r = _localRow; r < localHeight; r += _localRows) {
        int readRow = groupStartRow + r - %halo%;
        // Step across columns. Note the offset read the halo.
        for (int c = _localColumn; c < localWidth; c += _localColumns) {
          int readColumn = groupStartCol + c - %halo%;
          // Read if it is in bounds, otherwise use correct border policy.
          if ((readRow < _rows) && (readColumn < _columns) && (readRow >= 0) && (readColumn >= 0)) {
            int row = readRow;
            int column = readColumn;
            localImage[r][c] = readElementNonlocal(@in0); // (row, column, tensorElement)
          } else {
            int row = min(max(readRow, 0), _rows - 1);
            int column = min(max(readColumn, 0), _columns - 1);
            localImage[r][c] = readElementNonlocal(@in0); // (row, column, tensorElement)
          }
        }
      }
      barrier(CLK_LOCAL_MEM_FENCE);
#undef localWidth
#undef localHeight

      // Image tile is cached, so do the convolution. Each work-item will filter
      // around its start location (starting from the filter radius left and up).
      // The spatial filter components are read from in1, the range component
      // is computed on the fly.
      float sum = 0.0f;        // Numerator of bilateral filter, eqn (1a)
      float normalize = 0.0f;  // Denominator of bilateral filter, eqn (1b)

      // Read the pixel at the "center" of the filter window for this thread.
      float centerPixel =
         localImage[%halo% + _localRow][%halo% + _localColumn];

      // Move column pointer to upper left corner of filter window.
      column = 0;
      for (int r = _localRow; r < _localRow + %filterSize%; r++){
        for (int c = _localColumn; c < _localColumn + %filterSize%; c++) {

          float remotePixel = localImage[r][c];
          float spatialDistance = readNonlocal(@in1);

          // Compute Gaussian of difference between center and remote pixel.
          float range = centerPixel - remotePixel;
          float rangeDistance = native_exp(range * range * %expScale%) *
             %rangeScale%;
          float w = spatialDistance * rangeDistance;
          normalize += w;
          sum += w * remotePixel;

          column++;
        }
      }
      // Avoid division by zero, which can happen if input image is zero.
      normalize = fmax(normalize, 0.0000000001f);

      // write (_row, _column, tensorElement)
      if (_row < _rows && _column < _columns)
        @outElement0 = sum / normalize;
    } // End loop over tensor elements.
              """

  val codeString = code.toString.
          replaceAll("%filterSize%", filterSize.toString).
          replaceAll("%halo%", halo.toString).
          replaceAll("%rangeScale%",
            (1f / (rangeSigma * math.sqrt(2* math.Pi))).toString).
          replaceAll("%expScale%",
             (-1f / (rangeSigma * rangeSigma)).toString)
  addCode(codeString)
  //  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object BilateralFilterHyperKernel {

  /** Compute the backward divergence for a 2D vector or rank-2 tensor field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, spatialFilter: Matrix, rangeSigma: Float,
            operation: Opcode, resultType: FieldType): AbstractKernel =
  {
    require(resultType == in.fieldType)
    //require(operation == BilateralFilter2DOp)

    new BilateralFilterHyperKernel(in, spatialFilter, rangeSigma, operation, resultType)
  }
}

