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
import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, HyperKernel}
import cogx.compiler.parser.op.ConvolveColumns2DOp


/** Convolve the columns of a field with a filter.  Used in conjunction with
  * ConvolveRows2DHyperKernel to implement separable convolution.
  *
  * NOTE: Needs more work, only supports the simplest convolution options. XXX
  *
  * @author Greg Snider
  *
  * @param in The input virtual field registers driving this kernel.
  * @param filter The filter kernel to be convolved with in.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class ConvolveColumns2DHyperKernel private (in: VirtualFieldRegister,
                                            filter: VirtualFieldRegister,
                                            operation: ConvolveColumns2DOp,
                                            resultType: FieldType)
        extends HyperKernel(operation,
          Array(in, filter),
          resultType, SmallTensorAddressing)
{
  val filterSize = filter.fieldType.columns
  val convolution: Boolean =
    operation.filterOrientation == ConvolutionOrientation

  // Since the output image is the same size as the input image, we have three
  // different ways we can handle the border pixels: border fill, zero
  // fill, or cyclic. The opcode tells us which to do. Border fill/Cyclic is
  // more expensive.
  private val fillMode = operation.borderPolicy

  // Field input 1 is the filter. By marking this as constant, the GPU can
  // access it more efficiently in constant memory rather than global memory.
  makeInputConstant(1)

  //
  // First step is to read in a tile of the input image into local memory
  // that all work-items in the work group can use to do the convolution.
  // Their is one work-item (thread) per output pixel. This tile must be
  // padded all around to pull in parts of the image needed by the
  // convolution, which means that each thread will generally have to read
  // in more than one pixel, depending on the kernel size. Much of the code
  // for this kernel is modelled after the example in the book "Heterogeneous
  // Computing with OpenCL." See that book also for some of the following
  // comments. The biggest difference from the book is that our output image
  // is the same size as in the input image, while the book trims the output
  // as a function of the kernel size.

  val code = new StringBuffer
  code append """
    // Declare local memory for caching a tile of the input field.
#define halo ((%filterSize% - 1) / 2)
#define localWidth (_localColumns)
#define localHeight (_localRows + 2 * halo + 1)
    __local float localImage[localHeight][localWidth];

    // Determine where each work group starts writing.
    const int groupStartCol = get_group_id(0) * _localColumns;
    const int groupStartRow = get_group_id(1) * _localRows;

    // Cache the data to local memory. If the needed memory location falls
    // outside the input image, use proper border policy. Note
    // that each work-item is only reading a small number of entries into
    // the local memory (check the 'for' loops).
    // Step across rows. Note the offset to read the halo.
    for (int r = _localRow; r < localHeight; r += _localRows) {
      int readRow = groupStartRow + r - halo;
      // Step across columns. Note the offset read the halo.
      for (int c = _localColumn; c < localWidth; c += _localColumns) {
        int readColumn = groupStartCol + c;
        // Read if it is in bounds, otherwise use correct border policy.
        if ((readRow < _rows) && (readColumn < _columns) && (readRow >= 0) && (readColumn >= 0)) {
            row = readRow;
            column = readColumn;
            localImage[r][c] = readNonlocal(@in0); // (row, column)
        } else {
              """

  val borderCode = fillMode match {
    case BorderZero =>
      """
       localImage[r][c] = 0.0f;
      """
    case BorderClamp =>
      """
       row = min(max(readRow, 0), _rows - 1);
       column = min(max(readColumn, 0), _columns - 1);
       localImage[r][c] = readNonlocal(@in0); // (row, column)
      """
    case BorderCyclic =>
      """
       row = 0;
       if (readRow < 0) {
         row = readRow + _rows;
       }
       if (readRow < _rows && readRow >=0) {
         row = readRow;
       }
       if (readRow >= _rows) {
         row = readRow - _rows;
       }

       column = 0;
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
    case _ => throw new RuntimeException("border policy not implemented here")
  }

  code append borderCode

  code append """
        }
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE);

    // Image tile is cached, so do the convolution.
    float sum = 0.0f;
    int r = _localRow;
    for (int i = 0; i < %filterSize%; i++) {
      // column computation needed by readNonlocal
      if (%convolution%)
        // Must flip kernel for convolution
        column = %filterSize% - 1 - i;
      else
        column = i;
      sum += localImage[r++][_localColumn] * readNonlocal(@in1);
    }
    @out0 = sum;

#undef halo
#undef localWidth
#undef localHeight
              """

  val codeString = code.toString.
          replaceAll("%filterSize%", filterSize.toString).
          replaceAll("%convolution%", (if (convolution) 1 else 0).toString)
  addCode(codeString)

  val Debug = false
  if (Debug)
    debugCompile()
}

/** Factory object for creating kernels of this type. */
private[cogx]
object ConvolveColumns2DHyperKernel {
  /** Convolve the rows of a field with a filter.
    *
    * @param in The input virtual field register driving this kernel.
    * @param filter The filter kernel to be convolved with in.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, filter: VirtualFieldRegister,
            operation: ConvolveColumns2DOp,
            resultType: FieldType): HyperKernel =
  {
    // Check requested policies to see if we can support them. Very few are
    // supported yet, this needs more work. XXX
    operation.borderPolicy match {
      case BorderZero =>
      case BorderClamp =>
      case BorderCyclic =>
      case x =>
        require(requirement = false, "unsupported border policy: " + x)
    }

    // Check field constraints. Currently this can only handle 2D scalar fields.
    require(in.fieldType.fieldShape.dimensions == 2,
      "Sorry, only 2D row convolution is currently supported")
    require(in.fieldType == resultType,
      "output type must equal input type")

    // Check filter constraints.
    val filterLayers = filter.fieldType.layers
    val filterRows = filter.fieldType.rows
    val filterSize = filter.fieldType.columns
    require(filterRows == 1 && filterLayers == 1,
      "row filter must be 1D")
    require(filter.fieldType.tensorShape.dimensions == 0,
      "row filter must be scalar")
    require(filterSize % 2 == 1,
      "row filter must be of odd size")


    // Everything OK, create the hyperkernel.
    new ConvolveColumns2DHyperKernel(in, filter, operation, resultType)
  }
}

