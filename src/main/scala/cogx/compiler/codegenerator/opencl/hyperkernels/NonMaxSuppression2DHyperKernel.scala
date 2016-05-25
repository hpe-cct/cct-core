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

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, TensorElementAddressing, SmallTensorAddressing, HyperKernel}
import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.NonMaximumSuppressionOp
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Forces locally non-maximum pixels to zero for both scalar fields and
  * vector fields.
  *
  * Locality is defined to be the 8 nearest neighbors to a given pixel. If a
  * pixel is greater than or equal to any of those neighbors, it's value is
  * left intact on the output, otherwise it's set to zero. Note that border
  * pixels have only 5 nearest neighbors and corner pixels have only 3 nearest
  * neighbors, so those are the only ones checked.
  *
  * Vector fields are treated as though they were an array of scalar fields,
  * so non-maximum suppression is executed independently on each.
  *
  * @author Greg Snider
  *
  * @param in The virtual field register of the input field (scalar or vector)
  *        that will have its locally non-maximum values suppressed to zero in
  *        the output field.
  * @param op The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class NonMaxSuppression2DHyperKernel private (in: VirtualFieldRegister,
                                              op: Opcode,
                                              resultType: FieldType,
                                              addressMode: AddressingMode)
        extends HyperKernel(op, Array(in), in.fieldType, addressMode)
{
  // Size of neighborhood in each dimension.
  val neighborhoodSize = 3

  val readType = addressing.clType(resultType).name
  val initVal = "((" + readType + ") -MAXFLOAT)"
  // First step is to read in a tile of the input image into local memory
  // that all work-items in the work group can use to do the convolution.
  // There is one work-item (thread) per output pixel. This tile must be
  // padded all around to pull in parts of the image needed by the
  // convolution, which means that each thread will generally have to read
  // in more than one pixel, depending on the kernel size. Much of the code
  // for this kernel is modelled after the example in the book "Heterogeneous
  // Computing with OpenCL." See that book also for some of the following
  // comments. The biggest difference from the book is that our output image
  // is the same size as in the input image, while the book trims the output
  // as a function of the kernel size.

  val code = new StringBuffer

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  code append """
    // Declare local memory for caching a tile of the input field.
#define halo ((%neighborhoodSize% - 1) / 2)
#define localWidth (_localColumns + 2 * halo + 1)
#define localHeight (_localRows + 2 * halo + 1)
    __local %readType% localImage[localHeight][localWidth];

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
        int readColumn = groupStartCol + c - halo;
        // Read if it is in bounds, otherwise set to large negative number
        if ((readRow < _rows) && (readColumn < _columns) && (readRow >= 0) && (readColumn >= 0)) {
            row = readRow;
            column = readColumn;
            localImage[r][c] = readNonlocal(@in0); // (row, column)
        } else {
            localImage[r][c] = %initVal%; // outside of image
        }
      }
    }
    barrier(CLK_LOCAL_MEM_FENCE);

    // Image tile is cached, search local neighborhood. Each work-item searches
    // around its start location (starting from the filter radius left and up).
    %readType% center = localImage[_localRow + halo][_localColumn + halo];
    %readType% maximum = center;

    column = 0;
    for (int r = _localRow; r < _localRow + %neighborhoodSize%; r++){
        int c = _localColumn;
              """

  // Unroll the inner loop (unroll pragma is apparently unreliable).
  for (column <- 0 until neighborhoodSize)
    code append """
        maximum = fmax(localImage[r][c++], maximum);
        column++;
                """
  code append """
    }
    @out0 = center * greaterThanEqual(center, maximum);

#undef halo
#undef localWidth
#undef localHeight
              """

  val codeString =
    code.toString.replaceAll("%neighborhoodSize%", neighborhoodSize.toString).
            replaceAll("%readType%", readType).
            replaceAll("%initVal%", initVal)

  addCode(codeString)

  val Debug = false
  if (Debug)
    debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object NonMaxSuppression2DHyperKernel {

  /** Create a hyperkernel for non-maximum suppression.
    *
    * @param in The virtual field register of the input field (scalar or vector)
    *        that will have its locally non-maximum values suppressed to zero in
    *        the output field.
    * @param operation Opcode for hyperkernel.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): HyperKernel =
  {
    val inType = in.fieldType
    require(inType.fieldShape.dimensions == 2)
    require(inType.tensorShape.dimensions <= 1) // scalar or vector field
    require(inType == resultType)

    require(operation == NonMaximumSuppressionOp)
    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new NonMaxSuppression2DHyperKernel(in, operation, resultType, addressing)
  }
}

