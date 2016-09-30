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

package cogx.compiler.codegenerator.opencl.hyperkernels.domaintransform

import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.codegenerator.opencl.fragments.{ClampToEdge, AddressingMode, SmallTensorAddressing, HyperKernel}
import cogx.cogmath.algebra.real.Logarithm
import cogx.platform.opencl.WorkGroupParameters

/** Implements the domain transform on the rows of a color field. See the paper
  * "Domain transform for edge-aware image and video processing," Gastal and
  * Oliveira, 2011, for a description.
  *
  * @param in An array of three fields: (1) The color field to be filtered;
   *       (2) The domain transform field for the image which will be used to
   *       "warp" the domain; (3) A 0-D field which holds the box filter
   *       radius.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode Addressing mode for this kernel (must be small tensor).
  *
  * @author Greg Snider
  */
private[cogx]
class ColorDomainFilterHyperKernel private[hyperkernels] (
                                              in: Array[VirtualFieldRegister],
                                              operation: Opcode,
                                              resultType: FieldType,
                                              addressMode: AddressingMode,
                                              filterRows: Boolean)
        extends HyperKernel(operation, in, resultType, addressMode, ClampToEdge)
        with Logarithm
{
  require(addressMode == SmallTensorAddressing, "only works for small tensor fields")


  /** Max threads per workgroup, which is hardware dependent. We need a better
    * way of finding this out for a given target.
    */
  val MaxThreadsPerWorkGroup = 1024
  private val rows = in(0).fieldType.rows
  private val columns = in(0).fieldType.columns
  require(rows <= MaxThreadsPerWorkGroup, "image too big for this operator")
  require(columns <= MaxThreadsPerWorkGroup, "image too big for this operator")

  // Work group computation.
  // Our strategy is for each work group to work on a single line (row or
  // column) of the image, with one thread for each pixel in the line.
  override lazy val workGroup =
    if (filterRows)
      new WorkGroupParameters(2,
        globalLayers  = 1,
        globalRows    = rows,
        globalColumns = columns,
        localLayers  = 1,
        localRows    = 1,
        localColumns = columns
      )
    else
      new WorkGroupParameters(2,
        globalLayers  = 1,
        globalRows    = rows,
        globalColumns = columns,
        localLayers  = 1,
        localRows    = rows,
        localColumns = 1
      )

  // Parameters for kernel.
  val lineIndex = if (filterRows) "_column" else "_row"
  val lineLength = if (filterRows) columns else rows
  val doublingIterations = log2(roundUpPowerOf2(lineLength)).toInt

  // The kernel code
  val prolog: String =
    if (filterRows)
      """
        | // Read in the domain transform.
        |
        | __local float domainTransform[%lineLength%];
        | domainTransform[_column] = read(@in1);
      """.stripMargin
    else
      """
        | // Read in the domain transform. The work group is reading
        | // down a column of the image using the _row index, but across the
        | // domain transform buffer. So we must do a nonlocal read here to
        | // use _row as the column index for the domain transform field.
        |
        | __local float domainTransform[%lineLength%];
        | row = _column;
        | column = _row;
        | domainTransform[_row] = readNonlocal(@in1);
      """.stripMargin
  val code =
    """
      | // The prolog reads in the domain transform.
      | %prolog%
      |
      | // Read in a row of the image. We use a single array and concatenate
      | // all the red values, followed by the green values, followed by the
      | // blue values.
      | __local float rgb_A[3 * %lineLength%];
      | float4 pixel = read(@in0);
      | rgb_A[%lineIndex%] = pixel.x;
      | rgb_A[%lineIndex% + %lineLength%] = pixel.y;
      | rgb_A[%lineIndex% + 2 * %lineLength%] = pixel.z;
      | barrier(CLK_LOCAL_MEM_FENCE);
      |
      | // Integrate using recursive doubling to get the integral image.
      | // We alternate between the A buffers and a second set of
      | // B buffers that we create here. For a description of
      | // recursive doubling, see "Fast summed-area table generation and its
      | // applications," Hensley et al, 2005.
      | __local float rgb_B[3 * %lineLength%];
      | __local float* rgb_1 = rgb_A;
      | __local float* rgb_2 = rgb_B;
      |
      | int stride = 1;
      | for (int i = 0; i < %doublingIterations%; i++) {
      |   {
      |     // red
      |     float center = rgb_1[%lineIndex% + 0 * %lineLength%];
      |     float left;
      |     int leftIndex = %lineIndex% - stride;
      |     if (leftIndex >= 0)
      |       left = rgb_1[leftIndex + 0 * %lineLength%];
      |     else
      |       left = 0.0f;
      |     rgb_2[%lineIndex% + 0 * %lineLength%] = center + left;
      |   }
      |   {
      |     // green
      |     float center = rgb_1[%lineIndex% + 1 * %lineLength%];
      |     float left;
      |     int leftIndex = %lineIndex% - stride;
      |     if (leftIndex >= 0)
      |       left = rgb_1[leftIndex + 1 * %lineLength%];
      |     else
      |       left = 0.0f;
      |     rgb_2[%lineIndex% + 1 * %lineLength%] = center + left;
      |   }
      |   {
      |     // blue
      |     float center = rgb_1[%lineIndex% + 2 * %lineLength%];
      |     float left;
      |     int leftIndex = %lineIndex% - stride;
      |     if (leftIndex >= 0)
      |       left = rgb_1[leftIndex + 2 * %lineLength%];
      |     else
      |       left = 0.0f;
      |     rgb_2[%lineIndex% + 2 * %lineLength%] = center + left;
      |   }
      |   barrier(CLK_LOCAL_MEM_FENCE);
      |
      |   // Double the stride each iteration.
      |   stride = stride << 1;
      |
      |   // Swap buffers
      |   __local float* temp = rgb_1;
      |   rgb_1 = rgb_2;
      |   rgb_2 = temp;
      | }
      |
      | // Now that we have the integral image and the domain transform,
      | // we need to do a binary search around each pixel to find the
      | // upper and lower indices for the box filter. To simplify, we use
      | // the same stride for all threads at a given step, starting with a
      | // large stride, shrinking by a factor of 2 on each step. Note that
      | // we clamp the index at the boundaries to stay inside of local
      | // memory. Also note that the upper and lower indices are inclusive.
      | float center = domainTransform[%lineIndex%];
      | float radius = readScalar(@in2);
      | float lower = center - radius;
      | float upper = center + radius;
      | int lowerIndex = %lineIndex%;
      | int upperIndex = %lineIndex%;
      | for (int level = %doublingIterations%; level >= 0; level--) {
      |   int stride = 1 << level;
      |
      |   // Try expanding lower index down.
      |   int trialLowerIndex = max(lowerIndex - stride, 0);
      |   int inBounds = domainTransform[trialLowerIndex] >= lower;
      |   lowerIndex = mul24(inBounds, trialLowerIndex) +
      |     mul24(1 - inBounds, lowerIndex);
      |
      |   // Try expanding upper index up.
      |   int trialUpperIndex = min(upperIndex + stride, %lineLength% - 1);
      |   inBounds = domainTransform[trialUpperIndex] <= upper;
      |   upperIndex = mul24(inBounds, trialUpperIndex) +
      |     mul24(1 - inBounds, upperIndex);
      | }
      |
      | // Bump lower index down so it's exclusive to make summed area
      | // calculation easier
      | lowerIndex -= 1;
      |
      | // Compute the box filter from result. If the lower index is less than
      | // 0, we must correct for that.
      | float span = upperIndex - lowerIndex;
      | int lowerInBounds = lowerIndex >= 0;
      | float red = (rgb_1[upperIndex] -
      |   lowerInBounds * rgb_1[lowerIndex * lowerInBounds]) / span;
      | float green = (rgb_1[upperIndex + %lineLength%] -
      |   lowerInBounds * rgb_1[(lowerIndex + %lineLength%) * lowerInBounds]) / span;
      | float blue = (rgb_1[upperIndex + 2 * %lineLength%] -
      |   lowerInBounds * rgb_1[(lowerIndex + 2 * %lineLength%) * lowerInBounds]) / span;
      |
      | // Write out result
      | pixel = (float4) (red, green, blue, 1.0f);
      |
      | @out0 = pixel;
    """
  val codeString = code.toString.stripMargin.
          replaceAll("%prolog%", prolog).
          replaceAll("%lineLength%", lineLength.toString).
          replaceAll("%doublingIterations%", doublingIterations.toString).
          replaceAll("%lineIndex%", lineIndex)
  addCode(codeString)
  //debugCompile
}
