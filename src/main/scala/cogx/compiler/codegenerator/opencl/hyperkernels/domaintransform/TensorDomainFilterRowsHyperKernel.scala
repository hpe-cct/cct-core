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
import cogx.compiler.codegenerator.opencl.fragments._
import cogx.compiler.parser.op.NaryOpcode
import cogx.cogmath.algebra.real.Logarithm
import cogx.platform.opencl.WorkGroupParameters
import cogx.compiler.codegenerator.common.FieldPolicies._


/** Implements the domain transform on the rows of a small tensor field,
  * where "small" means either a scalar field or a vector field with length 2
  * vectors.
  *
  * See the paper
  * "Domain transform for edge-aware image and video processing," Gastal and
  * Oliveira, 2011, for a description.
  *
  * @param in An array of three fields: (1) The small tensor field to be filtered;
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
class TensorDomainFilterRowsHyperKernel private[hyperkernels] (
                                       in: Array[VirtualFieldRegister],
                                       operation: Opcode,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode, ClampToEdge)
        with Logarithm
{
  require(addressMode == BigTensorAddressing)

  /** Max threads per workgroup, which is hardware dependent. We need a better
    * way of finding this out for a given target.
    */
  val MaxThreadsPerWorkGroup = 1024
  private val rows = in(0).fieldType.rows
  private val columns = in(0).fieldType.columns
  require(rows <= MaxThreadsPerWorkGroup, "image too big for this operator")
  require(columns <= MaxThreadsPerWorkGroup, "image too big for this operator")

  // Work group computation.
  // Our strategy is for each work group to work on a single row of the
  // of the input, with one thread for each pixel in the row.
  override lazy val workGroup =
    new WorkGroupParameters(2,
      globalLayers  = 1,
      globalRows    = rows,
      globalColumns = columns,
      localLayers  = 1,
      localRows    = 1,
      localColumns = columns
    )

  // Parameters for kernel.
  val lineLength = columns
  val doublingIterations = log2(roundUpPowerOf2(lineLength)).toInt

  // NOTE: The image to be filtered, in(0), may be any tensor field. But the
  // guiding domain transform, in(1), may either be a scalar field or tensor
  // field that exactly matches the tensor shape of in(0). If the guide is a
  // scalar field, all tensor slices are filtered using the guide, otherwise
  // the in(0) slices are filtered one-to-one with the guide, in(1), slices.
  // We determine here which case we have.
  val input = in(0)
  val guide = in(1)
  val inputIsScalar = input.fieldType.tensorShape.dimensions == 0
  val guideIsScalar = guide.fieldType.tensorShape.dimensions == 0
  if (!inputIsScalar && !guideIsScalar)
    require(input.fieldType.tensorShape == guide.fieldType.tensorShape,
      "illegal guide tensor shape for input")

  // The kernel code.
  var code = ""

  // Code to read in the guide.
  val readDomainTransform =
    """
      |   // Read in the domain transform.
      |   __local float domainTransform[%lineLength%];
      |   domainTransform[_column] = readElement(@in1);
      |
    """.stripMargin

  if (guideIsScalar)
    // Only need to read the guide in once.
    code += readDomainTransform
  if (!inputIsScalar) {
    // Must iterate over multiple tensor elements
    code +=
       """
         | // Outer loop, over all tensor elements.
         | for (tensorElement = 0; tensorElement < _tensorElements; tensorElement++) {
       """.stripMargin
    if (!guideIsScalar)
      // Must read in a different guide for each tensor element
      code += readDomainTransform
  }
  code +=
    """
      |   // Read in a row of the image (or tensor slice)
      |   __local float line_A[%lineLength%];
      |   line_A[_column] = readElement(@in0);
      |   barrier(CLK_LOCAL_MEM_FENCE);
      |
      |   // Integrate using recursive doubling to get the integral image.
      |   // We alternate between the A buffers and a second set of
      |   // B buffers that we create here. For a description of
      |   // recursive doubling, see "Fast summed-area table generation and its
      |   // applications," Hensley et al, 2005.
      |
      |   __local float line_B[%lineLength%];
      |   __local float* line_1 = line_A;
      |   __local float* line_2 = line_B;
      |
      |   int stride = 1;
      |   for (int i = 0; i < %doublingIterations%; i++) {
      |     float center = line_1[_column];
      |     float left;
      |     int leftIndex = _column - stride;
      |     if (leftIndex >= 0)
      |       left = line_1[leftIndex];
      |     else
      |       left = 0.0f;
      |     line_2[_column] = center + left;
      |     barrier(CLK_LOCAL_MEM_FENCE);
      |
      |     // Double the stride each iteration.
      |     stride = stride << 1;
      |
      |     // Swap buffers
      |     __local float* temp = line_1;
      |     line_1 = line_2;
      |     line_2 = temp;
      |   }
      |   barrier(CLK_LOCAL_MEM_FENCE);
      |
      |   // Now that we have the integral image and the domain transform,
      |   // we need to do a binary search around each pixel to find the
      |   // upper and lower indices for the box filter. To simplify, we use
      |   // the same stride for all threads at a given step, starting with a
      |   // large stride, shrinking by a factor of 2 on each step. Note that
      |   // we clamp the index at the boundaries to stay inside of local
      |   // memory. Also note that the upper and lower indices are inclusive.
      |
      |   float center = domainTransform[_column];
      |   float radius = readScalar(@in2);
      |   float lower = center - radius;
      |   float upper = center + radius;
      |   int lowerIndex = _column;
      |   int upperIndex = _column;
      |   for (int level = %doublingIterations%; level >= 0; level--) {
      |     int stride = 1 << level;
      |
      |     // Try expanding lower index down.
      |     int trialLowerIndex = max(lowerIndex - stride, 0);
      |     int inBounds = domainTransform[trialLowerIndex] >= lower;
      |     lowerIndex = mul24(inBounds, trialLowerIndex) +
      |       mul24(1 - inBounds, lowerIndex);
      |
      |     // Try expanding upper index up.
      |     int trialUpperIndex = min(upperIndex + stride, %lineLength% - 1);
      |     inBounds = domainTransform[trialUpperIndex] <= upper;
      |     upperIndex = mul24(inBounds, trialUpperIndex) +
      |       mul24(1 - inBounds, upperIndex);
      |   }
      |
      |   // Bump lower index down so it's exclusive to make summed area
      |   // calculation easier
      |   lowerIndex -= 1;
      |
      |   // Compute the box filter from result. If the lower index is less than
      |   // 0, we must correct for that.
      |
      |   float span = upperIndex - lowerIndex;
      |   int lowerInBounds = lowerIndex >= 0;
      |   float pixel = (line_1[upperIndex] -
      |     lowerInBounds * line_1[lowerIndex * lowerInBounds]) / span;
      |   barrier(CLK_LOCAL_MEM_FENCE);
      |
      |   // Write out result
      |   @outElement0 = pixel;
      |
    """.stripMargin

  if (!inputIsScalar)
    // Must terminate loop over all tensor elements.
    code += "}\n"

  val codeString = code.
          replaceAll("%lineLength%", lineLength.toString).
          replaceAll("%doublingIterations%", doublingIterations.toString)
  addCode(codeString)
  //debugCompile
}

/** Factory for creating hyper kernels that implement domain transform filtering
  * on rows of a tensor field.
  *
  * @author Greg Snider
  */
private[cogx]
object TensorDomainFilterRowsHyperKernel {

  /** Create a hyperkernel that implements the domain transform for the rows
    * of an input color field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister],
            operation: NaryOpcode,
            resultType: FieldType): HyperKernel =
  {
    require(in.length == 3)
    require(in(0).fieldType.dimensions == 2)
    val expectedResultType = in(0).fieldType
    require(resultType == expectedResultType)
    val addressing = BigTensorAddressing
    require(!isColorField(in(0).fieldType))
    new TensorDomainFilterRowsHyperKernel(in, operation, resultType, addressing)
  }
}