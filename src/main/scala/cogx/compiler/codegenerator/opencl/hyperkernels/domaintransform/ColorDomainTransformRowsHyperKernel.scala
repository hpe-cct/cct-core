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

import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.DomainTransformRowsOp
import cogx.platform.opencl.WorkGroupParameters
import cogx.cogmath.algebra.real.Logarithm
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Implements the domain transform on the rows of a color field. See the paper
  * "Domain transform for edge-aware image and video processing," Gastal and
  * Oliveira, 2011, for a description.
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode Addressing mode for this kernel (must be small tensor).
  *
  * @author Greg Snider
  */
private[cogx]
class ColorDomainTransformRowsHyperKernel private (in: VirtualFieldRegister,
                                              operation: DomainTransformRowsOp,
                                              resultType: FieldType,
                                              addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode, ClampToEdge)
        with Logarithm
{
  require(in.fieldType.fieldShape.dimensions == 2, "only works on 2D fields")
  require(addressMode == SmallTensorAddressing, "only works for small tensor fields")

  /** Max threads per workgroup, which is hardware dependent. We need a better
    * way of finding this out for a given target.
    */
  val MaxThreadsPerWorkGroup = 1024
  private val rows = in.fieldType.rows
  private val columns = in.fieldType.columns
  require(rows <= MaxThreadsPerWorkGroup, "image too big for this operator")
  require(columns <= MaxThreadsPerWorkGroup, "image too big for this operator")

  // Work group computation.
  // Our strategy is for each work group to work on a single row of the
  // image, with one thread for each pixel in the row.
  override lazy val workGroup =
    new WorkGroupParameters(2) {
      localLayers  = 1
      localRows    = 1
      localColumns = columns
      globalLayers  = 1
      globalRows    = rows
      globalColumns = columns
    }

  // Parameters for kernel.
  val scale = operation.spaceSigma / operation.rangeSigma
  val doublingIterations = log2(roundUpPowerOf2(columns)).toInt

  // The kernel code
  val code =
    """
      | // Read in a row of the image
      | __local float4 imageRow[%rowLength%];
      | imageRow[_column] = read(@in0);
      | barrier(CLK_LOCAL_MEM_FENCE);
      |
      | // Compute the derivative of the domain transform, eqn (12).
      | __local float transformA[%rowLength%];
      | float4 g = fabs(imageRow[_column].xyzw - imageRow[max(_column - 1, 0)].xyzw);
      | float ctPrime = (g.x + g.y + g.z) * %scale% + 1.0f;
      | transformA[_column] = ctPrime;
      | barrier(CLK_LOCAL_MEM_FENCE);
      |
      | // Integrate using recursive doubling to get the domain transform,
      | // eqn (11). We alternate between the transformA buffer and a second
      | // buffer, transformB, that we create here. For a description of
      | // recursive doubling, see "Fast summed-area table generation and its
      | // applications," Hensley et al, 2005.
      | __local float transformB[%rowLength%];
      | __local float* buffer1 = transformA;
      | __local float* buffer2 = transformB;
      | __local float* temp;
      |
      | int stride = 1;
      | for (int i = 0; i < %doublingIterations%; i++) {
      |   float center = buffer1[_column];
      |   float left;
      |   int leftIndex = _column - stride;
      |   if (leftIndex >= 0)
      |     left = buffer1[leftIndex];
      |   else
      |     left = 0.0f;
      |   buffer2[_column] = center + left;
      |   barrier(CLK_LOCAL_MEM_FENCE);
      |
      |   // Double the stride each iteration.
      |   stride = stride << 1;
      |
      |   // Swap buffers
      |   temp = buffer1;
      |   buffer1 = buffer2;
      |   buffer2 = temp;
      | }
      |
      | // Write out the domain transform, ct.
      | @out0 = buffer1[_column];
    """
  val codeString = code.toString.stripMargin.
          replaceAll("%rowLength%", columns.toString).
          replaceAll("%doublingIterations%", doublingIterations.toString).
          replaceAll("%scale%", scale.toString)
  addCode(codeString)
  //debugCompile
}

/** Factory for DomainTransformRowsHyperKernel. */
private[cogx]
object ColorDomainTransformRowsHyperKernel {

  /** Create a hyperkernel that implements the domain transform for the rows
    * of an input color field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister],
            operation: DomainTransformRowsOp,
            resultType: FieldType): HyperKernel =
  {
    require(in.length == 1)
    require(in(0).fieldType.dimensions == 2)
    require(isColorField(in(0).fieldType))
    val expectedResultType =
      new FieldType(in(0).fieldType.fieldShape, Shape(), Float32)
    require(resultType == expectedResultType)
    val addressing = SmallTensorAddressing
    new ColorDomainTransformRowsHyperKernel(in(0), operation, resultType, addressing)
  }
}