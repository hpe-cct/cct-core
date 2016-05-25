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
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.OrientedNonMaximumSuppressionOp

/** Non-maximum suppression for scalar fields and vector fields, guided by an
  * orientation field.
  *
  * **** NOT WORKING YET!!!!! ***
  *
  * The orientation field defines the local orientation of the signal. Suppressing
  * a pixel occurs only when either of the interpolated pixels on either side of
  * the pixel, perpendicular to the direction of flow (such as along an edge)
  * are greater than the pixel. It that occurs, the pixel is driven to zero.
  *
  * Vector fields are treated as though they were an array of scalar fields,
  * so non-maximum suppression is executed independently on each.
  *
  * @author Greg Snider
  *
  * @param in The virtual field register of the input field (scalar or vector)
  *        that will have its locally non-maximum values suppressed to zero in
  *        the output field.
  * @param orientation The orientation field, used to guide the non max
  *        suppression. The is a scalar field which specifieds the orientation,
  *        in interval -Pi/2 to Pi /2, at each point in the `in` input.
  * @param op The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @author Greg Snider
  */
private[cogx]
class OrientedNonMaxSuppression2DHyperKernel  private(
                                                             in: VirtualFieldRegister,
                                                             orientation: VirtualFieldRegister,
                                                             op: Opcode,
                                                             resultType: FieldType,
                                                             addressMode: AddressingMode)
        extends HyperKernel(op, Array(in, orientation), in.fieldType, addressMode)
{
  // Size of neighborhood in each dimension.
  val neighborhoodSize = 3
  // Type of image data value read, depending on address mode.
  val dataType = addressing.clType(resultType)
  val code = new StringBuffer

  // Read in a tile of the input image into local memory.
  // There is one work-item (thread) per output pixel. This tile must be
  // padded all around with a halo of size 2 to do the local, oriented
  // comparisons.
  val Halo = 2
  code append LocalTensorMemory2D(inShape = in.fieldType.fieldShape,
    inType = dataType, topHalo = Halo, rightHalo = Halo,
    bottomHalo = Halo, leftHalo = Halo,
    borderProcessing = BorderClamp)

  // Now read in the local orientation, 0 to Pi.
//  code append "  float orientation = (read(@in1) + (M_PI / 2.0f)) / 2.0f;\n"
  code append "  float orientation = -read(@in1) + M_PI;\n"
  if (addressMode == TensorElementAddressing)
    code append "  tensorElement = _tensorElement;\n"

  // Core algorithm modeled on Peter Kovesi's computer vision library
  // function nonmaxsup.m

  // Read the "pixels" to the left and right of the pixel owned by this thread,
  // using bilinear interpolation.
  code append
          """
            | // Radius from center pixel to neighbors on either side for
            | // detecting local max. Must be in the range 1.2 to 1.5
            | const float radius = 1.3f;
            |
            | // Offsets to neighbors on either side
            | float xOffset = radius * cos(orientation);
            | float yOffset = radius * sin(orientation);
            |
            | // Fractional offset of *Offset relative to integral location
            | float horizFraction = xOffset - floor(xOffset);
            | float vertFraction = yOffset - floor(yOffset);
            |
            | // SIDE 1 ------------------------------------
            | // float (x, y) location on one side of current point.
            | float x = _localColumn + xOffset;
            | float y = _localRow - yOffset;
            |
            | // Get integer locations that surround (x, y):
            | int fx = (int) floor(x);
            | int cx = (int) ceil(x);
            | int fy = (int) floor(y);
            | int cy = (int) ceil(y);
            |
            | float topLeft = localImage[fy + %halo%][fx + %halo%];
            | float topRight = localImage[fy + %halo%][cx + %halo%];
            | float bottomLeft = localImage[cy + %halo%][fx + %halo%];
            | float bottomRight = localImage[cy + %halo%][cx + %halo%];
            |
            | float upperAvg = topLeft + horizFraction * (topRight - topLeft);
            | float lowerAvg = bottomLeft + horizFraction * (bottomRight - bottomLeft);
            |
            | // The value of the pixel on one side:
            | float v1 = upperAvg + vertFraction * (lowerAvg - upperAvg);
            |
            | // SIDE 2 ------------------------------------
            | // float (x, y) location on one side of current point.
            | x = _localColumn - xOffset;
            | y = _localRow + yOffset;
            |
            | // Get integer locations that surround (x, y):
            | fx = (int) floor(x);
            | cx = (int) ceil(x);
            | fy = (int) floor(y);
            | cy = (int) ceil(y);
            |
            | topLeft = localImage[fy + %halo%][fx + %halo%];
            | topRight = localImage[fy + %halo%][cx + %halo%];
            | bottomLeft = localImage[cy + %halo%][fx + %halo%];
            | bottomRight = localImage[cy + %halo%][cx + %halo%];
            |
            | upperAvg = topLeft + horizFraction * (topRight - topLeft);
            | lowerAvg = bottomLeft + horizFraction * (bottomRight - bottomLeft);
            |
            | // The value of the pixel on the other side:
            | float v2 = upperAvg + vertFraction * (lowerAvg - upperAvg);
            |
            | // We now have values on both sides: v1 and v2.
            | // If the center pixel is greater than both, we have a local max.
            | float center = localImage[_localRow + %halo%][_localColumn + %halo%];
            | int isLocalMax = (center > v1) && (center > v2);
            |
            | // Write out either 0 or local max.
            | float result = center * isLocalMax;
            | @out0 = result;
          """.stripMargin

  val codeString = code.toString.
          replaceAll("%dataType%", dataType.toString).
          replaceAll("%halo%", Halo.toString)
  addCode(codeString)

  val Debug = false
  if (Debug)
    debugCompile()
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object OrientedNonMaxSuppression2DHyperKernel {

  /** Create a hyperkernel for non-maximum suppression.
    *
    * @param in The virtual field register of the input field (scalar or vector)
    *        that will have its locally non-maximum values suppressed to zero in
    *        the output field.
    * @param operation Opcode for hyperkernel.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, orientation: VirtualFieldRegister,
            operation: Opcode, resultType: FieldType): HyperKernel =
  {
    val inType = in.fieldType
    require(inType.fieldShape.dimensions == 2)
    require(inType.tensorShape.dimensions == 0) // scalar field
    require(inType == resultType)

    require(operation == OrientedNonMaximumSuppressionOp)
    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new OrientedNonMaxSuppression2DHyperKernel(in, orientation,
      operation, resultType, addressing)
  }
}

