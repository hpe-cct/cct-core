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

import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ForwardGradientOp

/** Kernel that computes the forward gradient for a 2D scalar or vector field
  *
  * This uses SmallTensorAddressing so that it can be merged more easily with
  * other HyperKernels.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class ForwardGradient2DHyperKernel private (in: VirtualFieldRegister,
                                            operation: Opcode,
                                            resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, SmallTensorAddressing)
{
  val inType = in.fieldType
  require(inType.tensorShape.dimensions <= 1,
    "must be scalar or vector field")
  val clType: CLType = inType.tensorShape.points match {
    case 1 => CLFloat
    case 2 => CLFloat2
    case x => throw new RuntimeException("Bad input field type for 2D gradient.")
  }


  val code = new StringBuffer
  // First step is to read in a tile of the input image into local memory.
  // For forward gradient, we need a halo along the right and bottom edges
  // only.
  val localTile = LocalTensorMemory2D(inType.fieldShape, clType,
    topHalo = 0, rightHalo = 1, bottomHalo = 1, leftHalo = 0,
    borderProcessing = BorderZero)
  code.append(localTile)
  code append """
    // Image tile is cached, so do the filtering. We need the data at
    // (row, col), (row + 1, col) and (row, col + 1) to compute the forward
    // differences.
    %tensorType% rowDiff = %tensorZero%;
    %tensorType% colDiff = %tensorZero%;

    // On right edge and bottom, gradient is left to be 0.
    if (_row < (_rows - 1) && _column < (_columns - 1)) {

      %tensorType% element00 = localImage[_localRow][_localColumn];
      %tensorType% element01 = localImage[_localRow][_localColumn + 1];
      %tensorType% element10 = localImage[_localRow + 1][_localColumn];

      // Compute vertical derivative (rows, first coordinate)
      rowDiff = element10 - element00;

      // Compute horiz derivative (columns, first coordinate) and write it out.
      colDiff = element01 - element00;

      //if (_row == _rows - 1 || _column == _columns - 1) {
      //  colDiff = %tensorZero%;
      //  rowDiff = %tensorZero%;
      //}
    }

    // Fix the following for vector fields XXX
    @out0 = (float2)(rowDiff, colDiff);
  """

  val codeString = code.toString.
          replaceAll("%tensorType%", clType.name).
          replaceAll("%tensorZero%", clType.zero)

  addCode(codeString)

  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ForwardGradient2DHyperKernel {

  /** Compute the forward gradient for a 2D scalar or vector field.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): AbstractKernel =
  {
    val inType = in.fieldType

    require(inType.tensorShape.dimensions == 0)

    require(resultType.tensorShape.dimensions == inType.tensorShape.dimensions + 1)
    require(inType.dimensions == 2)
    require(isSmallTensorField(inType))
    require(inType.tensorShape.dimensions <= 1)
    require(operation == ForwardGradientOp)
    new ForwardGradient2DHyperKernel(in, operation, resultType)
  }
}

