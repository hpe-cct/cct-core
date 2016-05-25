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
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.fragments._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.CentralGradientOp

/** Computes the central-difference gradient for a 2D scalar field, producing
  * a 2D vector field of the same size. This uses clamped border constraints
  * to minimize gradient distortion at the borders.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class CentralGradient2DHyperKernel private (in: VirtualFieldRegister,
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
  val localTile = LocalTensorMemory2D(inType.fieldShape, clType,
    topHalo = 1, rightHalo = 1, bottomHalo = 1, leftHalo = 1,
    borderProcessing = BorderClamp)
  code.append(localTile)
  code append """
    // Image tile is cached, so do the filtering. We need the data at
    // (row - 1, col), (row, col - 1), (row, col + 1) and (row + 1, col + 1) to compute the forward
    // differences.  The indices below are shifted: upper-left == element00.

    %tensorType% element01 = localImage[_localRow][_localColumn + 1];
    %tensorType% element10 = localImage[_localRow + 1][_localColumn];
    %tensorType% element12 = localImage[_localRow + 1][_localColumn + 2];
    %tensorType% element21 = localImage[_localRow + 2][_localColumn + 1];

    // Compute vertical derivative
    %tensorType% rowDiff = (element21 - element01)*0.5f;

    // Compute horiz derivative
    %tensorType% colDiff = (element12 - element10)*0.5f;

    // Fix the following for vector fields XXX
    @out0 = (float2)(rowDiff, colDiff);
  """

  val codeString = code.toString.
          replaceAll("%tensorType%", clType.name)

  addCode(codeString)

  //debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object CentralGradient2DHyperKernel {

  /** Compute the forward gradient for a 2D scalar or vector field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): AbstractKernel =
  {
    val inType = in.fieldType

    require(inType.elementType == Float32)

    require(resultType.tensorShape.dimensions == inType.tensorShape.dimensions + 1)
    require(inType.dimensions == 2)
    require(isSmallTensorField(inType))
    require(inType.tensorShape.dimensions <= 1)
    require(operation == CentralGradientOp)
    new CentralGradient2DHyperKernel(in, operation, resultType)
  }
}

