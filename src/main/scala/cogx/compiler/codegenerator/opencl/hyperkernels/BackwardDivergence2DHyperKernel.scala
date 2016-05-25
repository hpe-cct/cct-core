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
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.BackwardDivergenceOp

/** Kernel that computes the backward divergence for a 2D tensor field of
  * rank 1 or 2.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class BackwardDivergence2DHyperKernel private (in: VirtualFieldRegister,
                                               operation: Opcode,
                                               resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, SmallTensorAddressing)
{
  val inType = in.fieldType
  require(inType.tensorShape == Shape(2), "input must be vector field")
  val clType = CLFloat2

  val code = new StringBuffer
  // First step is to read in a tile of the input image into local memory.
  // For forward gradient, we need a halo along the right and bottom edges
  // only.
  val localTile = LocalTensorMemory2D(inType.fieldShape, clType,
    topHalo = 1, rightHalo = 0, bottomHalo = 0, leftHalo = 1,
    borderProcessing = BorderZero)
  code.append(localTile)

  // Vector field tile is cached, so do the filtering. We need the data at
  // (row, col), (row - 1, col) and (row, col - 1) to compute the backward
  // differences.
  code append """
    %tensorType% element00 = localImage[_localRow + 1][_localColumn + 1];
    %tensorType% element01 = localImage[_localRow + 1][_localColumn];
    %tensorType% element10 = localImage[_localRow][_localColumn + 1];

    float divergence = 0.0f;
    if (_row < _rows - 1)
      divergence -= element01.y;
    if (_column < _columns - 1)
      divergence -= element10.x;
    if (_column < _columns - 1 && _row < _rows - 1) {
      divergence += element00.x;
      divergence += element00.y;
    }
    @out0 = divergence;
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
object BackwardDivergence2DHyperKernel {

  /** Create a kernel to compute the backward divergence of a 2D vector or rank-2 tensor field.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, operation: Opcode, resultType: FieldType): AbstractKernel =
  {
    val inType = in.fieldType

    require(inType.elementType == Float32)

    require(resultType.tensorShape.dimensions == inType.tensorShape.dimensions - 1)
    require(inType.dimensions == 2)
    require(isSmallTensorField(inType))
    require(operation == BackwardDivergenceOp)
    new BackwardDivergence2DHyperKernel(in, operation, resultType)
  }
}


