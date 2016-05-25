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
import cogx.cogmath.algebra.real.Matrix
import cogx.compiler.codegenerator.opencl.cpukernels.FixedVectorKernel
import cogx.compiler.parser.op.{LocalMin2DOp, LocalMax2DOp}

/** Find local maxima or minima in a 2D scalar field.
  *
  * For each point in a 2D scalar field, search a small neighborhood of that
  * point and return the largest / smallest scalar found.
  *
  * The neighborhood is defined by a kernel which is centered on the point. A
  * non-zero value in the kernel means the corresponding point in the field
  * is part of the neighborhoo, while a zero implies the point should be
  * ignored.
  *
  * For example, the kernel
  * {{{
  *     1 1 0
  *     1 1 0
  *     0 0 0
  * }}}
  * specifies the 2 x 2 neighborhood for the maximum value search.
  *
  * @author Greg Snider
  *
  * @param in The input virtual field registers driving this kernel.
  * @param neighbors The local neighborhood to be searched.
  * @param operation The binary opcode for this operation.
  * @param borderPolicy Border handling policy.
  * @param findMaxima True if searching for maxima, false if searching for
  *        minima.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class LocalExtrema2DHyperKernel private (in: VirtualFieldRegister,
                                 neighbors: Matrix,
                                 operation: Opcode,
                                 borderPolicy: BorderPolicy,
                                 findMaxima: Boolean,
                                 resultType: FieldType)
        extends HyperKernel(operation,
          Array(in, FixedVectorKernel(neighbors.toVector).outputs(0)),
          resultType, SmallTensorAddressing)
{
  val inType = in.fieldType
  require(neighbors.rows == neighbors.columns && (neighbors.rows % 2 == 1),
    "neighborhood must be square and have odd size")
  require(inType.tensorShape.dimensions == 0,
    "localMax only defined for scalar fields")
  val filterSize = neighbors.rows
  val halo = filterSize / 2

  // Since the output image is the same size as the input image, we have three
  // different ways we can handle the border pixels: border fill, zero
  // fill, or cyclic. The opcode tells us which to do. Border fill/Cyclic is
  // more expensive.
  private val fillMode = borderPolicy

  // Field input 1 is the neighborhood. By marking this as constant, the GPU can
  // access it more efficiently in constant memory rather than global memory.
  makeInputConstant(1)

  val code = new StringBuffer
  // First step is to read in a tile of the input image into local memory.
  code.append(LocalTensorMemory2D(inType.fieldShape, CLFloat,
    halo, halo, halo, halo, fillMode))

  code append """
    // Image tile is cached, so do the search. Each work-item will search
    // around its start location (starting from the filter radius left and up).
    """
  if (findMaxima)
    code.append("float extremeValue = -MAXFLOAT;\n")
  else
    code.append("float extremeValue = MAXFLOAT;\n")
  code append """
    int isNeighbor = 0;
    float neighborValue = 0.0f;

    column = 0;
    for (int r = _localRow; r < _localRow + %filterSize%; r++){
        int c = _localColumn;
              """

  // Unroll the inner loop (unroll pragma is apparently unreliable).
  for (column <- 0 until filterSize) {
    code append """
        isNeighbor = (readNonlocal(@in1) != 0.0f);
        neighborValue = localImage[r][c++];
    """

    if (findMaxima)
      code append """
        if (isNeighbor && (neighborValue > extremeValue))
          extremeValue = neighborValue;
                  """
    else
      code append """
        if (isNeighbor && (neighborValue < extremeValue))
          extremeValue = neighborValue;
                  """
    code append """
        column++;
    """
  }
  code append """
    }

    @out0 = extremeValue;
              """

  val codeString = code.toString.replaceAll("%filterSize%", filterSize.toString)

  addCode(codeString)
  //  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object LocalExtrema2DHyperKernel {

  /** Create a hyperkernel for finding local maxima.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param op The binary opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: VirtualFieldRegister, op: Opcode, resultType: FieldType): HyperKernel = {
    var findMaxima = false
    var filter: Matrix = null
    var borderPolicy: BorderPolicy = null
    op match {
      case o: LocalMax2DOp =>
        findMaxima = true
        require(o.filterOrientation == CrossCorrelationOrientation)
        filter = o.kernel
        borderPolicy = o.borderPolicy
      case o: LocalMin2DOp =>
        findMaxima = false
        require(o.filterOrientation == CrossCorrelationOrientation)
        filter = o.kernel
        borderPolicy = o.borderPolicy
      case _ =>
        throw new RuntimeException("illegal opcode")
    }
    require(filter.rows == filter.columns)
    require(filter.rows % 2 == 1)
    val inType = in.fieldType
    require(inType.dimensions == 2)
    require(inType == resultType)
    new LocalExtrema2DHyperKernel(in, filter, op, borderPolicy, findMaxima, resultType)
  }
}
