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
import cogx.compiler.parser.op.SubfieldsOp

/** Create a hyper kernel that can extract all subfields of a scalar field and
  * stack them into a 0-D vector field.
  *
  * Takes a 2D scalar field as input and produces a 0D vector field as output,
  * where each layer of the vector field is a K x K subfield of the input.
  * The layers of the vector field cover the the input field densely
  * without extending beyond the boundaries.
  *
  * Example:
  *
  * For an M x N input scalar field and K x K subfields, the output vector
  * field will have a vector of length (M - K + 1) x (N - K + 1).
  *
  * @author Greg Snider
  *
  * @param in Virtual field register of the input scalar field from which to extract all subfields.
  * @param resultType Type of the output matrix field.
  * @param opcode Opcode for this operator.
  */
private[cogx]
class Subfields2DHyperKernel private (in: VirtualFieldRegister,
                                      opcode: SubfieldsOp,
                                      resultType: FieldType)
        extends HyperKernel(opcode, Array(in), resultType, BigTensorAddressing)
{
  require(in.fieldType.tensorShape.dimensions == 0, "scalar field required")
  require(in.fieldType.fieldShape.dimensions == 2, "2D field required")

  // The input field must fit completely in local memory. Since that memory
  // is at least 32 kB by the OpenCL 1.1 spec, this means we can hold up to
  // 8K floats. We scale this down and require that the input be no larger
  // than 64 x 64, which is 4K floats. (Which, in theory, allows another kernel
  // to run in parallel and still have some local memory).
  val MaxInputSize = 64
  val MaxDiameter = 15
  val inRows = in.fieldType.fieldShape(0)
  val inColumns = in.fieldType.fieldShape(1)
  val subfieldSize = opcode.diameter
  val vectorLength = (inRows - subfieldSize + 1) * (inColumns - subfieldSize + 1)
  require(inRows <= MaxInputSize, "input field too large for subfields op")
  require(inColumns <= MaxInputSize, "input field too large for subfields op")
  require(resultType.fieldShape(0) == subfieldSize, "illegal output type")
  require(resultType.fieldShape(1) == subfieldSize, "illegal output type")
  require(resultType.tensorShape.dimensions == 1, "need vector field output")
  require(resultType.tensorShape(0) == vectorLength, "wrong length vector")
  require(subfieldSize <= inRows, "subfields diameter too big")
  require(subfieldSize <= inColumns, "subfields diameter too big")
  require(subfieldSize <= MaxDiameter, "subfield diameter too big")

  val code = new StringBuilder
  // Read in the entire input field to local memory.
  code append
          """
            |  __local float localImage[%inRows%][%inColumns%];
            |
            |  // Cache the small input field to local memory.
            |  for (int r = _localRow; r < %inRows%; r += _localRows) {
            |    for (int c = _localColumn; c < %inColumns%; c += _localColumns) {
            |      int layer = 0;
            |      int row = r;
            |      int column = c;
            |      int tensorElement = 0;
            |      localImage[r][c] = readElementNonlocal(@in0); // (row, column)
            |    }
            |  }
            |  barrier(CLK_LOCAL_MEM_FENCE);
            |  if (_row >= _rows || _column >= _columns)
            |    return;
          """.stripMargin


//  code append LocalTensorMemory2D(in.fieldType.fieldShape, CLFloat,
//    0, 0, 0, 0, BorderZero)

  // Compute the number of subfields.
  code append
          """
            |  int subRows = %inRows% - %subfieldSize% + 1;
            |  int subCols = %inColumns% - %subfieldSize% + 1;
            |  int subfields = subRows * subCols;
            |  int filterSize = %subfieldSize%;
          """.stripMargin

  // We now loop and write one subfield as one layer in the output vector
  // field for each iteration of the inner loop.
  code append
          """
            |  int tensorElement = 0;
            |  for (int subRow = 0; subRow < subRows; subRow++) {
            |    for (int subCol = 0; subCol < subCols; subCol++) {
            |      float pixel = localImage[_row + subRow][_column + subCol];
            |      @outElement0 = pixel; // (_row, _column, tensorElement)
            |      tensorElement += 1;
            |    }
            |  }
          """.stripMargin

  addCode(code.toString().
          replaceAll("%subfieldSize%", subfieldSize.toString).
          replaceAll("%inRows%", inRows.toString).
          replaceAll("%inColumns%", inColumns.toString)
  )

  val Debug = false
  if (Debug) {
    debugCompile()
  }
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Subfields2DHyperKernel {
  /** Create a hyper kernel that can extract all subfields of a scalar field and
    * stack them into a 2-D vector field.
    *
    * @param in Virtual field register of the input scalar field from which to extract all subfields.
    * @param opcode Opcode for this operator.
    */
  def apply (in: VirtualFieldRegister, opcode: SubfieldsOp, resultType: FieldType): HyperKernel = {
    new Subfields2DHyperKernel(in, opcode, resultType)
  }
}
