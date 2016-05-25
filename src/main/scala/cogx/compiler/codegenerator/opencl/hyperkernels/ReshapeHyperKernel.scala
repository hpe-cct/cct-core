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
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.parser.op.ReshapeOp

/** Reshapes a field without changing the underlying field data layout.
  *
  * This kernel had a major overhaul to correct the threading strategy and interpretation of the layout.
  *
  * The layout *had* been assumed to be based on the following ordering of indices:
  *
  * (slowest changing) row-index column-index  layer-index tensor-element-index (fastest changing)
  *
  * In truth, the Cog layout has the following ordering of indices:
  *
  * (slowest changing) tensor-element-index layer-index row-index column-index (fastest changing)
  *
  *
  *
  *
  * @author Matthew Pickett and Dick Carter
  *
  * @param in The input virtual field register to be reshaped.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ReshapeHyperKernel private (in: VirtualFieldRegister,
                                       operation: ReshapeOp,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode)
{

  val inType = in.fieldType

  val inRows = math.max(inType.rows,1)
  val inColumns = math.max(inType.columns, 1)
  val inLayers = math.max(inType.layers, 1)
  val inRowElems = math.max(inType.tensorRows, 1)
  val inColumnElems = math.max(inType.tensorColumns, 1)
  val inElems = inRowElems*inColumnElems
  val inRowFactor = inColumns
  val inLayerFactor = inRows*inColumns
  val inTensorElementFactor = inLayers*inRows*inColumns

  val outRows = math.max(resultType.rows, 1)
  val outColumns = math.max(resultType.columns, 1)
  val outLayers = math.max(resultType.layers, 1)
  val outRowElems = math.max(resultType.tensorRows, 1)
  val outColumnElems = math.max(resultType.tensorColumns, 1)
  val outElems = outRowElems*outColumnElems
  val outRowFactor = outColumns
  val outLayerFactor = outRows*outColumns
  val outTensorElementFactor = outLayers*outRows*outColumns


  val code = new StringBuffer
  val indexCode = resultType.fieldShape.dimensions match {
    case 0 => s"    int index = _tensorElement*$outTensorElementFactor;\n"
    case 1 => s"    int index = _tensorElement*$outTensorElementFactor + _column;\n"
    case 2 => s"    int index = _tensorElement*$outTensorElementFactor + _row*$outRowFactor + _column;\n"
    case 3 => s"    int index = _tensorElement*$outTensorElementFactor + _layer*$outLayerFactor + _row*$outRowFactor + _column;\n"
    case _ => throw new RuntimeException("incorrect number of field dimensions")
  }
  code append indexCode
  code append s"        int tensorElement = index/$inTensorElementFactor;\n"
  code append s"        int layer = (index - tensorElement*$inTensorElementFactor)/$inLayerFactor;\n"
  code append s"        int row = (index - tensorElement*$inTensorElementFactor - layer*$inLayerFactor)/$inRowFactor;\n"
  code append s"        int column = index - tensorElement*$inTensorElementFactor - layer*$inLayerFactor - row*$inRowFactor;\n"
  code append "        @out0 = readElementNonlocal(@in0);\n"
  addCode(code.toString)

  //      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ReshapeHyperKernel {

  /** Create a hyperkernel that reshapes a field.
    *
    * @param in The input virtual field register
    * @param op The unary opcode for this operation.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, op: ReshapeOp): HyperKernel = {
    // The ReshapeHyperkernel was revised as of libcog 4.3 to behave differently in certain cases.
    // One could think of this as a "bug fix" since the intent was to preserve the same data layout during
    // reshapings, and the old kernel did not always do this.

    // Rather than throw an exception (which would preclude unit testing the new functionality), we print
    // out a warning to alert the user that the model behavior has changed:

    if (op.checkLegacyReshape) {
      val inType = in.fieldType
      val inTypeTricky = inType.fieldShape.points > 1 && inType.tensorShape.points > 1
      val outTypeTricky = op.fieldShape.points > 1 && op.tensorShape.points > 1

      if (inType.fieldShape.dimensions >= 3 ||
        op.fieldShape.dimensions >= 3 ||
        (inTypeTricky || outTypeTricky) && (inType.tensorShape.points != op.tensorShape.points))
        println(s"**** Warning: behavior of reshape in this context has changed as of libcog 4.3.  Intype = $inType, op = $op")
    }

    val resultType = in.fieldType.resize(op.fieldShape).resizeTensor(op.tensorShape)
    val addressingMode =
      if (resultType.tensorShape.dimensions > 0)
        TensorElementAddressing
      else
        SmallTensorAddressing

    new ReshapeHyperKernel(in, op, resultType, addressingMode)
  }
}