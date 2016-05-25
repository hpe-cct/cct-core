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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, TensorElementAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ExpandBorderOp

/** Kernel that expands a 2D field to a larger 2D field by expanding
  * the border outwards, wrapping around in a torus. Element (0, 0) of
  * the input field is mapped to element (0, 0) in the larger output field.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class ExpandBorderHyperKernel private (in: VirtualFieldRegister,
                                      operation: ExpandBorderOp,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode)
{
  val inType = in.fieldType
  val dim = inType.dimensions
  val code = new StringBuffer

  if (addressMode == TensorElementAddressing)
    code append "    tensorElement = _tensorElement;\n"

  operation.borderPolicy match {
    case BorderClamp =>
      // Compute col to read for outColumn
      val inColumns = inType.columns
      val outColumns = resultType.columns
      val colApron = (outColumns - inColumns) / 2
      code append "    column = min(_column, " + (inColumns - 1) + ");\n"
      code append "    if (_column >= " + (inColumns + colApron) + ")\n"
      code append "        column = 0;\n"

      if (dim > 1) {
        // Compute row to read for outRow
        val inRows = inType.rows
        val outRows = resultType.rows
        val rowApron = (outRows - inRows) / 2
        code append "    row = min(_row, " + (inRows - 1) + ");\n"
        code append "    if (_row >= " + (inRows + rowApron) + ")\n"
        code append "        row = 0;\n"
      }

      if (dim > 2) {
        // Compute layer to read for outLayer
        val inLayers = inType.layers
        val outLayers = resultType.layers
        val layerApron = (outLayers - inLayers) / 2
        code append "    layer = min(_layer, " + (inLayers - 1) + ");\n"
        code append "    if (_layer >= " + (inLayers + layerApron) + ")\n"
        code append "        layer = 0;\n"
      }
      code append "    @out0 = readNonlocal(@in0);\n"

    case BorderZero =>
      val readZero = addressMode.clType(resultType).zero
      code append "    if ( "
      code append "(_column >= " + inType.columns + ")"
      if (dim > 1)
        code append " || (_row >= " + inType.rows + ")"
      if (dim > 2)
        code append " || (_layer >= " + inType.layers + ")"
      code append " ) {\n"
      code append "    @out0 = " + readZero + ";\n"
      code append " } else {\n"
      // We don't want to merge into this expanded input without further
      // enhancements to the merger, so we throw in a gratuitous non-local read
      // to stymie the merger.
      code append setLayerRowColumn(resultType, "_layer", "_row", "_column")
      code append "    @out0 = readNonlocal(@in0);\n"
      code append " }\n"

    case BorderCyclic =>
      def setIndex(name: String, inSize: Int, outSize: Int) {
        val apron = (outSize - inSize)/2
        val snippet =
          """
            | if (_%name% >= %inSize% + %apron%) {
            |   %name% = (_%name% - %outSize%) % %inSize%;
            |   if (%name% < 0) %name% += %inSize%;
            | }
            | else {
            |   %name% = _%name% % %inSize%;
            | }
          """.stripMargin.
          replaceAll("%name%", name).
          replaceAll("%inSize%", inSize.toString).
          replaceAll("%outSize%", outSize.toString).
          replaceAll("%apron%", apron.toString)
        code append snippet
      }
      setIndex("column", inType.columns, resultType.columns)
      if (dim > 1)
        setIndex("row", inType.rows, resultType.rows)
      if (dim > 2)
        setIndex("layer", inType.layers, resultType.layers)
      code append "    @out0 = readNonlocal(@in0);\n"

    case x =>
      throw new RuntimeException("Unsupported border policy: " + x)
  }
  addCode(code.toString)
//        debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ExpandBorderHyperKernel {

  /** Create a HyperKernel that expands a 2D field to a larger 2D field by
    * expanding the border outwards, wrapping around in a torus.
    *
    * @param in The input virtual field register driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: VirtualFieldRegister, operation: ExpandBorderOp, resultType: FieldType): HyperKernel = {

    val inType = in.fieldType
    require(inType.dimensions >= 1)
    val expectedResultFieldShape = operation.resultShape
    require(expectedResultFieldShape.dimensions == inType.dimensions)
    val expectedResultType = new FieldType(expectedResultFieldShape, inType.tensorShape, inType.elementType)
    require(resultType == expectedResultType)
    for (i <- 0 until resultType.dimensions)
      require(resultType.fieldShape(i) >= inType.fieldShape(i),"Expanding to a smaller fieldShape!")
    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing
    new ExpandBorderHyperKernel(in, operation, resultType, addressing)
  }
}

