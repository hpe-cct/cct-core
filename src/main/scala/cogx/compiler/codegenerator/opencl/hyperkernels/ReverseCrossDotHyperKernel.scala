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

import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, HyperKernel}
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op.ReverseCrossDotOp

/** ReverseCrossDotKernel takes a "weights" field (generally a matrix or vector field) and
  * a scalar field and performs a "cross-decorrelation." This is conceptually a "crossDot"
  * operation run backwards. See Cog programming manual for a description.
  *
  * Our current memory layout makes efficient implementation of this function difficult.
  * In this simple coding, the loads are not coalesced and this will be very slow.
  *
  * Reimplementation proposal: rather than have one thread per output element,
  * better might be to have one work group per output element.  Say the workgroup
  * was sized 16x16: the kernel would then step this template over the weight
  * matrix, each thread summing z*x.  Then, the 256 threads of the workgroup would
  * perform a reduction much like the ScalarReduceHyperKernel.  Finally, thread(0)
  * of each local workgroup would write the reduced value as one scalar element
  * of the output field.  This approach has coalesced memory reads of the large
  * weights field, although the output writes would be single floats.  -RJC
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class ReverseCrossDotHyperKernel private (in: Array[VirtualFieldRegister],
                                            operation: Opcode,
                                            resultType: FieldType)
        extends HyperKernel(operation, in, resultType, SmallTensorAddressing) {

  val code = new StringBuffer

  val weightsFieldType = in(0).fieldType
  val scalarFieldType = in(1).fieldType
  val inDim = scalarFieldType.dimensions

  // When all threads read the same input value (as with the scalar field here),
  // having the field read into the separate constant memory cache is a win.
  makeInputConstant(1)

  code append "    tensorElement = "
  resultType.dimensions match {
    case 2 =>   code append "_row * " + resultType.columns + " + _column;\n"
    case 1 =>   code append "_column;\n"
    case 0 =>   code append "0;\n"
    case x => throw new RuntimeException("Unsupported result field dimension " + x)
  }
  code append "    float result = 0.0f;\n"
  if (inDim > 2)
    code append "    for (layer = 0; layer < " + scalarFieldType.layers + "; layer++)\n"
  if (inDim > 1)
    code append "        for (row = 0; row < " + scalarFieldType.rows + "; row++)\n"
  code append "            for (column = 0; column < " + scalarFieldType.columns + "; column++)\n"
  code append "                result += readElementNonlocal(@in0) * readNonlocal(@in1);\n"
  code append "    @out0 = result;\n"
  addCode(code.toString)
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object ReverseCrossDotHyperKernel extends HyperHelper {

  /** Create a hyperkernel that performs a "cross-decorrelation."
    *
    * @param in The input fields driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    val weightsFieldType = in(0).fieldType
    val scalarFieldType = in(1).fieldType

    require(weightsFieldType.tensorOrder <= 2)
    require(scalarFieldType.tensorOrder == 0)
    require(weightsFieldType.fieldShape == scalarFieldType.fieldShape)

    val expectedResultType = new FieldType(weightsFieldType.tensorShape, Shape(), Float32)
    require(expectedResultType == resultType)
    require(operation == ReverseCrossDotOp)

    new ReverseCrossDotHyperKernel(in, operation, resultType)
  }
}
