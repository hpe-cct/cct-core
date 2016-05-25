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
import cogx.compiler.parser.op.CrossDotOp

/** A CrossDotHyperKernel takes a "weights" field (generally a matrix or vector field) and
  * a scalar field and performs a cross-correlation. See Cog programming manual for a
  * description of this operation.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The virtual field registers driving this kernel: Array(weightsField, scalarField)
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  */
private[cogx]
class CrossDotHyperKernel private (in: Array[VirtualFieldRegister],
                          operation: Opcode,
                          resultType: FieldType)
        extends HyperKernel(operation, in, resultType, SmallTensorAddressing)
{

  val scalarFieldType = in(1).fieldType
  val scalarFieldDim = scalarFieldType.dimensions

  val code = new StringBuilder
  if (scalarFieldDim > 1)
    code.append("    const int scalarFieldRows = " + scalarFieldType.rows + ";\n")
  code.append("        const int scalarFieldColumns = " + scalarFieldType.columns + ";\n")

  // Dot product at this point of weights field and entire scalar field.
  code.append("        float dotProduct = 0.0f;\n")
  code.append("        tensorElement = 0;\n")

  // At each field point, loop over weights elements and combine with scalar.
  if (scalarFieldDim > 1)
    code.append("    for (row = 0; row < scalarFieldRows; row++) {\n ")
  code.append("        for (column = 0; column < scalarFieldColumns; column++) {\n ")
  code.append("            float scalar = readNonlocal(@in1);\n")
  code.append("            float weightsElement = readElement(@in0);\n")
  code.append("            dotProduct += weightsElement * scalar;\n")
  code.append("            tensorElement += 1;\n")
  code.append("        }\n")
  if (scalarFieldDim > 1)
    code.append("    }\n")
  code.append("    @out0 = dotProduct;\n")
  addCode(code.toString())
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object CrossDotHyperKernel extends HyperHelper {

  /**
   * Create a hyperkernel that performs the CrossDot function.
   *
   * @param in The virtual field registers driving this kernel: Array(weightsField, scalarField)
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return The synthesized hyperkernel.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType): HyperKernel = {
    val weightsFieldType = in(0).fieldType
    val scalarFieldType = in(1).fieldType
    val expectedResultType = new FieldType(weightsFieldType.fieldShape, Shape(), Float32)
    require(expectedResultType == resultType)
    require(weightsFieldType.tensorOrder <= 2)
    require(scalarFieldType.tensorOrder == 0)
    require(weightsFieldType.tensorShape == scalarFieldType.fieldShape)
    require(operation == CrossDotOp)

    new CrossDotHyperKernel(in, operation, resultType)
  }
}


