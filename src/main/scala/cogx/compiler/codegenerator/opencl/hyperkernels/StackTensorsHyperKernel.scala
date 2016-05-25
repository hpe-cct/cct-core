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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.parser.op.TensorStackOp

/** Stacks the tensors of two or more real fields (each having the exact same
  * field and tensor shape) into a result field of identical field shape as
  * the inputs, but with tensors of higher order.
  *
  * @author Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class StackTensorsHyperKernel private (in: Array[VirtualFieldRegister],
                                      operation: TensorStackOp,
                                      resultType: FieldType,
                                      addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode) {

  // Read the index of the desired field.
  val code = new StringBuffer


  addressMode match {
    case SmallTensorAddressing =>
      val outType = addressMode.clType(resultType).name

      code append "   @out0 = (" + outType + ") ("
      code append (0 until in.length).map(i => "read(@in" + i + ")").mkString(", ")
      code append ");\n"
    case TensorElementAddressing =>
      val inType = in(0).fieldType
      val inTypeName = addressMode.clType(inType).name
      val inTypeZero = addressMode.clType(inType).zero
      code append setLayerRowColumn(inType, "_layer", "_row", "_column")
      val inTensorPoints = in(0).fieldType.tensorShape.points
      code append "   int select = _tensorElement / " + inTensorPoints + ";\n"
      code append "   tensorElement = _tensorElement - select * " + inTensorPoints + ";\n"
      // Generate a switch statement to perform the read on the desired field.
      code append "   " + inTypeName + " result = " + inTypeZero +";\n"
      code append "   switch (select) {\n"
      for (i <- 0 until in.length) {
        code append "     case " + i + ":\n"
        code append "         result = readNonlocal(@in" + i + ");\n"
        code append "         break;\n"
      }
      code append "   }\n"
      code append "   @out0 = result;\n"
    case _ => throw new RuntimeException("Addressing mode not supported by this kernel: " + addressMode)
  }
  addCode(code.toString)
//        debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object StackTensorsHyperKernel extends HyperHelper {

  /** Create a HyperKernel that stacks the tensors of two or more real fields.
    *
    * @param in The input virtual field registers driving this kernel.
    * @param operation The opcode for this operation.
    * @param resultType The FieldType of the result of this kernel.
    * @return Synthesized hyperkernel for the operation.
    */
  def apply(in: Array[VirtualFieldRegister], operation: TensorStackOp, resultType: FieldType): HyperKernel = {

    require(in.length > 0)
    // Can't predict output type exactly from inputs because stacked ScalarTensors
    // could be a VectorTensor or a DyadTensor.  Thus, rather than build an
    // 'expectedFieldType', we check the components of resultType separately.

    // FieldShape check:
    val inType = in(0).fieldType
    for(i <- 1 until in.length)
      require(inType == in(i).fieldType)
    require(inType.fieldShape == resultType.fieldShape)
    // TensorShape check:
    // This is a bit tricky, since the code generator might be building up
    // a matrix field of 2x2 matrices out of 4 scalar fields. Thus, we only
    // check that enough Floats are supplied by the inputs.
    val suppliedFloats = in.length * inType.tensorShape.points
    require(suppliedFloats == resultType.tensorShape.points)
    // TensorType check:
    val resultTensorDim = resultType.tensorShape.dimensions
    inType.tensorShape.dimensions match {
      case 0 =>
        require(resultTensorDim == 1 || resultTensorDim == 2)
      case 1 =>
        require(resultTensorDim == 2)
      case _ =>
        throw new RuntimeException("Stacking of tensors of dim " + inType.tensorShape.dimensions + " not supported by this kernel.")
    }

    val addressing = bestAddressMode(in, resultType)
    new StackTensorsHyperKernel(in, operation, resultType, addressing)
  }
}

