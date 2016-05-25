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

import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, SmallTensorAddressing, TensorElementAddressing, HyperKernel}
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.parser.op.{TensorReduceMaxOp, TensorReduceMinOp, TensorReduceSumOp, TensorReductionOp}
import cogx.compiler.codegenerator.opencl.OpcodeToFunction
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import TensorReduceHyperKernel._

/** Reduces a VectorField or MatrixField to a ScalarField by reducing each tensor in that
  * field to a scalar.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field register driving this kernel.
  * @param operation The TensorReductionOp opcode, specifying which type of
  *                  reduction: sum, min, or max.
  * @param resultType The type of the resulting scalar field.
  */
private[cogx]
class TensorReduceHyperKernel private (in: VirtualFieldRegister,
                                       val operation: TensorReductionOp,
                                       resultType: FieldType,
                                       addressMode: AddressingMode)
        extends HyperKernel(operation, Array(in), resultType, addressMode) {

  val reductionOp = OpcodeToFunction(operation)

  val code = new StringBuilder
  val inType = in.fieldType
  val outType = addressMode.clType(resultType).name
  val outZero = addressMode.clType(resultType).zero
  /** This kernel is not as extensible as we'd like: we still need to hard-code
    * the initial 'zero' element.  Might be avoided by initializing to the
    * first element read.  Could also provide zero from the opcode.
    */
  val initVal = operation match {
    case x:TensorReduceSumOp => outZero
    case x:TensorReduceMinOp => "((" + outType + ") FLT_MAX)"
    case x:TensorReduceMaxOp => "((" + outType + ") -FLT_MAX)"
    case x => throw new RuntimeException("unknown op: " + x)
  }

  // First if-clause handles block reductions to vector fields
  if (!isTensor0Field(resultType)) {
    code append setLayerRowColumn(inType, "_layer", "_row", "_column")
    operation match {
      case x: TensorReduceSumOp =>
        val InnerLoopCount = 32
        val outerLoopCount =  operation.factor / InnerLoopCount
        val epilogCount = operation.factor - outerLoopCount * InnerLoopCount
        code append s"    $outType reduced = $initVal;\n"
        code append s"        $outType partialReduced = $initVal;\n"
        code append s"        $outType newReduced, newPartialReduced, reducedChange;\n"
        code append s"        int i, j, k;\n"
        code append s"        tensorElement = _tensorElement * ${operation.factor};\n"
        code append s"        for (i = 0; i < $outerLoopCount; i++) {\n"
        code append s"            for (j = 0; j < $InnerLoopCount; j++, tensorElement++) {\n"
        code append s"                partialReduced = ${reductionOp}(readNonlocal(@in0), partialReduced);\n"
        code append s"            }\n"
        // Knuth et.al. TwoSum algorithm: overhead is 6 flops, but outside inner loop and largely hidden by memory latency
        code append s"            newReduced = reduced + partialReduced;\n"
        code append s"            reducedChange = newReduced - reduced;\n"
        code append s"            newPartialReduced = (reduced - (newReduced - reducedChange)) + (partialReduced - reducedChange);\n"
        code append s"            reduced = newReduced;\n"
        code append s"            partialReduced = newPartialReduced;\n"
        code append s"        }\n"
        code append s"        for (k = 0; k < $epilogCount; k++, tensorElement++) {\n"
        code append s"            partialReduced = ${reductionOp}(readNonlocal(@in0), partialReduced);\n"
        code append s"        }\n"
        code append s"        @out0 = reduced + partialReduced;\n"
      case _ =>
        code append s"    $outType reduced = $initVal;\n"
        code append s"    for (tensorElement = _tensorElement * ${operation.factor}; tensorElement < (_tensorElement + 1)*${operation.factor}; tensorElement++) {\n"
        code append s"        reduced = ${reductionOp}(readNonlocal(@in0), reduced);\n"
        code append s"    }\n"
        code append s"    @out0 = reduced;\n"
    }
  }
  else if (isBigTensorField(inType)) {
    operation match {
      case x: TensorReduceSumOp =>
        val InnerLoopCount = 32
        val outerLoopCount =  inType.tensorShape.points / InnerLoopCount
        val epilogCount = inType.tensorShape.points - outerLoopCount * InnerLoopCount
        code append s"    $outType reduced = $initVal;\n"
        code append s"        $outType partialReduced = $initVal;\n"
        code append s"        $outType newReduced, newPartialReduced, reducedChange;\n"
        code append s"        int i, j, k;\n"
        code append s"        tensorElement = 0;\n"
        code append s"        for (i = 0; i < $outerLoopCount; i++) {\n"
        code append s"            for (j = 0; j < $InnerLoopCount; j++, tensorElement++) {\n"
        code append s"                partialReduced = ${reductionOp}(readElement(@in0), partialReduced);\n"
        code append s"            }\n"
        // Knuth et.al. TwoSum algorithm: overhead 6 flops, but outside inner loop and largely hidden by memory latency
        code append s"            newReduced = reduced + partialReduced;\n"
        code append s"            reducedChange = newReduced - reduced;\n"
        code append s"            newPartialReduced = (reduced - (newReduced - reducedChange)) + (partialReduced - reducedChange);\n"
        code append s"            reduced = newReduced;\n"
        code append s"            partialReduced = newPartialReduced;\n"
        code append s"        }\n"
        code append s"        for (k = 0; k < $epilogCount; k++, tensorElement++) {\n"
        code append s"            partialReduced = ${reductionOp}(readElement(@in0), partialReduced);\n"
        code append s"        }\n"
        code append s"        @out0 = reduced + partialReduced;\n"
      case _ =>
        code append s"    $outType reduced = $initVal;\n"
        code append s"    for (tensorElement = 0; tensorElement < ${inType.tensorShape.points}; tensorElement++) {\n"
        code append s"        reduced = ${reductionOp}(readElement(@in0), reduced);\n"
        code append s"    }\n"
        code append s"    @out0 = reduced;\n"
    }
  }
  else if (inType.tensorShape.points == 1) {
    // probably a rare case, but the .sN syntax is not valid on a scalar
    code append "    @out0 = read(@in0);\n"
  }
  else {
    // perform reduction on a small tensor
    val inTypeName = SmallTensorAddressing.clType(inType).name
    code append s"    $inTypeName inVal = read(@in0);\n"
    code append s"    @out0 = " + reduce("inVal", reductionOp, 0, inType.tensorShape.points) + ";\n"
  }
  addCode(code.toString)
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object TensorReduceHyperKernel {

  /**
   * Create a hyperkernel that reduces a VectorField or MatrixField to a
   * ScalarField by reducing each tensor in that field to a scalar.
   *
   * @param in The input virtual field register driving this kernel.
   * @param operation The TensorReductionOp opcode, specifying which type of
   *                  reduction: sum, min, or max.
   * @param resultType The type of the resulting scalar field.
   * @return The synthesized hyperkernel.
   */
  def apply(in: VirtualFieldRegister, operation: TensorReductionOp, resultType: FieldType): HyperKernel = {
    val inType = in.fieldType
    val expectedResultType =
      if (inType.tensorShape.points == operation.factor)
        inType.resizeTensor(Shape())
      else
        inType.resizeTensor(Shape(inType.tensorShape.points / operation.factor))

    require(expectedResultType == resultType,
     s"internal error: expecting tensor reduce of FieldType ${in.fieldType} by factor ${operation.factor} to be $expectedResultType, found $resultType.")

    val addressing =
      if (isTensor0Field(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new TensorReduceHyperKernel(in, operation, resultType, addressing)
  }

  /** Create a binary-tree reduction of a number of inputs.  This takes advantage
    * of the associativity of the reduction op to avoid the GPU's high penalty
    * for reusing results immediately.
    *
    * @param variable  The vector variable that supplies the elements of the reduction.
    * @param reductionOp The reduction operation as a string, e.g. "add" or "max".
    * @param fromIndex  The starting index, typically 0.
    * @param untilIndex One past the ending index, typically the length of the vector.
    * @return  The string representing the reduction, e.g. "add(add(x.s0,x.s1),add(x.s2,x.s3))"
    */
  def reduce(variable: String, reductionOp: String, fromIndex: Int, untilIndex: Int): String = {
    def toHexChar(i: Int) = {
      require(i >= 0 && i < 16)
      "0123456789ABCDEF"(i)
    }

    if (untilIndex == fromIndex + 1)
      variable + ".s" + toHexChar(fromIndex)
    else {
      val midPoint = (fromIndex + untilIndex) / 2
      reductionOp + "(" + reduce(variable, reductionOp, fromIndex, midPoint) + "," +
                          reduce(variable, reductionOp, midPoint, untilIndex) + ")"
    }
  }

}

