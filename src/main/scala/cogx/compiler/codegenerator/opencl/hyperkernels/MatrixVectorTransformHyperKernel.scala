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
import cogx.platform.types.{FieldMemoryLayoutImpl, VirtualFieldRegister, Opcode, FieldType}
import cogx.compiler.parser.op.MatrixTransformVectorOp
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Transform all vectors in a vector field (second input) with corresponding
  * matrices in a matrix field (first input).  The field Shapes must match, or
  * at least one must be 0D.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @param outputsPerThread How many result tensor elements each thread must generate.
  * @param maxConstantBufferSize The maximum amount of constant memory permitted on the platform.
  */
private[cogx]
class MatrixVectorTransformHyperKernel private (in: Array[VirtualFieldRegister],
                                                operation: Opcode,
                                                resultType: FieldType,
                                                addressMode: AddressingMode,
                                                outputsPerThread: Int,
                                                maxConstantBufferSize: Long)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  val matrixType = in(0).fieldType
  val matrixShape = matrixType.tensorShape
  val matrixColumns = matrixShape(1)
  val matrixFieldSizeBytes = 4 * matrixShape.points * new FieldMemoryLayoutImpl(matrixType).pageSize

  val vectorType = in(1).fieldType
  val vectorShape = vectorType.tensorShape
  val vectorLength = vectorShape(0)
  val vectorFieldSizeBytes = 4 * vectorShape.points * new FieldMemoryLayoutImpl(vectorType).pageSize

  // Put a single matrix in constant memory if it fits
  if (is0DField(matrixType) && !is0DField(vectorType) && matrixFieldSizeBytes <= maxConstantBufferSize)
    makeInputConstant(0)

  // Put a single vector in constant memory if it fits
  if (!is0DField(matrixType) && is0DField(vectorType) && vectorFieldSizeBytes <= maxConstantBufferSize)
    makeInputConstant(1)

  val resultTensorPoints = resultType.tensorShape.points
  // workFieldType has a reduced tensor depth by a factor of 'outputsPerThread'
  override lazy val workFieldType =
    resultType.resizeTensor(Shape((resultTensorPoints + outputsPerThread - 1)/ outputsPerThread))

  val code = new StringBuffer
  code append "\n"

  // Is this the first iteration that should not be performed?
  private def isFirstPrunedIteration(i: Int) = {
    val lastValidPlaneModulus = (resultType.tensorShape.points - 1) % outputsPerThread
    i == lastValidPlaneModulus + 1
  }
  // Will the last workgroup (with _tensorElement == _tensorElements) need to bypass
  // some of the customary processing (to avoid reading or writing outside valid field memory)?
  def iterationPruningNeeded = resultTensorPoints % outputsPerThread != 0

  if (outputsPerThread == 1) {
    code append  "    int outVectorIndex = _tensorElement;\n"
    code append  "    float sum = 0.0f;\n"
    code append s"    for (int i = 0; i < $vectorLength; i++) {\n"
    // Read matrix element
    code append s"        tensorElement = $matrixColumns * outVectorIndex + i;\n"
    code append setLayerRowColumn(resultType, "_layer", "_row", "_column")
    code append  "        float matrixElement = readNonlocal(@in0);\n"
    code append  "        tensorElement = i;\n"
    code append  "        float vectorElement = readNonlocal(@in1);\n"
    code append  "        sum += matrixElement * vectorElement;\n"
    code append  "    }\n"
    code append  "    @out0 = sum;\n"
  }
  else {
    code append setLayerRowColumn(resultType, "_layer", "_row", "_column")
    code append s"        int baseOutVectorIndex = _tensorElement * $outputsPerThread;\n"
    for (outIndex <- 0 until outputsPerThread)
      code append  s"        float sum$outIndex = 0.0f;\n"
    code append  s"        float matrixElement;\n"
    code append s"        for (int i = 0; i < $vectorLength; i++) {\n"
    // Read vector element
    code append  "            tensorElement = i;\n"
    code append  "            float vectorElement = readNonlocal(@in1);\n"
    for (outIndex <- 0 until outputsPerThread) {
      if (iterationPruningNeeded && isFirstPrunedIteration(outIndex))
        code append s"        if (_tensorElement != _tensorElements - 1) {\n"
      // Read matrix element
      code append s"            tensorElement = $matrixColumns * (baseOutVectorIndex + $outIndex) + i;\n"
      code append  "            matrixElement = readNonlocal(@in0);\n"
      code append s"            sum$outIndex += matrixElement * vectorElement;\n"
      if (iterationPruningNeeded && outIndex == outputsPerThread - 1)
        code append s"        }\n"
    }
    code append  "        }\n"
    for (outIndex <- 0 until outputsPerThread) {
      if (iterationPruningNeeded && isFirstPrunedIteration(outIndex))
        code append s"        if (_tensorElement != _tensorElements - 1) {\n"
      code append s"        tensorElement = baseOutVectorIndex + $outIndex;\n"
      code append s"        @outElementNonlocal0 = sum$outIndex;\n"
      if (iterationPruningNeeded && outIndex == outputsPerThread - 1)
        code append s"        }\n"
    }
  }

  addCode(code.toString)
//  debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object MatrixVectorTransformHyperKernel {
  // Value of 30 is based on a Titan Black with 15 SM's running 2 workgroups/SM.
  val WorkGroupsToFullyUtilizeGPU = 30
  val ThreadsToFullyUtilizeGPU = WorkGroupsToFullyUtilizeGPU * 16 * 16

  /** Largest number of output tensor elements that may be generated by each thread */
  val MaxOutputsPerThread = 8

  /**
   * Creates a kernel that performs the vector field transform.
   *
   * @param in The two input virtual field registers driving this kernel.
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @param maxConstantBufferSize The maximum amount of constant memory permitted on the platform.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: Opcode,
            resultType: FieldType, maxConstantBufferSize: Long): HyperKernel =
  {
    require(operation == MatrixTransformVectorOp)
    val matrixType = in(0).fieldType
    val matrixShape = matrixType.tensorShape
    val matrixColumns = matrixShape(1)
    val matrixRows = matrixShape(0)

    val vectorType = in(1).fieldType
    val vectorShape = vectorType.tensorShape
    val vectorLength = vectorShape(0)
    require(matrixColumns == vectorLength)

    require(matrixType.dimensions == 0 ||
            vectorType.dimensions == 0 ||
            matrixType.fieldShape == vectorType.fieldShape,
      "Matrix and Vector field shape must match or be 0D: " +
              matrixType.fieldShape + " " + vectorType.fieldShape)
    require(matrixShape.dimensions == 2, "Bad matrix shape: " + matrixShape)
    require(vectorShape.dimensions == 1, "Bad vector shape: " + vectorShape)

    val addressingMode = TensorElementAddressing
    // Calculate the ideal outputsPerThread, starting from MaxOutputsPerThread, going down to 1.
    // No need to process 8 outputs per thread if the output depth is 6 (for example).  Also,
    // full-GPU utilization considerations come into play.
    def outputsPerThread: Int = {
      val resultTensorPoints = resultType.tensorShape.points
      for (outsPerThread <- math.min(MaxOutputsPerThread, resultTensorPoints) to 1 by -1) {
        val workgroupsPerTensor = (resultTensorPoints + outsPerThread - 1) / outsPerThread
        val threads = resultType.fieldShape.points * workgroupsPerTensor
        if (threads >= ThreadsToFullyUtilizeGPU)
          return outsPerThread
      }
      1
    }

    val expectedResultFieldShape =
      if (matrixType.dimensions == 0)
        vectorType.fieldShape
      else
        matrixType.fieldShape
    require(resultType.fieldShape == expectedResultFieldShape,
      "Result field shape mismatch: expected " + expectedResultFieldShape + ", actual " + resultType.fieldShape)
    val expectedResultTensorShape = Shape(matrixRows)
    require(resultType.tensorShape == expectedResultTensorShape,
      "Result tensor shape mismatch: expected " + expectedResultTensorShape + ", actual " + resultType.tensorShape)
    new MatrixVectorTransformHyperKernel(in, operation, resultType, addressingMode, outputsPerThread, maxConstantBufferSize)
  }
}
