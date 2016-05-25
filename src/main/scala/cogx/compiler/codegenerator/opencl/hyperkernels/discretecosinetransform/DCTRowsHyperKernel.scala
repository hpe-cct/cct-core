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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.compiler.parser.op.{DCTInverseRowsOp, DCTRowsOp, UnaryOpcode}
import cogx.platform.types.{FieldType, VirtualFieldRegister}

/** Performs the 2D DCT (discrete cosine transform) on the *rows* of a 2D
  * input scalar field.
  *
  * @author Dick Carter and Greg Snider
  *
  * @param in The input field to be trimmed.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @return The synthesized hyperkernel.
  */
private[cogx]
class DCTRowsHyperKernel(in: VirtualFieldRegister,
                         operation: UnaryOpcode, //FFT2DSubOp,
                         resultType: FieldType)
        extends HyperKernel(operation, Array(in), resultType, BigTensorAddressing)
{
  val rows = in.fieldType.rows
  val columns = in.fieldType.columns
  var forward = true //(operation.dir == Forward)
  operation match {
    case DCTRowsOp =>
      forward = true
    case DCTInverseRowsOp =>
      forward = false
    case x =>
      throw new RuntimeException("internal error " + getClass.getSimpleName)
  }
  val workDimensions = DCT2DKernelCache.workDimensions(rows, columns, forward)
  val sourceCode = DCT2DKernelCache.sourceCode(rows, columns, forward)

  // The DCT runs with a 1D thread organization.  We pick up the local and
  // global workItem counts from the workDimensions object created by the
  // DCT planner and set the kernel launch parameters to match.  The kernel
  // runs in BigTensorAddressing mode since each thread writes multiple output
  // elements.

  override lazy val workFieldType =
    new FieldType(Shape(workDimensions.gWorkItems.toInt),
      resultType.tensorShape, resultType.elementType)
  override lazy val workGroup =
    HyperKernel.computeWorkGroupParameters(workFieldType, addressing, 1,
      workDimensions.lWorkItems.toInt)

  val realInput = !isComplexField(in.fieldType)
  val realOutput = !isComplexField(resultType)

  //val dir = operation.dir
  val dir = if (forward) -1 else 1
  val dirName = if (forward) "forward" else "inverse"
  /** supply values for former arguments to the FFT kernel */
  def postProcess(source: String) = source.
          replaceAll("%dirVal%", dir.toString).
          replaceAll("%dirName%", dirName).
          replaceAll("%batchSize%", workDimensions.batchSize.toString).
          replaceAll("%realInput%", realInput.toString).
          replaceAll("%realOutput%", realOutput.toString)

  addCode(postProcess(sourceCode))

  val Debug = false
  if (Debug) {
    println("workDimensions")
    workDimensions.print
    println("workFieldType = " + workFieldType)
    println("workGroup: ")
    workGroup.print
    println
    println

    debugCompile
  }
}
