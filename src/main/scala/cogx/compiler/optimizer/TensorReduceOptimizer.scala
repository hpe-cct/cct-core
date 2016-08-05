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

package cogx.compiler.optimizer

import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.codegenerator.opencl.hyperkernels.{ConvolveTiledHyperKernel2, SliceVectorsHyperKernel, ConvolveHyperKernel, TensorReduceHyperKernel}
import cogx.compiler.parser.op._
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.{BorderValid, UseSmallTensorWhenBest}

/** Optimizer of kernel DAGs.
  *
  * This optimizer recognizes a ConvolveKernel(vectorMode = ProjectFrame) driving a TensorReduceHyperKernel and merges
  * them into a single ConvolveKernel(vectorMode = ProjectFrameBlockReduceSum) kernel that does the entire task,  Also,
  * this optimizer recognizes a ConvolveKernel(vectorMode = BackProjectFrame) driving a TensorReduceHyperKernel and merges
  * them into a single ConvolveKernel(vectorMode = BackProjectFrameBlockReduceSum) kernel that does the entire task,
  *
  * @author Dick Carter
  */
private[cogx]
object TensorReduceOptimizer extends Optimizer {
  private[cogx] val Enabled = true

  /** "Horizontally" merge all HyperKernels in `dag` when possible.
    *
    * @param dag Kernel circuit to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(dag: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, report: Boolean = true) = {
    val answer =
      if (!Enabled) {
        if (Cog.verboseOptimizer) {
          println("    *** TensorReduce Optimizer: disabled")
        }
        0
      }
      else {
        if (Cog.verboseOptimizer) {
          println("    *** TensorReduce Optimizer: starting (" + dag.size + " nodes)")
        }
        val initialDagSize = dag.size
        // Pre-order flattening not technically necessary, but it makes for the
        // most understandable MergedOp.toString() in the kernel DAG printout
        val kernels = dag.flattenPreorder
        for (kernel <- kernels) {
          if (!kernel.isDead) {
            kernel match {
              case secondReduceKernel: TensorReduceHyperKernel =>
                secondReduceKernel.inputs(0).source match {
                  case firstReduceKernel: TensorReduceHyperKernel =>
                    val outputType = secondReduceKernel.outputs(0).fieldType
                    val middleType = secondReduceKernel.inputs(0).fieldType
                    val inputType = firstReduceKernel.inputs(0).fieldType
                    val firstFactor = firstReduceKernel.operation.factor
                    val secondFactor = secondReduceKernel.operation.factor
                    val combinedFactor = firstFactor * secondFactor
                    val opsMatch = firstReduceKernel.operation match {
                      case x: TensorReduceSumOp => secondReduceKernel.operation.isInstanceOf[TensorReduceSumOp]
                      case x: TensorReduceMinOp => secondReduceKernel.operation.isInstanceOf[TensorReduceMinOp]
                      case x: TensorReduceMaxOp => secondReduceKernel.operation.isInstanceOf[TensorReduceMaxOp]
                    }
                    val okToMerge =
                      opsMatch &&
                        (inputType.fieldShape == middleType.fieldShape) &&
                        (inputType.fieldShape == outputType.fieldShape) &&
                        (inputType.tensorShape.points % firstFactor == 0) &&
                        (middleType.tensorShape.points % secondFactor == 0) &&
                        (inputType.tensorShape.points == outputType.tensorShape.points * combinedFactor) &&
                        (secondReduceKernel.inputs(0).sinks.length == 1) &&
                        !firstReduceKernel.outputs(0).probed
                    if (okToMerge) {
                      val newOp: TensorReductionOp = firstReduceKernel.operation match {
                        case x: TensorReduceSumOp => TensorReduceSumOp(combinedFactor)
                        case x: TensorReduceMinOp => TensorReduceMinOp(combinedFactor)
                        case x: TensorReduceMaxOp => TensorReduceMaxOp(combinedFactor)
                      }
                      val combinedReduceKernel = TensorReduceHyperKernel(firstReduceKernel.inputs(0), newOp, outputType)
                      require(combinedReduceKernel.resultTypes(0) == secondReduceKernel.resultTypes(0), "Internal compiler error.")
                      combinedReduceKernel.outputs(0).stealProbeAndNameFrom(secondReduceKernel.outputs(0))
                      combinedReduceKernel.outputs(0).stealSinksFrom(secondReduceKernel.outputs(0))
                      secondReduceKernel.removeFromCircuit(mustDo = true)

                      if (Cog.verboseKernelMerging) {
                        println("Merging " + firstReduceKernel + " ---> " + secondReduceKernel)
                        println("Result is " + combinedReduceKernel)
                        println("********************************************")
                      }
                    }
                  case _ => // Not the case we care about here
                }
              case _ => // Not the case we care about here
            }
          }
        }
        // We now have to fix up recurrences
        fixRecurrences(dag)
        val removedKernels = initialDagSize - dag.size
        if (Cog.verboseOptimizer)
          println("    *** TensorReduce Optimizer: " + removedKernels + " kernel" +
                  (if (removedKernels == 1) "" else "s") + " removed.")
        removedKernels
      }
    answer
  }
}
