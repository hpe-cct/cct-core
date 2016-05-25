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

import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.parser.op._
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLPlatformParams

/** Optimizer of kernel DAGs that combined MatrixMatrixTransformHyperKernels and TensorTransposeHyperKernels
  *
  * The MatrixMatrixTransformHyperKernel (which does matrix-matrix multiplies) can perform a transpose of either or
  * both inputs without significant impact on performance.  This optimizer looks for a distinct TransposeHyperKernel
  * connected to the MatrixMatrixTransformHyperKernel and emits a single MatrixMatrixTransformHyperKernel with the
  * "transpose input" flags suitably altered.  Specific optimizations performed are:
  *
  ********************************************************************************************************************
  *
  * From:
  *         +------------------------------------------------------+      +---------------------------------+
  * in1 ->--| transposeIn1                                         |      |                                 |
  *         |                  MatrixMatrixTransformHyperKernel    |--->--|   TensorTransposeHyperKernels   |--->--
  * in2 ->--| transposeIn2                                         |      |                                 |
  *         +------------------------------------------------------+      +---------------------------------+
  *
  * To:
  *         +------------------------------------------------------+
  * in2 ->--| !transposeIn1                                        |
  *         |                  MatrixMatrixTransformHyperKernel    |--->         // Note swapped inputs
  * in1 ->--| !transposeIn2                                        |
  *         +------------------------------------------------------+
  *
  ********************************************************************************************************************
  *
  * From:
  *         +--------------------------------+
  *         |                                |        +------------------------------------------------------+
  * in1 ->--|   TensorTransposeHyperKernel   |---->---| transposeIn1                                         |
  *         |                                |        |                     MatrixMatrixTransformHyperKernel |--->--
  *         +--------------------------------+   +----| transposeIn2                                         |
  *                                              |    +------------------------------------------------------+
  *                                    in2 ->----+
  * To:
  *         +------------------------------------------------------+
  * in1 ->--| !transposeIn1                                        |
  *         |                  MatrixMatrixTransformHyperKernel    |--->--
  * in2 ->--| transposeIn2                                         |
  *         +------------------------------------------------------+
  *
  ********************************************************************************************************************
  *
  * ...and similar to the above optimization for a TransposeHyperKernel present at the second input.
  *
  * Should a TransposeHyperKernel be found on all 3 I/O's of the MatrixMatrixTransformHyperKernel, this optimizer
  * will remove them all through a successive application of the individual specific optimizations.
  *
  * @author Dick Carter
  */
private[cogx]
object TransformTransposeOptimizer extends Optimizer {
  private[cogx] val Enabled = true

  /** Merge all TransposeHyperKernels into neighboring MatrixMatrixTransformHyperKernels in `dag` when possible.
    *
    * @param dag Kernel circuit to be optimized
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired
    * @return  The number of optimizations made
    */
  def optimize(dag: KernelCircuit, platformParams: OpenCLPlatformParams, report: Boolean = true) = {
    val answer =
      if (!Enabled) {
        if (Cog.verboseOptimizer) {
          println("    *** TransformTranspose Optimizer: disabled")
        }
        0
      }
      else {
        if (Cog.verboseOptimizer) {
          println("    *** TransformTranspose Optimizer: starting (" + dag.size + " nodes)")
        }
        val initialDagSize = dag.size
        // Pre-order flattening not technically necessary, but it makes for the
        // most understandable MergedOp.toString() in the kernel DAG printout
        val kernels = dag.flattenPreorder
        for (kernel <- kernels) {
          if (!kernel.isDead) {
            kernel.opcode match {
              case MatrixTransposeOp =>
                val tensorTransposeKernel = kernel.asInstanceOf[TensorTransposeHyperKernel]
                tensorTransposeKernel.inputs(0).source.opcode match {
                  case mmOp: MatrixTransformMatrixOp =>
                    val mmMultiplyKernel = tensorTransposeKernel.inputs(0).source
                    val okToMerge = (tensorTransposeKernel.inputs(0).sinks.length == 1) && !mmMultiplyKernel.outputs(0).probed
                    if (okToMerge) {
                      val newOp = MatrixTransformMatrixOp(!mmOp.transposeIn2, !mmOp.transposeIn1)
                      val mmIn1 = mmMultiplyKernel.inputs(0)
                      val mmIn2 = mmMultiplyKernel.inputs(1)
                      val newMmMultiply =
                        MatrixMatrixTransformHyperKernel(Array(mmIn2, mmIn1), newOp,
                          tensorTransposeKernel.resultType)
                      newMmMultiply.outputs(0).stealProbeAndNameFrom(tensorTransposeKernel.outputs(0))
                      newMmMultiply.outputs(0).stealSinksFrom(tensorTransposeKernel.outputs(0))
                      tensorTransposeKernel.removeFromCircuit(mustDo = true)

                      if (Cog.verboseKernelMerging) {
                        println("Merging " + mmMultiplyKernel + " ---> " + tensorTransposeKernel)
                        println("Result is " + newMmMultiply)
                        println("********************************************")
                      }
                    }
                  case _ => // Not the case we care about here
                }
              case mmOp: MatrixTransformMatrixOp =>
                val mmMultiplyKernel = kernel.asInstanceOf[HyperKernel]
                mmMultiplyKernel.inputs(0).source match {
                  case tensorTransposeKernel: TensorTransposeHyperKernel =>
                    // Since the absorbing of the transpose into the matrix multiply comes "for free", we don't worry
                    // if there are other sinks on the transpose kernel output.  Maybe those sinks are other matrix multiply
                    // kernels and the transpose will eventually be removed.
                    val okToMerge = !tensorTransposeKernel.outputs(0).probed
//                    val okToMerge = (mmMultiplyKernel.inputs(0).sinks.length == 1) && !tensorTransposeKernel.outputs(0).probed
                    if (okToMerge) {
                      val newOp = MatrixTransformMatrixOp(!mmOp.transposeIn1, mmOp.transposeIn2)
                      val mmIn1 = tensorTransposeKernel.inputs(0)
                      val mmIn2 = mmMultiplyKernel.inputs(1)
                      val newMmMultiplyKernel =
                        MatrixMatrixTransformHyperKernel(Array(mmIn1, mmIn2), newOp,
                          mmMultiplyKernel.resultTypes(0))
                      newMmMultiplyKernel.outputs(0).stealProbeAndNameFrom(mmMultiplyKernel.outputs(0))
                      newMmMultiplyKernel.outputs(0).stealSinksFrom(mmMultiplyKernel.outputs(0))
                      mmMultiplyKernel.removeFromCircuit(mustDo = true)

                      if (Cog.verboseKernelMerging) {
                        println("Merging " + tensorTransposeKernel + " (1st input) ---> " + mmMultiplyKernel)
                        println("Result is " + newMmMultiplyKernel)
                        println("********************************************")
                      }
                    }
                  case _ => {
                    mmMultiplyKernel.inputs(1).source match {
                      case tensorTransposeKernel: TensorTransposeHyperKernel =>
                        val okToMerge = !tensorTransposeKernel.outputs(0).probed
                        if (okToMerge) {
                          val newOp = MatrixTransformMatrixOp(mmOp.transposeIn1, !mmOp.transposeIn2)
                          val mmIn1 = mmMultiplyKernel.inputs(0)
                          val mmIn2 = tensorTransposeKernel.inputs(0)
                          val newMmMultiplyKernel =
                            MatrixMatrixTransformHyperKernel(Array(mmIn1, mmIn2), newOp,
                              mmMultiplyKernel.resultTypes(0))
                          newMmMultiplyKernel.outputs(0).stealProbeAndNameFrom(mmMultiplyKernel.outputs(0))
                          newMmMultiplyKernel.outputs(0).stealSinksFrom(mmMultiplyKernel.outputs(0))
                          mmMultiplyKernel.removeFromCircuit(mustDo = true)

                          if (Cog.verboseKernelMerging) {
                            println("Merging " + tensorTransposeKernel + " (2nd input) ---> " + mmMultiplyKernel)
                            println("Result is " + newMmMultiplyKernel)
                            println("********************************************")
                          }
                        }
                      case _ => // Not the case we care about here
                    }
                  }
                }
              case _ => // Not the case we care about here
            }
          }
        }
        // We now have to fix up recurrences
        fixRecurrences(dag)
        val removedKernels = initialDagSize - dag.size
        if (Cog.verboseOptimizer)
          println("    *** TransformTranspose Optimizer: " + removedKernels + " kernel" +
                  (if (removedKernels == 1) "" else "s") + " removed.")
        removedKernels
      }
    answer
  }
}
