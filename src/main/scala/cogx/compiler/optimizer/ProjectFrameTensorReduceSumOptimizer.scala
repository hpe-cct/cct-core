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
import cogx.platform.opencl.OpenCLPlatformParams
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
object ProjectFrameTensorReduceSumOptimizer extends Optimizer {
  private[cogx] val Enabled = true
  // The tiled convolve combined with tensor reduction is currently slower than the conventional approach
  // due to register pressure.  More work is needed on the ConvolveTiledHyperKernel2
  private[cogx] val TiledConvolveEnable = false

  /** "Horizontally" merge all HyperKernels in `dag` when possible.
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
          println("    *** ProjectFrame/TensorReduceSum Optimizer: disabled")
        }
        0
      }
      else {
        if (Cog.verboseOptimizer) {
          println("    *** ProjectFrame/TensorReduceSum Optimizer: starting (" + dag.size + " nodes)")
        }
        val initialDagSize = dag.size
        // Pre-order flattening not technically necessary, but it makes for the
        // most understandable MergedOp.toString() in the kernel DAG printout
        val kernels = dag.flattenPreorder
        for (kernel <- kernels) {
          if (!kernel.isDead) {
            kernel match {
              case reduceKernel: TensorReduceHyperKernel =>
                reduceKernel.inputs(0).source match {
                  case convolveKernel: ConvolveHyperKernel =>
                    val imageType = convolveKernel.inputs(0).fieldType
                    val batchSize = convolveKernel.operation.batchSize
                    val imageVectorSize = imageType.tensorShape.points
                    val convolveOutputType = reduceKernel.inputs(0).fieldType
                    val reduceFactor = reduceKernel.operation.factor
                    // We could also optimize vectorMode == PlaneByPlane where the image and
                    // filter lengths are the same.  Do we need a thread-count analysis here
                    // to make sure this is always a win?
                    val okToMerge =
                      (convolveKernel.operation.vectorMode == ProjectFrame && Cog.projectFrameMerging ||
                        convolveKernel.operation.vectorMode == BackProjectFrame && Cog.backProjectFrameMerging) &&
                      (reduceKernel.inputs(0).sinks.length == 1) &&
                      !convolveKernel.outputs(0).probed &&
                      (reduceFactor * batchSize == imageVectorSize)
                    if (okToMerge) {
                      val newVectorMode = convolveKernel.operation.vectorMode match {
                        case ProjectFrame => ProjectFrameBlockReduceSum
                        case BackProjectFrame => BackProjectFrameBlockReduceSum
                        case _ => throw new RuntimeException("Unexpected vector mode.")
                      }
                      val oldOp = convolveKernel.operation
                      val newOp = ConvolveOp(oldOp.borderPolicy, oldOp.filterOrientation,
                        oldOp.samplingPolicy, newVectorMode, oldOp.batchSize)
                      // Tiled convolve requires BorderValid convolution
                      val useTiledConvolve = TiledConvolveEnable && (newOp.borderPolicy == BorderValid)
                      val newResultType =
                        if (useTiledConvolve) {
                          ConvolveTiledHyperKernel2.outputFieldType(convolveKernel.inputs(0).fieldType, convolveKernel.inputs(1).fieldType,
                            newOp.vectorMode)
                        }
                        else {
                          ConvolveHyperKernel.outputFieldType(convolveKernel.inputs(0).fieldType, convolveKernel.inputs(1).fieldType,
                            newOp.borderPolicy, newOp.samplingPolicy, newOp.vectorMode, newOp.batchSize)
                        }
                      val newConvolveKernel =
                        if (useTiledConvolve) {
                          ConvolveTiledHyperKernel2(convolveKernel.inputs.toArray, newOp,
                            newResultType)
                       }
                        else {
                          ConvolveHyperKernel(convolveKernel.inputs.toArray, newOp,
                            newResultType, UseSmallTensorWhenBest, platformParams)
                        }
                      val lastKernel =
                        if (newResultType.tensorShape == Shape(1))
                          SliceVectorsHyperKernel(newConvolveKernel.outputs(0), TensorSliceOp(0), newResultType.resizeTensor(Shape()))
                        else
                          newConvolveKernel
                      require(lastKernel.resultTypes(0) == reduceKernel.resultTypes(0), "Internal compiler error.")
                      lastKernel.outputs(0).stealProbeAndNameFrom(reduceKernel.outputs(0))
                      lastKernel.outputs(0).stealSinksFrom(reduceKernel.outputs(0))
                      reduceKernel.removeFromCircuit(mustDo = true)

                      if (Cog.verboseKernelMerging) {
                        println("Merging " + convolveKernel + " ---> " + reduceKernel)
                        if (newConvolveKernel != lastKernel)
                          println("Result is " + newConvolveKernel + " ---> " + lastKernel)
                        else
                          println("Result is " + newConvolveKernel)
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
          println("    *** ProjectFrame/TensorReduceSum Optimizer: " + removedKernels + " kernel" +
                  (if (removedKernels == 1) "" else "s") + " removed.")
        removedKernels
      }
    answer
  }
}
