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
import cogx.compiler.codegenerator.opencl.hyperkernels.{ConvolveHyperKernel, ConvolveTiledHyperKernel2, SliceVectorsHyperKernel, TensorReduceHyperKernel}
import cogx.compiler.parser.op._
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.{BorderValid, CrossCorrelationOrientation, UpsampleInputConvolution, UseSmallTensorWhenBest}
import cogx.runtime.execution.Profiler

/** Optimizer of kernel DAGs.
  *
  * This optimizer recognizes a ConvolveKernel(vectorMode = ProjectFrame) driving a TensorReduceHyperKernel and merges
  * them into a single ConvolveKernel(vectorMode = ProjectFrameBlockReduceSum) kernel that does the entire task,  Also,
  * this optimizer recognizes a ConvolveKernel(vectorMode = BackProjectFrame) driving a TensorReduceHyperKernel and merges
  * them into a single ConvolveKernel(vectorMode = BackProjectFrameBlockReduceSum) kernel that does the entire task,
  *
  * Finally, this optimizer helps create an optimized FilterAdjointBlockReduceSum through an admittedly complicated path:
  *
  * Before this kernel-circuit-level optimizer is run, the user creates the following sequence of operations
  * at the SyntaxTree level:
  *
  *               crossCorrelateFilterAdjoint(...).blockReduceSum(batchSize)
  *
  * The VectorFieldGenerator, will translate each of these operations into their respective kernels.  The
  * crossCorrelateFilterAdjoint operation, when translated, knows nothing of the following blockReduceSum, so a
  * sometimes inefficient ConvolveToSmallFieldHyperKernel is used that requires its own TensorReduceHyperKernel to
  * complete its job.  The first pass KernelCircuit is thus:
  *
  *        ConvolveToSmallFieldHyperKernel -> TensorReduceHyperKernel(x) -> TensorReduceHyperKernel(batchSize)
  *
  * The next step in transforming this kernel sequence is performed by the TensorReduceOptimizer, which combines
  * the kernels into the following sequence:
  *
  *        ConvolveToSmallFieldHyperKernel -> TensorReduceHyperKernel(x * batchSize)
  *
  * Finally, this optimizer is run and recognizes the sequence of a convolve kernel (one that has a
  * ConvolveOp(vectorMode=FilterAdjoint) ) followed by a tensor reduction down to the tensor shape expected of an
  * end-to-end FilterAdjointBlockReduceSum.  After checking that the ConvolveHyperkernel factory method is prepared
  * to handle the new opcode, this optimizer replaces the two-kernel convolve-reduce sequence by the result of
  * ConvolveHyperKernel(vectorMode=FilterAdjointBlockReduceSum).  This torturous sequence of steps could be avoided
  * if we did optimizations at the SyntaxTree level.
  *
  * @author Dick Carter
  */
private[cogx]
object ProjectFrameTensorReduceSumOptimizer extends Optimizer {
  private[cogx] val Enabled = true

  /** "Horizontally" merge all HyperKernels in `dag` when possible.
    *
    * @param dag Kernel circuit to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param profiler The profiler to use to pick the best variant
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(dag: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, profiler: Profiler, report: Boolean = true) = {
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
                reduceKernel.inputs(0).source.opcode match {
                  case convolveOp: AbstractConvolveOp =>
                    val convolveKernel = reduceKernel.inputs(0).source
                    val batchSize = convolveOp.batchSize
                    val in0VectorLength = convolveKernel.inputs(0).fieldType.tensorShape.points
                    val in1VectorLength = convolveKernel.inputs(1).fieldType.tensorShape.points
                    val convolveOutputType = reduceKernel.inputs(0).fieldType
                    val reduceFactor = reduceKernel.operation.factor
                    val resultVectorSize = reduceKernel.outputs(0).fieldType.tensorShape.points
                    // We could also optimize vectorMode == PlaneByPlane where the image and
                    // filter lengths are the same.  Do we need a thread-count analysis here
                    // to make sure this is always a win?
                    val okToMergeTest1 = reduceKernel.inputs(0).sinks.length == 1 && !convolveKernel.outputs(0).probed
                    val okToMergeTest2 = convolveOp.vectorMode match {
                      case ProjectFrame =>
                        val planesPerImage = in0VectorLength / batchSize
                        val numLogicalFilters = in1VectorLength / planesPerImage
                        Cog.projectFrameMerging  && (resultVectorSize == numLogicalFilters * batchSize)
                      case BackProjectFrame =>
                        val numLogicalFilters = in0VectorLength / batchSize
                        val planesPerImage = in1VectorLength / numLogicalFilters
                        Cog.backProjectFrameMerging && (resultVectorSize == planesPerImage * batchSize)
                      case FilterAdjoint =>
                        val planesPerImage = in0VectorLength / batchSize
                        val numLogicalFilters = in1VectorLength / batchSize
                        Cog.filterAdjointMerging &&
                          convolveOp.samplingPolicy.isInstanceOf[UpsampleInputConvolution] &&
                          convolveOp.filterOrientation == CrossCorrelationOrientation &&
                          batchSize > 1 &&
                          (resultVectorSize == planesPerImage * numLogicalFilters) &&
                        ConvolveHyperKernel.canUseFilterAdjointBlockReduceSum(convolveKernel.inputs.toArray, convolveOp, convolveOutputType.fieldShape, codeGenParams)
                      case _ =>
                        false
                    }
                    val okToMerge = okToMergeTest1 && okToMergeTest2
                    if (okToMerge) {
                      val newVectorMode = convolveOp.vectorMode match {
                        case ProjectFrame => ProjectFrameBlockReduceSum
                        case BackProjectFrame => BackProjectFrameBlockReduceSum
                        case FilterAdjoint => FilterAdjointBlockReduceSum
                        case _ => throw new RuntimeException("Unexpected vector mode.")
                      }
                      val newOp = ConvolveOp(convolveOp.borderPolicy, convolveOp.filterOrientation,
                        convolveOp.samplingPolicy, newVectorMode, convolveOp.batchSize)
                      val newResultType =
                          ConvolveHyperKernel.outputFieldType(convolveKernel.inputs(0).fieldType, convolveKernel.inputs(1).fieldType,
                            newOp.borderPolicy, newOp.samplingPolicy, newOp.vectorMode, newOp.batchSize)
                      val newConvolveKernel =
                          ConvolveHyperKernel(convolveKernel.inputs.toArray, newOp, newResultType, UseSmallTensorWhenBest, codeGenParams, profiler)
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
