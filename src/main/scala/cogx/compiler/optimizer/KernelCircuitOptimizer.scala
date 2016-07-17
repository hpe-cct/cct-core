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
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLKernelCodeGenParams

/** Optimizes a kernel circuit using a variety of approaches.
  *
  * @author Greg Snider
  */
private[cogx]
object KernelCircuitOptimizer extends Optimizer {
  val Enabled = true

  /** Optimize `circuit` by a number of means.
    *
    * @param kernelCircuit Kernel circuit to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(kernelCircuit: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, report: Boolean = true) = {
    var optimizations = 0
    if (Enabled) {
      optimizations += DeadKernel.optimize(kernelCircuit, codeGenParams)
      optimizations += RedundantInputs.optimize(kernelCircuit, codeGenParams)
      optimizations += CommonSubexpression.optimize(kernelCircuit, codeGenParams)
      optimizations += ProjectFrameTensorReduceSumOptimizer.optimize(kernelCircuit, codeGenParams)

      // Loop over a list of optimizers whose improvements may create further
      // optimization opportunities.  Keep going until no further optimizations
      // are possible.

      // The TransformTranspose optimizer takes a few passes to fully absorb all the transpose kernels.
      // We want this complete before the transpose kernels might be merged into multi-output kernels
      optimizations += loopOptimize(kernelCircuit, codeGenParams, report, Array(TransformTransposeOptimizer))

      // Other optimizers that might take a few passes worst case
      val dependentOptimizers = Array(HyperKernelMerger, HyperKernelMultiOutputMerger)
      optimizations += loopOptimize(kernelCircuit, codeGenParams, report, dependentOptimizers)

      // Reshape remover- no other kernel creation should happen after this, so do this last

      optimizations += loopOptimize(kernelCircuit, codeGenParams, report, Array(ReshapeRemover))

      if (Cog.verboseOptimizer) {
        println("Post-optimizer kernel circuit:")
        kernelCircuit.print()
      }
    } else
      println("WARNING: Cog optimizer is disabled.")
    optimizations
  }

  /** Optimize `circuit` by a number of means and keep looping over the optimizers until no improvements are seen.
    *
    * @param kernelCircuit Kernel circuit to be optimized
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired
    * @param optimizers The optimizers to loop over until no further improvement is seen
    * @return  The number of optimizations made
    */
  private def loopOptimize(kernelCircuit: KernelCircuit, platformParams: OpenCLKernelCodeGenParams, report: Boolean, optimizers: Array[Optimizer]) = {
    var optimizations = 0
    var numOptimizers = optimizers.length
    var consecutiveFails = 0
    var i = 0
    while(consecutiveFails <= numOptimizers - 1) {
      val optimizer = optimizers(i)
      val improvements = optimizer.optimize(kernelCircuit, platformParams)
      optimizations += improvements
      if (improvements == 0)
        consecutiveFails += 1
      else
        consecutiveFails = 0
      i = (i + 1) % numOptimizers
    }
    optimizations
  }
}
