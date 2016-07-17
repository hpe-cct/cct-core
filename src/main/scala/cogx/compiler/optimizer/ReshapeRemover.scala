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
import cogx.platform.opencl.OpenCLKernelCodeGenParams

/** Optimizer of kernel DAGs that removes the ReshapeHyperKernel
  *
  * The ReshapeHyperKernel is introduced to permit a field of one shape to be copied to another one of a different shape.
  * The copy does not transform the data layout and so can be eliminated.  This should only be done after all code
  * generation and other optimizations have taken place, since a downstream kernel of the snipped-out reshape with
  * suddenly see its input change its FieldType.  A bit dangerous frankly, so this should be handled differently.
  *
  * @author Dick Carter
  */
private[cogx]
object ReshapeRemover extends Optimizer {
  private[cogx] val Enabled = true

  /** Merge all TransposeHyperKernels into neighboring MatrixMatrixTransformHyperKernels in `dag` when possible.
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
          println("    *** ReshapeRemover Optimizer: disabled")
        }
        0
      }
      else {
        if (Cog.verboseOptimizer) {
          println("    *** ReshapeRemover Optimizer: starting (" + dag.size + " nodes)")
        }
        val initialDagSize = dag.size
        // Pre-order flattening not technically necessary, but it makes for the
        // most understandable MergedOp.toString() in the kernel DAG printout
        val kernels = dag.flattenPreorder
        for (kernel <- kernels) {
          if (!kernel.isDead) {
            kernel.opcode match {
              case op: ReshapeOp =>
                val okToRemove =  !kernel.outputs(0).probed
                if (okToRemove) {
                  kernel.inputs(0).stealSinksFrom(kernel.outputs(0))
                  kernel.removeFromCircuit(mustDo = true)
                }
                if (Cog.verboseKernelMerging) {
                  println("Removing " + kernel + " driven by " + kernel.inputs(0).source)
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
