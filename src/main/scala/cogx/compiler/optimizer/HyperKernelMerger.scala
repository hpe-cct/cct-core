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
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLPlatformParams

/** Optimizer of kernel trees.
  *
  * This looks for chains and trees of mergeable HyperKernels and merges them.
  *
  * @author Greg Snider
  */
private[cogx]
object HyperKernelMerger extends Optimizer {

  /** Merge all HyperKernels in "dag" when possible.
    *
    * @param dag Kernel circuit to be optimized
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired
    * @return  The number of optimizations made
    */
  def optimize(dag: KernelCircuit, platformParams: OpenCLPlatformParams, report: Boolean = true): Int = {
    if (Cog.verboseOptimizer) {
      println("    *** HyperKernelMerger: starting (" + dag.size + " nodes)")
      //Timer.go
    }
    val initialDagSize = dag.size
    var done = false
    // Pre-order flattening not technically necessary, but it makes for the
    // most understandable MergedOp.toString() in the kernel DAG printout
    while (!done) {
      val kernels = dag.flattenPreorder
      var mergesThisPass = 0
      for (kernel <- kernels) {
        if (!kernel.isDead) {
          kernel match {
            case hyper: HyperKernel =>
              var sinkKernel = hyper
              while(sinkKernel != null) {
                sinkKernel.findMergeableInput match {
                  case Some(hyperIn) =>
                    val mergedKernel = HyperKernel.doMerge(sink = sinkKernel,  source = hyperIn)
                    if (Cog.verboseKernelMerging) {
                      println("Merging " + hyperIn + " ---> " + sinkKernel)
                      println("Result is " + mergedKernel)
                      println("********************************************")
                    }
                    // Keep going with this merged kernel
                    sinkKernel = mergedKernel
                    mergesThisPass += 1
                  case None =>
                    sinkKernel = null
                }
              }

            case _ =>
          }
        }
      }
      done = mergesThisPass == 0
    }
    // We now have to fix up recurrences
    fixRecurrences(dag)
    val removedKernels = initialDagSize - dag.size
    if (Cog.verboseOptimizer)
      println("    *** HyperMerger: " + removedKernels + " kernel" +
              (if (removedKernels == 1) "" else "s") + " removed.")
    removedKernels
  }

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, report: Boolean): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], report)

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], true)
}
