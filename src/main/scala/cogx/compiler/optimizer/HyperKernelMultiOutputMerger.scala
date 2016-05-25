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
import cogx.cogmath.collection.IdentityHashSetDeterministic
import cogx.platform.opencl.OpenCLPlatformParams

/** Optimizer of kernel DAGs.
  *
  * This looks for kernels that have the same set of input edges and merges them
  * "horizontally", creating a multi-output merged kernel. This is a conservative
  * approach that is mostly-likely always a performance win.  If instead we
  * allowed the kernels to have additional unshared inputs, then we must be
  * careful that the additional inputs are not dependent on any of the kernels'
  * outputs.  Also, the additional inputs might become available late in the
  * simulation cycle, thus unnecessarily delaying the launch of one of the
  * component kernels that lacks these additional inputs.
  *
  * @author Dick Carter
  */
private[cogx]
object HyperKernelMultiOutputMerger extends Optimizer {
  private[cogx] val Enabled = true

  /** "Horizontally" merge all HyperKernels in `dag` when possible.
    *
    * @param dag Kernel circuit to be optimized
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired
    * @return  The number of optimizations made
    */
  def optimize(dag: KernelCircuit, platformParams: OpenCLPlatformParams, report: Boolean = true): Int = {
    val answer =
      if (!Enabled) {
        if (Cog.verboseOptimizer) {
          println("    *** HyperKernel Multi-output Merger: disabled")
        }
        0
      }
      else {
        if (Cog.verboseOptimizer) {
          println("    *** HyperKernel Multi-output Merger: starting (" + dag.size + " nodes)")
        }
        val initialDagSize = dag.size
        // Pre-order flattening not technically necessary, but it makes for the
        // most understandable MergedOp.toString() in the kernel DAG printout
        val kernels = dag.flattenPreorder
        for (kernel <- kernels) {
          if (!kernel.isDead) {
            kernel match {
              case hyper: HyperKernel =>
                // merge candidates must come from the set of nodes on this kernel's
                // input edges.  Start with the set of nodes on the first input
                // edge, then gradually narrow the list by considering the remaining
                // input edges.
                var mergeCandidates: IdentityHashSetDeterministic[HyperKernel] = null
                hyper.inputs.foreach(inputEdge => if (mergeCandidates == null || mergeCandidates.size > 0) {
                  val lessMergeCandidates = new IdentityHashSetDeterministic[HyperKernel]()
                  inputEdge.sinks.foreach( k => k match {
                    case candidate: HyperKernel =>
                      if (hyper.numInputs == candidate.numInputs && !(hyper == candidate) &&
                              (mergeCandidates == null || mergeCandidates.contains(candidate)) &&
                              hyper.canShareMultiOutputKernel(candidate))
                        lessMergeCandidates += candidate
                    case _ =>
                  })
                  mergeCandidates = lessMergeCandidates
                })
                var mergedKernel = hyper
                if (mergeCandidates != null)
                  mergeCandidates.foreach( candidate => {
                    val biggerKernel = HyperKernel.doMerge(mergedKernel, candidate)
                    if (Cog.verboseKernelMerging) {
                      println("Merging " + candidate + " ---> " + mergedKernel)
                      println("Result is " + biggerKernel)
                      println("********************************************")
                    }
                    // Keep going with this merged kernel
                    mergedKernel = biggerKernel
                  })

              case _ =>
            }
          }
        }
        // We now have to fix up recurrences
        fixRecurrences(dag)
        val removedKernels = initialDagSize - dag.size
        if (Cog.verboseOptimizer)
          println("    *** Multi-output HyperMerger: " + removedKernels + " kernel" +
                  (if (removedKernels == 1) "" else "s") + " removed.")
        removedKernels
      }
    answer
  }

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, report: Boolean): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], report)

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], true)
}
