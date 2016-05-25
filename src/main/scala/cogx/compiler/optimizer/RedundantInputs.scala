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

/** Removes redundant (duplicated) inputs on kernels by replacing each such
  * kernel with another without the redundancy.
  *
  * @author Greg Snider
  */
private[cogx]
object RedundantInputs extends Optimizer {
  /** Replace kernels with redundant inputs in `kernelCircuit` with
    * kernels that have not redundant inputs.
    *
    * @param kernelCircuit Kernel circuit to be optimized
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired
    * @return  The number of optimizations made
    */
  def optimize(kernelCircuit: KernelCircuit, platformParams: OpenCLPlatformParams, report: Boolean = true): Int = {
    if (Cog.verboseOptimizer) {
      println("    *** RedundantInputs: starting (" + kernelCircuit.size + " nodes)")
      //Timer.go
    }
    var done = false
    var fixes = 0
    while (!done) {
      findBadKernel(kernelCircuit) match {
        case Some(kernel) =>
          val sleeker = HyperKernel.removeRedundantInputs(kernel)
          for (i <- 0 until kernel.numOutputs)
            sleeker.outputs(i).stealProbeAndNameFrom(kernel.outputs(i))
          fixes += 1
        case None =>
          done = true
      }
    }
    fixRecurrences(kernelCircuit)
    if (Cog.verboseOptimizer || (report && fixes > 0))
      println("    *** RedundantInputs: " + fixes + " kernel" +
              (if (fixes == 1) "" else "s") +
              " had duplicate inputs removed.")
    fixes
  }

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, report: Boolean): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], report)

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit): Int =
    optimize(circuit, null.asInstanceOf[OpenCLPlatformParams], true)

  /** Find a kernel in `dag` that has duplicated inputs, if any. */
  private def findBadKernel(dag: KernelCircuit): Option[HyperKernel] = {
    dag.traversePreorder {
      _ match {
        case hyper: HyperKernel =>
          if (hyper.hasDuplicatedInputs)
            return Some(hyper)
        case _ =>
      }
    }
    None
  }
}