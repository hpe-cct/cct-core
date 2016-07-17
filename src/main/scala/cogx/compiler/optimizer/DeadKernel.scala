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
import cogx.compiler.codegenerator.opencl.cpukernels.ConstantFieldKernel
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.parameters.Cog
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.AbstractKernel

/** Removes kernels that don't contribute to any probed value (this includes kernels with
  * recurrences, since they are currently marked as probed).  This operation is equivalent
  * to "dead code elimination" in conventional compiler terminology.
  *
  * @author Dick Carter
  */
private[cogx]
object DeadKernel extends Optimizer {
  /** Remove kernels that drive no probed value or model state.
    *
    * @param kernelCircuit Kernel circuit to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(kernelCircuit: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, report: Boolean = true): Int = {
    if (Cog.verboseOptimizer) {
      println(s"    *** DeadKernel: starting (${kernelCircuit.size}} nodes)")
      //Timer.go
    }
    var done = false
    var numFixes = 0
    while (!done) {
      findUselessKernel(kernelCircuit) match {
        // Don't remove recursively, because probes not respected
        case Some(kernel) => kernel.removeFromCircuit(mustDo = true, recursive = false)
          numFixes += 1
        case None =>
          done = true
      }
    }
    // FixRecurrences clean-up probably not necessary since since kernels involved with recurrences should stay
    fixRecurrences(kernelCircuit)
    if (Cog.verboseOptimizer || (report && numFixes > 0)) {
      val kernels = if (numFixes == 1) "kernel" else "kernels"
      println(s"    *** DeadKernel: $numFixes dangling $kernels removed.")
    }
    numFixes
  }

  /** Find a kernel in `dag` that has duplicated inputs, if any. */
  private def findUselessKernel(dag: KernelCircuit): Option[AbstractKernel] = {

    def hasUsefulOutput(kernel: AbstractKernel) =
      kernel.outputs.exists(outEdge => outEdge.sinks.length > 0 || outEdge.probed)

    dag.traversePreorder { //kernel =>
      // Don't do this blindly for all AbstractKernels.  For example, the user could have written
      // a CPU "User kernel" with no useful outputs that is executed for its side-effects.
      _ match {
        case k: ConstantFieldKernel => if (!hasUsefulOutput(k)) return Some(k)
        case k: HyperKernel =>         if (!hasUsefulOutput(k)) return Some(k)
        case _ =>
      }
    }
    None
  }

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, report: Boolean): Int =
    optimize(circuit, null.asInstanceOf[OpenCLKernelCodeGenParams], report)

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit): Int =
    optimize(circuit, null.asInstanceOf[OpenCLKernelCodeGenParams], true)
}