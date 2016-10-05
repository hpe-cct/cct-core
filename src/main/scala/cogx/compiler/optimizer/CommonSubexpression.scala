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

import cogx.platform.opencl.OpenCLKernelCodeGenParams

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.collection.mutable.HashSet
import cogx.platform.types.AbstractKernel
import cogx.compiler.codegenerator.KernelCircuit
import cogx.parameters.Cog
import cogx.runtime.execution.Profiler

/** Performs common subexpression elimination on kernel circuits.
  *
  * @author Greg Snider
  */
private[cogx]
object CommonSubexpression extends Optimizer {

  /** Optimize `circuit` by removing redundant subexpressions.
    *
    * This looks for AbstractKernels with the same opcodes and inputs and
    * removes one of the them, copying over its sinks to the other.
    *
    * @param circuit Kernel circuit to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param profiler The profiler to use to pick the best variant
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(circuit: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, profiler: Profiler, report: Boolean = true): Int = {
    if (Cog.verboseOptimizer)
      println("    *** CommonSubexpression: starting (" + circuit.size + " nodes)")
    // The hash set here relies on some subtle properties of AbstractKernels
    // since the "equals" operator is used to determine membership in the
    // set. Two abstract kernels are equal if their opcodes are equal (using
    // "equals") and have the same identical inputs (using "eq").
    val uniqueKernels = new HashSet[AbstractKernel]() {
      // Scala 2.10 made findEntry protected: following is work-around.
      def unprotectedFindEntry(kernel: AbstractKernel) = findEntry(kernel)
    }
    val kernels: Seq[AbstractKernel] = circuit.flatten
    var done = false
    while (!done) {
      var removedKernels = 0
      for (kernel <- kernels) {
        // We skip over dead kernels completely.
        if (!kernel.isDead) {
          uniqueKernels.unprotectedFindEntry(kernel) match {
            case Some(original: AbstractKernel) =>
              // Transfer over 'probed' designations
              for (outIndex <- 0 until kernel.numOutputs) {
                if (kernel.outputs(outIndex).probed)
                  original.markProbed(outIndex)
              }
              original stealOutputsFrom kernel
              removedKernels += 1
            case None =>
              uniqueKernels += kernel
          }
        }
      }
      done = removedKernels == 0
      uniqueKernels.clear()
    }
    fixRecurrences(circuit)
    val removedKernels = kernels.length - circuit.size
    if (Cog.verboseOptimizer)
      printf("    *** CommonSubexpression: %d kernel%s removed.\n",
        removedKernels, if (removedKernels != 1) "s" else "")
    removedKernels
  }

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, profiler: Profiler, report: Boolean): Int =
    optimize(circuit, null.asInstanceOf[OpenCLKernelCodeGenParams], profiler, report)

  /** Since this optimizer doesn't rely on platform parameters, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit, profiler: Profiler): Int =
    optimize(circuit, null.asInstanceOf[OpenCLKernelCodeGenParams], profiler, true)

  /** Since this optimizer doesn't rely on platform parameters or profiler, we provide this simpler interface. */
  def optimize(circuit: KernelCircuit): Int =
    optimize(circuit, null.asInstanceOf[OpenCLKernelCodeGenParams], null.asInstanceOf[Profiler], true)
}
