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
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.VirtualFieldRegister
import cogx.runtime.execution.Profiler

/** Interface for a component that optimizes a kernel circuit. Subclasses will
  * implement the "optimize" method that does an in-place optimization of the
  * kernel circuit passed to it, returning true if any improvements were made.
  *
  * @author Greg Snider
  */
private[cogx]
trait Optimizer {

  /** Compute and return an optimized DAG.
    *
    * @param dag  KernelCircuit possibly containing recurrent field kernels that is to be optimized.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param profiler The profiler to use to pick the best variant
    * @param report True if verbosity is desired.
    * @return  The number of optimizations made.
    */
  def optimize(dag: KernelCircuit, codeGenParams: OpenCLKernelCodeGenParams, profiler: Profiler, report: Boolean = true) : Int

  /** Fix up recurrences, post merge; MUST BE CALLED BY ALL OPTIMIZERS AT
    * THE COMPLETION OF THEIR OPTIMIZATION!
    *
    * Feedback paths from one kernel to another are not represented directly
    * in a KernelCircuit to prevent cycles (KernelCircuit must be acyclic). So
    * feedback is represented by an "out-of-band" pointer. The kernel
    * receiving the feedback is a RecurrentFieldKernel, which contains a
    * field that represents the kernel which is supplying the feedback. Since
    * the HyperKernel merger can't see that field, it must be patched up here.
    * The `mergeMap` contains the information we need. Every time a kernel is
    * removed by merging, a new kernel takes over its outputs. This maintains
    * the map of old to new so that we can fix things up.
    *
    * Similarly, Actuators maintain an "out-of-band" pointer to the kernel that
    * sources the actuator data stream, and this must be maintained as
    * optimizers remove the original kernel pointed to.
    *
    * @param dag KernelCircuit possibly containing recurrent field kernels that
    *        need to have their recurrent inputs fixed up.
    */
  protected def fixRecurrences(dag: KernelCircuit) {
    dag.traversePreorder {
      _ match {
        case rKernel: RecurrentFieldKernel =>
          rKernel.recurrence =
            dag.findStolenOutput(rKernel.recurrence).asInstanceOf[VirtualFieldRegister]
        case _ =>
      }
    }
  }
}