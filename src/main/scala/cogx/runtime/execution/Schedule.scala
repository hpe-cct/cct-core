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

package cogx.runtime.execution

import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.OpenCLAbstractKernel
import scala.collection.mutable.{ArrayBuffer,HashMap}
import cogx.platform.types.AbstractKernel
import cogx.runtime.allocation.circuit.{OutputProxyKernel, InputProxyKernel}

/** A schedule is an ordered list of the kernels in a kernel circuit
  * for a single GPU which orders the kernels in the circuit so that they
  * can be executed bottom-up. Each kernel in the schedule has its input
  * dependencies precessing it in the sequence.
  *
  * @author Greg Snider
  */
private[runtime]
object Schedule {

  /** Orders the kernels in `circuit` for bottom-up execution. Each kernel
    * in the sequence has its input dependencies preceeding it in the sequence.
    *
    * @param circuit The kernel circuit to be scheduled.
    * @return Ordered sequence of kernels for bottom-up scheduling.
    */
  def apply(circuit: KernelCircuit): Seq[OpenCLAbstractKernel] = {
    val kernels = new ArrayBuffer[OpenCLAbstractKernel]
    circuit.traversePostorder {
      kernel => kernels += kernel.asInstanceOf[OpenCLAbstractKernel]
    }
    kernels.toSeq
  }

  /** Orders the kernels in `subcircuit` for bottom-up execution based on a
    * global ordering suggested by circuit.  If we don't do this, then a
    * deadly embrace can occur between two GPUs.
    *
    * @param subcircuit The partitioned kernel circuit to be scheduled.
    * @param circuit The unpartitioned circuit that `subcircuit` is part of.
    * @return Ordered sequence of kernels for bottom-up scheduling.
    */
  def apply(subcircuit: KernelCircuit, circuit: KernelCircuit): Seq[OpenCLAbstractKernel] = {

    /** This map has kernel IDs from both the original circuit (if the kernel is
      * proxied) and from the partitioned circuit.  Only IDs relevant to this
      * subcircuit are held.
      */
    val kernelIdToKernelMap = new HashMap[Int,AbstractKernel]
    subcircuit.traversePostorder {
      kernel =>
        kernel match {
          case ipk: InputProxyKernel => kernelIdToKernelMap.put(ipk.proxiedKernelId.kernelID, ipk)
          case opk: OutputProxyKernel => kernelIdToKernelMap.put(opk.proxiedKernelId.kernelID, opk)
          case x: AbstractKernel => kernelIdToKernelMap.put(x.id, x)
        }
    }

    val kernels = new ArrayBuffer[OpenCLAbstractKernel]
    circuit.traversePostorder {
      kernel =>
        val kernelAliasId = kernel.aliases match {
          case Some(set) =>
            require(set.size == 1)
            set.head
          case None =>
            -1
        }
        kernelIdToKernelMap.get(kernelAliasId) match {
          case Some(x) =>
            kernels += x.asInstanceOf[OpenCLAbstractKernel]
          case None =>
        }
        kernelIdToKernelMap.get(kernel.id) match {
          case Some(k) =>
            kernels += k.asInstanceOf[OpenCLAbstractKernel]
          case None =>
        }
    }
    require(subcircuit.size == kernels.size,
      "Internal compiler error: Missing some kernels during scheduling.")
    kernels.toSeq
  }
}