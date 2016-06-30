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

package cogx.runtime.allocation

import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.{OpenCLAbstractKernel, OpenCLDevice, OpenCLFieldRegister}
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.cogmath.collection.IdentityHashSet
import cogx.platform.cpumemory.{BufferType, DirectBuffer}
import cogx.platform.types.VirtualFieldRegister

import scala.collection.mutable.ArrayBuffer
import cogx.parameters.Cog

/** Allocates field registers for all kernels in a circuit.
  *
  * PINNED BUFFER NOTES: code for pinned buffers is in place and can be
  * activated by using PinnedDirectBuffer rather than DirectBuffer in the
  * `allocate` methods. However, one should be prudent. We really need another
  * BufferType in addition to {PinnedDirect, Direct, Indirect}Buffer. Perhaps
  * DeviceOnlyBuffer, to prevent unnecessary allocation of CPU memory.
  *
  * A good strategy might be to allocated Pinned buffers for sensors and
  * actuators, plus buffers which are read or written by a CPU kernel. Unprobed
  * kernels should probably get NO cpu buffer at all (using DeviceOnlyBuffer)
  * to reduce memory pressure.
  *
  * This class, being in the runtime module, should be independent of the backend
  * platform (e.g. OpenCL).  Currently is depends on OpenCLDevices, OpenCLKernels,
  * OpenCLFieldRegisters, etc.    XXX -RJC
  *
  * @author Greg Snider
  */
private[runtime]
object AllocateFieldRegisters {

  /** Enable virtual field registers to share OpenCl buffers */
  val BufferSharing = Cog.bufferSharing

  /** Allocate field registers for all kernels in a circuit.
    *
    * Sensors, actuators, and recurrences are allocated flip-flops. Recurrences
    * share their flip-flops with their drivers. Everything else gets latches.
    *
    * @param circuit All kernels in this circuit will be allocated field
    *        registers.
    * @param device The device where all kernels are executed (multiple devices
    *        not yet supported).
    */
  def apply(circuit: KernelCircuit, device: OpenCLDevice, kernelEnqueueOrder: Option[Seq[OpenCLAbstractKernel]], bufferType: BufferType) {
    // Find recurrence or actuator drivers. They share flip-flops with the
    // recurrences or actuators they drive, so we first need to know who they are.
    val recurrenceDrivers = new IdentityHashSet[VirtualFieldRegister]
    circuit.traversePostorder {
      _ match {
        case kernel: RecurrentFieldKernel =>
          recurrenceDrivers += kernel.recurrence
        case kernel =>
      }
    }

    /** Keeps the latch sharing optimizer in sync with the latch/register
     * allocator below.
     */
    def requiresLatch(register: VirtualFieldRegister) = {
      val kernel = register.source
      kernel match {
        case kernel: RecurrentFieldKernel => false
        case kernel: OpenCLAbstractKernel => !recurrenceDrivers.contains(register)
        case _ => throw new RuntimeException("Unexpected kernel type.")
      }
    }

    /**
     * Select the latch allocator based on a number of criteria.  The GPUs in single-GPU
     * systems can probably commit to a static enqueue order of kernels.  Those in multi-GPU
     * systems may wish to only enqueue kernels that are not blocked on inputs from other GPUs,
     * and so may prefer a dynamic enqueue order.
     *
     * If the kernel enqueue order is statically known and if the OpenCL queue is not
     * out-of-order, then a more aggressive latch sharing can be achieved by the InOrderSharedLatchAllocator.
     * Otherwise, the more conservative sharing produced by the OutOfOrderSharedLatchAllocator is used.
     */

    val latchAllocator = kernelEnqueueOrder match {
      case Some(enqueueOrder) =>
        if (device.commandQueue.outOfOrderExecution)
          new OutOfOrderSharedLatchAllocator
        else
          new InOrderSharedLatchAllocator(enqueueOrder)
      case None =>
        new OutOfOrderSharedLatchAllocator
    } 

    // Analyze circuit for places where latches can be shared by kernels
    // If BufferSharing is not enabled, this lazy val will never be evaluated.
    lazy val sharedLatch = latchAllocator.calcSharedLatches(circuit, device, bufferType, requiresLatch)

    // Allocate and bind output registers and latches for all. Note that
    // recurrence drivers must share a flip-flop with their recurrence.
    // If you change this match, update `requiresLatch` above!!
    circuit.traversePostorder {
      _ match {
        case kernel: RecurrentFieldKernel =>
          require(kernel.outputs.length == 1)
          allocateFlipFlop(device, kernel.outputs(0), bufferType)
          val driver = kernel.recurrence
          require(recurrenceDrivers contains driver)
          driver.bindRegister(kernel.outputs(0).register)
        case kernel: ConstantFieldKernel =>
          // We ignore the specified bufferType for ConstantFieldKernels- they write their (never shared)
          // output once at reset so no need to allocate a pinned buffer.
          require(kernel.outputs.length == 1)
            allocateLatch(device, kernel.outputs(0), DirectBuffer)
        case kernel: OpenCLAbstractKernel =>
          kernel.outputs.foreach( virtualRegister =>
            if (!recurrenceDrivers.contains(virtualRegister))
              allocateLatch(device, virtualRegister, bufferType)
          )
      }
    }

    /** Allocate and bind a flip-flop to `virtualRegister`. */
    def allocateFlipFlop(device: OpenCLDevice, virtualRegister: VirtualFieldRegister, bufferType: BufferType) {
        // See class comments about pinned buffers
        val buffer0 = device.createFieldBuffer(virtualRegister.fieldType, bufferType)
        val buffer1 = device.createFieldBuffer(virtualRegister.fieldType, bufferType)
        val register = new OpenCLFieldRegister(buffer0, buffer1)
        virtualRegister.bindRegister(register)
    }

    /** Allocate and bind a latch to `virtualRegister`. */
    def allocateLatch(device: OpenCLDevice, virtualRegister: VirtualFieldRegister, bufferType: BufferType) {
      val register =
        if (BufferSharing)
          sharedLatch.get(virtualRegister) match {
            case Some(latch) => latch.register
            case None => // No latch?  Must be a DAG leaf node like a constant input or sensor.
              allocateFieldLatch(device, virtualRegister, bufferType)
          }
        else {
          // See class comments about pinned buffers
          allocateFieldLatch(device, virtualRegister, bufferType)
        }
      virtualRegister.bindRegister(register)
    }
  }


  /** Allocate a FieldRegister and OpenCL Buffer for this kernel on the device */
  def allocateFieldLatch(device: OpenCLDevice, virtualRegister: VirtualFieldRegister, bufferType: BufferType) = {
    val buffer =
      device.createFieldBuffer(virtualRegister.fieldType, bufferType)
    new OpenCLFieldRegister(buffer)
  }

}