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

import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}
import cogx.compiler.codegenerator.KernelCircuit
import cogx.parameters.Cog
import cogx.platform.cpumemory.BufferType
import cogx.platform.opencl.{OpenCLDeviceKernel, OpenCLAbstractKernel, OpenCLCpuKernel, OpenCLDevice}
import cogx.platform.types.{AbstractKernel, FieldType, VirtualFieldRegister}

import scala.collection.mutable.ArrayBuffer

/**
  * An allocator of shared latches that are guaranteed to produce correct results under the assumption
  * that kernels are executed in the order they are enqueued with no overlap.
  *
  * @author Dick Carter
  */
class InOrderSharedLatchAllocator(kernelEnqueueOrder: Seq[OpenCLAbstractKernel]) extends SharedLatchAllocator {
  /** Process the kernel circuit from inputs to outputs, looking for latch
    * sharing opportunities.
    */
  def calcSharedLatches(circuit: KernelCircuit, device: OpenCLDevice, bufferType: BufferType,
                                requiresLatch: (VirtualFieldRegister) => Boolean) = {
    /** Existing SharedLatches in a collection that permits intelligent sharing choices. */
    val latches = new SharedLatchesByUtilization
    /** Map from a kernel to the set of kernels guaranteed to have executed before it. */
    val precursors =
      new IdentityHashMap[AbstractKernel, IdentityHashSet[AbstractKernel]]()
    /** Queue of kernels ready to have their outputs assigned to a latch. */
    val readyKernels = new IdentityHashSet[AbstractKernel]()
    /** Map from a kernel to the number of input kernels yet to be processed. */
    val numPendingInputKernels = new IdentityHashMap[AbstractKernel, Int]()
    /** Tally of buffers optimized away through sharing */
    var buffersRemoved = 0
    /** Top-of-KernelCircuit kernels.  These 'seal' the latch they share.
      * Kernels outputs that drive recurrences have no apparent outputs. */
    val rootKernels = new IdentityHashSet[AbstractKernel]() {
      circuit.roots.foreach(this.add(_))
    }

    /** Kernels that have already executed. */
    val executedDeviceKernels = new IdentityHashSet[AbstractKernel]()

    /** Kernels that have already executed. */
    lazy val executedKernels = new IdentityHashSet[AbstractKernel]()

    /** Leaves of the kernel DAG- the Constant and Recurrence kernels should not be part of the
      * returned map- they will be given flip-flops or unshared latches by the caller of this routine.
      * Any device kernels (as is possible with the 0-input GPUOperators favored by the Profiler) become
      * "ready kernels".
      */
    val leaves = new IdentityHashSet[AbstractKernel]()
    circuit.leaves.foreach( _ match {
      case zeroInputDeviceKernel: OpenCLDeviceKernel =>
        precursors(zeroInputDeviceKernel) = new IdentityHashSet[AbstractKernel]()
        readyKernels += zeroInputDeviceKernel
      case otherLeaf => leaves.add(otherLeaf)
    })

    /** Are all the kernels that use the latch (the `lastConsumers`) guaranteed
      * to have executed (are they in the precursors set of this kernel)?
      */
    def latchOKToUse(latch: SharedLatch, kernel: AbstractKernel) = {
      if (latch.isSealed)
        false
      else {
        val kernelPrecursors = precursors(kernel)
        val properOKToUse =
          if (kernel.isInstanceOf[OpenCLDeviceKernel]) {
            latch.lastConsumers.find(kernelUse =>
              !kernelPrecursors.contains(kernelUse) && !executedDeviceKernels.contains(kernelUse)) match {
              case Some(unexecutedKernel) => false
              case None => true
            }
          }
          else {
            latch.lastConsumers.find(kernelUse =>
              !kernelPrecursors.contains(kernelUse)) match {
              case Some(unexecutedKernel) => false
              case None => true
            }
          }

        if (Cog.checkLegacyBufferSharing) {
          val problematicOKToUse =
            latch.lastConsumers.find(kernelUse => !executedKernels.contains(kernelUse)) match {
              case Some(unexecutedKernel) => false
              case None => true
            }

          if (problematicOKToUse && !properOKToUse)
            println("Warning: earlier Cog versions 4.1.47 thru 4.3.5 may have improperly run kernel " + kernel)
        }
        properOKToUse
      }
    }

    /** Propogate some kernel info to its sinks, then remove it from the maps */
    def postProcessKernel(kernel: AbstractKernel) = {
      // In prep for updating sinks' precursors, add this kernel to its set
      val precursorsPlusMe =
        precursors.getOrElseUpdate(kernel, new IdentityHashSet[AbstractKernel]())
      precursorsPlusMe += kernel

      var myPrecursorsPassedOnward = false
      kernel.outputs.foreach(output => {
        output.sinks.foreach(sink => {
          // Update the precursor sets for this kernel's sinks
          // The following is a performance optimization in which the precursor
          // sets can be just moved simply to a single sink without copying.
          precursors.get(sink) match {
            case Some(sinkPrecursors) =>
              sinkPrecursors.putAll(precursorsPlusMe)
            case None =>
              if (!myPrecursorsPassedOnward) {
                precursors(sink) = precursorsPlusMe
                myPrecursorsPassedOnward = true
              }
              else
                precursors(sink) = precursorsPlusMe.copy
          }
          // decrease the number of the sinks' pending inputs and maybe mark ready
          val numSinkInputs = sink.inputs.length
          var numPendingInputs =
            numPendingInputKernels.getOrElseUpdate(sink, numSinkInputs)
          require(numPendingInputs > 0, "InOrderSharedLatchAllocator: compiler internal error.")
          // Optimizers should have consolidated all uses of a given input to one, but just in case...
          var inputIndex = 0
          while (numPendingInputs > 0 && inputIndex < numSinkInputs) {
            if (output eq sink.inputs(inputIndex))
              numPendingInputs -= 1
            inputIndex += 1
          }
          numPendingInputKernels(sink) = numPendingInputs
          if (numPendingInputs == 0)
            readyKernels += sink   // add sink to readyKernels
        })
      })
      // Remove this kernel from the precursor map since it's no longer needed
      precursors.remove(kernel)
      if (kernel.isInstanceOf[OpenCLDeviceKernel])
        executedDeviceKernels.add(kernel)
      // Only maintain executedKernels if we care about warning the user of past misbehaviors
      if (Cog.checkLegacyBufferSharing)
        executedKernels.add(kernel)
    }

    /** Since GPU kernels currently do not mark the cpu-portion of their output
      * buffer invalid when run, any buffer looked at by a cpu kernel cannot be
      * further shared by a downstream kernel.  The problem is that when the
      * cpu kernel runs, it will do a read() of the input, which will mark the
      * buffer's cpu portion valid.  If that buffer is later written by a device
      * kernel that shares the buffer, the updated value will never be seen by
      * another cpu kernel or probe (the read of the new GPU data will never
      * be triggered, because the buffer's cpu data is already marked valid).
      *
      * We need to further study whether the asynchronous probe mechanism
      * can force a premature validation of the CPU portion of buffers, which
      * would present false data to CPU kernel readers (this is not a buffer
      * sharing issue).   XXX  -RJC
      * */
    def drivesCPUKernels(virtualRegister: VirtualFieldRegister) =
      virtualRegister.sinks.map(_.isInstanceOf[OpenCLCpuKernel]).foldLeft(false)(_ || _)

    /** Should this kernel, once it's permitted to share a latch, then prohibit
      * other kernels from being added to the kernel-pool for this latch?
      */
    def sealLatch(virtualRegister: VirtualFieldRegister) =
      rootKernels.contains(virtualRegister.source) ||
        virtualRegister.sinks.length == 0 ||
        virtualRegister.probed ||
        drivesCPUKernels(virtualRegister) ||
        virtualRegister.source.isInstanceOf[OpenCLCpuKernel]

    // First process inputs without allocating any latches
    leaves.foreach(input => postProcessKernel(input))

    // Main loop: kernels are processed after all their inputs have been
    // processed, at which point the kernel's precursor set is known to be complete.
    // Main loop: kernels are processed in the order in which they will be enqueued, skipping leaf kernels.
    kernelEnqueueOrder.filter(!leaves.contains(_)).foreach( kernel => {
      require(readyKernels.contains(kernel),
      "InOrderSharedLatchAllocator: kernel launch order doesn't respect kernel DAG dependencies:" + kernel)
      readyKernels -= kernel

      // Kernels driving no other kernels are dangerous for this approach, since
      // that suggests the output buffer assigned to it can be reused immediately.
      // These kernels are generally the top-of-circuit root kernels that we
      // wouldn't want to have sharing buffers
      kernel.outputs.foreach( virtualRegister => {
        if (requiresLatch(virtualRegister)) {
          // Any latch of the proper size?
          val candidateLatches = latches.get(virtualRegister.fieldType)
          candidateLatches.find(latchOKToUse(_,kernel)) match {
            case Some(latch) => // Found a latch that can be used by this kernel
              latch.addVirtualRegister(virtualRegister, sealLatch(virtualRegister))
              buffersRemoved += 1
            case None => // No latch of proper size can be used; make a new one
              val newLatch = new SharedLatch(device, virtualRegister, sealLatch(virtualRegister), bufferType)
              latches.addLatch(virtualRegister.fieldType, newLatch)
          }
        }
      })
      postProcessKernel(kernel)
    })

    if (Cog.verboseOptimizer) {
      latches.values.foreach(sharedLatch => {
        print("Field Register(Latch) (" + sharedLatch.register.master + ", "  +
          sharedLatch.register.slave + ") ")
        println(sharedLatch.virtualRegisters.mkString("; "))
      })
    }

    if (Cog.verboseOptimizer)
      println("    *** BufferSharing: " + buffersRemoved + " buffers removed.")

    // Convert the variously grouped SharedLatches into a map of Kernel -> SharedLatch
    val answer = new IdentityHashMap[VirtualFieldRegister, SharedLatch]() {
      latches.values.foreach(latch =>
          latch.virtualRegisters.foreach(register => this.put(register, latch))
        )
    }
    answer
  }

}
