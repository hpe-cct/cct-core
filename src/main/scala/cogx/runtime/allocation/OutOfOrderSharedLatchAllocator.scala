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
import cogx.platform.opencl.{OpenCLCpuKernel, OpenCLDevice}
import cogx.platform.types.{AbstractKernel, FieldType, VirtualFieldRegister}

import scala.collection.mutable.ArrayBuffer

/**
  * An allocator of shared latches that are guaranteed to produce correct results even with
  * an out-of-order command queue.
  *
  * Produces a set of shared latches guaranteed to work on any kernel launch order
  * that preserves the semantics of the calculation.  For an OpenCL out-of-order queue,
  * the kernels are allowed to run in an order unrelated to the order in which the kernels
  * were enqueued, although ordering established by event linking is preserved.
  */
class OutOfOrderSharedLatchAllocator extends SharedLatchAllocator {
  /** Process the kernel circuit from inputs to outputs, looking for latch
    * sharing opportunities.
    */
  def calcSharedLatches(circuit: KernelCircuit, device: OpenCLDevice, bufferType: BufferType,
                                requiresLatch: (VirtualFieldRegister) => Boolean) = {
    /** Already used SharedLatches, grouped by FieldType. */
    val latches = new collection.mutable.HashMap[FieldType, ArrayBuffer[SharedLatch]]()
    /** Map from a kernel to the set of kernels guaranteed to have executed before it. */
    val precursors =
      new IdentityHashMap[AbstractKernel, IdentityHashSet[AbstractKernel]]()
    /** Queue of kernels ready to have their outputs assigned to a latch. */
    val readyKernels = collection.mutable.Queue[AbstractKernel]()
    /** Map from a kernel to the number of input kernels yet to be processed. */
    val numPendingInputKernels = new IdentityHashMap[AbstractKernel, Int]()
    /** Tally of buffers optimized away through sharing */
    var buffersRemoved = 0
    /** Top-of-KernelCircuit kernels.  These 'seal' the latch they share.
      * Kernels outputs that drive recurrences have no apparent outputs. */
    val rootKernels = new IdentityHashSet[AbstractKernel]() {
      circuit.roots.foreach(this.add(_))
    }

    /** Are all the kernels that use the latch (the `lastConsumers`) guaranteed
      * to have executed (are they in the precursors set of this kernel)?
      */
    def latchOKToUse(latch: SharedLatch, kernel: AbstractKernel) = {
      if (latch.isSealed)
        false
      else {
        val kernelPrecursors = precursors(kernel)
        latch.lastConsumers.find(kernelUse => !kernelPrecursors.contains(kernelUse)) match {
          case Some(unexecutedKernel) => false
          case None => true
        }
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
          require(numPendingInputs > 0)
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
    circuit.leaves.foreach(input => postProcessKernel(input))

    // Main loop: kernels are processed after all their inputs have been
    // processed, at which point the kernel's precursor set is known to be complete.
    while (readyKernels.length > 0) {
      val kernel = readyKernels.dequeue()

      // Kernels driving no other kernels are dangerous for this approach, since
      // that suggests the output buffer assigned to it can be reused immediately.
      // These kernels are generally the top-of-circuit root kernels that we
      // wouldn't want to have sharing buffers
      kernel.outputs.foreach( virtualRegister => {
        if (requiresLatch(virtualRegister)) {
          // Any latch of the proper size?
          latches.get(virtualRegister.fieldType) match {
            case Some(sizedLatches) => // Found some latches of the proper size
              // Look for a latch whose `lastConsumers` are all known to have
              // executed (they are in this kernels precursors set)
              sizedLatches.find(latchOKToUse(_,kernel)) match {
                case Some(latch) => // Found a latch that can be used by this kernel
                  latch.addVirtualRegister(virtualRegister, sealLatch(virtualRegister))
                  buffersRemoved += 1
                case None => // No latch of proper size can be used; make a new one
                  sizedLatches += new SharedLatch(device, virtualRegister, sealLatch(virtualRegister), bufferType)
              }
            case None => // No latch of proper size exists, allocate a new one
              val newLatch = new SharedLatch(device, virtualRegister, sealLatch(virtualRegister), bufferType)
              latches.put(virtualRegister.fieldType, ArrayBuffer(newLatch))
          }
        }
      })
      postProcessKernel(kernel)
    }

    if (Cog.verboseOptimizer) {
      latches.foreach(sizeGroup => {
        val fieldType = sizeGroup._1
        val sizedLatches = sizeGroup._2
        println("********** " + fieldType + "**********")
        sizedLatches.foreach(sharedLatch => {
          print("Field Register(Latch) (" + sharedLatch.register.master + ", "  +
            sharedLatch.register.slave + ") ")
          println(sharedLatch.virtualRegisters.mkString("; "))
        })
      })
    }

    if (Cog.verboseOptimizer)
      println("    *** BufferSharing: " + buffersRemoved + " buffers removed.")

    // Convert map of FieldType->List[SharedLatch] to map of Kernel->SharedLatch
    val answer = new IdentityHashMap[VirtualFieldRegister, SharedLatch]() {
      latches.values.foreach(sizedLatches =>
        sizedLatches.foreach(latch =>
          latch.virtualRegisters.foreach(register => this.put(register, latch))
        )
      )
    }
    answer
  }

}
