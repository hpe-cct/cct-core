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

import cogx.parameters.Cog
import cogx.platform.opencl._
import cogx.utilities.AnnotateThread
import com.jogamp.opencl.CLEvent
import cogx.runtime.allocation.AllocateFieldRegisters
import cogx.platform.types.AbstractKernel
import scala.collection.mutable
import cogx.platform.cpumemory.AbstractFieldMemory
import cogx.runtime.FieldID
import cogx.runtime.resources.GPU

/** Actor which supervises computation of a kernel circuit on a GPU.
  *
  * @param device OpenCL GPU managed by this supervisor.
  * @param gpu An object that allows late binding of the orderedKernels sequence (for multinode)
  *
  * @author Greg Snider
  */
private[runtime]
class GPUSupervisor(device: OpenCLDevice, gpu: GPU)
{
  // Bring `circuit` and `orderedKernels` into scope. */
  import gpu._

  // The following approach to producing `orderedKernels` resulted in deadlocks
  // in a multi-GPU setting.  The responsibility of determining the order in which
  // kernels are added to the queues must be done by the partitioner unless
  // a smarter (cpu-kernel-driven ?) dynamic scheduler is adopted.  -RJC

  /** Kernels ordered so that they can be executed bottom-up. */
//  private val orderedKernels: Seq[OpenCLAbstractKernel] = Schedule(circuit)


  /** The outputs of the computation. */
  private val rootKernels: Seq[OpenCLAbstractKernel] = {
    val sequence = circuit.roots
    sequence.map(_.asInstanceOf[OpenCLAbstractKernel])
  }

  /** Map of kernel ID to kernel. */
  private val idToKernel = new mutable.HashMap[Integer, AbstractKernel] {
    for (kernel <- orderedKernels)
      this(kernel.id) = kernel
  }

  /** Output events at "roots" that will be triggered when computation is done.*/
  private val rootOutputTriggers = new Array[CLEvent](rootKernels.length)

  /** Pre start initialization and allocation. */
  def preStart() {

    // Add Cog function to thread name to aide thread probing tools like jconsole
    AnnotateThread(getClass.getSimpleName + "-" + gpu.deviceIndex)

    // Allocate registers for the fields.
    AllocateFieldRegisters(circuit, device, Some(orderedKernels))

    // Install the kernels.
    for (kernel <- orderedKernels)
      installKernel(kernel)


    // Instantiate the kernels.
    for (kernel <- orderedKernels) {
      kernel match {
        case k: OpenCLCpuKernel =>
          device.instantiateKernel(k)
        case k: OpenCLDeviceKernel =>
          device.instantiateKernel(k)
        case x =>
          throw new RuntimeException("Illegal AbstractKernel class: " +
                  x.getClass.toString)
      }
    }
  }

  /** Post stop deallocation. */
  def postStop() {
    // TO DO XXX   deallocate field registers, uninstall / deinstantiate kernels
  }

  /** Reset the computation to its initial state. */
  def reset() {
    for (kernel <- orderedKernels)
      kernel.reset()

    // At this point we need to do a single evaluation to "prime" the network
    // for feedback. The problem is that the kernel buffers, other than the
    // inputs, contain garbage at this point, so we need to propagate the
    // inputs to the outputs so that every buffer contains valid data as a
    // result of being reset.
    evaluate()

    if (Cog.profile)
      for (kernel <- orderedKernels)
        kernel.stats.reset()
  }

  /** Advance the computation by one step. */
  def evaluate() {

    // Clock output registers, mark masters invalid since we're about to
    // overwrite them
    for (kernel <- orderedKernels)
      kernel.phase0Clock()

    // Keep a queue of the last 2 kernels to aide error reporting for launch failures
    var lastKernel: OpenCLAbstractKernel = null
    var nextToLastKernel: OpenCLAbstractKernel = null
    // Clock the circuit bottom-up, being prepared to handle kernel launch failures
    for (kernel <- orderedKernels) {
      try {
        kernel.phase1Clock()
        nextToLastKernel = lastKernel
        lastKernel = kernel
      }
      catch {
        case e: Exception =>
          val rethrownMsg = new StringBuilder("\nLaunch of the following GPU kernel generated an exception: " + kernel)
          if (lastKernel != null)
            rethrownMsg append "\nThe kernel launched one before this: " + lastKernel
          if (nextToLastKernel != null)
            "\nThe kernel launched two before this (*** THE KERNEL LIKELY TO HAVE CAUSED A RUNTIME EXCEPTION UNDER NVIDIA OPENCL ***): " + nextToLastKernel
          rethrownMsg append "\nOpenCL exception message: " + e.getMessage()
          rethrownMsg append "\nOpenCL exception stack trace:\n" + e.getStackTrace
          throw new RuntimeException(rethrownMsg.toString)
      }
    }

    ///////////////////////////////////////////////////////////////////////////
    //
    // We now do a "hard" synchronization. This is inappropriate for dynamic
    // pipelining, but is simpler for now. This must be revisited when we
    // implement "running" directly rather than the current "single stepping"
    // and especially when we move dynamic pipelining (where the flush would
    // undesirable). XXX

    // My interpretation of the OpenCL 1.1 spec, Appendix A is this:
    //
    // If you have multiple command queues manipulating the same memory object,
    // event objects should be used to ensure the proper sequencing of reads
    // and writes of the memory object.  Also, when a command in queue "A" is set
    // to wait for a command in queue "B", queue "B" should be flushed to ensure
    // the device driver actually pushs the queue "B" command to the device
    // (instead of using some lazy evaluation technique perhaps).
    //
    // Now earlier the code below did a "clfinish," but I believe only a clflush
    // is required.  Technically, the OpenCLWaitForEvents call does an implicit
    // clflush, so even the clflush is probably not necessary.  However, since
    // the OpenCLWaitForEvents is not associated with a queue directly, I felt
    // is safer to perform the clflush, whose execution time is probably
    // hidden behind the subsequent WaitForEvents.    -RJC

    device.commandQueue.flush()

    // Wait for roots to finish. Actuators run as CPU kernels, so even though
    // all GPU processes have completed at this point, actuators may not have.

    // Now wait for roots/outputs of computation to finish, then return.
    for (r <- 0 until rootKernels.length) {
      rootOutputTriggers(r) = rootKernels(r).done.getEvent(0)
      require(rootOutputTriggers(r) != null)
    }
    device.platform.waitForEvents(rootOutputTriggers)

    // End of hard synchronization
    //
    ///////////////////////////////////////////////////////////////////////////

    // We now have to release all events in all kernels.
    for (kernel <- orderedKernels)
      kernel.phase2Clock()

    // Print out kernel statistics every 'ProfileSize' cycles
    if (Cog.profile && orderedKernels.size > 0) {
      val kernelsWithSamples =
        orderedKernels.filter(kernel => kernel.stats.totalSamples > 0 &&
                kernel.stats.totalSamples % Cog.profileSize == 0)
      val slowToFastKernels =
        kernelsWithSamples.sortWith(_.stats.avg > _.stats.avg)
      val slowToFastDeviceKernels = slowToFastKernels.filter(_.isInstanceOf[OpenCLDeviceKernel])
      val slowToFastCpuKernels = slowToFastKernels.filter(_.isInstanceOf[OpenCLCpuKernel])
      if (slowToFastKernels.size > 0) {
        println("************ kernel execution times (taken over " +
                Cog.profileSize + " steps) ************")
        println()
        val numDeviceKernels = slowToFastDeviceKernels.size
        if (numDeviceKernels > 0) {
          println("Device kernels: ")
          for (kernel <- slowToFastDeviceKernels)
            println("  " + kernel.stats + " | " + kernel)
          println
        }
        if (slowToFastCpuKernels.size > 0) {
          println("CPU kernels: ")
          for (kernel <- slowToFastCpuKernels)
            println("  " + kernel.stats + " | " + kernel)
          println
        }
        val totalAvgDeviceUsecs = slowToFastDeviceKernels.map(_.stats.avg).foldLeft(0.0)(_ + _)
        // Number below is a swag, as it varies depending on OS and kernel DAG structure
        // Value of 15.0 was set to accurately predict the speed of the performance.detection.TrainNetwork app
        // on a Titan Black on linux with no profiling.
        val KernelLaunchOverheadUsecs = 15.0
        val totalAvgDeviceUsecsWithOverhead =
          slowToFastDeviceKernels.map(k => math.max(KernelLaunchOverheadUsecs,k.stats.avg)).foldLeft(0.0)(_ + _)
        val maxAvgCpuUsecs = slowToFastCpuKernels.map(_.stats.avg).foldLeft(0.0)(_ max _)


        if (numDeviceKernels > 0)
          printf("Total avg execution time of %d device kernels = %.1f usec (sim freq = %.1f Hz)\n\n",
            slowToFastKernels.size, totalAvgDeviceUsecs, 1000000.0/totalAvgDeviceUsecs)
        if (slowToFastCpuKernels.size > 0)
          printf("Max Cpu kernel execution time = %.1f usec\n\n", maxAvgCpuUsecs)

        val totalKernelLaunchOverheadUsecs =  totalAvgDeviceUsecsWithOverhead - totalAvgDeviceUsecs
//        val totalKernelLaunchOverheadUsecs =  numDeviceKernels * KernelLaunchOverheadUsecs
//        val deviceCycleTimeWithOverhead = totalAvgDeviceUsecs + totalKernelLaunchOverheadUsecs
        val cycleTimeDictatedByMaxCpuKernel = maxAvgCpuUsecs > totalAvgDeviceUsecsWithOverhead
//        val cycleTimeDictatedByMaxCpuKernel = maxAvgCpuUsecs > deviceCycleTimeWithOverhead
        val cycleTimeEstimateUsecs =
          if (cycleTimeDictatedByMaxCpuKernel) {
            println("Cycle time dictated by longest CPU kernel!")
            maxAvgCpuUsecs
          }
          else
            totalAvgDeviceUsecsWithOverhead
//            deviceCycleTimeWithOverhead

        val cycleTimeEstimateSecs = cycleTimeEstimateUsecs/1000000
        val estimatedSimFreq = 1/cycleTimeEstimateSecs
        printf("Estimated execution time with %.0f usec launch overhead = %.1f usec (sim freq = %.1f Hz)\n\n",
          KernelLaunchOverheadUsecs, cycleTimeEstimateUsecs, estimatedSimFreq)
        if (!cycleTimeDictatedByMaxCpuKernel)
          printf("Estimated kernel launch overhead = %.1f%%\n\n",
            100.0 * totalKernelLaunchOverheadUsecs / cycleTimeEstimateUsecs)

        slowToFastKernels.foreach(_.stats.reset())
      }
    }

  }

  /** Install a kernel so it can be executed. */
  private def installKernel(kernel: AbstractKernel) {
    kernel match {
      case k: OpenCLCpuKernel =>
        device.addKernel(k)
      case k: OpenCLDeviceKernel =>
        device.addKernel(k)
      case x =>
        throw new RuntimeException("Illegal AbstractKernel class: " +
                x.getClass.toString)
    }
  }

  /** Read a field into CPU memory.
    *
    * @param fieldID Unique identifier of the field.
    * @return The field in a CPU memory buffer.
    */
  def readField(fieldID: FieldID): AbstractFieldMemory = {
    val kernel = idToKernel.get(fieldID.kernelID) match {
      case Some(_kernel) => _kernel
      case None => throw new RuntimeException(s"Internal compiler error: read of field $fieldID is not possible.")
    }
    val kernelOutput = fieldID.kernelOutput
    val buffer =
      kernel.asInstanceOf[OpenCLAbstractKernel].outputRegister(kernelOutput).slave
    val memory = device.platform.fieldMemoryAllocator.copy(buffer.read.asInstanceOf[AbstractFieldMemory])
    memory
  }
}