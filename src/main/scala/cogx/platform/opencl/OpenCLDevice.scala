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

package cogx.platform.opencl

import com.jogamp.opencl.{CLCommandQueue, CLDevice, CLEvent, CLEventList}
import cogx.cogmath.collection.{IdentityHashSet, SynchronizedIdentityHashSet}
import cogx.platform.types.{FieldMemoryLayoutImpl, FieldType}
import cogx.platform.cpumemory.{AbstractFieldMemory, BufferType}
import cogx.parameters.Cog
import com.jogamp.opencl.llb.CLDeviceBinding
import cogx.platform.opencl.OpenCLEventCache._
import com.jogamp.opencl.CLException.CLInvalidQueuePropertiesException

/** Presents a simplified interface to an OpenCL device.
  *
  * We use a policy of exactly one device per context, and one command queue
  * per device, thus those abstractions are buried within this class.
  *
  * @author Greg Snider
  * @param clDevice Low-level OpenCL device.
  * @param platform Cog's platform object associated with this device.
  * @param profilerUse Should command queues used by this device enable profiling?
  */
private[cogx]
class OpenCLDevice private[opencl](val clDevice: CLDevice,
                                   val platform: OpenCLPlatform,
                                   profilerUse: Boolean)
{
  /** Buffers for this device. */
  private var fieldBuffers = new IdentityHashSet[OpenCLBuffer]
  /** Command queue used to hold all commands to the device. */
  val commandQueue = new OpenCLParallelCommandQueue(this, profilerUse || Cog.profile)
  /** Compiled program to run on this device. */
  private var _program: OpenCLProgram = null// = new OpenCLProgram(this)
  /** Kernels which run on the CPU (but which require GPU buffers). */
  private val cpuKernels =
    new SynchronizedIdentityHashSet[OpenCLCpuKernel]
  /** Kernels which run on the device / GPU. */
  private val deviceKernels =
    new SynchronizedIdentityHashSet[OpenCLDeviceKernel]
  /** The context of this device. */
  def clContext = platform.clContext

  def program: OpenCLProgram = {
    if (_program == null) {
      _program = new OpenCLProgram(this, profilerUse)
    }
    _program
  }

  // Still developing a solution for the following problem:
  //
  // Suppose you are compiling and running multiple compute graphs simultaneously.  You wouldn't want the profiling
  // that takes place during compilation to be effected by the other compute graphs that might have started running.
  // We can probably assume that a kernel being run is never preempted, but what about switching in the middle of
  // a step?  If this occurs, the L2 cache behavior could be quite different.
  //
  // It's tempting to have a single "step" lock on a device (at least for all the ComputeGraphs running under a single JVM).
  // The commented lock below might be useful for that.  But what if the driver does preempt a thread mid step, while it
  // holds the device lock?  Would the new context have control of the GPU, but be locked-out of submitting kernels?
  // This might never occur, since why would the driver switch to a thread/context that has submitted no kernels to run?
  //
  // If a single lock proves to be a problem, one solution might be patterned after the reader/writer semaphore problem.
  // A profiler could be considered a "writer" that is granted exclusive use of the GPU, while a running ComputeGraph is
  // like a "reader" that can use the GPU with other readers, but not a writer.  We'd have to think about fairness so that
  // a Profiler would not be starved from use of the GPU if other ComputeGraphs were running.

  /** A unique object for the Device under this JVM. Better than using the device index because of possible filtering. */
  val lock = Symbol(clDevice.getID().toString())

  /** Strip name of extra non-identifying words. */
  private def cleanName(name: String) = name.replace(" Corporation", "").replace(", Inc.", "").replace("(R)", "")

  /** The manufacturer of the device, with some attempt to remove uninteresting bits. */
  def vendor: String = cleanName(clDevice.getVendor())

  /** The name of the device. */
  def name: String = cleanName(clDevice.getName())

  /** The name of the device with the vendor name added (if absent), suitable for the name of the Profiler cache file. */
  def fullNameNoSpaces: String = {
    val unsanitized =
      if (name.startsWith(vendor))
        name
      else
        s"$vendor $name"
    unsanitized.replaceAll("[^A-Za-z0-9]", "_")
  }

  /** Return true if this is an NVidia OpenCL platform. */
  def isNVidia =
    vendor.toLowerCase.contains("nvidia")

  /** Return true if this is an Intel OpenCL platform. */
  def isIntel =
    vendor.toLowerCase.contains("intel")

  /** Return true if this is an AMD OpenCL platform. */
  def isAMD =
    vendor.toLowerCase.contains("advanced micro devices")

  /** Function for getting around an apparent bug in AMD's implementation of
    * clWaitForEvents (though it could be a bug in JOCL instead).
    */
  def waitForEvents(events: Seq[CLEvent]): Unit = {
    if (isAMD) {
      // Wait for each event one at a time. AMD is rumored to have problems
      // if this is not done.
      for (event <- events)
        if (event != null)
          (new CLEventList(CLEventFactory, event)).waitForEvents
        else
          println("WARNING: null event found on event list !!!")
    } else {
      // Wait for all events (spec says this is legal).
      if (events.length > 0) {
        val eventList = new CLEventList(CLEventFactory, events: _*)
        eventList.waitForEvents
      }
    }
  }


  /** The number of threads launched into the pipeline simultaneously, with no synchronization required. */
  def warpSize: Int = {
    if (isNVidia) {
      try {
        clDevice.getCLAccessor.getLong(CLDeviceBinding.CL_DEVICE_WARP_SIZE_NV).toInt
      }
      catch {
        case e: Exception =>
          val default = 32
          println(s"Warning: attempt to query warpsize on device $this fails, using $default.")
          default
      }
    }
    else if (isAMD) {
      val CL_DEVICE_WAVEFRONT_WIDTH_AMD = 0x4043
      try {
        clDevice.getCLAccessor.getLong(CL_DEVICE_WAVEFRONT_WIDTH_AMD).toInt
      }
      catch {
        case e: Exception =>
          val default = 32 // Most AMD GPUs have a wavefront of 64, but older ones use 32.
          println(s"Warning: attempt to query wavefront size on device $this fails, using $default.")
          default
      }
    }
    else
      1  // We have little experience with other vendor GPUs or CPUs, so fall back to the conservative value 1
  }

  /** The amount of local memory ("shared memory" in NVIDIA's parlance) per streaming multiprocessor. */
  def localMemSize: Long = {
    val MaxLocalMemSizeExposed = 64 * 1024L
    math.min(clDevice.getLocalMemSize, MaxLocalMemSizeExposed)
  }

  /** The amount of global memory on the device. */
  def globalMemSize: Long = {
    clDevice.getGlobalMemSize
  }

  /** The largest allocation of constant memory permitted. */
  def maxConstantBufferSize: Long = {
    val MaxConstantBufferSizeExposed = 256 * 1024L
    math.min(clDevice.getMaxConstantBufferSize, MaxConstantBufferSizeExposed)
  }

  /** The largest global memory allocation of a single buffer permitted (in bytes). */
  def maxMemAllocSize: Long = {
    clDevice.getMaxMemAllocSize
  }

  /** The number of compute units ("streaming multiprocessors" in the NVIDIA parlance. */
  def maxComputeUnits: Long = {
    clDevice.getMaxComputeUnits
  }

  /** The number of compute units ("streaming multiprocessors" in the NVIDIA parlance. */
  def maxWorkgroupSize: Long = {
    clDevice.getMaxWorkGroupSize
  }

  // Not particularly useful- this is the sum of all the GPU's SM's L1 caches, not the L2 cache!
  /** The size of the GPU's cache of global memory (in bytes). */
  def globalMemCacheSize: Long = {
    clDevice.getGlobalMemCacheSize
  }

  /** Can the device execute kernels in parallel or out-of-order w.r.t. the CommandQueue enqueueing order? */
  private[cogx] lazy val supportsOutOfOrderCommandQueues = {
    val supported = clDevice.getQueueProperties.contains(CLCommandQueue.Mode.OUT_OF_ORDER_MODE)
    if (!supported)
      println(s"Warning: ignoring possible request for out-of-order command queue (not supported on $clDevice).")
    supported
  }

  /** Can the device measure kernel execution times? */
  private[cogx] lazy val supportsProfiledKernelExecution = {
    // The OpenCL spec says that profiling support is required, but just in case...
    val supported = clDevice.getQueueProperties.contains(CLCommandQueue.Mode.PROFILING_MODE)
    if (!supported)
      println(s"Warning: ignoring possible request for kernel execution profiling (not supported on $clDevice).")
    supported
  }


  /** A bundle of parameters that affect kernel code generation and optimization.  These values are
    * device-specific, so use these parameters only if the resulting kernel is guaranteed to be run
    * on this device, e.g. if the ComputeGraph is constructed with this device index as an argument. */
  def kernelCodeGenParams =
    OpenCLKernelCodeGenParams(
      maxMemAllocSize,
      maxConstantBufferSize,
      localMemSize,
      warpSize)

  /** Add device `kernel` to this device, but do not instantiate it, meaning
    * that it is not yet runnable.
    */
  def addKernel(kernel: OpenCLDeviceKernel) {
    program.addKernelSourceCode(kernel.kernelCode)
    deviceKernels += kernel
  }

  /** Add CPU `kernel` to this device, but do not instantiate it, meaning that
    * it is not yet runnable.
    */
  def addKernel(kernel: OpenCLCpuKernel) {
    cpuKernels += kernel
  }

  /** Instantiate device `kernel` so that it is now runnable. This will force
    * the allocation of OpenCL resources for the kernel.
    */
  def instantiateKernel(kernel: OpenCLDeviceKernel) {
    require(deviceKernels contains kernel)
    val kernelName = new KernelSourceCode(kernel.kernelCode).nameAsRun
    val clKernel = program.getProgram(resourceDescriptor _).createCLKernel(kernelName)
    require(clKernel != null, "Unable to create CLKernel for " + kernelName)
    kernel.instantiate(this, clKernel)
  }

  /** Instantiate CPU `kernel` so that it is now runnable. This will force the
    * allocation of OpenCL resources for the kernel.
    */
  def instantiateKernel(kernel: OpenCLCpuKernel) {
    require(cpuKernels contains kernel)
    kernel.instantiate(this)
  }

  /** Remove OpenCL resources assigned to this kernel so that it is no longer
    * runnable.
    */
  private def deinstantiateKernel(kernel: OpenCLDeviceKernel) {
    kernel.release()
  }

  /** Remove OpenCL resources assigned to this kernel so that it is no longer
    * runnable.
    */
  private def deinstantiateKernel(kernel: OpenCLCpuKernel) {
    kernel.release()
  }

  /** Create a field buffer for this device (living in GPU/CPU memory).
    *
    * @param fieldType The type of the scalar field held in the buffer.
    * @param bufferType The type of buffer to be allocated.
    */
  def createFieldBuffer[T <: AbstractFieldMemory](fieldType: FieldType,
                                                  bufferType: BufferType, gpuBufferCapacityBytes: Long): OpenCLBuffer =
  {
    def toMByte(sizeBytes: Long) = math.round(sizeBytes.toDouble/1024/1024)

    if (gpuBufferCapacityBytes > maxMemAllocSize)
      println(s"Warning: Field of type $fieldType has size ${toMByte(gpuBufferCapacityBytes)} MB, which exceeds the max size allocatable for device $this (${toMByte(maxMemAllocSize)} MB).  Expect badness.")
    val buffer: OpenCLBuffer = synchronized {
      val buf = new OpenCLBuffer(gpuBufferCapacityBytes, fieldType, commandQueue, bufferType, platform.fieldMemoryAllocator)
      fieldBuffers += buf
      buf
    }
    buffer
  }

  /** Create a field buffer for this device (living in GPU/CPU memory).
    *
    * @param fieldType The type of the scalar field held in the buffer.
    * @param bufferType The type of buffer to be allocated.
    */
  def createFieldBuffer[T <: AbstractFieldMemory](fieldType: FieldType, bufferType: BufferType): OpenCLBuffer =
  {
    createFieldBuffer(fieldType, bufferType, new FieldMemoryLayoutImpl(fieldType).longBufferSizeBytes)
  }

  /** Destroy a previously created field `buffer`, releasing all OpenCL
    * resources for it.
    *
    * @param buffer The buffer to be released.
    */
  private def destroyFieldBuffer(buffer: OpenCLBuffer) {
    require(fieldBuffers contains buffer)
    synchronized {
      buffer.release()
      fieldBuffers -= buffer
    }
  }

  /** Total global memory footprint in bytes of the fieldBuffers assigned to this device. */
  def allocatedGlobalMemorySizeBytes = fieldBuffers.toSeq.map(_.globalMemorySize).foldLeft(0L)(_ + _)

  def globalMemoryFootprint: String = {
    val sizeBytes = allocatedGlobalMemorySizeBytes
    if (sizeBytes < 10 * 1024)
      sizeBytes.toString + " bytes"
    else if (sizeBytes < 10 * 1024 * 1024)
      math.round(sizeBytes / 1024.0).toString  + " KB"
    else
      math.round(sizeBytes / (1024.0 * 1024.0)).toString  + " MB"
  }

  /** A description of resources used by this device. */
  def resourceDescriptor = {
    val resourceCount =
      fieldBuffers.size + deviceKernels.size + cpuKernels.size
    if (resourceCount > 0) {
      val snippet = new StringBuilder
      snippet append s"Allocated resources for ${this.toString}\n"
      snippet append s"  ${fieldBuffers.size} field buffers ($globalMemoryFootprint)\n"
      snippet append s"  ${deviceKernels.length} device kernels\n"
      snippet append s"  ${cpuKernels.length} CPU kernels\n"
      snippet.toString
    }
    else
      ""
  }

  /** Release all OpenCL resources for this device,  The device can no
    * longer be reused because of the release of the commandQueues.
    */
  private[opencl] def release() {
    releaseForReuse()
    // command queue
    if (Cog.verboseOpenCLDevices)
      println("  releasing command queue for " + this.toString)
    commandQueue.release
  }

  /** Release all OpenCL resources for this device, EXCEPT do not release
    * the command queue or the device itself. This leaves it ready to be
    * reused.
    */
  private[cogx] def releaseForReuse(): Unit = {
    val resourcesToRelease =
      fieldBuffers.size + deviceKernels.size + cpuKernels.size +
        (if (_program != null) 1 else 0)

    if (Cog.verboseOpenCLDevices && resourcesToRelease > 0)
      println("Releasing OpenCLDevice resources: " + this.toString)

    // memory objects  (buffers)
    if (Cog.verboseOpenCLDevices && fieldBuffers.size > 0)
      printf("  releasing %d field buffers\n", fieldBuffers.size)
    fieldBuffers.foreach(destroyFieldBuffer(_))
    fieldBuffers = new IdentityHashSet[OpenCLBuffer]

    // kernels
    if (Cog.verboseOpenCLDevices && deviceKernels.size > 0)
      printf("  releasing %d device kernels\n", deviceKernels.length)
    deviceKernels.foreach(deinstantiateKernel(_))
    deviceKernels.clear
    if (Cog.verboseOpenCLDevices && cpuKernels.size > 0)
      printf("  releasing %d CPU kernels\n", cpuKernels.length)
    cpuKernels.foreach(deinstantiateKernel(_))
    cpuKernels.clear

    // programs
    if (_program != null) {
      if (Cog.verboseOpenCLDevices)
        println("  releasing program")
      _program.getProgram().release()
      _program = null
    }
  }

  /** A string descriptor of the device, used for debugging. */
  override def toString = clDevice.toString
}