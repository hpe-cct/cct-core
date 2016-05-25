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

import com.jogamp.opencl.{CLDevice, CLContext}
import cogx.cogmath.collection.{SynchronizedIdentityHashSet, IdentityHashSet}
import cogx.platform.types.FieldType
import cogx.platform.cpumemory.{BufferType, AbstractFieldMemory}
import cogx.parameters.Cog

/** Presents a simplified interface to an OpenCL device.
  *
  * We use a policy of exactly one device per context, and one command queue
  * per device, thus those abstractions are buried within this class.
  *
  * @author Greg Snider
  *
  * @param clDevice Low-level OpenCL device.
  * @param platform Cog's platform object associated with this device.
  * @param profile Should command queues used by this device enable profiling?
  */
private[cogx]
class OpenCLDevice private[opencl](val clDevice: CLDevice,
                                   val platform: OpenCLPlatform,
                                   profile: Boolean)
{
  /** Buffers for this device. */
  private var fieldBuffers = new IdentityHashSet[OpenCLBuffer[_]]
  /** Command queue used to hold all commands to the device. */
  val commandQueue = new OpenCLParallelCommandQueue(this, profile)
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
      _program = new OpenCLProgram(this)
    }
    _program
  }

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
    val clKernel = program.getProgram(resourceDescriptor).createCLKernel(kernelName)
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
                                                  bufferType: BufferType):
     OpenCLBuffer[T] =
  {
    var buffer: OpenCLBuffer[T] = null
    synchronized {
      buffer = new OpenCLBuffer[T](fieldType, commandQueue, bufferType, platform.fieldMemoryAllocator)
      fieldBuffers += buffer
    }
    buffer
  }

  /** Destroy a previously created field `buffer`, releasing all OpenCL
    * resources for it.
    *
    * @param buffer The buffer to be released.
    */
  private def destroyFieldBuffer(buffer: OpenCLBuffer[_]) {
    require(fieldBuffers contains buffer)
    synchronized {
      buffer.release()
      fieldBuffers -= buffer
    }
  }

  /** Total global memory footprint in bytes of the fieldBuffers assigned to this device. */
  def totalGlobalMemorySizeBytes = fieldBuffers.toSeq.map(_.globalMemorySize).foldLeft(0L)(_ + _)

  def globalMemoryFootprint: String = {
    val sizeBytes = totalGlobalMemorySizeBytes
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

  /** Release all OpenCL resources for this device, EXCEPT do not release
    * the command queue or the device itself. This leaves it ready to be
    * reused.
    */
  private[opencl] def release() {
    val resourcesToRelease =
      fieldBuffers.size + deviceKernels.size + cpuKernels.size +
              (if (_program != null) 1 else 0)

    if (Cog.verboseOpenCLDevices && resourcesToRelease > 0)
      println("Releasing OpenCLDevice resources: " + this.toString)

    // memory objects  (buffers)
    if (Cog.verboseOpenCLDevices && fieldBuffers.size > 0)
      printf("  releasing %d field buffers\n", fieldBuffers.size)
    fieldBuffers.foreach(destroyFieldBuffer(_))
    fieldBuffers = new IdentityHashSet[OpenCLBuffer[_]]

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
    // command queue
    if (Cog.verboseOpenCLDevices)
      println("  releasing command queue for " + this.toString)
    commandQueue.release
  }

  /** A string descriptor of the device, used for debugging. */
  override def toString = clDevice.toString
}