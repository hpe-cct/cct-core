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

import java.nio.FloatBuffer

import com.jogamp.opencl._
import cogx.platform.cpumemory._
import com.jogamp.opencl.CLMemory.Mem
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import com.jogamp.opencl.CLImageFormat.{ChannelType, ChannelOrder}

/** OpenCL buffers that hold fields. Each field consists of an optional
  * CPU (host) part and a GPU (device) part. See base class for a more
  * complete description.
  *
  * @param fieldType Type of the field held in the buffer.
  * @param commandQueue OpenCL command queue available to this buffer.
  * @param bufferType The type of buffer to be allocated.
  * @param fieldMemoryAllocator The allocator to use for the cpu-side memory.
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
class OpenCLBuffer[T <: AbstractFieldMemory] private[opencl]
  (val fieldType: FieldType,
   commandQueue: OpenCLParallelCommandQueue,
   bufferType: BufferType,
   fieldMemoryAllocator: FieldMemory)
        extends AbstractFieldBuffer[T]
{
  /** Debug flag. */
  private val Verbose = false
  /** Flag that's true when cpuMemory holds data equal to the GPU data. */
  private var _cpuMemoryValid = false
  /** Synchronization object for changes to cpuMemoryValid */
  private val cpuMemoryValidLock = new AnyRef()
  /** Synchronization object for changes to the clMemory based on cpuMemory() and deviceBuffer() calls.
    * If cpuMemory() is called first, followed by deviceBuffer(), then the clMemory will be
    * created with the cpuMemory's NIO buffer.  However, if deviceBuffer() is called before cpuMemory(),
    * the clMemory is created without an associated NIO buffer.  Only if and when cpuMemory() is called
    * does the NIO buffer get created and bound to the clMemory (via the "cloneWith" call).
    *
    * Note that clImages are not used extensively, and usually at the periphery of the KernelCircuit DAG,
    * so they always allocate their NIO buffer (i.e. not in a lazy fashion).
    */
  private val clMemoryLock = new AnyRef()

  /** Unique ID for the buffer, used only for debugging. */
  private val id = OpenCLBuffer.add()

  /** Memory on the CPU side, lazily instantiated. */
  private var _cpuMemory = null.asInstanceOf[T]

  private def clContext = commandQueue.clContext

  /** Memory on the CPU side, lazily instantiated. */
  def cpuMemory: T = clMemoryLock.synchronized {
    if (_cpuMemory == null) {
      _cpuMemory = bufferType match {
        case PinnedDirectBuffer =>
          require(commandQueue != null, "need command queue for pinned buffer")
          fieldMemoryAllocator.pinned(fieldType, commandQueue).asInstanceOf[T]
        case DirectBuffer =>
          fieldMemoryAllocator.direct(fieldType).asInstanceOf[T]
        case IndirectBuffer =>
          fieldMemoryAllocator.indirect(fieldType).asInstanceOf[T]
      }
      /** Attach the NIO buffer to a clMemory if it already exists. */
      if (!isImage2dBuffer && _deviceBuffer != null)
        _deviceBuffer = _deviceBuffer.cloneWith[FloatBuffer](_cpuMemory.directBuffer.asInstanceOf[FloatBuffer])
    }
    _cpuMemory
  }

  /** OpenCl Memory lazily instantiated with optional NIO-buffer component. */
  private var _deviceBuffer: CLMemory[_] = null

  /** Memory on the device side. */
  def deviceBuffer: CLMemory[_] = clMemoryLock.synchronized[CLMemory[_]] {
    if (_deviceBuffer == null) {
      _deviceBuffer = if (isImage2dBuffer) {
        // UNORM_INT8 maps 0x00 -> 0xFF channel bytes to 0.0f -> 1.0f on the GPU
        val imageFormat = new CLImageFormat(ChannelOrder.RGBA, ChannelType.UNORM_INT8)
        val width = fieldType.fieldShape(1)
        val height = fieldType.fieldShape(0)
        clContext.createImage2d(cpuMemory.directBuffer, width, height, imageFormat,
          Mem.READ_WRITE)
      } else if (_cpuMemory == null) {
        /** If there's no _cpuMemory, we may never need one (this could be a buffer written
          * and read by GPU kernels for a field that is never probed).  Rather than allocate
          * an NIO buffer, just create the clMemory based on the size of the buffer with no
          * CPU-side component.
          */
        val bufSizeBytes = (new FieldMemoryLayout(fieldType)).bufferSizeBytes
        clContext.createBuffer(bufSizeBytes, Mem.READ_WRITE)
      } else {
        clContext.createBuffer(cpuMemory.directBuffer, Mem.READ_WRITE)
      }
    }
    _deviceBuffer
  }

  /** The size in bytes of the global memory footprint of this buffer. */
  def globalMemorySize = deviceBuffer.getCLSize()

  /** Start an asynchronous read of a field from GPU memory --> CPU memory,
    * returning an event which will signal completion of the read, or None
    * if no read is needed.
    *
    * @return Some(event) if a synchronous read is necessary to bring the
    *         field data to the CPU; None if the CPU cache is valid and no
    *         read is required.
    */
  /*
  def prefetch(): Option[CLEvent] = {
    if (!cpuMemoryValid) {
      val event = CLUserEvent.create(context)
      if (isImage2dBuffer)
        commandQueue.putReadImage2d(deviceBuffer.asInstanceOf[CLImage2d[_]],
          new CLEventList(CLEventFactory, event))
      else
        commandQueue.putReadBuffer(deviceBuffer.asInstanceOf[CLBuffer[_]],
          new CLEventList(CLEventFactory, event))
      // We eagerly
      cpuMemoryValid = true

      // Transfer started, return event that will trigger when done.
      Some(event)
    } else {
      // Cached copy is valid.
      None
    }
  }
  */

  /** Read the field. Synchronized to avoid unnecessary duplication of reads */
  def read: T = cpuMemoryValidLock.synchronized {
    if (!_cpuMemoryValid) {
      if (isImage2dBuffer)
        copyImage2dFromGPU()
      else
        copyFromGPU()
      _cpuMemoryValid = true
    }
    cpuMemory
  }

  /** Write the field synchronously to the GPU, blocking until complete. */
  def write: Unit =  {
    if (Verbose)
      println(toString + ".write START...")
    // If we're writing from cpu memory to the gpu, the cpu memory BETTER
    // be valid. It's the caller's resonsibility to ensure that.
    _cpuMemoryValid = true
    if (isImage2dBuffer)
      copyImage2dToGPU()
    else
      copyToGPU()
    if (Verbose)
      println(toString + ".write DONE")
  }

  /** Invalidates the cpuMemory, should be called whenever it gets stale
    * due to a kernel computation.
    */
  def invalidateCpuMemory: Unit = cpuMemoryValidLock.synchronized {
    _cpuMemoryValid = false
  }

  /** Invalidates the cpuMemory, should be called whenever it gets stale
    * due to a kernel computation.
    */
  def cpuMemoryValid: Boolean = cpuMemoryValidLock.synchronized {
    _cpuMemoryValid
  }

  /** Internal test of buffer type.
    *
    * @return True if this is an image buffer, false for ordinary buffer.
    */
  private def isImage2dBuffer =
    fieldType.elementType == Uint8Pixel

  /** Internal test of buffer type.
    *
    * @return True if this is an image buffer, false for ordinary buffer.
    */
  private def isComplexBuffer =
    fieldType.elementType == Complex32

  /** Release OpenCL buffer resource (garbage collector gets direct buffer). */
  private[cogx] def release() {
    deviceBuffer.release()
  }

  private[cogx] def isReleased: Boolean = {
    deviceBuffer.isReleased
  }

  /** Copy field data from cpuMemory to the GPU memory, blocking until
    * completion.
    */
  private def copyToGPU() {
    // touch cpuMemory to make sure it's instantiated
    cpuMemory
    commandQueue.putWriteBuffer(deviceBuffer.asInstanceOf[CLBuffer[_]])
  }

  /** Copy field data to cpuMemory from the GPU memory, blocking until
    * completion.
    */
  private def copyFromGPU() {
    // touch cpuMemory to make sure it's instantiated
    cpuMemory
    commandQueue.putReadBuffer(deviceBuffer.asInstanceOf[CLBuffer[_]])
  }

  /** Copy image from cpuMemory to the GPU memory, blocking until
    * completion.
    */
  private def copyImage2dToGPU(blocking: Boolean = true) {
    commandQueue.putWriteImage2d(deviceBuffer.asInstanceOf[CLImage2d[_]])
  }

  /** Copy field data to cpuMemory from the GPU memory, blocking until
    * completion.
    */
  private def copyImage2dFromGPU(blocking: Boolean = true) {
    commandQueue.putReadImage2d(deviceBuffer.asInstanceOf[CLImage2d[_]])
  }

  /** Debugging string. */
  override def toString: String =
    "OpenCLBuffer" + id

  //////////////////////////////////////////////////////////////////////////////
  //
  // The remaining methods may not be needed anymore
  //
  //////////////////////////////////////////////////////////////////////////////


  /** Debug flag. */
  val VerboseCopy = false
  /** Enable multiple threads to perform copies for big fields */
  val ParallelCopy = true
  /** To protect OS, limit the number of threads any copy can use */
  val MaxCopyThreads = 8   // This setting should be 1 or more
  /** Minimum field elements before additional copy threads get created */
  val FloatsPerCopy = 1024L * 1024  // represents about 1 msec of work

  /** Copy data "from" --> "to". */
  /*
  protected def copyFloatToBuffer(from: Array[Float], to: CLMemory[FloatBuffer]) {
    if (VerboseCopy)
      println("Copy CPU --> GPU: " + from.length + " Floats.")
    to.getBuffer.put(from)
    to.getBuffer.rewind
  }
  */

  /** Copy data "from" --> "to". */
  /*
  protected def copyBufferToFloat(from: CLMemory[FloatBuffer], to: Array[Float]) {
    if (VerboseCopy)
      println("Copy CPU <-- GPU: " + to.length + " Floats.")
    val buffer = from.getBuffer
    buffer.get(to)
    buffer.rewind
  }
  */

  /** Copy data "from" --> "to". */
  /*
  protected def copyFloatToDirectBuffer(from: Array[Float], toDirect: FloatBuffer) {
    if (VerboseCopy)
      println("Copy CPU --> Direct Memory: " + from.length + " Floats.")
    //    val startTime = java.lang.System.nanoTime()
    if (ParallelCopy)
      copyFloatToDirectBufferParallel(from, toDirect)
    else
      toDirect.put(from)
    //    val endTime = java.lang.System.nanoTime()
    //    val durationMS = (endTime - startTime) / 1000000.0
    //    println("Copy took " + durationMS + " ms")
    toDirect.rewind
  }
  */

  /** Uses scala futures to spread out copy job over multiple threads */
  /*
  private def copyFloatToDirectBufferParallel(from: Array[Float], toDirect: FloatBuffer) {

    def partialCopy(buffer: FloatBuffer, startIndex: Int, untilIndex: Int) {
      buffer.position(startIndex)
      val copyLength = untilIndex - startIndex
      buffer.put(from, startIndex, copyLength)
    }
    val totalLength = from.length.toLong
    val desiredThreads = 1 + totalLength/FloatsPerCopy
    val numCopyThreads = math.min(desiredThreads, MaxCopyThreads).toInt

    throw new RuntimeException("Code needs to be ported to Akka actors.")
  }
  */

  /** Copy data "from" --> "to". */
  /*
  protected def copyDirectBufferToFloat(fromDirect: FloatBuffer, to: Array[Float]) {
    if (VerboseCopy)
      println("Copy CPU <-- GPU: " + to.length + " Floats.")
    fromDirect.get(to)
    fromDirect.rewind
  }
  */
}

/** Companion object for OpenCLBuffer.
  */
private[cogx]
object OpenCLBuffer {
  /** Number of buffers that have been allocated. */
  private var bufferCount = 0

  /** Create a unique ID for a buffer. */
  private def add(): Int = {
    bufferCount += 1
    bufferCount
  }
}

