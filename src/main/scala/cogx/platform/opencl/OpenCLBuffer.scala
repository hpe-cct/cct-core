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

import java.lang.reflect.Constructor
import java.nio.{Buffer, FloatBuffer}

import cogx.platform.constraints.FieldLimits
import com.jogamp.opencl._
import cogx.platform.cpumemory._
import com.jogamp.opencl.CLMemory.Mem
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import com.jogamp.opencl.CLImageFormat.{ChannelOrder, ChannelType}
import cogx.platform.opencl.OpenCLEventCache._
import com.jogamp.opencl.CLException.CLMemObjectAllocationFailureException
import com.jogamp.opencl.llb.CL

/** OpenCL buffers that hold fields. Each field consists of an optional
  * CPU (host) part and a GPU (device) part. See base class for a more
  * complete description.
  *
  * These buffers are shared amongst VirtualFieldRegisters in a KernelCircuit, but a VFR
  * driving a CPU kernel or one that is probed will prevent the shared buffer from being
  * used further.  Thus, while there may be multiple different-sized uses of the GPU portion,
  * the CPU memory need only look like one specific FieldType.  A future goal for this
  * class is to tease away any notion of the FieldType.  This may not be totally feasible
  * since "image" memories and "buffer" memories can not be shared even if they are the same size.
  *
  * @param gpuBufferCapacityBytes The maximum size field this buffer can hold in GPU memory.
  * @param cpuUseFieldType Type of the field held in the buffer (as viewed from the CPU)
  * @param commandQueue OpenCL command queue available to this buffer.
  * @param requestedBufferType The type of buffer to attempt to allocate.
  * @param fieldMemoryAllocator The allocator to use for the cpu-side memory.
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
class OpenCLBuffer private[opencl]
  (gpuBufferCapacityBytes: Long,
   cpuUseFieldType: FieldType,
   commandQueue: OpenCLParallelCommandQueue,
   requestedBufferType: BufferType,
   fieldMemoryAllocator: FieldMemory)
        extends AbstractFieldBuffer with FieldLimits
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
  private var _cpuMemory = null.asInstanceOf[AbstractFieldMemory]

  /** Actual cpu memory type: heap, non-heap pageable or non-heap pinned. */
  private var bufferType = requestedBufferType

  private def clContext = commandQueue.clContext

  private val cpuAndGPUSizesMatch = {
    val cpuSizeBytes = new FieldMemoryLayoutImpl(cpuUseFieldType).longBufferSizeBytes
    if (cpuSizeBytes > gpuBufferCapacityBytes)
      throw new RuntimeException("Internal error: not expecting cpu direct buffer size to be greater than gpu global mem size.")
    else
      cpuSizeBytes == gpuBufferCapacityBytes
  }

  /** Memory on the CPU side, lazily instantiated. */
  def cpuMemory: AbstractFieldMemory = clMemoryLock.synchronized {
    if (_cpuMemory == null) {
      _cpuMemory = requestedBufferType match {
        case PinnedDirectBuffer =>
          require(commandQueue != null, "need command queue for pinned buffer")
          try {
            fieldMemoryAllocator.pinned(cpuUseFieldType, commandQueue)
          }
          catch {
            case e: CLMemObjectAllocationFailureException =>
              // The following reallocation may not be particularly helpful- the first thing the driver
              // will do is try to allocate pinned memory, and that will probably fail.  Also, the
              // exception may be due to exhaustion of GPU global memory, and resorting to
              // pageable direct memory will only defer the gpu memory allocation until first use.
              println("Downgrading from pinned to pageable memory for buffer " + _deviceBuffer)
              bufferType = DirectBuffer
              fieldMemoryAllocator.direct(cpuUseFieldType)
          }
        case DirectBuffer =>
          fieldMemoryAllocator.direct(cpuUseFieldType)
        case IndirectBuffer =>
          fieldMemoryAllocator.indirect(cpuUseFieldType)
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
        val width = cpuUseFieldType.fieldShape(1)
        val height = cpuUseFieldType.fieldShape(0)
        clContext.createImage2d(cpuMemory.directBuffer, width, height, imageFormat,
          Mem.READ_WRITE)
      } else if (_cpuMemory == null && gpuBufferCapacityBytes <= Int.MaxValue.toLong) {
        // If there's no _cpuMemory, we may never need one (this could be a buffer written
        // and read by GPU kernels for a field that is never probed).  Rather than allocate
        // an NIO buffer, just create the clMemory based on the size of the buffer with no
        // CPU-side component.
        //
        // Since the buffer size can be represented by an unsigned int, we use the standard
        // JOCL interface for buffer creation.
        //
        clContext.createBuffer(toIntBufferSizeBytes(gpuBufferCapacityBytes), Mem.READ_WRITE)
      } else if (_cpuMemory == null && gpuBufferCapacityBytes > Int.MaxValue.toLong) {
        // Since the buffer size *can't* be represented by an unsigned int, we use reflection
        // to bypass the standard JOCL interface (implementation dependent, so probably fragile).
        OpenCLBuffer.createLargeBuffer(clContext, gpuBufferCapacityBytes, Mem.READ_WRITE)
      } else if (_cpuMemory != null && cpuAndGPUSizesMatch) {
        clContext.createBuffer(cpuMemory.directBuffer, Mem.READ_WRITE)
      } else {
        val gpuMemOnly = clContext.createBuffer(toIntBufferSizeBytes(gpuBufferCapacityBytes), Mem.READ_WRITE)
        gpuMemOnly.cloneWith[FloatBuffer](_cpuMemory.directBuffer.asInstanceOf[FloatBuffer])
      }
    }
    _deviceBuffer
  }

  // Convert a bufferSize that's a Long to an unsigned Int, throwing an exception if this is not possible.
  private def toIntBufferSizeBytes(bufferSizeBytes: Long) = {
    if (bufferSizeBytes > MaxDirectBufferSizeBytes)
      throw new RuntimeException(s"Buffer size $bufferSizeBytes bytes exceeds maximimum supported size of $MaxDirectBufferSizeBytes bytes.")
    else
      bufferSizeBytes.toInt
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
  def read: AbstractFieldMemory = cpuMemoryValidLock.synchronized {
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
  private def isImage2dBuffer = cpuUseFieldType.isImage

  /** Internal test of buffer type.
    *
    * @return True if this is an image buffer, false for ordinary buffer.
    */
  private def isComplexBuffer =
    cpuUseFieldType.elementType == Complex32

  /** Release OpenCL buffer resource (garbage collector gets direct buffer). */
  private[cogx] def release(): Unit = clMemoryLock.synchronized {
    if (_deviceBuffer != null  && !_deviceBuffer.isReleased)
      _deviceBuffer.release()
  }

  /** An CLEventList to hold completion events for asynchronous CPU -> GPU data transfers. */
  private lazy val copyToGPUEventList = new CLEventList(CLEventFactory, 1)

  /** Copy field data from cpuMemory to the GPU memory, blocking until completion. */
  private def copyToGPU() {
    // touch cpuMemory to make sure it's instantiated
    cpuMemory
    // To achieve simultaneous bidirectional transfers, one must use both pinned buffers and the
    // non-blocking form of the putWriteBuffer call (even though the completion event is waited for immediately).
    // We only use the non-blocking putWriteBuffer call with pinned buffers since the runtime was
    // seen to deadlock when used with pageable direct buffers.  One cure for the deadlock was to LD_PRELOAD
    // the "libsleep.so" library which forces the polling NVIDIA driver to sleep periodically.  One theory then
    // on the cause of the hanging is that an Actor thread of the runtime is scheduled to the same core
    // as a presumably higher-priority NVIDIA driver thread that polls continuously.
    if (bufferType == PinnedDirectBuffer) {
      commandQueue.putWriteBufferAsync(deviceBuffer.asInstanceOf[CLBuffer[_]], copyToGPUEventList)
      copyToGPUEventList.waitForEvents()
      checkStatus(copyToGPUEventList, "copyToGPU")
      copyToGPUEventList.release()
    }
    else
      commandQueue.putWriteBuffer(deviceBuffer.asInstanceOf[CLBuffer[_]])
  }

  /** An CLEventList to hold completion events for asynchronous GPU -> CPU data transfers. */
  private lazy val copyFromGPUEventList = new CLEventList(CLEventFactory, 1)

  /** Check the completion status of the first event in `eventList`. */
  private def checkStatus(eventlist: CLEventList, routineName: String) {
    val event = eventlist.getEvent(0)
    event.getStatus match {
      case CLEvent.ExecutionStatus.ERROR =>
        throw new RuntimeException(s"$toString $routineName fails with error code ${event.getStatusCode}")
      case CLEvent.ExecutionStatus.COMPLETE =>
      case other =>
        throw new RuntimeException(s"$toString $routineName  sees unexpected status: $other (expecting COMPLETE).")
    }
  }

  /** Copy field data to cpuMemory from the GPU memory, blocking until completion. */
  private def copyFromGPU() {
    // touch cpuMemory to make sure it's instantiated
    cpuMemory
    // To achieve simultaneous bidirectional transfers, one must use both pinned buffers and the
    // non-blocking form of the putReadBuffer call (even though the completion event is waited for immediately).
    // We only use the non-blocking putReadBuffer call with pinned buffers since the runtime was
    // seen to deadlock when used with pageable direct buffers.  See copyToGPU() for further comments.
    if (bufferType == PinnedDirectBuffer) {
      commandQueue.putReadBufferAsync(deviceBuffer.asInstanceOf[CLBuffer[_]], copyFromGPUEventList)
      copyFromGPUEventList.waitForEvents()
      checkStatus(copyFromGPUEventList, "copyFromGPU")
      copyFromGPUEventList.release()
    }
    else
      commandQueue.putReadBuffer(deviceBuffer.asInstanceOf[CLBuffer[_]])
  }

  // The copying of image memories is treated a bit differently than buffer memories in that the blocking
  // form of the copy is never used withing the OpenCLParallelCommandQueue implementation.  The reason for
  // this is that the JOCL image copy interface is not thread safe and needs synchronization barriers.  Thus,
  // we use the non-blocking form of the copy in all cases (i.e. for both pinned and pageable memories).

  /** Copy image from cpuMemory to the GPU memory, blocking until completion. */
  private def copyImage2dToGPU(blocking: Boolean = true) {
    commandQueue.putWriteImage2d(deviceBuffer.asInstanceOf[CLImage2d[_]])
  }

  /** Copy field data to cpuMemory from the GPU memory, blocking until completion. */
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

  /** Create a buffer that can have a size greater than Int.MaxValue, using reflection to get
    * around the unfortunate use of Int's in the JOCL wrapping of the OpenCL interface.  The
    * JOCL code that is side-stepped is the following:
    *
    * In CLContext.java:
    *
    *  // Constructor with unfortunate int size parameter
    * public final CLBuffer<?> createBuffer(int size, int flags) {     <-- 'int' use is crux of the problem
    *    CLBuffer<?> buffer = CLBuffer.create(this, size, flags);
    *    memoryObjects.add(buffer);                                    <-- need reflection for this
    *    return buffer;
    * }
    *
    * In CLBuffer.java:
    *
    * // Constructor
    * protected CLBuffer(CLContext context, long size, long id, int flags) {   <-- 'long' use is helpful.
    *    this(context, null, size, id, flags);
    * }
    *
    * // Method with unfortunate int size parameter
    * static CLBuffer<?> create(CLContext context, int size, int flags) {
    *
    *     if(isHostPointerFlag(flags)) {
    *         throw new IllegalArgumentException("no host pointer defined");
    *     }
    *
    *     CL binding = context.getPlatform().getCLBinding();
    *     int[] result = new int[1];
    *     long id = binding.clCreateBuffer(context.ID, flags, size, null, result, 0);
    *     checkForError(result[0], "can not create cl buffer");
    *
    *     return new CLBuffer(context, size, id, flags);              <-- need reflection for this
    * }
    *
    * Most of the important work can be done from the public interfaces, up to and including
    * the binding.clCreateBuffer call.  Reflection is needed to then access the CLBuffer constructor,
    * and finally to register the buffer with the CLContext via memoryObjects.add().
    *
    * Note that the size argument in the binding.clCreateBuffer is cast to a long, if you can believe
    * the IntelliJ IDEA bytecode disassembler.
    *
    * @param clContext The underlying JOCL context
    * @param sizeBytes The size of the buffer to allocate, importantly as a Long.
    * @param memFlags  READ_ONLY, WRITE_ONLY, READ_WRITE, etc. flags for the buffer creation.
    * @return          The created JOCL CLBuffer
    */
  def createLargeBuffer(clContext: CLContext, sizeBytes: Long, memFlags: Mem*): CLBuffer[_] = {

    val memFlagArray = memFlags.toArray
    val flags: Int = Mem.flagsToInt(memFlagArray)

    def isHostPointerFlag(flags: Array[Mem]) =
      flags.contains(Mem.COPY_BUFFER) || flags.contains(Mem.USE_BUFFER)
    if (isHostPointerFlag(memFlagArray))
      throw new IllegalArgumentException("no host pointer defined")

    val binding: CL = clContext.getCL()
    val result: Array[Int] = new Array[Int](1)
    val id: Long = binding.clCreateBuffer(clContext.ID, flags, sizeBytes, null.asInstanceOf[Buffer], result, 0)
    CLException.checkForError(result(0), s"can not create cl buffer of size $sizeBytes")

    try {
      // Use reflection to invoke the CLBuffer(context, size, id, flags) constructor

      val clBufferConstructor: Constructor[CLBuffer[FloatBuffer]] = classOf[CLBuffer[FloatBuffer]].getDeclaredConstructor(classOf[CLContext], classOf[Long], classOf[Long], classOf[Int])
      clBufferConstructor.setAccessible(true)
      // The Scala compiler wants an Object for each argument, so boxing was necessary.  Does it then automatically unbox?
      val buffer = clBufferConstructor.newInstance(clContext, Long.box(sizeBytes), Long.box(id), Int.box(flags))

      // Now use reflection to invoke clContext.memoryObjects.add(buffer) so the buffer will get released properly
      // when the clContext is released

      val memoryObjectsField = classOf[CLContext].getDeclaredField("memoryObjects")
      memoryObjectsField.setAccessible(true)
      val memoryObjects = memoryObjectsField.get(clContext).asInstanceOf[java.util.Set[CLMemory[_]]]
      memoryObjects.add(buffer)

      buffer

    }
    catch {
      case e: Exception =>
        throw new RuntimeException(s"Creation of $sizeBytes byte buffer via reflection fails: exception is $e.")
    }
  }
}
