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

import cogx.parameters.Cog
import com.jogamp.opencl._
import java.nio.ByteBuffer
import cogx.platform.opencl.OpenCLEventCache._

/** A command queue with the same interface (more or less) as a CLCommandQueue,
  * but offers more parallelism.
  *
  * Although OpenCL offers out-of-order command queues, it doesn't appear that
  * anyone actually implements it (our own experiments suggest Nvidia doesn't,
  * and postings online suggest AMD doesn't either). The usual advice is to
  * implement multiple command queues for each device and fill them up with
  * independent requests.
  *
  * By centralizing that here, we allow better scheduling of requests across
  * the command queues since we can track the dependencies. We can also
  * prevent interference between synchronous readBuffer / writeBuffer calls
  * which are used by CPU kernels, and asynchronous compute requests used
  * by GPU kernels.
  *
  * NOTE: CLCommandQueues in OpenCL 1.1 are thread-safe, so no synchronized
  * needed here on the methods.
  *
  * @param device The device that the command queue controls.
  * @param profile Enable kernel profiling.
  *
  * @author Greg Snider
  */
private[cogx]
class OpenCLParallelCommandQueue (device: OpenCLDevice, val profile: Boolean = true) {
  /** Enable out-of-order execution in raw command queues. */
  val outOfOrderExecution = Cog.outOfOrderExecution
  /** Command queue for synchronous reads and writes. */
  private val readWriteQueue = createCommandQueue()
  /** Command queue for asynchronous kernel execution. There will be more... */
  private val executionQueue = createCommandQueue()
  /** The CLContext in which the CommandQueue lives. */
  def clContext: CLContext = device.clContext

  /** Release the commandQueue. */
  def release() {
    if (!readWriteQueue.isReleased)
      readWriteQueue.release()
    if (!executionQueue.isReleased)
      executionQueue.release()
  }

  /** Wait until all command queues finish. Not recommended. Superset of flush
    * functionality. */
  def finish() {
    executionQueue.finish()
    readWriteQueue.finish()
  }

  /** Ensure that all enqueued commands will be submitted to the device, but
    * don't wait for their completion (or their submission for that matter). */
  def flush() {
    executionQueue.flush()
    readWriteQueue.flush()
  }

  /** Synchronous, blocking call to get the byte buffer for `buffer`. */
  def putMapBuffer(buffer: CLBuffer[_], flag: CLMemory.Map): ByteBuffer = readWriteQueue.synchronized {
    readWriteQueue.putMapBuffer(buffer, flag, true)
  }

  /** Launch 1D `kernel` asynchronously when `condition` triggers, triggering
    * `events`. The `events` parameter must be an empty CLEventList of length
    * 1; this will be filled in with the output event that will be triggered
    * when execution completes.
    */
  def put1DRangeKernel(kernel: CLKernel, globalWorkOffset: Long,
                       globalWorkSize: Long, localWorkSize: Long,
                       condition: CLEventList, events: CLEventList)
  {
    // I've commented out these flushes for two reasons:
    // 1. We're not using OpenCL buffers across multiple devices, since
    //    NVidia handles this case very inefficiently.
    // 2. The only case I can think of is when a CPU kernel does a buffer
    //    write followed by a device kernel buffer read.  The CPU kernel
    //    currently copies the buffer down to the device with a blocking
    //    copy- i.e. the copy is complete when the CPU kernel signals its
    //    OutputTrigger event.   -RJC

    // The combination of removing these flushes and turning the finish into
    // a flush in the GPUSupervisor resulted in a 7% speedup on Windows and
    // a 12% speedup on Linux for the Performance.CFNetRegression

    // Flush required by OpenCL 1.1 spec to sync multiple command queues since
    // devices are allowed to cache state.
    //    readWriteQueue.flush
    executionQueue.put1DRangeKernel(kernel, globalWorkOffset,
      globalWorkSize, localWorkSize, condition, events)
  }

  /** Launch 2D `kernel` asynchronously when `condition` triggers, triggering
    * `events`. The `events` parameter must be an empty CLEventList of length
    * 1; this will be filled in with the output event that will be triggered
    * when execution completes.
    */
  def put2DRangeKernel(kernel: CLKernel, globalWorkOffsetX: Long,
                       globalWorkOffsetY: Long, globalWorkSizeX: Long,
                       globalWorkSizeY: Long, localWorkSizeX: Long,
                       localWorkSizeY: Long,
                       condition: CLEventList, events: CLEventList)
  {
    // Flush required by OpenCL 1.1 spec to sync multiple command queues since
    // devices are allowed to cache state.
    //    readWriteQueue.flush
    executionQueue.put2DRangeKernel(kernel, globalWorkOffsetX,
      globalWorkOffsetY, globalWorkSizeX,
      globalWorkSizeY, localWorkSizeX,
      localWorkSizeY,
      condition, events)
  }

  /** Launch 3D `kernel` asynchronously when `condition` triggers, triggering
    * `events`. The `events` parameter must be an empty CLEventList of length
    * 1; this will be filled in with the output event that will be triggered
    * when execution completes.
    */
  def put3DRangeKernel(kernel: CLKernel, globalWorkOffsetX: Long,
                       globalWorkOffsetY: Long, globalWorkOffsetZ: Long,
                       globalWorkSizeX: Long, globalWorkSizeY: Long,
                       globalWorkSizeZ: Long, localWorkSizeX: Long,
                       localWorkSizeY: Long, localWorkSizeZ: Long,
                       condition: CLEventList, events: CLEventList)
  {
    // Flush required by OpenCL 1.1 spec to sync multiple command queues since
    // devices are allowed to cache state.
    //    readWriteQueue.flush
    executionQueue.put3DRangeKernel(kernel, globalWorkOffsetX,
      globalWorkOffsetY, globalWorkOffsetZ,
      globalWorkSizeX, globalWorkSizeY,
      globalWorkSizeZ, localWorkSizeX,
      localWorkSizeY, localWorkSizeZ,
      condition, events)
  }

  /** Blocking write of a CLBuffer. */
  def putWriteBuffer(deviceBuffer: CLBuffer[_]) {
    readWriteQueue.putWriteBuffer(deviceBuffer, true)
  }

  /** Read a CLBuffer (Device --> CPU).
    *
    * @param deviceBuffer Buffer which will be copied to host.
    * @param outputEvent If non-null, this implies an asynchronous read--this
    *        will be filled in with the event that will be triggered when the read
    *        is complete. If null or missing, this is a synchronous read which
    *        will block until complete.
    */
  def putReadBuffer(deviceBuffer: CLBuffer[_], outputEvent: CLEventList = null) {
    if (outputEvent == null) {
    // Blocking read
      readWriteQueue.putReadBuffer(deviceBuffer, true)
    } else {
      // Asynchronouos read
      readWriteQueue.putReadBuffer(deviceBuffer, false, outputEvent)
    }
  }

  /** Write a CLImage2d (CPU -> Device) asynchronously (returns immediately).
    *
    * This call is protected by a synchronization block because Jogamp-JOCL's
    * wrapping of clEnqueueWriteImage is not thread safe.  The 'ibA' data
    * member field (and others) within CLCommandQueue are the source of the
    * problem.
    *
    * @param deviceBuffer Buffer which will be receive the host data.
    * @param outputEvent Eventlist that will be filled in with the event that
    *        will be triggered when the write is complete.
    */
  def putWriteImage2dAsync(deviceBuffer: CLImage2d[_],
                          outputEvent: CLEventList): Unit = readWriteQueue.synchronized {
    readWriteQueue.putWriteImage(deviceBuffer, false, outputEvent)
  }

  /** Write a CLImage2d (CPU -> Device) synchronously (blocks until complete).
    *
    * @param deviceBuffer Buffer which will be receive the host data.
    */
  def putWriteImage2d(deviceBuffer: CLImage2d[_]) {
    // Blocking write, implemented with asynchronous write followed by wait to
    // minimize the time spent in synchronized putWriteImage routine.
    val eventList = new CLEventList(CLEventFactory, 1)
    putWriteImage2dAsync(deviceBuffer, eventList)
    val event = eventList.getEvent(0)
    eventList.waitForEvents()
    event.getStatus match {
      case CLEvent.ExecutionStatus.ERROR =>
        throw new RuntimeException(toString + " putWriteImage2d" +
                " fails with error code " + event.getStatusCode)
      case CLEvent.ExecutionStatus.COMPLETE =>
      case other =>
        throw new RuntimeException(toString + "Unexpected status for " + toString +
                ": " + other + " (expecting COMPLETE)")
    }
    event.release()
  }

  /** Read a CLImage2d (Device --> CPU) asynchronously (returns immediately).
    *
    *
    * This call is protected by a synchronization block because Jogamp-JOCL's
    * wrapping of clEnqueueReadImage is not thread safe.  The 'ibA' data
    * member field (and others) within CLCommandQueue are the source of the
    * problem.
    *
    * @param deviceBuffer Buffer which will be copied to host.
    * @param outputEvent Eventlist that will be filled in with the event that
    *        will be triggered when the read is complete.
    */
  def putReadImage2dAsync(deviceBuffer: CLImage2d[_],
                          outputEvent: CLEventList): Unit = readWriteQueue.synchronized {
    readWriteQueue.putReadImage(deviceBuffer, false, outputEvent)
  }

  /** Read a CLImage2d (Device --> CPU).  Blocks until read is complete.
    *
    * @param deviceBuffer Buffer which will be copied to host.
    */
  def putReadImage2d(deviceBuffer: CLImage2d[_]) {
      // Blocking read, implemented with asynchronous read followed by wait to
      // minimize the time spent in synchronized putReadImage routine.
      val eventList = new CLEventList(CLEventFactory, 1)
      putReadImage2dAsync(deviceBuffer, eventList)
      val event = eventList.getEvent(0)
      eventList.waitForEvents()
      event.getStatus match {
        case CLEvent.ExecutionStatus.ERROR =>
          throw new RuntimeException(toString + " putReadImage2d" +
                  " fails with error code " + event.getStatusCode)
        case CLEvent.ExecutionStatus.COMPLETE =>
        case other =>
          throw new RuntimeException(toString + "Unexpected status for " + toString +
                  ": " + other + " (expecting COMPLETE)")
      }
      event.release()
  }

  /** Internal creation of a CLCommandQueue. */
  private def createCommandQueue(): CLCommandQueue = {
    if (profile && outOfOrderExecution)
      device.clDevice.createCommandQueue(
        CLCommandQueue.Mode.PROFILING_MODE,
        CLCommandQueue.Mode.OUT_OF_ORDER_MODE)
    else if (profile)
      device.clDevice.createCommandQueue(
        CLCommandQueue.Mode.PROFILING_MODE)
    else if (outOfOrderExecution)
      device.clDevice.createCommandQueue(
        CLCommandQueue.Mode.OUT_OF_ORDER_MODE)
    else
      device.clDevice.createCommandQueue
  }
}