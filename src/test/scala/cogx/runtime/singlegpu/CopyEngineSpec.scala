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

package cogx.runtime.singlegpu

import scala.language.reflectiveCalls
import cogx.cogmath.geometry.Shape
import cogx.parameters.Cog
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import cogx.platform.opencl._
import cogx.runtime.execution.CogActorSystem
import cogx.platform.cpumemory.{BufferType, DirectBuffer, PinnedDirectBuffer, ScalarFieldMemory}
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.opencl.OpenCLEventCache._
import com.jogamp.opencl.{CLBuffer, CLEvent, CLEventList}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, Future}

/** Test code to explore how many DMA engines the GPU has and how many command queues are needed
  * to achieve overlapped operation.  The transfer bandwidths are reported for uni- and bidirectional
  * transfers.  The user is left to draw conclusions about the number of DMA copy engines the GPU has
  * and whether concurrent copies are achieved.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class CopyEngineSpec extends FunSuite with MustMatchers {

  /** Set up Actor system to handle Futures of bidirectional test. */
  val system = CogActorSystem()
  implicit val ec: ExecutionContext = system.dispatcher

  /** Make a command queue for a device. */
  def makeQueue(device: OpenCLDevice) = device.clDevice.createCommandQueue()

  /** Convert a measured duration in nanoseconds to a bandwidth for the fixed buffer size used in these tests. */
  def xferRate(timeNanos: Long, fieldElements: Long) = 4.0 * fieldElements / timeNanos

  /** Make a OpenCL Buffer on a device with the specified bufferType and size. */
  def makeCLBuf(device: OpenCLDevice, fieldElements: Long, bufferType: BufferType) = {
    val fieldType = new FieldType(Shape(fieldElements.toInt), Shape(), Float32)
    val buf = device.createFieldBuffer(fieldType, bufferType).asInstanceOf[OpenCLBuffer[ScalarFieldMemory]]
    // Instantiate cpu portion of buffer
    buf.cpuMemory
    val clbuf = buf.deviceBuffer.asInstanceOf[CLBuffer[_]]
    clbuf
  }

  def timeNanos(f: => Unit) = {
    val start = System.nanoTime()
    f
    System.nanoTime() - start
  }

  /** Number of floats in the transfers of these tests. */
  val desiredFieldElements = 100 * 1024 * 1024L

  def elementsToMB(fieldElements: Long) = fieldElements * 4 / 1024 / 1024

  /** The number of fieldElements in the transfers, perhaps lowered to meet the capabilities of the device. */
  def testFieldElements(device: OpenCLDevice) = {
    val actualFieldElements = math.min(desiredFieldElements, device.maxMemAllocSize/4)
    if (actualFieldElements < desiredFieldElements)
      println(s"Downsizing buffers to size ${elementsToMB(actualFieldElements)} MB, rather than the usual ${elementsToMB(desiredFieldElements)}.")
    actualFieldElements
  }

  sealed abstract class CopyDir(val commandName: String)
  case object GPUtoCPU extends CopyDir("putReadBuffer")
  case object CPUtoGPU extends CopyDir("putWriteBuffer")

  /** Perform a single copy between the CPU and GPU and measure the bandwidth. */
  def unidirectionalCopyTest(bufferType: BufferType, dir: CopyDir): Double = {
    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    val bandwidth =
      try {
        val device = platform.devices(0)
        val fieldElements = testFieldElements(device)
        val clbuf1 = makeCLBuf(device, fieldElements, bufferType)
        val queue1 = makeQueue(device)

        val start = System.nanoTime()
        val copyTimeNanos =
        dir match {
          case GPUtoCPU => timeNanos(queue1.putReadBuffer(clbuf1, true))
          case CPUtoGPU => timeNanos(queue1.putWriteBuffer(clbuf1, true))
        }
        // An alternate form of the above- might hang if used with pageable buffers.

//        val eventList = new CLEventList(CLEventFactory, 1)
//
//        val start = System.nanoTime()
//        dir match {
//          case GPUtoCPU => queue1.putReadBuffer(clbuf1, false, eventList)
//          case CPUtoGPU => queue1.putWriteBuffer(clbuf1, false, eventList)
//        }
//        eventList.waitForEvents()
//        val copyTimeNanos = System.nanoTime() - start
//        checkStatus(eventList, dir.commandName)
//        eventList.release()
        xferRate(copyTimeNanos, fieldElements)
      }
      finally
        platform.release()
    bandwidth
  }

  /** Pinned buffer transfers caused JVM crashes on AMD GPUs with APPSDK 2.9.1.  Bypass until qualified. */
  def runIfQualified(f: => Unit): Unit = {
    if (OpenCLPlatform().devices(0).isNVidia || Cog.pinnedBuffers) {
      f
    }
    else
      println("Test is currently bypassed on this platform.")
  }

  /** Measure the transfer bandwidth of pinned cpu memory to gpu memory. */
  test("CPU pinned memory -> GPU copy performance") {
    runIfQualified {
      // Do it once as a warm-up
      unidirectionalCopyTest(PinnedDirectBuffer, CPUtoGPU)
      val bandwidth = unidirectionalCopyTest(PinnedDirectBuffer, CPUtoGPU)
      println(f"CPU pinned   -> GPU data xfer BW measured at $bandwidth%5.2f GB/sec")
    }
  }

  /** Measure the transfer bandwidth of gpu memory to pinned cpu memory. */
  test("GPU -> CPU pinned memory copy performance") {
    runIfQualified {
      // Do it once as a warm-up
      unidirectionalCopyTest(PinnedDirectBuffer, GPUtoCPU)
      val bandwidth = unidirectionalCopyTest(PinnedDirectBuffer, GPUtoCPU)
      println(f"GPU -> CPU pinned   data xfer BW measured at $bandwidth%5.2f GB/sec")
    }
  }

  /** Measure the transfer bandwidth of pageable cpu memory to gpu memory. */
  test("CPU pageable memory -> GPU copy performance") {
    // Do it once as a warm-up
    unidirectionalCopyTest(DirectBuffer, CPUtoGPU)
    val bandwidth = unidirectionalCopyTest(DirectBuffer, CPUtoGPU)
    println(f"CPU pageable -> GPU data xfer BW measured at $bandwidth%5.2f GB/sec")
  }

  /** Measure the transfer bandwidth of gpu memory to pageable cpu memory. */
  test("GPU -> CPU pageable memory copy performance") {
    // Do it once as a warm-up
    unidirectionalCopyTest(DirectBuffer, GPUtoCPU)
    val bandwidth = unidirectionalCopyTest(DirectBuffer, GPUtoCPU)
    println(f"GPU -> CPU pageable data xfer BW measured at $bandwidth%5.2f GB/sec")
  }


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

  /** Perform two simultaneous copies in opposite directions between the CPU and GPU and measure the bandwidth. */
  def concurrentBidirectionalCopyTest(bufferType: BufferType, twoCommandQueus: Boolean): Double = {
    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()
    var bandwidth = 0.0

    try {
      val device = platform.devices(0)
      val fieldElements = testFieldElements(device)
      val clbuf1 = makeCLBuf(device, fieldElements, bufferType)
      val clbuf2 = makeCLBuf(device, fieldElements, bufferType)

      val queue1 = makeQueue(device)
      val queue2 =
        if (twoCommandQueus)
          makeQueue(device)
        else
          queue1

      // Blocking write, implemented with asynchronous write followed by wait to
      // minimize the time spent in synchronized putWriteImage routine.
      if (bufferType == PinnedDirectBuffer) {
        val writeEventList = new CLEventList(CLEventFactory, 1)
        val readEventList = new CLEventList(CLEventFactory, 1)

        val start = System.nanoTime()
        val reader = Future[Boolean] {
          queue2.putReadBuffer(clbuf2, false, readEventList)
          readEventList.waitForEvents()
          checkStatus(readEventList, "putReadBuffer")
          true
        }
        queue1.putWriteBuffer(clbuf1, false, writeEventList)
        writeEventList.waitForEvents()
        checkStatus(writeEventList, "putWriteBuffer")
        Await.result(reader, 60 seconds)
        val duration = System.nanoTime() - start
        bandwidth = xferRate(duration, fieldElements) * 2
        readEventList.release()
        writeEventList.release()
      }
      else {
        // For pageable buffers, we use the blocking form of the copy commands, since
        // they were never seen to deadlock.  This precludes overlapped copies, but this
        // apparently is not possible with pageable buffers anyway for NV.
        val start = System.nanoTime()
        val reader = Future[Boolean] {
          queue2.putReadBuffer(clbuf2, true)
          true
        }
        queue1.putWriteBuffer(clbuf1, true)
        Await.result(reader, 10 seconds)
        val duration = System.nanoTime() - start
        bandwidth = xferRate(duration, fieldElements) * 2
      }
    }
    finally
      platform.release()
    bandwidth
  }

  /** Measure the bidirectional transfer bandwidth of gpu memory to pinned cpu memory via 2 simultaneous transfers. */
  test("GPU <-> CPU pinned memory copy performance") {
    runIfQualified {
      // Do it once as a warm-up
      concurrentBidirectionalCopyTest(PinnedDirectBuffer, true)
      val bandwidth = concurrentBidirectionalCopyTest(PinnedDirectBuffer, true)
      println(f"CPU pinned   <-> GPU aggregate data xfer BW measured at $bandwidth%5.2f GB/sec (two command queues)")
    }
  }

  /** Measure the bidirectional transfer bandwidth of gpu memory to pinned cpu memory via 2 simultaneous transfers. */
  test("GPU <-> CPU pinned memory copy performance one command queue") {
    runIfQualified {
      // Do it once as a warm-up
      concurrentBidirectionalCopyTest(PinnedDirectBuffer, false)
      val bandwidth = concurrentBidirectionalCopyTest(PinnedDirectBuffer, false)
      println(f"CPU pinned   <-> GPU aggregate data xfer BW measured at $bandwidth%5.2f GB/sec (one command queue)")
    }
  }

  /** Measure the bidirectional transfer bandwidth of gpu memory to pinned cpu memory via 2 simultaneous transfers. */
  test("GPU <-> CPU pageable memory copy performance") {
    // Do it once as a warm-up
    concurrentBidirectionalCopyTest(DirectBuffer, true)
    val bandwidth = concurrentBidirectionalCopyTest(DirectBuffer, true)
    println(f"CPU pageable <-> GPU aggregate data xfer BW measured at $bandwidth%5.2f GB/sec (two command queues)")
  }

  /** Measure the bidirectional transfer bandwidth of gpu memory to pinned cpu memory via 2 simultaneous transfers. */
  test("GPU <-> CPU pageable memory copy performance one command queue") {
    // Do it once as a warm-up
    concurrentBidirectionalCopyTest(DirectBuffer, false)
    val bandwidth = concurrentBidirectionalCopyTest(DirectBuffer, false)
    println(f"CPU pageable <-> GPU aggregate data xfer BW measured at $bandwidth%5.2f GB/sec (one command queue)")
  }

}
