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

package cogx.compiler.codegenerator.opencl.cpukernels

/** Can be used with Sensors to force them to supply a data stream
  * at a fixed rate. If the computation is compute bound, the rate will
  * degrade gracefully. This can put the caller to sleep, so it should only
  * be used by kernels that run on their own thread.
  *
  * @param framesPerSecond Desired rate at which the sensor will supply an
  *        input.
  *
  * @author Greg Snider
  */
private[cogx]
class RateSynchronizer(framesPerSecond: Double) {
  /** Nanoseconds in a second. */
  private val NanosecondsPerSecond = 1.0e9
  /** Desired interval between successive frames, in nanoseconds. */
  private val frameSpacingNanoseconds: Long =
    ((1.0 / framesPerSecond) * NanosecondsPerSecond).toLong
  /** Starting time for data collection. */
  private var startTime: Long = System.nanoTime
  /** Last synchronization time. */
  private var lastStartTime = System.nanoTime

  /** Start collecting data for the current frame. */
  def startDataCollection() {
    startTime = System.nanoTime
  }

  /** Data collection done. This will block to synchronize at the desired rate.*/
  def endDataCollection() {
    val resultDelay = System.nanoTime  - startTime
    //val deltaTime = System.nanoTime - lastStartTime
    lastStartTime = startTime
    val sleepTime: Long = frameSpacingNanoseconds - resultDelay
    sleepForNanoseconds(sleepTime)
  }

  /** Put the calling thread to sleep for `nanoseconds`. */
  private def sleepForNanoseconds(nanoseconds: Long) {
    val MinSleepTime = 10000L  // in nanoseconds
    if (nanoseconds > MinSleepTime) {
      val NanosecondsPerMillisecond = 1000000L
      val millisec: Long = nanoseconds / NanosecondsPerMillisecond
      //println("sleep " + millisec + " msec")
      val nanosec = (nanoseconds % NanosecondsPerMillisecond).toInt
      Thread.sleep(millisec, nanosec)
    }
  }
}
