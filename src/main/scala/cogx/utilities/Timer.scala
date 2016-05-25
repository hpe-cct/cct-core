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

package cogx.utilities


/** This timer class can be used to time the execution time of functions or blocks of code.
  * A timer is created with an enable that allows it to avoid System.nanoTime calls when
  * no timing function is desired without a lot of if-thens:
  * {{{
  * val enableTiming = false   // Set to true if timing statistics are needed
  * val timer = new Timer(enableTiming)
  * val duration = timer(myImportantFunction)    // Timer makes System.nanoTime call only if enabled
  * }}}
  * Alternatively, one can use the timer created above as in:
  * {{{
  *  timer.start
  *  <some block of code>
  *  timer.stop
  *  if (enableTiming)
  *    println ("The duration of the last execution of the block was: " + timer.duration +
  *      timer.units.designator + ", cumulative stats are: " + timer)
  * }}}
  *
  * User: Dick Carter
  * Date: 9/4/11
  *
  */



private [cogx] class Timer(val enabled: Boolean = true, val units: Timer.Units = Timer.mS) {
  var numTrials = 0
  var duration = 0.0
  var minDuration = 0.0
  private var sumDurations = 0.0
  private var sumDurationsSquared = 0.0
  private var startTime = 0.0
  private var withinInterval = false

  def this(units: Timer.Units) = this(true, units)

  def start {
    duration = 0.0
    if (enabled) {
      startTime = System.nanoTime
      withinInterval = true
    }
  }

  def stop {
    if (enabled) {
      require(withinInterval, "Timer: Must call start before stop")
      duration = (System.nanoTime - startTime) * units.scaleFactor
      minDuration = if (numTrials == 0) duration else math.min(minDuration, duration)
      sumDurations += duration
      sumDurationsSquared += duration * duration
      numTrials += 1
      withinInterval = false
    }
  }

  def reset {
    numTrials = 0
    sumDurations = 0.0
    sumDurationsSquared = 0.0
    withinInterval = false
  }

  def avg = if (numTrials > 0) (sumDurations / numTrials) else 0.0

  def stddev = {
    if (numTrials > 0) {
      val average = sumDurations / numTrials
      val variance = sumDurationsSquared / numTrials - average * average
      math.sqrt(variance)
    }
    else
      0.0
  }

  /**time a function by applying it to the timer as in timer(myFunc())
   *  This returns 0 if the timer is not enabled and makes no System.nanoTime calls
   */
  def apply(functionToTime: => Unit): Double = {
    start
    functionToTime
    stop
    duration
  }

  override def toString =
    "(%4.1f %s, %4.1f %s, %4.1f %s, %4.1f %s)".format(
      avg, units.designator,
      duration, units.designator,
      minDuration, units.designator,
      stddev, units.designator)
}


private [cogx] object Timer {
  sealed abstract class Units(val scaleFactor: Double, val designator: String)
  case object nS extends Units(1.0, "nS")
  case object uS extends Units(1.0/1000, "uS")
  case object mS extends Units(1.0/1000000, "mS")
  case object S  extends Units(1.0/1000000000, "S")
}
