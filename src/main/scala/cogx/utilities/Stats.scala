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


/** The Stats class generates some common statistics like average and standard
  * deviation for a set of samples.
  *
  * To use:
  * {{{
  *   val stats = new Stats
  *   stats.addSample(1.0)
  *   stats.addSample(2.0)
  *   require (stats.avg == 1.5)
  * }}}
  *
  * If you know that you want to use the default toString() method and the data
  * represents microseconds, you can create the Stats instance as in:
  *
  * {{{
  *   val stats = new Stats(" us")
  *   println(stats)
  * }}}
  *
  *
  * User: Dick Carter
  * Date: 1/29/2014
  *
  */

private [cogx] class Stats(val unitsDesignator: String = "") {
  var numSamples = 0
  var numOutliers = 0
  var minSample = 0.0
  var maxSample = 0.0
  private var sum = 0.0
  private var sumSquared = 0.0

  /* Incorporate this good sample into the statistics */
  def addSample(sample: Double) {
    minSample = if (numSamples == 0) sample else math.min(minSample, sample)
    maxSample = if (numSamples == 0) sample else math.max(maxSample, sample)
    sum += sample
    sumSquared += sample * sample
    numSamples += 1
  }
  
  /** Note that a sample was observed that should not effect the stats */
  def addOutlier() { numOutliers += 1}
  
  def totalSamples = numSamples + numOutliers

  /** Throw away all the samples */
  def reset(): Unit = {
    numSamples = 0
    numOutliers = 0
    minSample = 0.0
    maxSample = 0.0
    sum = 0.0
    sumSquared = 0.0
  }

  /** The average of the samples */
  def avg = if (numSamples > 0) (sum / numSamples) else 0.0

  /** The standard deviation of the samples */
  def stddev = {
    if (numSamples > 0) {
      val average = sum / numSamples
      val variance = sumSquared / numSamples - average * average
      math.sqrt(variance)
    }
    else
      0.0
  }

  /** Convert the stats to a string with the supplied units designator */
  def toString(designator: String) = {
    "min = %5.1f%s, avg = %5.1f%s, max = %5.1f%s, stdDev = %4.1f%s".format(
      minSample, designator,
      avg, designator,
      maxSample, designator,
      stddev, designator)  +
    (if (numOutliers > 0) "[outliers = " + numOutliers + "]" else "")
  }

  override def toString = toString(unitsDesignator)
}