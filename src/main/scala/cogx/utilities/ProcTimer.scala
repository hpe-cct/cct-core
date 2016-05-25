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

/**
  * Created by IntelliJ IDEA.
  * User: Dick Carter
  * Date: Mar 25, 2010
  * Time: 9:44:22 AM
  */

@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private  object ProcTimer {

  private def durationMs(procToTime: => Unit): Double = {
    val procStartTime = System.nanoTime
    procToTime
    val procEndTime = System.nanoTime
    (procEndTime - procStartTime) / 1000000.0
  }

  def apply(timeEnabled: Boolean, procToTime: => Unit): Double = {
    if (timeEnabled)
      durationMs(procToTime)
    else {
      procToTime
      0.0
    }
  }

  def apply(procToTime: => Unit) = durationMs(procToTime)
}