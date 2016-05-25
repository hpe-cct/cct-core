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

/** Simple mechanism for timing a code path.
  *
  * To use:
  * {{{
  *   val time = new Time
  *   ...// code here
  *   time.print  // time since construction
  *   ...// code here
  *   time.reset  // time since construction
  *   ...// code here
  *   time.print  // time since last reset
  * }}}
  *
  * This will print out the time to execute the code in braces.
  *
  * @author Greg Snider
  */

private [utilities] class Time {
  println("--- time starting")
  private var startTime = System.nanoTime

  /** Print elapsed time since construction or last reset. */
  def print {
    val endTime = System.nanoTime
    val elapsed = (endTime - startTime) / 1e9
    println("--- elapsed time: " + elapsed.toFloat + " seconds")
  }

  /** Reset time keeping. */
  def reset {
    println("--- time reset")
    startTime = System.nanoTime
  }
}

/** Alternate simple mechanism for timing a code path.
  *
  * To use:
  *
  * {{{
  *   Time {
  *     // arbitrary code goes here
  *   }
  * }}}
  */
private [cogx] object Time {
  def apply(code: => Unit) {
    val time = new Time
    code
    time.print
  }
}
