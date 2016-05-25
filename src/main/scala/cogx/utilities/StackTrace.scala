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

/** A mix-in that allows you to print out a stack trace from within your code
  * without halting execution. This skips the top 3 elements of the stack to
  * avoid printing out the stack within these methods.
  *
  * @author Greg Snider
  */

@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private trait StackTrace {
  /** Index to start printing out call stack (skips the print method). */
  val SkipIndex = 3

  /** Print out the complete stack. */
  def printStackTrace() {
    val elements: Array[StackTraceElement] =
      Thread.currentThread().getStackTrace()
    println("stack trace:")
    for (i <- SkipIndex until elements.length)
      println("   " + elements(i).getClassName() + "."
        + elements(i).getMethodName())
  }

  /** Print out, at most, the top "count" elements of the stack. */
  def printStackTrace(count: Int) {
    val elements: Array[StackTraceElement] =
      Thread.currentThread().getStackTrace()
    val number = (count + SkipIndex) min elements.length
    println("stack trace (top " + (number - SkipIndex) + "):")
    for (i <- SkipIndex until number)
      println("   " + elements(i).getClassName() + "." +
        elements(i).getMethodName())
  }
}