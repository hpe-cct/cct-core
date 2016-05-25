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

package cogx.cogmath.collection

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class CircularBufferSpec extends FunSuite with MustMatchers {

  val Verbose = false
  test("Basic") {
    val Length = 10
    val buffer = new CircularBuffer(Length)
    for (i <- 0 until 2 * Length)
      buffer += i
    if (Verbose)
      println("   " + buffer.toString)
    for (i <- 0 until buffer.length)
      buffer.toArray(i) == Length + i
  }
}