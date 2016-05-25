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

package cogx.cogmath.algebra.real

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class LogarithmSpec extends FunSuite with MustMatchers with Logarithm {

  test("Basic") {
    require(log2(2) == 1)
    require(log2(4) == 2)
    require(log2(256) == 8)
    require(log2(1024) == 10)
    require(!isPowerOf2(2047))
    require(isPowerOf2(2048))
    require(!isPowerOf2(2049))
    require(roundUpPowerOf2(255) == 256)
    require(roundUpPowerOf2(256) == 256)
    require(roundUpPowerOf2(257) == 512)
  }
}