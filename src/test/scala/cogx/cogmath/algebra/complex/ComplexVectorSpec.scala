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

package cogx.cogmath.algebra.complex

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code for ComplexVector.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexVectorSpec extends FunSuite with MustMatchers {

  test("DotProduct") {
    val v1 = ComplexVector(Complex(1, 2), Complex(3, 4), Complex(5, 6))
    val v2 = ComplexVector(Complex(4, 3), Complex(5, 2), Complex(7, 9))
    val product = v1 dot v2
    val expect = Complex(1, 2) * Complex(4, 3) +
            Complex(3, 4) * Complex(5, 2) +
            Complex(5, 6) * Complex(7, 9)
    require(product ~== expect)
  }
}