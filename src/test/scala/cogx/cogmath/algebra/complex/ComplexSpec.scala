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

/** Test code for  Complex numbers
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexSpec extends FunSuite with MustMatchers {
  test("Basic") {
    val a = new Complex(2, 3)
    require(a == Complex(2, 3))
    val b = new Complex(5, 7)
    require(b == Complex(5, 7))
    require(a.normSq == 2*2 + 3*3)
    require(a.magnitude == math.sqrt(2*2 + 3*3.0).toFloat)
    require(a.phase == math.atan(3.0 / 2).toFloat)
    require(b.normSq == 5*5 + 7*7)
    require(b.magnitude == math.sqrt(5*5 + 7*7.0).toFloat)
    require(b.phase == math.atan(7.0f / 5f).toFloat)
    val sum = a + b
    require(sum.real == 7 && sum.imaginary == 10)
    val diff = a - b
    require(diff.real == -3 && diff.imaginary == -4)
    val product = a * b
    require(product.real == 2*5 - 3*7)
    require(product.imaginary == 2*7 + 3*5)
    val q = a / 2.0f
    require(q.real == 1 && q.imaginary == 1.5)
    val e = a.exp
    require(e.real == (math.exp(2.0f) * math.cos(3.0f)).toFloat)
    require(e.imaginary == (math.exp(2.0) * math.sin(3.0)).toFloat)
    val x = new Complex(2, 3)
    val y = new Complex(5, 7)
    val xDivY = x / y
    val expectReal = (2*5 + 3*7) / (5*5 + 7*7.0f)
    val expectImag = (3*5 - 2*7) / (5*5 + 7*7.0f)
    val expect = new Complex(expectReal, expectImag)
    require(xDivY.real == expect.real && xDivY.imaginary == expect.imaginary)
  }

  test("Angles") {
    /*
    val degree0 = new Complex(1, 0)
    println(degree0.phase)
    val degree90 = new Complex(0, 1)
    println(degree90.phase)
    val degree180 = new Complex(-1, 0)
    println(degree180.phase)
    val degree270 = new Complex(0, -1)
    println(degree270.phase)
    */
  }

  test("Power") {
    val One = new Complex(1, 0)
    val a = new Complex(3, 5)
    val aInv = One / a
    require(a.power(-3) == aInv * aInv * aInv)
    require(a.power(-2) == aInv * aInv)
    require(a.power(-1) == aInv)
    require(a.power(0) == One)
    require(a.power(1) == a)
    require(a.power(2) == a * a)
    require(a.power(3) == a * a * a)
  }

  test("Polar") {
    //val c1 = Complex(0, 1)
    //val c2 = Complex.polar(1, Math.Pi / 2)
  }

  test("Exp") {
    import Complex._
    val a = Complex(5, 7)
    require(a.exp == expc(a))
    val b = I * 11
    require(b.exp == expc(b))
  }
}
