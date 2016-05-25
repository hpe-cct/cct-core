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

/** Test code for ComplexArray.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexArraySpec extends FunSuite with MustMatchers {

  test("basic") {
    val Size = 17
    val a = new ComplexArray(Size)
    require(a.length == Size)
    for (i <- 0 until Size)
      a(i) = Complex(i, -i)
    for (i <- 0 until Size)
      require(a(i) == Complex(i, -i))
    val b = a.mapComplex(_ * 2)
    for (i <- 0 until Size)
      require(b(i) == Complex(2 * i, -2 * i))
    val c = a.reduceLeft(_ + _)
    val expect = Complex(((Size) * (Size - 1)) / 2, -((Size) * (Size - 1)) / 2)
    require(expect == c)
  }

  test("arithmetic") {
    val a = new ComplexArray(Complex(1, -2), Complex(5, -7))
    val b = new ComplexArray(Complex(2, -3), Complex(-5, -11))
    val c = new ComplexArray(Complex(1, -2), Complex(5, -7))
    require(!(a == b))
    require(a == c)
    require(a + b == new ComplexArray(Complex(3, -5), Complex(0, -18)))
    require(a - b == new ComplexArray(Complex(-1, 1), Complex(10, 4)))
    require(a :* b == new ComplexArray(Complex(1, -2) * Complex(2, -3),
      Complex(5, -7) * Complex(-5, -11)))
    require(a + 1 == new ComplexArray(Complex(2, -2), Complex(6, -7)))
    require(a - 1 == new ComplexArray(Complex(0, -2), Complex(4, -7)))
    require(a * 2 == new ComplexArray(Complex(2, -4), Complex(10, -14)))
    require(a / 2 == new ComplexArray(Complex(0.5f, -1), Complex(2.5f, -3.5f)))

    val x = Complex(1, -1)
    require(a + x == new ComplexArray(Complex(2, -3), Complex(6, -8)))
    require(a - x == new ComplexArray(Complex(0, -1), Complex(4, -6)))
    require(a * x == new ComplexArray(Complex(1, -2) * x,
      Complex(5, -7) * x))
  }

  test("Concatenate") {
    val a = new ComplexArray(Complex(1, 1), Complex(2, 2), Complex(3, 3))
    val b = new ComplexArray(Complex(4, 4), Complex(5, 5), Complex(6, 6))
    val c = new ComplexArray(Complex(7, 7), Complex(8, 8), Complex(9, 9))
    val rows = Array(a, b, c)
    val concatenated = ComplexArray.concatenate(rows)
    for (i <- 0 until concatenated.length)
      require(concatenated(i) == Complex(i + 1, i + 1))
  }

  test("Slice") {
    val a = new ComplexArray(
      Complex(0, 0), Complex(1, 1), Complex(2, 2), Complex(3, 3)
    )
    val aSliced = a.slice(1, 3)
    require(aSliced.length == 2)
    require(aSliced(0) == Complex(1, 1))
    require(aSliced(1) == Complex(2, 2))
  }

  test("SliceAndCopyTo") {
    val a = new ComplexArray(
      Complex(0, 0), Complex(1, 1), Complex(2, 2), Complex(3, 3)
    )
    val b = new ComplexArray(6)
    a.sliceAndCopyTo(1, 3, b, 2)
    require(b(0) == Complex(0, 0))
    require(b(1) == Complex(0, 0))
    require(b(2) == a(1))
    require(b(3) == a(2))
    require(b(4) == Complex(0, 0))
    require(b(5) == Complex(0, 0))
  }

  test("Dot") {
    val a = new ComplexArray(Complex(1, -2), Complex(5, -7))
    val b = new ComplexArray(Complex(2, -3), Complex(-5, -11))
    val dotProduct = a dot b
    val expected = Complex(1, -2) * Complex(2, -3) +
            Complex(5, -7) * Complex(-5, -11)
    require(dotProduct == expected)
  }
}