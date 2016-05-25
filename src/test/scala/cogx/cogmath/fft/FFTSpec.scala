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

package cogx.cogmath.fft

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.algebra.complex.{ComplexImplicits, Complex, ComplexArray}
import java.util.Random

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class FFTSpec extends FunSuite with MustMatchers with ComplexImplicits {

  test("Spike") {
    val input = new ComplexArray(Complex(1,0), Complex(0,0), Complex(0,0),
      Complex(0,0), Complex(0,0), Complex(0,0), Complex(0,0), Complex(0,0))
    val inputCopy = input.copy

    val expectFreq = new ComplexArray(Complex(1,0), Complex(1,0), Complex(1,0),
      Complex(1,0), Complex(1,0), Complex(1,0), Complex(1,0), Complex(1,0))

    val fft = FFT(input.length)

    fft.transform(input)
    require(approxEqual(input, expectFreq))

    fft.inverseTransform(input)

    require(approxEqual(input, inputCopy * input.length))
  }

  test("DC") {
    val input = new ComplexArray(Complex(1,0), Complex(1,0), Complex(1,0),
      Complex(1,0), Complex(1,0), Complex(1,0), Complex(1,0), Complex(1,0))
    val inputCopy = input.copy

    require(approxEqual(input, inputCopy))
    val expectFreq = new ComplexArray(Complex(8,0), Complex(0,0), Complex(0,0),
      Complex(0,0), Complex(0,0), Complex(0,0), Complex(0,0), Complex(0,0))

    val fft = FFT(input.length)

    fft.transform(input)
    require(approxEqual(input, expectFreq))

    fft.inverseTransform(input)
    require(approxEqual(input, inputCopy * input.length))
  }

  test("Random") {
    val Size = 8
    val input = new ComplexArray(Size)
    val random = new Random
    for (i <- 0 until Size) {
      input(i) = Complex(random.nextFloat, random.nextFloat)
    }
    val inputCopy = input.copy
    val fft = FFT(input.length)
    fft.transform(input)
    fft.inverseTransform(input)
    require(approxEqual(input, inputCopy * input.length))
  }

  private def approxEqual(x: ComplexArray, y: ComplexArray): Boolean = {
    if (x.length != y.length)
      false
    else {
      for (i <- 0 until x.length)
        if (!approxEqual(x(i), y(i)))
          return false
      true
    }
  }

  private def approxEqual(x: Complex, y: Complex): Boolean =
    (x.real ~== y.real) && (x.imaginary ~== y.imaginary)
}