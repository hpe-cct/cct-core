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

import cogx.cogmath.algebra.real.{Vector, Logarithm}
import cogx.cogmath.algebra.complex.{ComplexArray, ComplexVector}
import cogx.cogmath.geometry.Shape

/** Object which performs a 1D FFT (and its inverse) on 1-dimensional, real
  * and complex data. The length of the data must be a power of 2.
  *
  * @author Greg Snider
  */

private [cogx] object FFT1D extends Logarithm {

  /** Take the 1D FFT of `data`. */
  private[cogx] def transform(data: Array[Float]): ComplexArray = {
    require(isPowerOf2(data.length),
      "FFT requires data size equal to a power of 2")
    val fft = FFT(data.length)
    val image = new ComplexVector(new Vector(data), new Vector(data.length))
    fft.transform(image.asComplexArray)
    val result = new ComplexArray(data.length)
    for (i <- 0 until data.length)
      result(i) = image(i)
    result
  }

  /** Take the 1D inverse FFT of `data`. */
  private[cogx] def inverseTransform(data: ComplexArray): ComplexArray = {
    require(isPowerOf2(data.length),
      "FFT requires data size equal to a power of 2")
    val fft = FFT(data.length)
    val image: ComplexArray = data.copy
    fft.inverseTransform(image)
    image
  }
}