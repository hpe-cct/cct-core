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

import cogx.cogmath.algebra.complex.ComplexArray
import cogx.cogmath.algebra.real.VirtualArray
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}

private[cogx] class FFT(size: Int) {
  /** Perform the FFT in-place on "data". */
  private[cogx] def transform(data: ComplexArray) {
    require(data.length == size)
    fft(data.viewRealPart, data.viewImaginaryPart)
  }

  /** Perform the inverse FFT in-place on "data". */
  private[cogx] def inverseTransform(data: ComplexArray) {
    require(data.length == size)
    inverseFft(data.viewRealPart, data.viewImaginaryPart)
  }

  private[cogx] def fft(real: VirtualArray, imag: VirtualArray): Unit = {
    require(real.length == imag.length)
    val dataRI = Array.fill[Double](2, real.length)(0.0)

    for (i <- 0 until real.length) {
      dataRI(0)(i) = real(i)
      dataRI(1)(i) = imag(i)
    }

    FastFourierTransformer.transformInPlace(dataRI, DftNormalization.STANDARD, TransformType.FORWARD)

    for (i <- 0 until real.length) {
      real.update(i, dataRI(0)(i).toFloat)
      imag.update(i, dataRI(1)(i).toFloat)
    }
  }

  private[cogx] def inverseFft(real: VirtualArray, imag: VirtualArray): Unit = {
    require(real.length == imag.length)
    val dataRI = Array.fill[Double](2, real.length)(0.0)

    for (i <- 0 until real.length) {
      dataRI(0)(i) = real(i)
      dataRI(1)(i) = imag(i)
    }

    FastFourierTransformer.transformInPlace(dataRI, DftNormalization.STANDARD, TransformType.INVERSE)

    for (i <- 0 until real.length) {
      real.update(i, (dataRI(0)(i) * real.length).toFloat)
      imag.update(i, (dataRI(1)(i) * real.length).toFloat)
    }
  }
}

/** Factory for creating 1-dimensional Fast Fourier Transforms, in-place,
  * for complex vectors.
  *
  * The vectors must have a length equal to a power of 2.
  * The maximum size vector for which the transform may be computed is
  * 2 ^ MaxPower.
  *
  * @author Greg Snider
  */
private [cogx] object FFT {
  val MaxPower = 12
  private lazy val transformCache: Array[FFT] = {
    val transforms = new Array[FFT](MaxPower)
    for (power <- 0 until MaxPower)
      transforms(power) = new FFT(1 << power)
    transforms
  }

  /** Get a transform object for an FFT of length "n" (power of 2). */
  private[cogx] def apply(n: Int): FFT = {
    //  private def getTransform(n: Int): FFT = {
    val m = (math.log(n) / math.log(2)).toInt    // # address bits
    require(n == (1 << m), "FFT length must be a power of 2 (size = " + n + ")")
    require(m >= 0 && m < MaxPower, "dimension " + n + " too big for FFT")
    transformCache(m)
  }
}
