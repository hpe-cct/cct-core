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

import cogx.cogmath.algebra.real.{VirtualArray, Tensor3, Logarithm}
import cogx.cogmath.algebra.complex.ComplexTensor3


/** Object which performs a 3D FFT of a 3-dimensional scalar field.
  *
  * The number of rows and columns in the data must each be a power of 2.
  *
  * @author Greg Snider
  */

private [cogx] object FFT3D extends Logarithm {

  /** Take the 3D FFT of "matrix". */
  private[cogx] def transform(matrix: Tensor3): ComplexTensor3 =
    transform(new ComplexTensor3(matrix.copy))

  private[cogx] def transform(imageIn: ComplexTensor3): ComplexTensor3 = {
    val image = imageIn.copy
    // Take the 1D transforms along arrays in each of the 3 directions.
    for (axis <- 0 until 3) {
      val real: Array[Array[VirtualArray]] = image.real.asArrays(axis)
      val imag: Array[Array[VirtualArray]] = image.imaginary.asArrays(axis)
      for (row <- 0 until real.length; col <- 0 until real(0).length) {
        val realArray = real(row)(col)
        val imagArray = imag(row)(col)
        val fft = FFT(realArray.length)
        fft.fft(realArray, imagArray)
      }
    }
    image
  }

  /** Take the 3D inverse FFT of "field". */
  private[cogx] def inverseTransform(imageIn: ComplexTensor3): ComplexTensor3 = {
    val image: ComplexTensor3 = imageIn.copy
    // Take the 1D transforms along arrays in each of the 3 directions.
    for (axis <- 0 until 3) {
      val real: Array[Array[VirtualArray]] = image.real.asArrays(axis)
      val imag: Array[Array[VirtualArray]] = image.imaginary.asArrays(axis)
      for (row <- 0 until real.length; col <- 0 until real(0).length) {
        val realArray = real(row)(col)
        val imagArray = imag(row)(col)
        val fft = FFT(realArray.length)
        fft.inverseFft(realArray, imagArray)
      }
    }
    image
  }
}
