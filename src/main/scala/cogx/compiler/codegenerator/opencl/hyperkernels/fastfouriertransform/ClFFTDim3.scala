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

package cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform

import cogx.cogmath.algebra.real.Logarithm._

/** The dimensions of a 1D, 2D, or 3D FFT.
  *
  * @param x Size of X dimension.
  * @param y Size of Y dimension (1 for 1D transform).
  * @param z Size of Z dimension (1 for 1D and 2D transforms).
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
class ClFFTDim3(val x: Int, val y: Int = 1, val z: Int = 1) {
  require(isPowerOf2(x))
  require(isPowerOf2(y))
  require(isPowerOf2(z))

  /** The dimension of the transform, opportunistically reduced for sizes == 1. */
  def dimension =
    if (y == 1 && z == 1)
      1
    else if (z == 1)
      2
    else
      3

  /**
    * Converts the FFT sizes into a String.
    */
  override def toString: String = {
    dimension match {
      case 1 => s"_size_${x}"
      case 2 => s"_size_${x}_${y}"
      case 3 => s"_size_${x}_${y}_${z}"
    }
  }

}
