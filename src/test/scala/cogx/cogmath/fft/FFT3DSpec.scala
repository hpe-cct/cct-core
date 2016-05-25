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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import java.util.Random
import cogx.cogmath.algebra.real.{PoorFloat, Tensor3}
import cogx.cogmath.algebra.complex.ComplexImplicits

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class FFT3DSpec extends FunSuite with MustMatchers with ComplexImplicits {

  test("Basic") {
    val rand = new Random
    val Layers = 4
    val Rows = 4
    val Columns = 4
    val InverseScale = Layers * Rows * Columns
    val image = new Tensor3(Layers, Rows, Columns)
    for (l <- 0 until Layers; r <- 0 until Rows; c <- 0 until Columns)
      image(l, r, c) = rand.nextFloat

    //println("input image")
    //image.print

    val freqImage = FFT3D.transform(image)

    //println("fourier domain")
    //freqImage.print

    val spaceImage = FFT3D.inverseTransform(freqImage).real

    //println("back to space domain")
    //spaceImage.print

    for (l <- 0 until Layers; r <- 0 until Rows; c <- 0 until Columns)
      require((spaceImage(l, r, c) ~== (image(l, r, c) * InverseScale)))
  }
}