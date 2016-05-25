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
import cogx.cogmath.algebra.complex.{ComplexImplicits, ComplexMatrix}

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class FFT2DSpec extends FunSuite with MustMatchers with ComplexImplicits {

  test("Basic") {
    val rand = new Random
    val Rows = 4
    val Columns = 8
    val InverseScale = Rows * Columns
    val image = new ComplexMatrix(Rows, Columns,
      (r: Int, c: Int) => rand.nextFloat - 0.5f)
    val freqImage = FFT2D.transform(image)
    val spaceImage = FFT2D.inverseTransform(freqImage)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(spaceImage(row, col) ~== image(row, col) * InverseScale)
    println("FFT2D passed.")
  }
}