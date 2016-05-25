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

package cogx.reference.memory

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import cogx.platform.types.Pixel

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefImageMemorySpec extends FunSuite with MustMatchers {

  test("2D") {
    def makePixel(x: Int, y: Int): Pixel = new Pixel{
      red = x.toByte
      green = y.toByte
      blue = (x + y).toByte
      //(x, y, x + y, x * y)
    }
    val Rows = 5
    val Columns = 7

    val memory1 = new RefImageMemory(Shape(Rows, Columns))
    val memory2 = new RefImageMemory(Shape(Rows, Columns),
      new Array[Byte](Rows * Columns * 4))

    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory1.write(row, col, makePixel(row, col))
      memory2.write(row, col, makePixel(row, col))
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      require(memory1.read(row, col) == makePixel(row, col))
      require(memory2.read(row, col) == makePixel(row, col))
    }

    require(memory1 == memory2)
  }

  test("3D") {
    def makePixel(s: Int, x: Int, y: Int): Pixel = new Pixel {
      red = s.toByte
      green = x.toByte
      blue = y.toByte
      //(s, x, y, x * y)
    }
    val Slices = 3
    val Rows = 5
    val Columns = 7

    val memory1 = new RefImageMemory(Shape(Slices, Rows, Columns))
    val memory2 = new RefImageMemory(Shape(Slices, Rows, Columns),
      new Array[Byte](Slices * Rows * Columns * 4))

    for (slice <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns) {
      memory1.write(slice, row, col, makePixel(slice, row, col))
      memory2.write(slice, row, col, makePixel(slice, row, col))
    }
    for (slice <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns) {
      require(memory1.read(slice, row, col) == makePixel(slice, row, col))
      require(memory2.read(slice, row, col) == makePixel(slice, row, col))
    }

    require(memory1 == memory2)
  }
}