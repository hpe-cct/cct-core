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

package cogx.platform.cpumemory

import cogx.platform.types.{Pixel, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import org.junit.runner.RunWith

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ImageMemorySpec extends FunSuite with MustMatchers {

  test("2D image") {
    val VectorLength = 3
//    val ref = new Pixel(0.123f, 0.6f, 0.192f)
    val ref = new Pixel(17, 19, 29)
    val pixel = new Pixel
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Rows, Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Uint8Pixel)
    val memory = new ColorFieldMemory(fieldType, IndirectBuffer)
    //def norm(value: Int): Float = (value / 255f)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, pixel)
      require(pixel.red == 0f &&
              pixel.green == 0f &&
              pixel.blue == 0f)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val pixel = new Pixel(row, col, row + col)
      memory.write(row, col, pixel)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, pixel)
      require(pixel.red == row &&
              pixel.green == col &&
              pixel.blue == row + col)

    }
    def generator(row: Int, col: Int) =
      new Pixel(0, row, col)
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, pixel)
      require(pixel ==
              new Pixel(0, row, col))
    }
  }
}