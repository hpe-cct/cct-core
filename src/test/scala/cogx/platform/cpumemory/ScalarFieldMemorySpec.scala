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

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
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
class ScalarFieldMemorySpec extends FunSuite with MustMatchers {

  test("0D scalar field") {
    val fieldType = new FieldType(Shape(), Shape(), Float32)
    val memory = new ScalarFieldMemory(fieldType, IndirectBuffer)
    require(memory.read() == 0)
    memory.write(1.234f)
    require(memory.read == 1.234f)
    def generator() = 9.87f
    memory.init(generator _)
    require(memory.read == 9.87f)
    val iterator = new Iterator[Float] {
      var count = 0
      def hasNext = count == 0
      def next: Float = 8.234f
    }
    memory.write(iterator)
    require(memory.read == 8.234f)
    for (value <- memory) {
      require(value == 8.234f, " actual: " + value)
    }
  }

  test("1D scalar field") {
    val Columns = 19
    val fieldType = new FieldType(Shape(Columns), Shape(), Float32)
    val memory = new ScalarFieldMemory(fieldType, IndirectBuffer)
    for (col <- 0 until Columns)
      require(memory.read(col) == 0)
    for (col <- 0 until Columns)
      memory.write(col, col + 1.234f)
    for (col <- 0 until Columns)
      require(memory.read(col) == col + 1.234f)
    def generator(col: Int) = col.toFloat
    memory.init(generator _)
    for (col <- 0 until Columns)
      require(memory.read(col) == col)
    val iterator = new Iterator[Float] {
      var count = 0
      def hasNext = count < Columns
      def next: Float = {
        count += 1
        (count - 1) * 99f
      }
    }
    memory.write(iterator)
    for (col <- 0 until Columns)
      require(memory.read(col) == col * 99f)
    var index = 0
    for (value <- memory) {
      require(value == index * 99f, " actual: " + value)
      index += 1
    }
  }

  test("2D scalar field") {
    val Rows = 7
    val Columns = 13
    val fieldType = new FieldType(Shape(Rows, Columns), Shape(), Float32)
    val memory = new ScalarFieldMemory(fieldType, IndirectBuffer)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(memory.read(row, col) == 0)
    for (row <- 0 until Rows; col <- 0 until Columns)
      memory.write(row, col, row * col + 1.234f)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(memory.read(row, col) == row * col + 1.234f)
    def generator(row: Int, col: Int) = row * 5 + col.toFloat
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(memory.read(row, col) == row * 5 + col.toFloat)
    val iterator = new Iterator[Float] {
      var count = 0
      def hasNext = count < Rows * Columns
      def next: Float = {
        count += 1
        (count - 1) * 99f
      }
    }
    memory.write(iterator)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      require(memory.read(row, col) == (row * Columns + col) * 99f)
    }
    var row = 0
    var column = 0
    for (value <- memory) {
      val expected = (row * Columns + column) * 99f
      require(value == expected, " actual: " + value)
      column += 1
      if (column == Columns) {
        column = 0
        row += 1
      }
    }
  }

  test("3D scalar field") {
    val Layers = 5
    val Rows = 7
    val Columns = 13
    val fieldType = new FieldType(Shape(Layers, Rows, Columns), Shape(), Float32)
    val memory = new ScalarFieldMemory(fieldType, IndirectBuffer)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns)
      require(memory.read(layer, row, col) == 0)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns)
      memory.write(layer, row, col, layer + row * col + 1.234f)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      require(memory.read(layer, row, col) == layer + row * col + 1.234f)
    }
    def generator(layer: Int, row: Int, col: Int) = layer + row * 5 + col.toFloat
    memory.init(generator _)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns)
      require(memory.read(layer, row, col) == layer + row * 5 + col.toFloat)
    val iterator = new Iterator[Float] {
      var count = 0
      def hasNext = count < Layers * Rows * Columns
      def next: Float = {
        count += 1
        (count - 1) * 99f
      }
    }
    memory.write(iterator)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      require(memory.read(layer, row, col) ==
              (layer * Rows * Columns + row * Columns + col) * 99f)
    }
    var layer = 0
    var row = 0
    var column = 0
    for (value <- memory) {
      val expected = (layer * Rows * Columns + row * Columns + column) * 99f
      require(value == expected, " actual: " + value)
      column += 1
      if (column == Columns) {
        column = 0
        row += 1
        if (row == Rows) {
          row = 0
          layer += 1
        }
      }
    }
  }
}