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
import cogx.platform.types.ElementTypes.Complex32
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import org.junit.runner.RunWith
import cogx.cogmath.algebra.complex.Complex

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexFieldMemorySpec extends FunSuite with MustMatchers {

  test("0D complex field") {
    val ref = new Complex(0.123f, 5.924f)
    val zero = new Complex(0, 0)
    val fieldShape = Shape()
    val tensorShape = Shape()
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexFieldMemory(fieldType, IndirectBuffer)
    var complex = memory.read()
    require(complex == zero)
    memory.write(ref)
    complex = memory.read()
    require(complex == ref)
    def generator() = new Complex(9.87f, 1.999f)
    memory.init(generator _)
    complex = memory.read()
    require(complex == new Complex(9.87f, 1.999f))
  }

  test("1D complex field") {
    val ref = new Complex(0.123f, 5.924f)
    val zero = new Complex(0, 0)
    val Columns = 19
    val fieldShape = Shape(Columns)
    val tensorShape = Shape()
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexFieldMemory(fieldType, IndirectBuffer)
    for (col <- 0 until Columns) {
      val complex = memory.read(col)
      require(complex == zero)
    }
    for (col <- 0 until Columns) {
      memory.write(col, new Complex(col, 9.87654f))
    }
    for (col <- 0 until Columns) {
      var complex = memory.read(col)
      require(complex == new Complex(col, 9.87654f))
    }
    def generator(col: Int) = new Complex(col.toFloat, 0f)
    memory.init(generator _)
    for (col <- 0 until Columns) {
      val complex = memory.read(col)
      require(complex == new Complex(col.toFloat, 0f))
    }
  }

  test("2D complex field") {
    val ref = new Complex(0.123f, 5.924f)
    val zero = new Complex(0, 0)
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Rows, Columns)
    val tensorShape = Shape()
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexFieldMemory(fieldType, IndirectBuffer)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(row, col)
      require(complex == zero)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.write(row, col, new Complex(row, col))
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(row, col)
      require(complex == new Complex(row, col))
    }
    def generator(row: Int, col: Int) = new Complex(col, row)
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(row, col)
      require(complex == new Complex(col, row))
    }
  }

  test("3D complex field") {
    val ref = new Complex(0.123f, 5.924f)
    val zero = new Complex(0, 0)
    val Layers = 7
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Layers, Rows, Columns)
    val tensorShape = Shape()
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexFieldMemory(fieldType, IndirectBuffer)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(layer, row, col)
      require(complex == zero)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      val complex = new Complex(layer + row, layer - col)
      memory.write(layer, row, col, complex)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(layer, row, col)
      require(complex == new Complex(layer + row, layer - col))
    }
    def generator(layer: Int, row: Int, col: Int) = new Complex(col, row + col)
    memory.init(generator _)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      val complex = memory.read(layer, row, col)
      require(complex == new Complex(col, row + col))
    }
  }
 }