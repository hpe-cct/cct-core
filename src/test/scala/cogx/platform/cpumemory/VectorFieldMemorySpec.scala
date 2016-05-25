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
import cogx.cogmath.algebra.real.Vector
import org.junit.runner.RunWith

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class VectorFieldMemorySpec extends FunSuite with MustMatchers {

  test("0D vector field") {
    val VectorLength = 3
    val ref = Vector(1.2f, 2.3f, 3.4f)
    val zero = Vector(0f, 0f, 0f)
    val fieldShape = Shape()
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new VectorFieldMemory(fieldType, IndirectBuffer)
    val vector = new Vector(VectorLength)
    memory.read(vector)
    require(vector == zero)
    memory.write(ref)
    memory.read(vector)
    require(vector == ref)
    def generator() = Vector(9.87f, 1.999f, 0.1f)
    memory.init(generator _)
    memory.read(vector)
    require(vector == Vector(9.87f, 1.999f, 0.1f))
  }

  test("1D vector field") {
    val VectorLength = 3
    val ref = Vector(1.2f, 2.3f, 3.4f)
    val zero = Vector(0f, 0f, 0f)
    val vector = new Vector(VectorLength)
    val Columns = 19
    val fieldShape = Shape(Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new VectorFieldMemory(fieldType, IndirectBuffer)
    for (col <- 0 until Columns) {
      memory.read(col, vector)
      require(vector == zero)
    }
    for (col <- 0 until Columns) {
      memory.write(col, ref + col)
    }
    for (col <- 0 until Columns) {
      memory.read(col, vector)
      require(vector == ref + col)
    }
    def generator(col: Int) = Vector(col.toFloat, 0f, 1f)
    memory.init(generator _)
    for (col <- 0 until Columns) {
      memory.read(col, vector)
      require(vector == Vector(col.toFloat, 0f, 1f))
    }
  }

  test("2D vector field") {
    val VectorLength = 3
    val ref = Vector(1.2f, 2.3f, 3.4f)
    val zero = Vector(0f, 0f, 0f)
    val vector = new Vector(VectorLength)
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Rows, Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new VectorFieldMemory(fieldType, IndirectBuffer)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, vector)
      require(vector == zero)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.write(row, col, ref + col * row)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, vector)
      require(vector == ref + col * row)
    }
    def generator(row: Int, col: Int) = Vector(col.toFloat, 0f, row.toFloat)
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, vector)
      require(vector == Vector(col.toFloat, 0f, row.toFloat))
    }
  }

  test("3D vector field") {
    val VectorLength = 3
    val ref = Vector(1.2f, 2.3f, 3.4f)
    val zero = Vector(0f, 0f, 0f)
    val vector = new Vector(VectorLength)
    val Layers = 7
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Layers, Rows, Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new VectorFieldMemory(fieldType, IndirectBuffer)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, vector)
      require(vector == zero)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.write(layer, row, col, ref + col * row + layer)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, vector)
      require(vector == ref + col * row + layer)
    }
    def generator(layer: Int, row: Int, col: Int) = Vector(col.toFloat, 0f, row.toFloat)
    memory.init(generator _)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, vector)
      require(vector == Vector(col.toFloat, 0f, row.toFloat))
    }
  }
}