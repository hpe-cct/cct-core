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
import cogx.cogmath.algebra.real.Matrix
import cogx.cogmath.geometry.Shape
import org.junit.runner.RunWith

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class MatrixFieldMemorySpec extends FunSuite with MustMatchers {

  test("0D matrix field") {
    val ref = Matrix(
      Array(1.2f, 2.3f, 3.4f),
      Array(6.1f, 9.2f, 0.4f)
    )
    val zero = Matrix(
      Array(0f, 0f, 0f),
      Array(0f, 0f, 0f)
    )
    val matrix = new Matrix(ref.rows, ref.columns)
    val fieldShape = Shape()
    val tensorShape = ref.shape
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new MatrixFieldMemory(fieldType, IndirectBuffer)
    memory.read(matrix)
    require(matrix == zero)
    memory.write(ref)
    memory.read(matrix)
    require(matrix == ref)
    def generator() = Matrix(Array(9.87f, 1.999f, 0.1f), Array(0f, 0f, 0f))
    memory.init(generator _)
    memory.read(matrix)
    require(matrix == Matrix(Array(9.87f, 1.999f, 0.1f), Array(0f, 0f, 0f)))
  }

  test("1D matrix field") {
    val ref = Matrix(
      Array(1.2f, 2.3f, 3.4f),
      Array(6.1f, 9.2f, 0.4f)
    )
    val zero = Matrix(
      Array(0f, 0f, 0f),
      Array(0f, 0f, 0f)
    )
    val matrix = new Matrix(ref.rows, ref.columns)
    val Columns = 19
    val fieldShape = Shape(Columns)
    val tensorShape = ref.shape
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new MatrixFieldMemory(fieldType, IndirectBuffer)
    for (col <- 0 until Columns) {
      memory.read(col, matrix)
      require(matrix == zero)
    }
    for (col <- 0 until Columns) {
      memory.write(col, ref + col)
    }
    for (col <- 0 until Columns) {
      memory.read(col, matrix)
      require(matrix == ref + col)
    }
    def generator(col: Int) = Matrix(Array(9.87f, 1.999f, col), Array(0f, 0f, 0f))
    memory.init(generator _)
    for (col <- 0 until Columns) {
      memory.read(col, matrix)
      require(matrix == Matrix(Array(9.87f, 1.999f, col), Array(0f, 0f, 0f)))
    }
  }

  test("2D matrix field") {
    val ref = Matrix(
      Array(1.2f, 2.3f, 3.4f),
      Array(6.1f, 9.2f, 0.4f)
    )
    val zero = Matrix(
      Array(0f, 0f, 0f),
      Array(0f, 0f, 0f)
    )
    val matrix = new Matrix(ref.rows, ref.columns)
    val Rows = 7
    val Columns = 19
    val fieldShape = Shape(Rows, Columns)
    val tensorShape = ref.shape
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new MatrixFieldMemory(fieldType, IndirectBuffer)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, matrix)
      require(matrix == zero)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.write(row, col, ref + col + row)
    }
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, matrix)
      require(matrix == ref + col + row)
    }
    def generator(row: Int, col: Int) =
      Matrix(Array(9.87f, 1.999f, col), Array(0f, 0f, row))
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, matrix)
      require(matrix ==
              Matrix(Array(9.87f, 1.999f, col), Array(0f, 0f, row)))
    }
  }

  test("3D matrix field") {
    val ref = Matrix(
      Array(1.2f, 2.3f, 3.4f),
      Array(6.1f, 9.2f, 0.4f)
    )
    val zero = Matrix(
      Array(0f, 0f, 0f),
      Array(0f, 0f, 0f)
    )
    val matrix = new Matrix(ref.rows, ref.columns)
    val Layers = 9
    val Rows = 7
    val Columns = 19
    val fieldShape = Shape(Layers, Rows, Columns)
    val tensorShape = ref.shape
    val fieldType = new FieldType(fieldShape, tensorShape, Float32)
    val memory = new MatrixFieldMemory(fieldType, IndirectBuffer)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, matrix)
      require(matrix == zero)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.write(layer, row, col, ref + col + row * layer)
    }
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, matrix)
      require(matrix == ref + col + row * layer)
    }
    def generator(layer: Int, row: Int, col: Int) =
      Matrix(Array(9.87f, 1.999f, col), Array(0f, layer, row))
    memory.init(generator _)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, matrix)
      require(matrix ==
              Matrix(Array(9.87f, 1.999f, col), Array(0f, layer, row)))
    }
  }
}