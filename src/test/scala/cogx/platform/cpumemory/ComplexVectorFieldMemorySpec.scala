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
import cogx.cogmath.algebra.complex.{Complex, ComplexVector}

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexVectorFieldMemorySpec extends FunSuite with MustMatchers {

  test("0D complex vector field") {
    val VectorLength = 3
    val ref =
      ComplexVector(Complex(1.2f, 2.1f), Complex(2.3f, 3.2f), Complex(3.4f, 4.3f))
    val zero = ComplexVector(Complex(0f, 0f), Complex(0f, 0f), Complex(0f, 0f))
    val fieldShape = Shape()
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexVectorFieldMemory(fieldType, IndirectBuffer)
    val vector = new ComplexVector(VectorLength)
    memory.read(vector)
    require(vector equals zero)
    memory.write(ref)
    memory.read(vector)
    require(vector == ref)
    def generator() = ComplexVector(Complex(9.87f, 1), Complex(1.999f, 2), Complex(0.1f, 3))
    memory.init(generator _)
    memory.read(vector)
    require(vector == ComplexVector(Complex(9.87f, 1), Complex(1.999f, 2), Complex(0.1f, 3)))
  }

  test("1D complex vector field") {
    val VectorLength = 3
    val ref =
      ComplexVector(Complex(1.2f, 2.1f), Complex(2.3f, 3.2f), Complex(3.4f, 4.3f))
    val zero = ComplexVector(Complex(0f, 0f), Complex(0f, 0f), Complex(0f, 0f))
    val Columns = 19
    val fieldShape = Shape(Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexVectorFieldMemory(fieldType, IndirectBuffer)
    val vector = new ComplexVector(VectorLength)
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
    def generator(col: Int) =
      ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(1f, 0f))
    memory.init(generator _)
    for (col <- 0 until Columns) {
      memory.read(col, vector)
      require(vector == ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(1f, 0f)))
    }
  }

  test("2D complex vector field") {
    val VectorLength = 3
    val ref =
      ComplexVector(Complex(1.2f, 2.1f), Complex(2.3f, 3.2f), Complex(3.4f, 4.3f))
    val zero = ComplexVector(Complex(0f, 0f), Complex(0f, 0f), Complex(0f, 0f))
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Rows, Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexVectorFieldMemory(fieldType, IndirectBuffer)
    val vector = new ComplexVector(VectorLength)
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
    def generator(row: Int, col: Int) =
      ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(row.toFloat, 0f))
    memory.init(generator _)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(row, col, vector)
      require(vector ==
              ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(row.toFloat, 0f)))
    }
  }

  test("3D complex vector field") {
    val VectorLength = 3
    val ref =
      ComplexVector(Complex(1.2f, 2.1f), Complex(2.3f, 3.2f), Complex(3.4f, 4.3f))
    val zero = ComplexVector(Complex(0f, 0f), Complex(0f, 0f), Complex(0f, 0f))
    val Layers = 7
    val Rows = 5
    val Columns = 19
    val fieldShape = Shape(Layers, Rows, Columns)
    val tensorShape = Shape(VectorLength)
    val fieldType = new FieldType(fieldShape, tensorShape, Complex32)
    val memory = new ComplexVectorFieldMemory(fieldType, IndirectBuffer)
    val vector = new ComplexVector(VectorLength)
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
    def generator(layer: Int, row: Int, col: Int) =
      ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(row.toFloat, 0f))
    memory.init(generator _)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      memory.read(layer, row, col, vector)
      require(vector ==
              ComplexVector(Complex(col.toFloat, 1), Complex(0f, 1f), Complex(row.toFloat, 0f)))
    }
  }

}