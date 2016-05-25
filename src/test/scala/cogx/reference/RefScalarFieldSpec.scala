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

package cogx.reference

import cogx.platform.types.{BorderValid, FieldType}
import cogx.platform.types.ElementTypes.Float32
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.{Matrix, Scalar, PoorFloat, Vector}
import cogx.reference.memory.RefFieldMemory

//import cog.util.test.CogMatchers

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefScalarFieldSpec extends FunSuite with MustMatchers {
  val Rows = 2
  val Columns = 3
  val Layers = 5

  test("ZeroConstructors") {
    val field1 = RefScalarField(Columns)
    val field2 = RefScalarField(Rows, Columns)
    val field3 = RefScalarField(Layers, Rows, Columns)
    require(field1.fieldShape == Shape(Columns))
    require(field2.fieldShape == Shape(Rows, Columns))
    require(field3.fieldShape == Shape(Layers, Rows, Columns))
    require(field1.tensorShape == Shape())
    require(field2.tensorShape == Shape())
    require(field3.tensorShape == Shape())
    require(field1.tensorSize == 1)
    require(field2.tensorSize == 1)
    require(field3.tensorSize == 1)
    require(field1.tensorOrder == 0)
    require(field2.tensorOrder == 0)
    require(field3.tensorOrder == 0)
    require(field1.points == Columns)
    require(field2.points == Rows * Columns)
    require(field3.points == Rows * Columns * Layers)
    for (col <- 0 until Columns) {
      require(field1.read(col) == 0)
      for (row <- 0 until Rows) {
        require(field2.read(row, col) == 0)
        for (layer <- 0 until Layers) {
          require(field3.read(layer, row, col) == 0)
        }
      }
    }
  }

  test("FunctionalConstructor") {
    def toValue1(col: Int) = col.toFloat
    def toValue2(row: Int, col: Int) = (row + 10 * col).toFloat
    def toValue3(layer: Int, row: Int, col: Int) =
      (row + 10 * col + 100 * layer).toFloat

    val field1 = RefScalarField(Columns, toValue1 _)
    val field2 = RefScalarField(Rows, Columns, toValue2 _)
    val field3 = RefScalarField(Layers, Rows, Columns, toValue3 _)

    for (col <- 0 until Columns) {
      require(field1.read(col) == toValue1(col))
      for (row <- 0 until Rows) {
        require(field2.read(row, col) == toValue2(row, col))
        for (layer <- 0 until Layers) {
          require(field3.read(layer, row, col) == toValue3(layer, row, col))
        }
      }
    }
  }

  test("EqualsAndCopy") {
    val field1 = RefScalarField.random(Layers, Rows, Columns)
    val field2 = RefScalarField.random(Layers, Rows, Columns)
    require(field1 == field1)
    require(!(field1 == field2))
    require(field1 != field2)
    val field1Copy = field1.copy
    require(field1 == field1Copy)
  }


  test("ScalarAlgebra") {
    val f = RefScalarField.random(Layers, Rows, Columns)
    val fPlus1 = f + 1
    val fMinus2 = f - 2
    val fTimes3 = f * 3
    val fDiv4 = f / 4
    val negF = -f
    for (i <- f.fieldShape.indices) {
      val col = i(0)
      val row = i(1)
      val layer = i(2)
      require(fPlus1.read(layer, row, col) == f.read(layer, row, col) + 1)
      require(fMinus2.read(layer, row, col) == f.read(layer, row, col) - 2)
      require(fTimes3.read(layer, row, col) == f.read(layer, row, col) * 3)
      require(fDiv4.read(layer, row, col) == f.read(layer, row, col) / 4)
      require(negF.read(layer, row, col) == f.read(layer, row, col) * -1)
    }
  }


  test("FieldAlgebra") {
    val f1 = RefScalarField.random(Layers, Rows, Columns)
    val f2 = RefScalarField.random(Layers, Rows, Columns)
    val sum = f1 + f2
    val diff = f1 - f2
    val product = f1 :* f2
    //val quotient = f1 :/ f2
    for (i <- f1.fieldShape.indices) {
      val col = i(0)
      val row = i(1)
      val layer = i(2)
      require(sum.read(layer, row, col) ==
              f1.read(layer, row, col) + f2.read(layer, row, col))
      require(diff.read(layer, row, col) ==
              f1.read(layer, row, col) - f2.read(layer, row, col))
      require(product.read(layer, row, col) ==
              f1.read(layer, row, col) * f2.read(layer, row, col))
      //require(quotient.read(layer, row, col) ==
      //        f1.read(layer, row, col) / f2.read(layer, row, col))
    }
  }


  test("Map") {
    val field = RefScalarField.random(Layers, Rows, Columns)
    val field3X = field.map(_ * 3.0f)
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers){
      require(field3X.read(layer, row, col) == field.read(layer, row, col) * 3)
    }
  }


  test("Reduce") {
    val field = RefScalarField.random(Layers, Rows, Columns)
    var sum = 0.0f
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers)
      sum += field.read(layer, row, col)
    val reduced = field.reduce(_ + _)
    require(new PoorFloat(sum) ~== reduced, "sum: " + sum + " reduced: " + reduced)
  }

  test("WinnerTakeAll") {
    val field = RefScalarField.random(Layers, Rows, Columns)
    val maxValue = field.reduce(_ max _)
    val winnerField = field.winnerTakeAll
    val winnerTotal = winnerField.reduce(_ + _)
    require(winnerTotal == 1.0f)
    var winnerRow = -1
    var winnerCol = -1
    var winnerLayer = -1

    var sum = 0f
    for (row <- 0 until field.rows;
         col <- 0 until field.columns;
         layer <- 0 until field.layers)
    {
      sum += winnerField.read(layer, row, col)
      if (winnerField.read(layer, row, col) == winnerTotal) {
        winnerRow = row
        winnerCol = col
        winnerLayer = layer
      }
    }

    require(winnerRow >= 0)
    require(field.read(winnerLayer, winnerRow, winnerCol) == maxValue)
  }


  test("Dot") {
    val field1 = RefScalarField.random(Layers, Rows, Columns)
    val field2 = RefScalarField.random(Layers, Rows, Columns)
    val autoCorrelation = field1 dot field1
    require(autoCorrelation.tensorOrder == 0)
    require(autoCorrelation.rows == Rows)
    require(autoCorrelation.columns == Columns)
    require(autoCorrelation.layers == Layers)
    val correlation = field1 dot field2
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers) {
      require(autoCorrelation == field1 :* field1)
      require(correlation == field1 :* field2)
    }
  }

  test("Subfield") {
    val field1 = RefScalarField.random(Rows)
    val field2 = RefScalarField.random(Rows, Columns)
    val field3 = RefScalarField.random(Rows, Columns, Layers)
    val rows = 1 to 1
    val columns = 1 to 2
    val layers = 2 to 4
    val subfield1 = field1.subfield(rows)
    val subfield2 = field2.subfield(rows, columns)
    val subfield3 = field3.subfield(rows, columns, layers)
    require(subfield1.fieldShape == Shape(rows.length))
    require(subfield2.fieldShape == Shape(rows.length, columns.length))
    require(subfield3.fieldShape == Shape(rows.length, columns.length, layers.length))
    for (row <- rows) {
      require(field1.read(row) == subfield1.read(row - 1))
      for (col <- columns) {
        require(field2.read(row, col) == subfield2.read(row - 1, col - 1))
        for (layer <- layers)
          require(field3.read(row, col, layer) ==
                  subfield3.read(row - 1, col - 1, layer - 2))
      }
    }
  }

  test("Subfields") {
    val Rows = 3
    val Columns = 4
    val Diameter = 3
    val Radius = Diameter / 2
    val fieldType =
      new FieldType(Shape(Rows, Columns), Shape(), Float32)
    val data = new RefFieldMemory(fieldType)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      data.write(new Scalar(row * 10 + col), row, col)
    }
    val matrix = new Matrix(Rows, Columns)
    for (row <- 0 until Rows; col <- 0 until Columns)
      matrix(row, col) = data.read(row, col).read(0)

    def read(row: Int, col: Int): Float = {
      matrix(row max 0 min (Rows - 1), col max 0 min (Columns - 1))
    }

    val field = new RefScalarField(data)
    val expanded: RefMatrixField = field.subfields(Diameter)
    for (r <- 0 until Rows; c <- 0 until Columns) {
      val m = expanded.read(r, c)
      val expectedMatrix = new Matrix(Diameter, Diameter)
      for (row <- -Radius to Radius; col <- -Radius to Radius) {
        expectedMatrix(row + Radius, col + Radius) =
                read(r + row, c + col)
      }
      require(m == expectedMatrix)
    }
  }

  test("SubfieldsValid") {
    val Rows = 11
    val Columns = 12
    val Diameter = 3
    val fieldType =
      new FieldType(Shape(Rows, Columns), Shape(), Float32)

    val data = new RefFieldMemory(fieldType)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      data.write(new Scalar(row * 10 + col), row, col)
    }
    val matrix = new Matrix(Rows, Columns)
    for (row <- 0 until Rows; col <- 0 until Columns)
      matrix(row, col) = data.read(row, col).read(0)

    def read(row: Int, col: Int): Float = {
      matrix(row max 0 min (Rows - 1), col max 0 min (Columns - 1))
    }

    val field = new RefScalarField(data)
    val expanded: RefMatrixField = field.subfields(Diameter, BorderValid)

    val outputShape = BorderValid.convolutionShape(field.fieldShape, Diameter)
    require(expanded.fieldShape == outputShape)
    val outRows = outputShape(0)
    val outColumns = outputShape(1)

    for (r <- 0 until outRows; c <- 0 until outColumns) {
      val m = expanded.read(r, c)
      val expectedMatrix = new Matrix(Diameter, Diameter)
      for (row <- 0 until Diameter; col <- 0 until Diameter) {
        expectedMatrix(row, col) = read(r + row, c + col)
      }
      require(m == expectedMatrix)
    }

  }

  test("Replicate") {
    val Rows1 = 11
    val Columns1 = 13
    val Rows2 = 17
    val Columns2 = 19
    val randomMatrix = new Matrix(Rows1, Columns1).randomize
    val field1 = RefScalarField(randomMatrix)
    val field2 = RefMatrixField(Rows2, Columns2, 1, (r: Int, c: Int, l: Int) =>
      new Matrix(Rows1, Columns1).randomize)
    val rep = field1 replicate field2
    for (row <- 0 until Rows1; col <- 0 until Columns1) {
      require(field1.read(row, col) == randomMatrix(row, col))
    }

    require(rep.rows == field2.rows)
    require(rep.columns == field2.columns)
    require(rep.layers == field2.layers)
    require(rep.tensorOrder == 2)
    require(rep.tensorShape(0) == Rows1)
    require(rep.tensorShape(1) == Columns1)
    for (row <- 0 until rep.rows;
         col <- 0 until rep.columns;
         layer <- 0 until rep.layers)
    {
      require(rep.read(layer, row, col) == randomMatrix)
    }
  }

  test("Shift") {
    val input = RefScalarField(6, (i) => if (i < 6) i.toFloat else 0f)
    val expectedA = RefScalarField(6, (i) => if (i < 6) ((i - 3) max 0) else 0f)
    require(input.shift(3) == expectedA)
    val expectedB = RefScalarField(6, (i) => if (i < 5) ((i + 1) max 0) else 0f)
    require(input.shift(-1) == expectedB)
    val data2 = Matrix(
      Array[Float](1, 2, 3, 4),
      Array[Float](4, 5, 6, 7),
      Array[Float](7, 8, 9, 10)
    )
    val data2Shift = Matrix(
      Array[Float](0, 0, 0, 0),
      Array[Float](2, 3, 4, 0),
      Array[Float](5, 6, 7, 0)
    )
    val input2 = RefScalarField(data2)
    val expected2 = RefScalarField(data2Shift)
    require(input2.shift(1, -1) == expected2)
  }

  test("ShiftCyclic") {
    val input = RefScalarField(6, (i) => if (i < 6) i.toFloat else 0f)
    val expectedA = RefScalarField(6, (i) => if (i < 6) ((i - 3 + 6) % 6) else 0f)
    require(input.shiftCyclic(3) == expectedA)
    val expectedB = RefScalarField(6, (i) => if (i < 6) ((i + 2) % 6) else 0f)
    require(input.shiftCyclic(-2) == expectedB)

    val data2 = Matrix(
      Array[Float](1, 2, 3, 4),
      Array[Float](4, 5, 6, 7),
      Array[Float](7, 8, 9, 10)
    )
    val data2Shift = Matrix(
      Array[Float](8, 9, 10, 7),
      Array[Float](2, 3, 4, 1),
      Array[Float](5, 6, 7, 4)
    )
    val input2 = RefScalarField(data2)
    val expected2 = RefScalarField(data2Shift)
    require(input2.shiftCyclic(1, -1) == expected2)
  }

  test("Slice") {
    val image = Matrix(
      Array(4f, 5f, 6f),
      Array(8f, 10f, 12f),
      Array(12f, 15f, 18f)
    )
    val field1 = RefScalarField(image)
    val row0 = field1.slice(0)
    val row1 = field1.slice(1)
    val row2 = field1.slice(2)
    require(row0 == RefScalarField(new Vector(Array(4f, 5f, 6f))))
    require(row1 == RefScalarField(new Vector(Array(8f, 10f, 12f))))
    require(row2 == RefScalarField(new Vector(Array(12f, 15f, 18f))))
  }

  test("Stack") {
    val field0 = RefScalarField.random(5, 7)
    val field1 = RefScalarField.random(5, 7)
    val field2 = RefScalarField.random(5, 7)
    val expected = RefScalarField(3, 5, 7,
      (layer, row, col) => {
        if (layer == 0) field0.read(row, col)
        else if (layer == 1) field1.read(row, col)
        else field2.read(row, col)
      }
    )
    val stack = field0.stack(field1, field2)
    require(stack == expected)
  }

  test("StackTensor") {
    val Rows = 3
    val Columns = 3
    val VectorLength = 4
    val vectorShape = Shape(VectorLength)
    val image0 = RefScalarField(Rows, Columns,
      (row, col) => row * 10 + col)
    val image1 = RefScalarField(Rows, Columns,
      (row, col) => row * 5 - col)
    val image2 = RefScalarField(Rows, Columns,
      (row, col) => row * 2 * col)
    val image3 = RefScalarField(Rows, Columns,
      (row, col) => row - col)

    val expected = RefVectorField(Rows, Columns,
      (row, col) => new Vector(
        Array[Float](row*10 + col, row*5 - col, row*2 * col, row - col))
    )
    val stackedField = image0.stackTensor(image1, image2, image3)
    require(stackedField == expected)
  }

  test("OuterProduct") {
    val field1 = RefScalarField(Matrix(
      Array[Float](2, 3),
      Array[Float](5, 7)
    ))
    val field2 = RefScalarField(new Vector(11f, 13f, 17f))
    val result = field1 ^ field2
    for (i <- 0 until field1.rows;
         j <- 0 until field1.columns;
         k <- 0 until field2.rows)
    {
      require(result.read(i, j, k) == field1.read(i, j) * field2.read(k))
    }
    val field3 = RefScalarField(19f)
    require((field1 ^ field3) == (field1 * 19))
    require((field3 ^ field1) == (field1 * 19))
  }

}