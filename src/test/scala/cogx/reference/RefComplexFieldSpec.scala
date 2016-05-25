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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.complex.{ComplexImplicits, Complex}

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefComplexFieldSpec extends FunSuite with MustMatchers with ComplexImplicits {
  val Rows = 2
  val Columns = 3
  val Layers = 5

  test("ZeroConstructors") {
    val field1 = RefComplexField(Rows)
    val field2 = RefComplexField(Rows, Columns)
    val field3 = RefComplexField(Layers, Rows, Columns)
    require(field1.fieldShape == Shape(Rows))
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
    require(field1.points == Rows)
    require(field2.points == Rows * Columns)
    require(field3.points == Layers * Rows * Columns)
    for (row <- 0 until Rows) {
      require(field1.read(row) == Complex(0, 0))
      for (col <- 0 until Columns) {
        require(field2.read(row, col) == Complex(0, 0))
        for (layer <- 0 until Layers) {
          require(field3.read(layer, row, col) == Complex(0, 0))
        }
      }
    }
  }

  test("FunctionalConstructor") {
    def toValue1(row: Int) = Complex(row.toFloat, 0)
    def toValue2(row: Int, col: Int) = Complex((row + 10 * col).toFloat, 0)
    def toValue3(layer: Int, row: Int, col: Int) =
      Complex((row + 10 * col + 100 * layer).toFloat, 0)

    val field1 = RefComplexField(Rows, toValue1 _)
    val field2 = RefComplexField(Rows, Columns, toValue2 _)
    val field3 = RefComplexField(Layers, Rows, Columns, toValue3 _)

    for (row <- 0 until Rows) {
      require(field1.read(row) == toValue1(row))
      for (col <- 0 until Columns) {
        require(field2.read(row, col) == toValue2(row, col))
        for (layer <- 0 until Layers) {
          require(field3.read(layer, row, col) == toValue3(layer, row, col))
        }
      }
    }
  }

  test("RealImaginaryConstructor") {
    val shape = Shape(2)
    val real = RefScalarField(2,
      (i) => i match {case 0 => 3f; case 1 => 5f; case _ => 0f})
    val imaginary = RefScalarField(2,
      (i) => i match {case 0 => 7f; case 1 => 11f; case _ => 0f})
    val fieldR = RefComplexField(real)
    val fieldC = RefComplexField(real, imaginary)
    require(fieldR.fieldShape == shape && fieldC.fieldShape == shape)
    require(fieldR.read(0) == Complex(3, 0))
    require(fieldR.read(1) == Complex(5, 0))
    require(fieldC.read(0) == Complex(3, 7))
    require(fieldC.read(1) == Complex(5, 11))

    // Test polar constructor
    val magnitude = RefScalarField(2,
      (i) => i match {case 0 => 3f; case 1 => 5f; case _ => 0f})
    val phase = RefScalarField(2,
      (i) => i match {case 0 => 0.27f; case 1 => 1.46f; case _ => 0f})
    val field = RefComplexField.polar(magnitude, phase)
    require(field.read(0).magnitude ~== magnitude.read(0))
    require(field.read(0).phase ~== phase.read(0))
    require(field.read(1).magnitude ~== magnitude.read(1))
    require(field.read(1).phase ~== phase.read(1))
  }

  test("EqualsAndCopy") {
    val field1 = RefComplexField.random(Layers, Rows, Columns)
    val field2 = RefComplexField.random(Layers, Rows, Columns)
    require(field1 == field1)
    require(!(field1 == field2))
    require(field1 != field2)
    val field1Copy = field1.copy
    require(field1 == field1Copy)
  }


  test("ScalarAlgebra") {
    val f = RefComplexField.random(Layers, Rows, Columns)
    val fPlus1 = f + 1
    val fMinus2 = f - 2
    val fTimes3 = f * 3
    val fDiv4 = f / 4
    val negF = -f
    for (i <- f.fieldShape.indices) {
      val layer = i(0)
      val row = i(1)
      val col = i(2)
      require(fPlus1.read(layer, row, col) == f.read(layer, row, col) + 1)
      require(fMinus2.read(layer, row, col) == f.read(layer, row, col) - 2)
      require(fTimes3.read(layer, row, col) == f.read(layer, row, col) * 3)
      require(fDiv4.read(layer, row, col) == f.read(layer, row, col) / 4)
      require(negF.read(layer, row, col) == f.read(layer, row, col) * -1)
    }
  }

  test("FieldAlgebra") {
    val f1 = RefComplexField.random(Layers, Rows, Columns)
    val f2 = RefComplexField.random(Layers, Rows, Columns)
    val sum = f1 + f2
    val diff = f1 - f2
    val product = f1 :* f2
    val quotient = f1 :/ f2
    for (i <- f1.fieldShape.indices) {
      val layer = i(0)
      val row = i(1)
      val col = i(2)
      require(sum.read(layer, row, col) ==
              f1.read(layer, row, col) + f2.read(layer, row, col))
      require(diff.read(layer, row, col) ==
              f1.read(layer, row, col) - f2.read(layer, row, col))
      require(product.read(layer, row, col) ==
              f1.read(layer, row, col) * f2.read(layer, row, col))
      require(quotient.read(layer, row, col) ==
              f1.read(layer, row, col) / f2.read(layer, row, col))
    }
  }

  test("Map") {
    val field = RefComplexField.random(Layers, Rows, Columns)
    val field3X = field.map(_ * 3.0f)
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers){
      require(field3X.read(layer, row, col) == field.read(layer, row, col) * 3)
    }
  }

  test("Reduce") {
    val field = RefComplexField.random(Layers, Rows, Columns)
    var sum = Complex(0, 0)
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers)
      sum += field.read(layer, row, col)
    val reduced = field.reduce(_ + _)
    require(sum ~== reduced, " expected: " + sum + " actual: " + reduced)
  }

  /*
  private def testBinaryMap {
    val field1 = ComplexRefScalarField.random(Rows, Columns, Layers)
    val field2 = ComplexRefScalarField.random(Rows, Columns, Layers)
    val minField = field1.binaryMap(field2, _ + _)

    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers) {
      require(minField.read(row, col, layer) ==
        (field1.read(row, col, layer) + field2.read(row, col, layer)))
    }
  }
  */

  test("Dot") {
    val field1 = RefComplexField.random(Layers, Rows, Columns)
    val field2 = RefComplexField.random(Layers, Rows, Columns)
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

  test("Slice") {
    val field = RefComplexField.random(2, 2)
    val row0 = field.slice(0)
    require(row0.read(0) == field.read(0, 0))
    require(row0.read(1) == field.read(0, 1))
    val row1 = field.slice(1)
    require(row1.read(0) == field.read(1, 0))
    require(row1.read(1) == field.read(1, 1))
  }

  test("Stack") {
    val field0 = RefComplexField.random(5, 7)
    val field1 = RefComplexField.random(5, 7)
    val field2 = RefComplexField.random(5, 7)
    val expected = RefComplexField(3, 5, 7,
      (layer, row, col) => {
        if (layer == 0) field0.read(row, col)
        else if (layer == 1) field1.read(row, col)
        else field2.read(row, col)
      }
    )
    val stack = field0.stack(field1, field2)
    require(stack == expected)
  }

  /*
  def testSubfield {
    val field1 = ComplexRefScalarField.random(Rows)
    val field2 = ComplexRefScalarField.random(Rows, Columns)
    val field3 = ComplexRefScalarField.random(Rows, Columns, Layers)
    val rows = 1 to 1
    val columns = 1 to 2
    val layers = 2 to 4
    val subfield1 = field1(rows)
    val subfield2 = field2(rows, columns)
    val subfield3 = field3(rows, columns, layers)
    require(subfield1.fieldShape == new Shape(rows.length))
    require(subfield2.fieldShape == new Shape(rows.length, columns.length))
    require(subfield3.fieldShape == new Shape(rows.length, columns.length, layers.length))
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
  */

  /*




  def testSubfields {
    val Rows = 3
    val Columns = 4
    val Diameter = 3
    val Radius = Diameter / 2
    val BorderFill = true
    val data = new Array[Float](Rows * Columns)
    var index = 0
    for (row <- 0 until Rows; col <- 0 until Columns) {
      data(index) = (row * 10 + col).toFloat
      index += 1
    }
    val matrix = new Matrix(Rows, Columns, data)
    def read(row: Int, col: Int): Float = {
      matrix(row max 0 min (Rows - 1), col max 0 min (Columns - 1))
    }

    //matrix.print
    val field = new RealField(Rows, Columns, 1, Array[Int](), data)
    val expanded = field.subfields(Diameter, BorderFill)
    for (r <- 0 until Rows; c <- 0 until Columns) {
      val m = expanded.matrixAt(r, c, 0)
      val expectedMatrix = new Matrix(Diameter, Diameter)
      for (row <- -Radius to Radius; col <- -Radius to Radius) {
        expectedMatrix(row + Radius, col + Radius) =
          read(r + row, c + col)
      }
      require(m === expectedMatrix)
      //println("RealField location (" + r + ", " + c + ")")
      //m.print
    }
    //println
  }



  def testCrossDot {
    val field1 = randomMatrixField
    val matrix = new Matrix(MatrixSize, MatrixSize); matrix.randomize
    val field2 = ComplexRefScalarField(matrix)
    val crossCorrelation = field1 crossDot field2
    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers) {
      val submatrix = field1.matrixAt(row, col, layer)
      val dotProduct = submatrix dot matrix
      require(crossCorrelation.realAt(row, col, layer) == dotProduct)
    }
  }

  def testPoundMinus {
    val M = 2
    val N = 3
    val X = 5
    val Y = 7
    val left = ComplexRefScalarField
    println
  }

  def testReplicate {
    val Rows1 = 11
    val Columns1 = 13
    val Rows2 = 17
    val Columns2 = 19
    val randomMatrix = new Matrix(Rows1, Columns1).randomize
    val field1 = ComplexRefScalarField(randomMatrix)
    val field2 = MatrixField(Rows2, Columns2, 1, (r: Int, c: Int, l: Int) =>
      new Matrix(Rows1, Columns1).randomize)
    val rep = field1 replicate field2
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
      require(rep.matrixAt(row, col, layer) === randomMatrix)
    }
  }

  */
}