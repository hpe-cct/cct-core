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
import cogx.cogmath.algebra.real.{Vector, Matrix}

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RefVectorFieldSpec extends FunSuite with MustMatchers {
  val Verbose = false
  val Layers = 5
  val Rows = 2
  val Columns = 3
  val VectorLength = 7

  test("ZeroConstructors") {
    val vectorShape = Shape(VectorLength)
    val field1 = RefVectorField(Rows, vectorShape)
    val field2 = RefVectorField(Rows, Columns, vectorShape)
    val field3 = RefVectorField(Layers, Rows, Columns, vectorShape)
    require(field1.fieldShape == Shape(Rows))
    require(field2.fieldShape == Shape(Rows, Columns))
    require(field3.fieldShape == Shape(Layers, Rows, Columns))
    require(field1.tensorShape == vectorShape)
    require(field2.tensorShape == vectorShape)
    require(field3.tensorShape == vectorShape)
    require(field1.tensorSize == VectorLength)
    require(field2.tensorSize == VectorLength)
    require(field3.tensorSize == VectorLength)
    require(field1.tensorOrder == 1)
    require(field2.tensorOrder == 1)
    require(field3.tensorOrder == 1)
    require(field1.points == Rows)
    require(field2.points == Rows * Columns)
    require(field3.points == Layers * Rows * Columns)
    require(field1.numbers == Rows * VectorLength)
    require(field2.numbers == Rows * Columns * VectorLength)
    require(field3.numbers == Layers * Rows * Columns * VectorLength)
    for (row <- 0 until Rows) {
      require(field1.read(row) == new Vector(VectorLength))
      for (col <- 0 until Columns) {
        require(field2.read(row, col) == new Vector(VectorLength))
        for (layer <- 0 until Layers) {
          require(field3.read(layer, row, col) == new Vector(VectorLength))
        }
      }
    }
  }

  test("FunctionalConstructor") {
    val vectorShape = Shape(3)
    def toValue1(col: Int) = new Vector(col.toFloat, 0, 0)
    def toValue2(row: Int, col: Int) = new Vector(row, col, 0)
    def toValue3(layer: Int, row: Int, col: Int) = new Vector(layer, row, col)

    val field1 = RefVectorField(Columns, toValue1 _)
    val field2 = RefVectorField(Rows, Columns, toValue2 _)
    val field3 = RefVectorField(Layers, Rows, Columns, toValue3 _)

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
    val vectorShape = Shape(VectorLength)
    val field1 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val field2 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    require(field1 == field1)
    require(!(field1 == field2))
    require(field1 != field2)
    val field1Copy = field1.copy
    require(field1 == field1Copy)
  }


  test("ScalarAlgebra") {
    val vectorShape = Shape(VectorLength)
    val f = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val fPlus1 = f + 1
    val fMinus2 = f - 2
    val fTimes3 = f * 3
    val fDiv4 = f / 4
    val negF = -f
    for (i <- f.fieldShape.indices) {
      require(fPlus1.read(i : _*) == f.read(i : _*) + 1)
      require(fMinus2.read(i : _*) == f.read(i : _*) - 2)
      require(fTimes3.read(i : _*) == f.read(i : _*) * 3)
      require(fDiv4.read(i : _*) == f.read(i : _*) / 4)
      require(negF.read(i : _*) == f.read(i : _*) * -1)
    }
  }

  def printField(v: RefVectorField) {
    if (Verbose) {
      println("RefVectorField: ")
      v.data.foreach(v => new Vector(v).print)
      println("----------")
    }
  }

  test("FieldAlgebra") {
    val vectorShape = Shape(VectorLength)
    val f1 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val f2 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val sum = f1 + f2
    val diff = f1 - f2
    val product = f1 :* f2
    val quotient = f1 :/ f2
    for (i <- f1.fieldShape.indices) {
      require(sum.read(i : _*) == f1.read(i : _*) + f2.read(i : _*))
      require(diff.read(i : _*) == f1.read(i : _*) - f2.read(i : _*))
      require(product.read(i : _*) == f1.read(i : _*) :* f2.read(i : _*))
      require(quotient.read(i : _*) == f1.read(i : _*) :/ f2.read(i : _*))
    }
  }

  test("VectorScalar") {
    val vectorShape = Shape(VectorLength)
    val f1 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val f2 = RefScalarField.random(Layers, Rows, Columns)
    val sum = f1 + f2
    val diff = f1 - f2
    val product = f1 :* f2
    val quotient = f1 :/ f2
    for (i <- f1.fieldShape.indices) {
      require(sum.read(i : _*) ==
              f1.read(i : _*) + f2.read(i : _*))
      require(diff.read(i : _*) ==
              f1.read(i : _*) - f2.read(i : _*))
      require(product.read(i : _*) ==
              f1.read(i : _*) * f2.read(i : _*))
      require(quotient.read(i : _*) ==
              f1.read(i : _*) / f2.read(i : _*))
    }
  }

  test("Map") {
    val vectorShape = Shape(VectorLength)
    val field = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val field3X: RefVectorField = field.map(_ * 3.0f)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns){
      require(field3X.read(layer, row, col) == field.read(layer, row, col) * 3)
    }
  }

  test("Reduce") {
    val vectorShape = Shape(VectorLength)
    val field = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    var sum = new Vector(VectorLength)
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns)
      sum += field.read(layer, row, col)
    val reduced = field.reduce(_ + _)
    require(sum ~== reduced, "expected: " + sum + " actual: " + reduced)
  }

  /*
  test("BinaryMap") {
    val vectorShape = new Shape(VectorLength)
    val field1 = RefVectorField.random(Rows, Columns, Layers, vectorShape)
    val field2 = RefVectorField.random(Rows, Columns, Layers, vectorShape)
    val minField = field1.binaryMap(field2, _ + _)

    for (row <- 0 until Rows; col <- 0 until Columns; layer <- 0 until Layers) {
      require(minField.read(row, col, layer) ==
        (field1.read(row, col, layer) + field2.read(row, col, layer)))
    }
  } */

  test("Dot") {
    val vectorShape = Shape(VectorLength)
    val field1 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val field2 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val autoCorrelation = field1 dot field1
    require(autoCorrelation.tensorOrder == 0)
    require(autoCorrelation.rows == Rows)
    require(autoCorrelation.columns == Columns)
    require(autoCorrelation.layers == Layers)
    val correlation = field1 dot field2
    for (layer <- 0 until Layers; row <- 0 until Rows; col <- 0 until Columns) {
      val vector1 = field1.read(layer, row, col)
      val vector2 = field2.read(layer, row, col)
      val corrResult = correlation.read(layer, row, col)
      val autoResult = autoCorrelation.read(layer, row, col)
      require(corrResult == (vector1 dot vector2))
      require(autoResult == (vector1 dot vector1))
    }
  }

  test("Slice") {
    val field = RefVectorField.random(2, 2, Shape(1))
    val row0 = field.slice(0)
    if (Verbose) {
      row0.read(0).print
      field.read(0,0).print
    }

    require(row0.read(0) == field.read(0, 0))
    require(row0.read(1) == field.read(0, 1))
    val row1 = field.slice(1)
    require(row1.read(0) == field.read(1, 0))
    require(row1.read(1) == field.read(1, 1))
  }

  test("Stack") {
    val vectorShape = Shape(11)
    val field0 = RefVectorField.random(5, 7, vectorShape)
    val field1 = RefVectorField.random(5, 7, vectorShape)
    val field2 = RefVectorField.random(5, 7, vectorShape)
    val expected = RefVectorField(3, 5, 7,
      (layer, row, col) => {
        if (layer == 0) field0.read(row, col)
        else if (layer == 1) field1.read(row, col)
        else field2.read(row, col)
      }
    )
    val stack = field0.stack(field1, field2)
    require(stack == expected)
  }

  test("SliceTensor") {
    val Rows = 2
    val Columns = 3
    val image = RefVectorField(Rows, Columns,
      (row, col) => new Vector(Array[Float](row, col, row + col)))

    val slice0 = image.sliceTensor(0)
    val slice1 = image.sliceTensor(1)
    val slice2 = image.sliceTensor(2)

    val expect0 = RefScalarField(Rows, Columns, (row, col) => row)
    val expect1 = RefScalarField(Rows, Columns, (row, col) => col)
    val expect2 = RefScalarField(Rows, Columns, (row, col) => row + col)

    require(slice0 == expect0)
    require(slice1 == expect1)
    require(slice2 == expect2)
  }

  test("Subfield") {
    val vectorShape = Shape(VectorLength)
    val field1 = RefVectorField.random(Columns, vectorShape)
    val field2 = RefVectorField.random(Rows, Columns, vectorShape)
    val field3 = RefVectorField.random(Layers, Rows, Columns, vectorShape)
    val layers = 2 to 4
    val rows = 1 to 1
    val columns = 1 to 2
    val subfield1 = field1.subfield(columns)
    val subfield2 = field2.subfield(rows, columns)
    val subfield3 = field3.subfield(layers, rows, columns)
    require(subfield1.fieldShape == Shape(columns.length))
    require(subfield2.fieldShape == Shape(rows.length, columns.length))
    require(subfield3.fieldShape == Shape(layers.length, rows.length, columns.length))

    for (col <- columns) {
      require(field1.read(col) == subfield1.read(col - 1))
      for (row <- rows) {
        require(field2.read(row, col) == subfield2.read(row - 1, col - 1))
        for (layer <- layers) {

          // 3D subfields broken.
          //          require(field3.read(layer, row, col) ==
          //            subfield3.read(layer - 2, row - 1, col - 1))
        }
      }
    }
  }

  test("CrossDot") {
    val imageRows = 13
    val scalarField = RefScalarField.random(imageRows)
    val vectorShape = Shape(imageRows)
    val vectorField = RefVectorField.random(Rows, Columns, vectorShape)
    val crossCorrelation: RefScalarField = vectorField crossDot scalarField
    val scalarFieldAsVector = scalarField.toTensor[Vector]
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val vector = vectorField.read(row, col)
      val dotProduct = scalarFieldAsVector dot vector
      require(crossCorrelation.read(row, col) == dotProduct)

    }
  }

  // Test reverseCrossDot.  The "weights" field here is a 2D RefVectorField.
  // The scalarField that drives the calculation takes its fieldShape
  // from the weights field, so it's also 2D.  However, the "result" field
  // takes its fieldShape from the tensorShape of the weights field, so
  // the result field is a 1D scalar field.
  test("ReverseCrossDot") {
    val imageRows = 13
    val scalarField = RefScalarField.random(Rows, Columns)
    val vectorShape = Shape(imageRows)
    val vectorField = RefVectorField.random(Rows, Columns, vectorShape)
    val reverseCorrelation = vectorField reverseCrossDot scalarField
    val scalarFieldAsMatrix = scalarField.toTensor[Matrix]
    val expectedField = RefScalarField(imageRows, (imageR) => {
      // create a "tensor slice" of the vectorField
      val matrix = Matrix(Rows, Columns, (r,c) => vectorField.read(r,c)(imageR))
      val expected = matrix dot scalarFieldAsMatrix
      expected
    })
    require(reverseCorrelation == expectedField)
  }
  /*




  test("Subfields") {
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
    val field = new RealFieldBase(Rows, Columns, 1, Array[Int](), data)
    val expanded = field.subfields(Diameter, BorderFill)
    for (r <- 0 until Rows; c <- 0 until Columns) {
      val m = expanded.matrixAt(r, c, 0)
      val expectedMatrix = new Matrix(Diameter, Diameter)
      for (row <- -Radius to Radius; col <- -Radius to Radius) {
        expectedMatrix(row + Radius, col + Radius) =
          read(r + row, c + col)
      }
      require(m === expectedMatrix)
      //println("RealFieldBase location (" + r + ", " + c + ")")
      //m.print
    }
    //println
  }



  test("CrossDot") {
    val field1 = randomMatrixField
    val matrix = new Matrix(MatrixSize, MatrixSize); matrix.randomize
    val field2 = RefVectorField(matrix)
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
    val left = RefVectorField
    println
  }

  def testReplicate {
    val Rows1 = 11
    val Columns1 = 13
    val Rows2 = 17
    val Columns2 = 19
    val randomMatrix = new Matrix(Rows1, Columns1).randomize
    val field1 = RefVectorField(randomMatrix)
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