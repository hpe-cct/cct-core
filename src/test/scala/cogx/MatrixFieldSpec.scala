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

package cogx

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.reference._
import cogx.helper.ScalarFieldBuilderInterface
import cogx.helper.ComplexFieldBuilderInterface
import cogx.helper.MatrixFieldBuilderInterface
import cogx.helper.VectorFieldBuilderInterface
import cogx.api.ImplicitConversions
import cogx.utilities.Array2D
import cogx.compiler.codegenerator.opencl.hyperkernels.MatrixVectorTransformHyperKernel

/** Test code for MatrixFields.
 */

@RunWith(classOf[JUnitRunner])
class MatrixFieldSpec extends FunSuite with MustMatchers
                      with ImplicitConversions
                      with ScalarFieldBuilderInterface
                      with ComplexFieldBuilderInterface
                      with MatrixFieldBuilderInterface
                      with VectorFieldBuilderInterface {
  val MatrixSize = 3
  val MatrixShape = Shape(MatrixSize, MatrixSize)

  val Optimize = true

  /** Test combining a dynamic scalar field and a constant. */
  test("matrix field / constant") {
    val Size = 5

    val R = 3.21f
    val initField = RefMatrixField.random(Size, Size, MatrixShape)

    // Tests that involve equality should involve a threshold that matches
    // at least one element.  Here are some elements:
    val elem1 = initField.read(0,0)(0,0)
    val elem2 = initField.read(Size-1,0)(0,0)
    val elem3 = initField.read(Size-1,Size-1)(0,0)
    val elem4 = initField.read(0,0)(MatrixSize-1,MatrixSize-1)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestMatrixField(initField)
      val sum = a + R
      val diff = a - R
      val product = a * R
      val quotient = a / R
      val maxa = max(a, 0.5f)
      val mina = min(a, 0.5f)
      val power = pow(a, 3.0f)
      val pown = pow(a, 3)

      val greater = a > 0.5f
      val greaterEq = a >= elem1
      val less = a < 0.5f
      val lessEq = a <= elem2
      val eq3 = a === elem3
      val nEq3 = a !=== elem4

      // Constant appears first in mutator expression
      val constSum = R + a
      val constDiff = R - a
      val constProduct = R * a
      val constQuotient = R / a

      probe(sum, a, diff, product, quotient, maxa, mina, power, pown, constSum, constDiff, constProduct, constQuotient,
        greater, greaterEq, less, lessEq, eq3, nEq3)
    }

    import graph._
    withRelease {
      step

      require(readMatrix(sum) == (readMatrix(graph.a) + R))
      require(readMatrix(diff) == (readMatrix(graph.a) - R))
      require(readMatrix(product) == (readMatrix(graph.a) * R))
      require(readMatrix(quotient) ~== (readMatrix(graph.a) / R))
      require(readMatrix(graph.maxa) ~==
              readMatrix(graph.a).map(m => m.map(_ max 0.5f)))
      require(readMatrix(graph.mina) ~==
              readMatrix(graph.a).map(m => m.map(_ min 0.5f)))
      require(readMatrix(power) ~== (readMatrix(graph.a) :* readMatrix(graph.a) :* readMatrix(graph.a)))
      require(readMatrix(pown) ~== (readMatrix(graph.a) :* readMatrix(graph.a) :* readMatrix(graph.a)))

      require(readMatrix(constSum) == (readMatrix(graph.a) + R))
      require(readMatrix(constDiff) == (-readMatrix(graph.a) + R))
      require(readMatrix(constProduct) == (readMatrix(graph.a) * R))
      require(readMatrix(constQuotient) ~== (readMatrix(graph.a).reciprocal * R))

      require(readMatrix(greater) == readMatrix(graph.a).map(matrixGreater(_, 0.5f)))
      require(readMatrix(greaterEq) == readMatrix(graph.a).map(matrixGreaterEq(_, elem1)))
      require(readMatrix(less) == readMatrix(graph.a).map(matrixLess(_, 0.5f)))
      require(readMatrix(lessEq) == readMatrix(graph.a).map(matrixLessEq(_, elem2)))
      require(readMatrix(eq3) == readMatrix(graph.a).map(matrixEq(_, elem3)))
      require(readMatrix(nEq3) == readMatrix(graph.a).map(matrixNotEq(_, elem4)))

    }

    def matrixGreater(v: Matrix, thresh: Float) = v.map(x => if (x > thresh) 1f else 0f)
    def matrixGreaterEq(v: Matrix, thresh: Float) = v.map(x => if (x >= thresh) 1f else 0f)
    def matrixLess(v: Matrix, thresh: Float) = v.map(x => if (x < thresh) 1f else 0f)
    def matrixLessEq(v: Matrix, thresh: Float) = v.map(x => if (x <= thresh) 1f else 0f)
    def matrixEq(v: Matrix, thresh: Float) = v.map(x => if (x == thresh) 1f else 0f)
    def matrixNotEq(v: Matrix, thresh: Float) = v.map(x => if (x != thresh) 1f else 0f)
  }

  /** Test combining two dynamic matrix fields. */
  test("matrix field / field") {
    val Size = 5
    val Constant = 0.123f

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestMatrixField(RefMatrixField.random(Size, Size, MatrixShape))
      val b = TestMatrixField(RefMatrixField.random(Size, Size, MatrixShape))
      val s = TestScalarField(RefScalarField.random(Size, Size))

      val sum = a + b
      val diff = a - b
      val product = a * b
      val quotient = a / b

      val sumS = a + s
      val diffS = a - s
      val productS = a * s
      val quotientS = a / s

      /** The scalar field can appear as the first operand. */
      val sSum = s + a
      val sDiff = s - a
      val sProduct = s * a
      val sQuotient = s / a

      val greater = a > b
      val greaterEq = a >= b
      val less = a < b
      val lessEq = a <= b
      val eq1 = a === a
      val eq2 = a === b
      val nEq1 = a !=== a
      val nEq2 = a !=== b

      /** A 0-dimensional scalar field, can be combined with any other. */
      val c = TestScalarField(RefScalarField(Constant))
      val sum0D = a + c
      val diff0D = a - c
      val product0D = a * c
      val quotient0D = a / c

      probe(sum, a, b, diff, product, quotient, sumS, s, diffS, productS, quotientS, sSum, sDiff, sProduct, sQuotient,
        greater, greaterEq, less, lessEq, eq1, eq2, nEq1, nEq2, sum0D, diff0D, product0D, quotient0D)
    }

    import graph._
    withRelease {
      step

      require(readMatrix(sum) == (readMatrix(graph.a) + readMatrix(b)))
      require(readMatrix(diff) == (readMatrix(graph.a) - readMatrix(b)))
      require(readMatrix(product) == (readMatrix(graph.a) :* readMatrix(b)))
      require(readMatrix(quotient) ~== (readMatrix(graph.a) :/ readMatrix(b)))

      require(readMatrix(sumS) == (readMatrix(graph.a) + readScalar(s)))
      require(readMatrix(diffS) == (readMatrix(graph.a) - readScalar(s)))
      require(readMatrix(productS) == (readMatrix(graph.a) :* readScalar(s)))
      require(readMatrix(quotientS) ~== (readMatrix(graph.a) :/ readScalar(s)))

      require(readMatrix(sSum) == (readMatrix(graph.a) + readScalar(s)))
      require(readMatrix(sDiff) == (-readMatrix(graph.a) + readScalar(s)))
      require(readMatrix(graph.sProduct) == (readMatrix(graph.a) :* readScalar(s)))
      require(readMatrix(sQuotient) ~== (readMatrix(graph.a).reciprocal :* readScalar(s)))

      val aa = readMatrix(graph.a)
      val bb = readMatrix(b)
      require(readMatrix(greater) == aa.combine(bb, greaterThan _))
      require(readMatrix(greaterEq) == aa.combine(bb, greaterThanEq _))
      require(readMatrix(less) == aa.combine(bb, lessThan _))
      require(readMatrix(lessEq) == aa.combine(bb, lessThanEq _))
      require(readMatrix(eq1) == aa.combine(aa, Eq _))
      require(readMatrix(eq2) == aa.combine(bb, Eq _))
      require(readMatrix(nEq1) == aa.combine(aa, notEq _))
      require(readMatrix(nEq2) == aa.combine(bb, notEq _))

      require(readMatrix(sum0D) == (readMatrix(graph.a) + Constant))
      require(readMatrix(diff0D) == (readMatrix(graph.a) - Constant))
      require(readMatrix(product0D) == (readMatrix(graph.a) * Constant))
      require(readMatrix(quotient0D) ~== (readMatrix(graph.a) / Constant))
    }

    def compare(a: Matrix, b: Matrix, f: (Float, Float) => Boolean): Matrix = {
      val result = new Matrix(a.rows, a.columns)
      for (row <- 0 until a.rows; col <- 0 until b.columns)
        result(row, col) = if (f(a(row, col), b(row, col))) 1f else 0f
      result
    }

    def greaterThan(a: Matrix, b: Matrix): Matrix = compare(a, b, _ > _)
    def greaterThanEq(a: Matrix, b: Matrix): Matrix = compare(a, b, _ >= _)
    def lessThan(a: Matrix, b: Matrix): Matrix = compare(a, b, _ < _)
    def lessThanEq(a: Matrix, b: Matrix): Matrix = compare(a, b, _ <= _)
    def Eq(a: Matrix, b: Matrix): Matrix = compare(a, b, _ == _)
    def notEq(a: Matrix, b: Matrix): Matrix = compare(a, b, _ != _)
  }

  /** Test applying unary operations on dynamic matrix fields. */
  test("matrix field / unary") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestMatrixField(
        RefMatrixField.random(Size, Size, MatrixShape) - 0.5f)
      val b = TestMatrixField(
        RefMatrixField.random(Size, Size, MatrixShape) + 0.5f)

      val aAbs = abs(a)
      val aAcos = acos(a)
      val aAsin = asin(a)
      val aCos = cos(a)
      val aCosh = cosh(a)
      val aExp = exp(a)
      val aLog = log(b)
      val aSignum = signum(a)
      val aSin = sin(a)
      val aSinh = sinh(a)
      val aSq = sq(a)
      val aSqrt = sqrt(b)
      val aTan = tan(a)
      val aTanh = tanh(a)

      val aNegative = -a

      probe(aAbs, a, aAcos, aAsin, aCos, aCosh, aExp, aLog, b, aSignum, aSin, aSinh, aSq, aSqrt, aTan, aTanh, aNegative)
    }

    //val Subsize = (Size + 1) / 2
    //val subsample = new DynamicMatrixField(Subsize, Subsize, MatrixShape) {
    //  this <== a.subsample
    //}
    import graph._
    withRelease {
      step

      require(readMatrix(aAbs) == readMatrix(graph.a).map(v => v.map(_.abs)))
      require(readMatrix(aAcos) ~== readMatrix(graph.a).map(v => v.map(e => math.acos(e).toFloat)))
      require(readMatrix(aAsin) ~== readMatrix(graph.a).map(v => v.map(e => math.asin(e).toFloat)))
      require(readMatrix(aCos) ~== readMatrix(graph.a).map(v => v.map(e => math.cos(e).toFloat)))
      require(readMatrix(aCosh) ~== readMatrix(graph.a).map(v => v.map(e => math.cosh(e).toFloat)))
      require(readMatrix(aExp) ~== readMatrix(graph.a).map(v => v.map(e => math.exp(e).toFloat)))
      require(readMatrix(aLog) ~== readMatrix(graph.b).map(v => v.map(e => math.log(e).toFloat)))
      require(readMatrix(aSignum) ~== readMatrix(graph.a).map(v => v.map(e =>
        if (e < 0) -1f else if (e > 0) 1f else 0f)))
      require(readMatrix(aSin) ~== readMatrix(graph.a).map(v => v.map(e => math.sin(e).toFloat)))
      require(readMatrix(aSinh) ~== readMatrix(graph.a).map(v => v.map(e => math.sinh(e).toFloat)))
      require(readMatrix(aSq) ~== readMatrix(graph.a).map(v => v.map(e => e * e)))
      require(readMatrix(aSqrt) ~== readMatrix(graph.b).map(v => v.map(e => math.sqrt(e).toFloat)))
      require(readMatrix(aTan) ~== readMatrix(graph.a).map(v => v.map(e => math.tan(e).toFloat)))
      require(readMatrix(aTanh) ~== readMatrix(graph.a).map(v => v.map(e => math.tanh(e).toFloat)))
      require(readMatrix(aNegative) == readMatrix(graph.a) * -1f)
    }
  }

  test("matrix field / dot") {
    val Rows = 13
    val Columns = 37

    // Test matrices of shape 1x1, 2x2, 3x3, 4x4, 5x5
    val NumTests = 5

    val fieldA = Array.tabulate(NumTests) { i =>
      RefMatrixField(Rows, Columns,
                     (row, col) => new Matrix(i+1, i+1).randomize)
                                         }
    val fieldB = Array.tabulate(NumTests) { i =>
      RefMatrixField(Rows, Columns,
                     (row, col) => new Matrix(i+1, i+1).randomize)
                                         }
    // The 2nd operand of 'dot' is allowed to be a 0D field
    val fieldB_0D = Array.tabulate(NumTests) { i =>
      RefMatrixField(new Matrix(i+1, i+1).randomize)
                                            }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val A = Array.tabulate(NumTests) { i => TestMatrixField(fieldA(i)) }
      val B = Array.tabulate(NumTests) { i => TestMatrixField(fieldB(i)) }
      val B_0D = Array.tabulate(NumTests) { i => TestMatrixField(fieldB_0D(i)) }

      val C = Array.tabulate(NumTests) { i => dot(A(i), B(i)) }
      val C_0D = Array.tabulate(NumTests) { i => dot(A(i), B_0D(i)) }

      // Concatenate all the arrays and then probe all the elements
      probe((A ++ B ++ C ++ B_0D ++ C_0D): _*)
    }

    import graph._
    withRelease {
      step
      // Check A dot B, where B is a 2D field
      for (i <- 0 until NumTests) {
        val a = readMatrix(A(i))
        val b = readMatrix(B(i))
        val c = readScalar(C(i))
        for (row <- 0 until Rows; col <- 0 until Columns) {
          val aMatrix = a.read(row, col)
          val bMatrix = b.read(row, col)
          val dotProduct = aMatrix dot bMatrix
          require(c.read(row, col) ~== dotProduct)
        }
      }
      // Check A dot B, where B is a 0D field
      for (i <- 0 until NumTests) {
        val a = readMatrix(A(i))
        val bMatrix = readMatrix(B_0D(i)).read()
        val c = readScalar(C_0D(i))
        for (row <- 0 until Rows; col <- 0 until Columns) {
          val aMatrix = a.read(row, col)
          val dotProduct = aMatrix dot bMatrix
          require(c.read(row, col) ~== dotProduct)
        }
      }
    }
  }

  /** Test MatrixField crossDot ScalarField operation */
  test("matrix field / cross dot") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s = TestScalarField(RefScalarField.random(MatrixSize, MatrixSize))
      val w = TestMatrixField(RefMatrixField.random(Size, Size, MatrixShape))
      val z = crossDot(w, s)

      probe(w, s, z)
    }

    import graph._
    withRelease {
      step
      for (row <- 0 until Size; col <- 0 until Size) {
        val wMatrix = readMatrix(w).read(row, col)
        val sMatrix = readScalar(s).toTensor[Matrix]
        val dotProduct = wMatrix dot sMatrix
        require(readScalar(z).read(row, col) ~== dotProduct)
      }
    }
  }

  /** Test MatrixField reverseCrossDot ScalarField operation */
  test("matrix field / reverse cross dot") {
    val Rows = 5
    val Columns = 7

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val z = TestScalarField(RefScalarField.random(Rows, Columns))
      val w = TestMatrixField(RefMatrixField.random(Rows, Columns, MatrixShape))
      val x = reverseCrossDot(w, z)

      probe(w, z, x)
    }

    import graph._
    withRelease {
      step
      var sum = new Matrix(MatrixShape(0), MatrixShape(1))
      for (r <- 0 until Rows; c <- 0 until Columns)
        sum += readMatrix(w).read(r, c) * readScalar(z).read(r, c)
      for (r <- 0 until MatrixSize; c <- 0 until MatrixSize)
        require(readScalar(x).read(r, c) ~== sum(r, c))
    }
  }

  /** Test the expand operator. */
  test("matrix field / expand border") {

    // 2D test parameters
    val InRows = 8
    val InColumns = 11
    val OutRows = 14
    val OutColumns = 36

    // 1D test parameters

    val InColumns_1D = 15
    val OutColumns_1D = 26

    // Test matrices of shape 1x1, 2x2, 3x3, 4x4, 5x5

    val NumTests = 5

    val inputImage = Array.tabulate(NumTests) { i =>
      RefMatrixField(InRows, InColumns,
                     (row, col) => new Matrix(i+1,i+1).randomize)
                                             }
    // Use Matrix.expand() on tensorSlices to create expected result
    val expectedImage = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1,i+1) { (r,c) =>
        RefScalarField(inputImage(i).sliceTensor(r,c).toTensor[Matrix].expand(OutRows, OutColumns, borderFill = true))
                                          }
      def matrixMaker(r: Int, c: Int) = Matrix(i+1, i+1, (matrixRow, matrixCol) => slices(matrixRow)(matrixCol).read(r,c))
      RefMatrixField(OutRows, OutColumns, matrixMaker _)
    }}

    // 1D test inputs and expected outputs

    val inputImage_1D = Array.tabulate(NumTests) { i =>
      RefVectorField(InColumns_1D,
        (col) => new Vector(i+1).randomize)
    }
    // Use Vector.expand() on tensorSlices to create expected result
    val expectedImage_1D = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        RefScalarField(inputImage_1D(i).sliceTensor(j).toTensor[Vector].expand(OutColumns_1D, borderFill = true))
      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(OutColumns_1D,
          (col) => new Vector(first.read(col)))
      else
        first.stackTensor(rest : _*)
    }}

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = Array.tabulate(NumTests) { i => TestMatrixField(inputImage(i)) }
      val expanded = Array.tabulate(NumTests) { i => expand(in(i), BorderClamp, OutRows, OutColumns) }
      val in_1D = Array.tabulate(NumTests) { i => TestVectorField(inputImage_1D(i)) }
      val expanded_1D = Array.tabulate(NumTests) { i => expand(in_1D(i), BorderClamp, OutColumns_1D) }

      // Concatenate all the arrays and then probe all the elements
      probe((expanded ++ expanded_1D): _*)
    }


    import graph._
    withRelease {
      step
      for (i <- 0 until NumTests)
        require(readMatrix(expanded(i)) == expectedImage(i))
      for (i <- 0 until NumTests)
        require(readVector(expanded_1D(i)) == expectedImage_1D(i))
    }
  }

  /** Test the (Int) operator. */
  test("matrix field / slice") {
    val image = RefMatrixField(3, 3,
                               (row, col) => Matrix(
                                 Array[Float](row, col),
                                 Array[Float](col, -row)
                               ))

    val expect0 = RefMatrixField(3, (col) => Matrix(
      Array[Float](0, col),
      Array[Float](col, 0)))
    val expect1 = RefMatrixField(3, (col) => Matrix(
      Array[Float](1, col),
      Array[Float](col, -1)))
    val expect2 = RefMatrixField(3, (col) => Matrix(
      Array[Float](2, col),
      Array[Float](col, -2)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestMatrixField(image)
      val row0 = field1(0)
      val row1 = field1(1)
      val row2 = field1(2)

      probe(row0, row1, row2)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(row0) == expect0)
      require(readMatrix(row1) == expect1)
      require(readMatrix(row2) == expect2)
    }
  }

  /** Test the (0D-ScalarField) operator (slice). */
  test("matrix field / slice point") {

    // 1D input

    val rand = new cogx.utilities.Random
    val shape1D = Shape(17)
    val matrixShape = Shape(4,3)
    val image1D = RefMatrixField.random(shape1D(0), matrixShape)
    val sliceToExtract1D = shape1D(0) * rand.nextFloat

    val shape2D = Shape(5, 7)
    val matrixShape2 = Shape(2, 2)
    val image2D = RefMatrixField.random(shape2D(0), shape2D(1), matrixShape2)
    val sliceToExtract2D = shape2D(0) * rand.nextFloat

    val shape3D = Shape(3, 9, 11)
    val matrixShape3 = Shape(1, 1)
    val image3D = RefMatrixField.random(shape3D(0), shape3D(1), shape3D(2), matrixShape3)
    val sliceToExtract3D = shape3D(0) * rand.nextFloat

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1D = TestMatrixField(image1D)
      val indexField1D = TestScalarField(RefScalarField(sliceToExtract1D))
      val slicedField0D = field1D(indexField1D)

      // 2D input

      val field2D = TestMatrixField(image2D)
      val indexField2D = TestScalarField(RefScalarField(sliceToExtract2D))
      val slicedField1D = field2D(indexField2D)

      // 3D input

      val field3D = TestMatrixField(image3D)
      val indexField3D = TestScalarField(RefScalarField(sliceToExtract3D))
      val slicedField2D = field3D(indexField3D)

      probe(slicedField0D, slicedField1D, slicedField2D)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(slicedField0D) == image1D.slice(sliceToExtract1D.toInt))
      require(readMatrix(slicedField1D) == image2D.slice(sliceToExtract2D.toInt))
      require(readMatrix(slicedField2D) == image3D.slice(sliceToExtract3D.toInt))
    }
  }

  /** Test the stack operator.  THIS WILL GO AWAY!!!!! */
  test("matrix field / stack") {
    val matrixShape = Shape(11, 13)
    val field0 = RefMatrixField.random(5, 7, matrixShape)
    val field1 = RefMatrixField.random(5, 7, matrixShape)
    val field2 = RefMatrixField.random(5, 7, matrixShape)
    val expected = RefMatrixField(3, 5, 7,
                                  (layer, row, col) => {
                                    if (layer == 0) field0.read(row, col)
                                    else if (layer == 1) field1.read(row, col)
                                    else field2.read(row, col)
                                  }
                                )


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f0 = TestMatrixField(field0)
      val f1 = TestMatrixField(field1)
      val f2 = TestMatrixField(field2)
      val fArray = Array(f0, f1, f2)
      val stack0 = stack(f0, f1, f2)
      val stack1 = stack(fArray)

      probe(stack0, stack1)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(stack0) == expected)
      require(readMatrix(stack1) == expected)
    }
  }

  test("matrix field / stack tensors") {
    val Rows = 3
    val Columns = 4
    val f00 = RefScalarField.random(Rows, Columns)
    val f01 = RefScalarField.random(Rows, Columns)
    val f10 = RefScalarField.random(Rows, Columns)
    val f11 = RefScalarField.random(Rows, Columns)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s00 = TestScalarField(f00)
      val s01 = TestScalarField(f01)
      val s10 = TestScalarField(f10)
      val s11 = TestScalarField(f11)
      val m = matrixField(Array(Array(s00, s01), Array(s10, s11)))

      probe(m)
    }

    import graph._
    withRelease {
      step
      val mm = readMatrix(m)
      require(mm.sliceTensor(0, 0) == f00)
      require(mm.sliceTensor(0, 1) == f01)
      require(mm.sliceTensor(1, 0) == f10)
      require(mm.sliceTensor(1, 1) == f11)
    }
  }

  test("matrix field / transpose") {
    // Try matrices of the following shapes: 1x1, 1x3, 2x1, 2x2, and 9x13.

    val FieldRows = 5
    val FieldColumns = 7

    // Test A: 1x1 shape

    val matrixShape_A = Shape(1, 1)
    val field_A = RefMatrixField.random(FieldRows, FieldColumns, matrixShape_A)
    val transposedField_A = RefMatrixField(FieldRows, FieldColumns,
                                           (row, col) => field_A.read(row, col).transpose)

    // Test B: 1x3 shape

    val matrixShape_B = Shape(1, 3)
    val field_B = RefMatrixField.random(FieldRows, FieldColumns, matrixShape_B)
    val transposedField_B = RefMatrixField(FieldRows, FieldColumns,
                                           (row, col) => field_B.read(row, col).transpose)
    // Test C: 2x1 shape
    val matrixShape_C = Shape(2, 1)
    val field_C = RefMatrixField.random(FieldRows, FieldColumns, matrixShape_C)
    val transposedField_C = RefMatrixField(FieldRows, FieldColumns,
                                           (row, col) => field_C.read(row, col).transpose)
    // Test D: 4x2 shape
    val matrixShape_D = Shape(4, 2)
    val field_D = RefMatrixField.random(FieldRows, FieldColumns, matrixShape_D)
    val transposedField_D = RefMatrixField(FieldRows, FieldColumns,
                                           (row, col) => field_D.read(row, col).transpose)

    // Test E: 9x13 shape
    val matrixShape_E = Shape(9, 13)
    val field_E = RefMatrixField.random(FieldRows, FieldColumns, matrixShape_E)
    val transposedField_E = RefMatrixField(FieldRows, FieldColumns,
                                           (row, col) => field_E.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f_A = TestMatrixField(field_A)
      val transposedF_A = transposeMatrices(f_A)

      val f_B = TestMatrixField(field_B)
      val transposedF_B = transposeMatrices(f_B)

      val f_C = TestMatrixField(field_C)
      val transposedF_C = transposeMatrices(f_C)

      val f_D = TestMatrixField(field_D)
      val transposedF_D = transposeMatrices(f_D)

      val f_E = TestMatrixField(field_E)
      val transposedF_E = transposeMatrices(f_E)

      probe(transposedF_A, transposedF_B, transposedF_C, transposedF_D, transposedF_E)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transposedF_A) == transposedField_A)
      require(readMatrix(transposedF_B) == transposedField_B)
      require(readMatrix(transposedF_C) == transposedField_C)
      require(readMatrix(transposedF_D) == transposedField_D)
      require(readMatrix(transposedF_E) == transposedField_E)
    }
  }


  test("matrix field / determinant") {
    val FieldRows = 7
    val FieldColumns = 8
    val refMatrixField = RefMatrixField.random(FieldRows, FieldColumns, Shape(2, 2))
    val refDeterminant = RefScalarField(FieldRows, FieldColumns,
      (row, col) => {
        val matrix: Matrix = refMatrixField.read(row, col)
        matrix(0, 0) * matrix(1, 1) - matrix(0, 1) * matrix(1, 0)
      }
    )
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val matrixField = TestMatrixField(refMatrixField)
      val determinant_ = determinant(matrixField)

      probe(determinant_)
    }

    import graph._
    withRelease {
      step
      require(readScalar(determinant_) ~== refDeterminant)
    }
  }
  /*
  test("matrix field / push") {

    // 1D MatrixField Stack, 0D MatrixField Slice, MatrixShape = 2x2
    def point1D(col: Int) = Matrix(Array(
      Array[Float](col, 2 * col + 1),
      Array[Float](3 * col, 4 * col - 3)
    ))
    val Columns_1D = 3
    val stack_1D = RefMatrixField(Columns_1D, point1D _)
    val stack0_1D = RefMatrixField(point1D(0))
    val stack1_1D = RefMatrixField(point1D(1))
    val stack2_1D = RefMatrixField(point1D(2))
    val slice_1D = RefMatrixField(point1D(5))

    // 2D MatrixField Stack, 1D MatrixField Slice, MatrixShape = 2x3
    def point2D(row: Int, col: Int) = Matrix(Array(
      Array[Float](row + col, row * col, row + 2),
      Array[Float](3 * col, 4 * col - row, -row - col)
    ))
    val Rows_2D = 4
    val Columns_2D = 5
    val stack_2D = RefMatrixField(Rows_2D, Columns_2D, point2D _)
    val stack0_2D = RefMatrixField(Columns_2D, point2D(0, _))
    val stack1_2D = RefMatrixField(Columns_2D, point2D(1, _))
    val stack2_2D = RefMatrixField(Columns_2D, point2D(2, _))
    val stack3_2D = RefMatrixField(Columns_2D, point2D(3, _))
    val slice_2D = RefMatrixField(Columns_2D, point2D(5, _))

    // 3D MatrixField Stack, 2D MatrixField Slice, MatrixShape = 3x2
    def point3D(depth: Int, row: Int, col: Int) = Matrix(Array(
      Array[Float](depth + row, row + col),
      Array[Float](row * col, col - depth),
      Array[Float](4 * depth - row, -row - col)
    ))
    val Depth_3D = 3
    val Rows_3D = 4
    val Columns_3D = 5
    val stack_3D = RefMatrixField(Depth_3D, Rows_3D, Columns_3D, point3D _)
    val stack0_3D = RefMatrixField(Rows_3D, Columns_3D, point3D(0, _, _))
    val stack1_3D = RefMatrixField(Rows_3D, Columns_3D, point3D(1, _, _))
    val stack2_3D = RefMatrixField(Rows_3D, Columns_3D, point3D(2, _, _))
    val slice_3D = RefMatrixField(Rows_3D, Columns_3D, point3D(5, _, _))

    val graph = new ComputeGraph(Optimize) with RefTestInterface2 {
      val a_1D = TestMatrixField(stack_1D)
      val b_1D = a_1D push TestMatrixField(slice_1D)

      val a_2D = TestMatrixField(stack_2D)
      val b_2D = a_2D push TestMatrixField(slice_2D)

      val a_3D = TestMatrixField(stack_3D)
      val b_3D = a_3D push TestMatrixField(slice_3D)
    }

    import graph._
    withRelease {
      step
      // check 1D stack
      require(readMatrix(a_1D).slice(0) == stack0_1D)
      require(readMatrix(a_1D).slice(1) == stack1_1D)
      require(readMatrix(a_1D).slice(2) == stack2_1D)

      require(readMatrix(b_1D).slice(0) == slice_1D)
      require(readMatrix(b_1D).slice(1) == stack0_1D)
      require(readMatrix(b_1D).slice(2) == stack1_1D)

      // check 2D stack
      require(readMatrix(a_2D).slice(0) == stack0_2D)
      require(readMatrix(a_2D).slice(1) == stack1_2D)
      require(readMatrix(a_2D).slice(2) == stack2_2D)
      require(readMatrix(a_2D).slice(3) == stack3_2D)

      require(readMatrix(b_2D).slice(0) == slice_2D)
      require(readMatrix(b_2D).slice(1) == stack0_2D)
      require(readMatrix(b_2D).slice(2) == stack1_2D)
      require(readMatrix(b_2D).slice(3) == stack2_2D)

      // check 3D stack
      require(readMatrix(a_3D).slice(0) == stack0_3D)
      require(readMatrix(a_3D).slice(1) == stack1_3D)
      require(readMatrix(a_3D).slice(2) == stack2_3D)

      require(readMatrix(b_3D).slice(0) == slice_3D)
      require(readMatrix(b_3D).slice(1) == stack0_3D)
      require(readMatrix(b_3D).slice(2) == stack1_3D)
    }
  }
  */

  test("matrix field / vector transform") {
    // Test 1: MatrixField and VectorField have the same 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrixShape = Shape(9, 13)
    val vectorShape = Shape(13)
    val matrixField = RefMatrixField.random(FieldRows, FieldColumns, matrixShape)
    val vectorField = RefVectorField.random(FieldRows, FieldColumns, vectorShape)

    val expected = RefVectorField(FieldRows, FieldColumns,
                                  (row, col) => matrixField.read(row, col) * vectorField.read(row, col))

    // Test 2: MatrixField is 0D, VectorField is 2D

    val FieldRows2 = 3
    val FieldColumns2 = 4
    val matrixShape2 = Shape(11, 6)
    val vectorShape2 = Shape(6)
    val matrixField2 = RefMatrixField.random(matrixShape2)
    val vectorField2 = RefVectorField.random(FieldRows2, FieldColumns2, vectorShape2)
    val expected2 = RefVectorField(FieldRows2, FieldColumns2,
                                   (row, col) => matrixField2.read() * vectorField2.read(row, col))

    // Test 3: MatrixField is 2D, VectorField is 0D

    val FieldRows3 = 9
    val FieldColumns3 = 2
    val matrixShape3 = Shape(7, 4)
    val vectorShape3 = Shape(4)
    val matrixField3 = RefMatrixField.random(FieldRows3, FieldColumns3, matrixShape3)
    val vectorField3 = RefVectorField.random(vectorShape3)
    val expected3 = RefVectorField(FieldRows3, FieldColumns3,
                                   (row, col) => matrixField3.read(row, col) * vectorField3.read())

    // Test 4: Big field, since MatrixVectorTransformHyperKernel
    // generates different BigTensorAddressing-mode code for this case

    val FieldRows4 = 100
    val FieldColumns4 = 100
    val matrixShape4 = Shape(3, 4)
    val vectorShape4 = Shape(4)
    val matrixField4 = RefMatrixField.random(FieldRows4, FieldColumns4, matrixShape4)
    val vectorField4 = RefVectorField.random(FieldRows4, FieldColumns4, vectorShape4)

    require(FieldRows4 * FieldColumns4 >=
            MatrixVectorTransformHyperKernel.ThreadsToFullyUtilizeGPU)

    val expected4 = RefVectorField(FieldRows4, FieldColumns4,
      (row, col) => matrixField4.read(row, col) * vectorField4.read(row, col))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m = TestMatrixField(matrixField)
      val v = TestVectorField(vectorField)
      val transformed = transform(m, v)

      val m2 = TestMatrixField(matrixField2)
      val v2 = TestVectorField(vectorField2)
      val transformed2 = transform(m2, v2)

      val m3 = TestMatrixField(matrixField3)
      val v3 = TestVectorField(vectorField3)
      val transformed3 = transform(m3, v3)

      val m4 = TestMatrixField(matrixField4)
      val v4 = TestVectorField(vectorField4)
      val transformed4 = transform(m4, v4)

      probe(transformed, transformed2, transformed3, transformed4)
    }

    import graph._
    withRelease {
      step
      require(readVector(transformed) ~== expected)
      require(readVector(transformed2) ~== expected2)
      require(readVector(transformed3) ~== expected3)
      require(readVector(transformed4) ~== expected4)
    }

    def sameSizeField(fieldRows: Int, fieldColumns: Int, matrixShape: Shape, vectorShape: Shape) {

      val matrixField = RefMatrixField.random(fieldRows, fieldColumns, matrixShape)
      val vectorField = RefVectorField.random(fieldRows, fieldColumns, vectorShape)

      val expected = RefVectorField(fieldRows, fieldColumns,
        (row, col) => matrixField.read(row, col) * vectorField.read(row, col))

      val cg = new ComputeGraph(Optimize) with RefTestInterface {
        val m = TestMatrixField(matrixField)
        val v = TestVectorField(vectorField)
        val transformed = transform(m, v)
        probe(transformed)
      }
      cg.withRelease {
        cg.step
        require(cg.readVector(cg.transformed) ~== expected)
      }
    }
    // Exercise different values of resultVectorPoints % 8 ( 8 being the OutputsPerThread setting)
    sameSizeField(100, 100, Shape(3,4), Shape(4))
    sameSizeField(101, 101, Shape(5,8), Shape(8))
    sameSizeField(111, 111, Shape(7,9), Shape(9))
    sameSizeField(111, 111, Shape(6,15), Shape(15))
    sameSizeField(50, 50, Shape(6,1), Shape(1))
    sameSizeField(111, 111, Shape(6,1), Shape(1))

  }

  test("matrix field / matrix transform") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(9, 13)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
                                  (row, col) => matrix1Field.read(row, col) * matrix2Field.read(row, col))

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(16, 10)
    val matrix2ShapeB = Shape(10, 2)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
                                   (row, col) => matrix1FieldB.read() * matrix2FieldB.read(row, col))

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(9, 13)
    val matrix2ShapeC = Shape(13, 33)
        val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
                                   (row, col) => matrix1FieldC.read(row, col) * matrix2FieldC.read())

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transformed = transform(m1, m2)

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transformedB = transform(m1B, m2B)

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transformedC = transform(m1C, m2C)

      probe(transformed, transformedB, transformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transformed) ~== expected)
      require(readMatrix(transformedB) ~== expectedB)
      require(readMatrix(transformedC) ~== expectedC)
    }
  }

  test("matrix field / matrix transform with output transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(9, 13)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col) * matrix2Field.read(row, col))

    val expectedTransposed = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => expected.read(row, col).transpose)

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(16, 10)
    val matrix2ShapeB = Shape(10, 2)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read() * matrix2FieldB.read(row, col))

    val expectedBTransposed = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => expectedB.read(row, col).transpose)

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(9, 13)
    val matrix2ShapeC = Shape(13, 33)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col) * matrix2FieldC.read())

    val expectedCTransposed = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => expectedC.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transposedTransformed = transposeMatrices(transform(m1, m2))

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transposedTransformedB = transposeMatrices(transform(m1B, m2B))

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transposedTransformedC = transposeMatrices(transform(m1C, m2C))

      probe(transposedTransformed, transposedTransformedB, transposedTransformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transposedTransformed) ~== expectedTransposed)
      require(readMatrix(transposedTransformedB) ~== expectedBTransposed)
      require(readMatrix(transposedTransformedC) ~== expectedCTransposed)
    }
  }

  test("matrix field / matrix transform with in1 transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(13, 9)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col).transpose * matrix2Field.read(row, col))

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(10, 16)
    val matrix2ShapeB = Shape(10, 2)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read().transpose * matrix2FieldB.read(row, col))

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(13, 9)
    val matrix2ShapeC = Shape(13, 33)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col).transpose * matrix2FieldC.read())

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transformed = transform(transposeMatrices(m1), m2)

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transformedB = transform(transposeMatrices(m1B), m2B)

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transformedC = transform(transposeMatrices(m1C), m2C)

      probe(transformed, transformedB, transformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transformed) ~== expected)
      require(readMatrix(transformedB) ~== expectedB)
      require(readMatrix(transformedC) ~== expectedC)
    }
  }

  test("matrix field / matrix transform with double in1 transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(9, 13)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col) * matrix2Field.read(row, col))

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(16, 10)
    val matrix2ShapeB = Shape(10, 2)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read() * matrix2FieldB.read(row, col))

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(9, 13)
    val matrix2ShapeC = Shape(13, 33)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col) * matrix2FieldC.read())

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transformed = transform(transposeMatrices(transposeMatrices(m1)), m2)

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transformedB = transform(transposeMatrices(transposeMatrices(m1B)), m2B)

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transformedC = transform(transposeMatrices(transposeMatrices(m1C)), m2C)

      probe(transformed, transformedB, transformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transformed) ~== expected)
      require(readMatrix(transformedB) ~== expectedB)
      require(readMatrix(transformedC) ~== expectedC)
    }
  }

  test("matrix field / matrix transform with in1 transpose driving multiple matrix multiplies") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(13, 9)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col).transpose * matrix2Field.read(row, col))

    // Test B: first MatrixField is 2D, second is 2D, reuse of Test A's first transposed input

    val matrix2ShapeB = Shape(13, 7)
    val matrix2FieldB = RefMatrixField.random(FieldRows, FieldColumns, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col).transpose * matrix2FieldB.read(row, col))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m1Transposed = transposeMatrices(m1)
      val m2 = TestMatrixField(matrix2Field)
      val transformed = transform(m1Transposed, m2)

      val m2B = TestMatrixField(matrix2FieldB)
      val transformedB = transform(m1Transposed, m2B)

      probe(transformed, transformedB)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transformed) ~== expected)
      require(readMatrix(transformedB) ~== expectedB)
    }
  }

  test("matrix field / matrix transform with in2 transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(9, 13)
    val matrix2Shape = Shape(7, 13)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col) * matrix2Field.read(row, col).transpose)

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(16, 10)
    val matrix2ShapeB = Shape(2, 10)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read() * matrix2FieldB.read(row, col).transpose)

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(9, 13)
    val matrix2ShapeC = Shape(33, 13)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col) * matrix2FieldC.read().transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transformed = transform(m1, transposeMatrices(m2))

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transformedB = transform(m1B, transposeMatrices(m2B))

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transformedC = transform(m1C, transposeMatrices(m2C))

      probe(transformed, transformedB, transformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transformed) ~== expected)
      require(readMatrix(transformedB) ~== expectedB)
      require(readMatrix(transformedC) ~== expectedC)
    }
  }

  test("matrix field / matrix transform with in1 and output transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(13, 9)
    val matrix2Shape = Shape(13, 7)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col).transpose * matrix2Field.read(row, col))

    val expectedTransposed = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => expected.read(row, col).transpose)

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(10, 16)
    val matrix2ShapeB = Shape(10, 2)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read().transpose * matrix2FieldB.read(row, col))

    val expectedBTransposed = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => expectedB.read(row, col).transpose)

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(13, 9)
    val matrix2ShapeC = Shape(13, 33)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col).transpose * matrix2FieldC.read())

    val expectedCTransposed = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => expectedC.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transposedTransformed = transposeMatrices(transform(transposeMatrices(m1), m2))

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transposedTransformedB = transposeMatrices(transform(transposeMatrices(m1B), m2B))

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transposedTransformedC = transposeMatrices(transform(transposeMatrices(m1C), m2C))

      probe(transposedTransformed, transposedTransformedB, transposedTransformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transposedTransformed) ~== expectedTransposed)
      require(readMatrix(transposedTransformedB) ~== expectedBTransposed)
      require(readMatrix(transposedTransformedC) ~== expectedCTransposed)
    }
  }

  test("matrix field / matrix transform with in2 and output transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(9, 13)
    val matrix2Shape = Shape(7, 13)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col) * matrix2Field.read(row, col).transpose)

    val expectedTransposed = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => expected.read(row, col).transpose)

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(16, 10)
    val matrix2ShapeB = Shape(2, 10)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read() * matrix2FieldB.read(row, col).transpose)

    val expectedBTransposed = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => expectedB.read(row, col).transpose)

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(9, 13)
    val matrix2ShapeC = Shape(33, 13)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col) * matrix2FieldC.read().transpose)

    val expectedCTransposed = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => expectedC.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transposedTransformed = transposeMatrices(transform(m1, transposeMatrices(m2)))

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transposedTransformedB = transposeMatrices(transform(m1B, transposeMatrices(m2B)))

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transposedTransformedC = transposeMatrices(transform(m1C, transposeMatrices(m2C)))

      probe(transposedTransformed, transposedTransformedB, transposedTransformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transposedTransformed) ~== expectedTransposed)
      require(readMatrix(transposedTransformedB) ~== expectedBTransposed)
      require(readMatrix(transposedTransformedC) ~== expectedCTransposed)
    }
  }

  test("matrix field / matrix transform with in1, in2 and output transpose") {
    // Test A: Both 1st and 2nd MatrixFields have a 2D FieldShape

    val FieldRows = 5
    val FieldColumns = 7
    val matrix1Shape = Shape(13, 9)
    val matrix2Shape = Shape(7, 13)
    val matrix1Field = RefMatrixField.random(FieldRows, FieldColumns, matrix1Shape)
    val matrix2Field = RefMatrixField.random(FieldRows, FieldColumns, matrix2Shape)

    val expected = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => matrix1Field.read(row, col).transpose * matrix2Field.read(row, col).transpose)

    val expectedTransposed = RefMatrixField(FieldRows, FieldColumns,
      (row, col) => expected.read(row, col).transpose)

    // Test B: first MatrixField is 0D, second is 2D

    val FieldRowsB = 1
    val FieldColumnsB = 6
    val matrix1ShapeB = Shape(10, 16)
    val matrix2ShapeB = Shape(2, 10)
    val matrix1FieldB = RefMatrixField.random(matrix1ShapeB)
    val matrix2FieldB = RefMatrixField.random(FieldRowsB, FieldColumnsB, matrix2ShapeB)
    val expectedB = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => matrix1FieldB.read().transpose * matrix2FieldB.read(row, col).transpose)

    val expectedBTransposed = RefMatrixField(FieldRowsB, FieldColumnsB,
      (row, col) => expectedB.read(row, col).transpose)

    // Test C: first MatrixField is 2D, second is 0D

    val FieldRowsC = 32
    val FieldColumnsC = 8
    val matrix1ShapeC = Shape(13, 9)
    val matrix2ShapeC = Shape(33, 13)
    val matrix1FieldC = RefMatrixField.random(FieldRowsC, FieldColumnsC, matrix1ShapeC)
    val matrix2FieldC = RefMatrixField.random(matrix2ShapeC)
    val expectedC = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => matrix1FieldC.read(row, col).transpose * matrix2FieldC.read().transpose)

    val expectedCTransposed = RefMatrixField(FieldRowsC, FieldColumnsC,
      (row, col) => expectedC.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m1 = TestMatrixField(matrix1Field)
      val m2 = TestMatrixField(matrix2Field)
      val transposedTransformed = transposeMatrices(transform(transposeMatrices(m1), transposeMatrices(m2)))

      val m1B = TestMatrixField(matrix1FieldB)
      val m2B = TestMatrixField(matrix2FieldB)
      val transposedTransformedB = transposeMatrices(transform(transposeMatrices(m1B), transposeMatrices(m2B)))

      val m1C = TestMatrixField(matrix1FieldC)
      val m2C = TestMatrixField(matrix2FieldC)
      val transposedTransformedC = transposeMatrices(transform(transposeMatrices(m1C), transposeMatrices(m2C)))

      probe(transposedTransformed, transposedTransformedB, transposedTransformedC)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(transposedTransformed) ~== expectedTransposed)
      require(readMatrix(transposedTransformedB) ~== expectedBTransposed)
      require(readMatrix(transposedTransformedC) ~== expectedCTransposed)
    }
  }

  test("0D matrix field / 0D matrix field transform") {

    def runTest(matrix1Rows: Int, matrix1Columns: Int, matrix2Columns: Int, simpleData: Boolean): Unit = {

      def initData1(r: Int, c: Int) = if (simpleData) 1f else (r + 2 * c).toFloat
      def initData2(r: Int, c: Int) = if (simpleData) 1f else (r - c).toFloat

      val matrix2Rows = matrix1Columns
      val matrix1Field = RefMatrixField(Matrix(matrix1Rows, matrix1Columns, (r: Int, c: Int) => initData1(r,c)))
      val matrix2Field = RefMatrixField(Matrix(matrix2Rows, matrix2Columns, (r: Int, c: Int) => initData2(r,c)))

      val expected = RefMatrixField(matrix1Field.read() * matrix2Field.read())

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val m1 = TestMatrixField(matrix1Field)
        val m2 = TestMatrixField(matrix2Field)
        val transformed = transform(m1, m2)

        probe(transformed)
      }
      import graph._
      withRelease {
        step
//        readMatrix(transformed).read().print
        require(readMatrix(transformed) ~== expected)
      }
    }

    runTest(32, 48, 64, simpleData = true)
    runTest(37, 53, 111, simpleData = false)
    runTest(8, 16, 16, simpleData = false)
    runTest(4, 32, 32, simpleData = false)
    runTest(2, 48, 31, simpleData = false)
    runTest(1, 17, 512, simpleData = false)
    runTest(16, 16, 8, simpleData = false)
    runTest(512, 64, 4, simpleData = false)
    runTest(32, 16, 2, simpleData = false)
    runTest(1024, 16, 1, simpleData = false)

  }

  test("0D matrix field / 0D matrix field transform, explicit miniTile variations") {

    def runTest(matrix1Rows: Int, matrix1Columns: Int, matrix2Columns: Int, simpleData: Boolean,
                miniTileRows: Int, miniTileColumns: Int): Unit = {

      def initData1(r: Int, c: Int) = if (simpleData) 1f else (r + 2 * c).toFloat
      def initData2(r: Int, c: Int) = if (simpleData) 1f else (2*r + c).toFloat

      val matrix2Rows = matrix1Columns
      val matrix1Field = RefMatrixField(Matrix(matrix1Rows, matrix1Columns, (r: Int, c: Int) => initData1(r,c)))
      val matrix2Field = RefMatrixField(Matrix(matrix2Rows, matrix2Columns, (r: Int, c: Int) => initData2(r,c)))

      val expected = RefMatrixField(matrix1Field.read() * matrix2Field.read())

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val m1 = TestMatrixField(matrix1Field)
        val m2 = TestMatrixField(matrix2Field)
        val transformed = transformTiled(m1, m2, miniTileRows, miniTileColumns)

        probe(transformed)
      }
      import graph._
      withRelease {
        step
//        readMatrix(transformed).read().print
        require(readMatrix(transformed) ~== expected)
      }
    }

    runTest(398, 580, 299, simpleData = false, 1, 1)
    runTest(999, 305, 765, simpleData = false, 2, 2)
    runTest(1035, 501, 888, simpleData = false, 2, 4)
    runTest(1000, 500, 800, simpleData = false, 4, 2)
    runTest(444, 555, 669, simpleData = false, 4, 4)

  }

  test("0D matrix field / 0D matrix field transform with transposed 1st input") {

    def runTest(matrix1Rows: Int, matrix1Columns: Int, matrix2Columns: Int, simpleData: Boolean): Unit = {

      def initData1(r: Int, c: Int) = if (simpleData) 1f else (r + 2 * c).toFloat
      def initData2(r: Int, c: Int) = if (simpleData) 1f else (r - c).toFloat

      val matrix2Rows = matrix1Rows
      val matrix1Field = RefMatrixField(Matrix(matrix1Rows, matrix1Columns, (r: Int, c: Int) => initData1(r,c)))
      val matrix2Field = RefMatrixField(Matrix(matrix2Rows, matrix2Columns, (r: Int, c: Int) => initData2(r,c)))

      val expected = RefMatrixField(matrix1Field.read().transpose * matrix2Field.read())

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val m1 = TestMatrixField(matrix1Field)
        val m2 = TestMatrixField(matrix2Field)
        val transformed = transform(transposeMatrices(m1), m2)

        probe(transformed)
      }
      import graph._
      withRelease {
        step
//        readMatrix(transformed).read().print
        require(readMatrix(transformed) ~== expected)
      }
    }

    runTest(32, 48, 64, simpleData = true)
    runTest(17, 16, 16, simpleData = false)
    runTest(37, 53, 111, simpleData = false)
    runTest(8, 16, 16, simpleData = false)
    runTest(4, 32, 32, simpleData = false)
    runTest(2, 48, 31, simpleData = false)
    runTest(1, 17, 512, simpleData = false)
    runTest(16, 16, 8, simpleData = false)
    runTest(512, 64, 4, simpleData = false)
    runTest(32, 16, 2, simpleData = false)
    runTest(1024, 16, 1, simpleData = false)

  }

  test("0D matrix field / 0D matrix field transform with transposed 2nd input") {

    def runTest(matrix1Rows: Int, matrix1Columns: Int, matrix2Rows: Int, simpleData: Boolean): Unit = {

      def initData1(r: Int, c: Int) = if (simpleData) 1f else (r + 2 * c).toFloat
      def initData2(r: Int, c: Int) = if (simpleData) 1f else (r - c).toFloat

      val matrix2Columns = matrix1Columns
      val matrix1Field = RefMatrixField(Matrix(matrix1Rows, matrix1Columns, (r: Int, c: Int) => initData1(r,c)))
      val matrix2Field = RefMatrixField(Matrix(matrix2Rows, matrix2Columns, (r: Int, c: Int) => initData2(r,c)))

      val expected = RefMatrixField(matrix1Field.read() * matrix2Field.read().transpose)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val m1 = TestMatrixField(matrix1Field)
        val m2 = TestMatrixField(matrix2Field)
        val transformed = transform(m1, transposeMatrices(m2))

        probe(transformed)
      }
      import graph._
      withRelease {
        step
//        readMatrix(transformed).read().print
        require(readMatrix(transformed) ~== expected)
      }
    }

    runTest(32, 48, 64, simpleData = true)
    runTest(17, 16, 16, simpleData = false)
    runTest(37, 53, 111, simpleData = false)
    runTest(8, 16, 16, simpleData = false)
    runTest(4, 32, 32, simpleData = false)
    runTest(2, 48, 31, simpleData = false)
    runTest(1, 17, 512, simpleData = false)
    runTest(16, 16, 8, simpleData = false)
    runTest(512, 64, 4, simpleData = false)
    runTest(32, 16, 2, simpleData = false)
    runTest(1024, 16, 1, simpleData = false)

  }

  test("0D matrix field / 0D matrix field transform with transposed output") {

    def runTest(matrix1Rows: Int, matrix1Columns: Int, matrix2Columns: Int, simpleData: Boolean): Unit = {

      def initData1(r: Int, c: Int) = if (simpleData) 1f else (r + 2 * c).toFloat
      def initData2(r: Int, c: Int) = if (simpleData) 1f else (r - c).toFloat

      val matrix2Rows = matrix1Columns
      val matrix1Field = RefMatrixField(Matrix(matrix1Rows, matrix1Columns, (r: Int, c: Int) => initData1(r,c)))
      val matrix2Field = RefMatrixField(Matrix(matrix2Rows, matrix2Columns, (r: Int, c: Int) => initData2(r,c)))

      val expected = RefMatrixField((matrix1Field.read() * matrix2Field.read()).transpose)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val m1 = TestMatrixField(matrix1Field)
        val m2 = TestMatrixField(matrix2Field)
        val transformed = transposeMatrices(transform(m1, m2))

        probe(transformed)
      }
      import graph._
      withRelease {
        step
        //        readMatrix(transformed).read().print
        require(readMatrix(transformed) ~== expected)
      }
    }

    runTest(32, 48, 64, simpleData = true)
    runTest(37, 53, 111, simpleData = false)
    runTest(8, 16, 16, simpleData = false)
    runTest(4, 32, 32, simpleData = false)
    runTest(2, 48, 31, simpleData = false)
    runTest(1, 17, 512, simpleData = false)
    runTest(16, 16, 8, simpleData = false)
    runTest(512, 64, 4, simpleData = false)
    runTest(32, 16, 2, simpleData = false)
    runTest(1024, 16, 1, simpleData = false)

  }
  /** Test the (Int) operator. */
  test("matrix field / tensor slice") {
    val Rows = 10
    val Columns = 12

    // Test A: Input is 2x3 BigTensorField, Output is length 3 SmallTensorField

    val image_A = RefMatrixField(Rows, Columns,
                                 (row, col) => Matrix(
                                   Array[Float](row, col, row + col),
                                   Array[Float](col, -row, row - col)
                                 ))

    val expect0_A = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(row, col, row + col))
    val expect1_A = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(col, -row, row - col))

    // Test B: Input is 2x5 BigTensorField, Output is length 5 BigTensorField

    val image_B = RefMatrixField(Rows, Columns,
                                 (row, col) => Matrix(
                                   Array[Float](row, col, row + col, row - col, row * col),
                                   Array[Float](col, -row, row - col, -col, -row - col)
                                 ))

    val expect0_B = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(row, col, row + col, row - col, row * col))
    val expect1_B = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(col, -row, row - col, -col, -row - col))


    // Test C: Input is 2x2 SmallTensorField, Output is length 2 SmallTensorField

    val image_C = RefMatrixField(Rows, Columns,
                                 (row, col) => Matrix(
                                   Array[Float](row, col),
                                   Array[Float](-col, -row)
                                 ))

    val expect0_C = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(row, col))
    val expect1_C = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(-col, -row))

    // Test D: Input is 1x1 SmallTensorField, Output is length 1 SmallTensorField

    val image_D = RefMatrixField(Rows, Columns,
                                 (row, col) => Matrix(
                                   Array[Float](100f * row + col)
                                 ))

    val expect0_D = RefVectorField(Rows, Columns,
                                   (row, col) => new Vector(100f * row + col))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field_A = TestMatrixField(image_A)
      val slice0_A = matrixRow(field_A, 0)
      val slice1_A = matrixRow(field_A, 1)

      val field_B = TestMatrixField(image_B)
      val slice0_B = matrixRow(field_B, 0)
      val slice1_B = matrixRow(field_B, 1)

      val field_C = TestMatrixField(image_C)
      val slice0_C = matrixRow(field_C, 0)
      val slice1_C = matrixRow(field_C, 1)

      val field_D = TestMatrixField(image_D)
      val slice0_D = matrixRow(field_D, 0)

      probe(slice0_A, slice1_A, slice0_B, slice1_B, slice0_C, slice1_C, slice0_D)
    }

    import graph._
    withRelease {
      step

      require(readVector(slice0_A) == expect0_A)
      require(readVector(slice1_A) == expect1_A)

      require(readVector(slice0_B) == expect0_B)
      require(readVector(slice1_B) == expect1_B)

      require(readVector(slice0_C) == expect0_C)
      require(readVector(slice1_C) == expect1_C)

      require(readVector(slice0_D) == expect0_D)
    }
  }

  /** Test the tensor reduction operators. */
  test("matrix field / tensor reduce") {
    val Rows = 3
    val Columns = 3
    val MatrixRows = 5
    val MatrixColumns = 7

    def expectedReducedSum(field: RefMatrixField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toVector.toArray.reduceLeft(_ + _))
    def expectedReducedMin(field: RefMatrixField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toVector.toArray.reduceLeft(_ min _))
    def expectedReducedMax(field: RefMatrixField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toVector.toArray.reduceLeft(_ max _))

    // Test matrices of shape 1x1, 2x2, 3x3, 4x4, 5x5

    val field = Array.tabulate(5) { i =>
      RefMatrixField(Rows, Columns,
                     (row, col) => new Matrix(i+1,i+1).randomize)
                                 }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m = Array.tabulate(5) { i => TestMatrixField(field(i)) }
      val mSum = Array.tabulate(5) { i => reduceSum(m(i)) }
      val mMax = Array.tabulate(5) { i => reduceMax(m(i)) }
      val mMin = Array.tabulate(5) { i => reduceMin(m(i)) }

      // Concatenate all the arrays and then probe all the elements
      probe((mSum ++ mMax ++ mMin): _*)
    }

    import graph._
    withRelease {
      step
      for (i <- 0 until 5) {
        // GPU sum done as binary tree, not reduceLeft, so answer is approx.
        require(readScalar(mSum(i)) ~== expectedReducedSum(field(i)))
        require(readScalar(mMax(i)) == expectedReducedMax(field(i)))
        require(readScalar(mMin(i)) == expectedReducedMin(field(i)))
      }
    }
  }

  /** Test the fieldReduceSum operator. */
  test("matrix field / field reduce sum") {
    val Rows = 3
    val Columns = 3
    val MatrixRows = 5
    val MatrixColumns = 7
    val field = RefMatrixField(Rows, Columns,
                               (row, col) => new Matrix(MatrixRows, MatrixColumns).randomize)
    val sum = new Matrix(MatrixRows, MatrixColumns)
    for (r <- 0 until Rows; c <- 0 until Columns)
      sum += field.read(r, c)
    val expectedReducedSum = RefMatrixField(sum)


    // Try another case: 0D field

    val sum0 = new Matrix(MatrixRows, MatrixColumns).randomize
    val field0 = RefMatrixField(sum0)
    val expectedReducedSum0 = RefMatrixField(sum0)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m = TestMatrixField(field)
      val mSum = fieldReduceSum(m)

      val m0 = TestMatrixField(field0)
      val mSum0 = fieldReduceSum(m0)

      probe(mSum, mSum0)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(mSum) ~== expectedReducedSum)
      require(readMatrix(mSum0) ~== expectedReducedSum0)
    }
  }

  /** Test the fieldReduceMax operator. */
  test("matrix field / field reduce max") {
    val Rows = 3
    val Columns = 3
    val MatrixRows = 5
    val MatrixColumns = 7
    val field = RefMatrixField(Rows, Columns,
                               (row, col) => new Matrix(MatrixRows, MatrixColumns).randomize)

    // Create a matrix that is the component-wise maximum of two matrices
    def matrixMax(a: Matrix, b: Matrix) = Matrix(a.rows, a.columns, (r,c) => math.max(a(r,c),b(r,c)))

    // Create a RefMatrixField that is the fieldReduceMax of a RefMatrixField
    def refMatrixFieldMax(field: RefMatrixField) = RefMatrixField(field.reduce(matrixMax(_ , _)))

    val expectedReducedMax = refMatrixFieldMax(field)


    // Try another case: 0D field

    val max0 = new Matrix(MatrixRows, MatrixColumns).randomize
    val field0 = RefMatrixField(max0)
    val expectedReducedMax0 = refMatrixFieldMax(field0)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m = TestMatrixField(field)
      val mSum = fieldReduceMax(m)

      val m0 = TestMatrixField(field0)
      val mSum0 = fieldReduceMax(m0)

      probe(mSum, mSum0)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(mSum) ~== expectedReducedMax)
      require(readMatrix(mSum0) ~== expectedReducedMax0)
    }
  }

  /** Test the fieldReduceMin operator. */
  test("matrix field / field reduce min") {
    val Rows = 3
    val Columns = 3
    val MatrixRows = 5
    val MatrixColumns = 7
    val field = RefMatrixField(Rows, Columns,
                               (row, col) => new Matrix(MatrixRows, MatrixColumns).randomize)

    // Create a matrix that is the component-wise minimum of two matrices
    def matrixMin(a: Matrix, b: Matrix) = Matrix(a.rows, a.columns, (r,c) => math.min(a(r,c),b(r,c)))

    // Create a RefMatrixField that is the fieldReduceMin of a RefMatrixField
    def refMatrixFieldMin(field: RefMatrixField) = RefMatrixField(field.reduce(matrixMin(_ , _)))

    val expectedReducedMin = refMatrixFieldMin(field)


    // Try another case: 0D field

    val min0 = new Matrix(MatrixRows, MatrixColumns).randomize
    val field0 = RefMatrixField(min0)
    val expectedReducedMin0 = refMatrixFieldMin(field0)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val m = TestMatrixField(field)
      val mSum = fieldReduceMin(m)

      val m0 = TestMatrixField(field0)
      val mSum0 = fieldReduceMin(m0)

      probe(mSum, mSum0)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(mSum) ~== expectedReducedMin)
      require(readMatrix(mSum0) ~== expectedReducedMin0)
    }
  }

  /** Test that the '+' '-' and '*' operators are commutative */
  test("matrix field / commutative") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field = TestMatrixField(RefMatrixField.random(10, 10, Shape(5, 5)))
      val plus = field + 1
      val plusReverse = 1f + field
      val minus = 1f - field
      val minusReverse = -field + 1
      val times = 2f * field
      val timesReverse = field * 2

      probe(plus, plusReverse, minus, minusReverse, times, timesReverse)
    }

    import graph._
    withRelease {
      step

      require(readMatrix(plus) == readMatrix(plusReverse))
      require(readMatrix(minus) == readMatrix(minusReverse))
      require(readMatrix(times) == readMatrix(timesReverse))
    }
  }

  test("matrix field / invert") {
    val Rows = 100
    val Columns = 100
    val identity = new Matrix(5, 5) {
      for (row <- 0 until rows)
        this(row, row) = 1f
    }
    // Making this "deterministically" random prevents this test
    // from possibly crashing the cogx regression.
    val rand = new Random(1835387618253L)
    val field = RefMatrixField(Rows, Columns, (_, _) => {
      new Matrix(5, 5) {
        for (r <- 0 until rows; c <- 0 until columns)
          this(r, c) = rand.nextFloat
      }
    })

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestMatrixField(field)
      val inverted = invertMatrices(input)

      probe(inverted)
    }

    import graph._
    withRelease {
      step

      val output: RefMatrixField = readMatrix(inverted)
      for (row <- 0 until Rows; col <- 0 until Columns) {
        val inMatrix = field.read(row, col)
        val outMatrix = output.read(row, col)
        val product = inMatrix * outMatrix
        if (!approxEqual(product, identity)) {
          println("row = " + row + ", column = " + col)
          println("in: ")
          inMatrix.print
          println("out: ")
          outMatrix.print
          println("in * out: ")
          product.print
          throw new RuntimeException("Matrix inverse known to be numerically unstable- fails here to produce identity matrix.")
        }
      }
    }

    // Matrix inversion is not numerically stable, so we must be very relaxed
    // about accepting the results.
    def approxEqual(a: Matrix, b: Matrix): Boolean = {
      if (a.rows != b.rows || a.columns != b.columns)
        false
      else {
        for (row <- 0 until a.rows; col <- 0 until a.columns)
          if (!approxFloatEqual(a(row, col), b(row, col)))
            return false
        true
      }
    }

    /** Return true if "this.value" and "y" are approximately equal. */
    def approxFloatEqual(x: Float, y: Float): Boolean = {
      // eps exponent was -15 .  Old GT200 architecture failed with -13 on log test.  -RJC
      val eps = math.pow(2.0, -6.0)
      if (x == 0 && y.abs < 10 * eps)
        true
      else if (y == 0 && x.abs < 10 * eps)
        true
      else
        (x - y).abs < 10 * eps * (x.abs max y.abs)
    }
  }

  /** Test 2D MatrixField convolve static 2D ScalarField */
  test("matrix field convolve scalar field, 2D") {
    // We assume that convolution on scalar fields works. Since convolution
    // is a linear operator, convolving a matrix field is equivalent to
    // convolving with a set of scalar fields, one scalar field for each
    // element of the matrix.
    val Rows = 4
    val Columns = 5
    val MatrixShape = Shape(2, 2)

    val image = RefMatrixField.random(Rows, Columns, MatrixShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel = ScalarField.random(3, 3)
      val matrixField = TestMatrixField(image)
      val scalarFields = Array[ScalarField](
        TestScalarField(image.sliceTensor(0, 0)),
        TestScalarField(image.sliceTensor(0, 1)),
        TestScalarField(image.sliceTensor(1, 0)),
        TestScalarField(image.sliceTensor(1, 1))
      )

      val filteredMatrixField = convolve(matrixField, kernel, BorderClamp)
      val filteredScalarFields = Array[ScalarField](
        convolve(scalarFields(0), kernel, BorderClamp),
        convolve(scalarFields(1), kernel, BorderClamp),
        convolve(scalarFields(2), kernel, BorderClamp),
        convolve(scalarFields(3), kernel, BorderClamp)
      )

      probe(filteredMatrixField)
      probe(filteredScalarFields: _*)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
              readScalar(filteredScalarFields(0)))
      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
              readScalar(filteredScalarFields(1)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
              readScalar(filteredScalarFields(2)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
              readScalar(filteredScalarFields(3)))
    }
  }

// This use of FFT-based convolution on MatrixFields is no longer supported.

//  /** Test 2D MatrixField convolve static 2D ScalarField using the FFT */
//  test("matrix field convolve scalar field, 2D, via FFT") {
//    val FilterSize = 15
//    val Rows = 4 * FilterSize
//    val Columns = 5 * FilterSize
//    val MatrixShape = Shape(2, 2)
//
//    val image = RefMatrixField.random(Rows, Columns, MatrixShape)
//
//    val graph = new ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface2 {
//      val kernel = ScalarField.random(FilterSize, FilterSize)
//      val matrixField = TestMatrixField(image)
//      val scalarFields = Array[ScalarField](
//        TestScalarField(image.sliceTensor(0, 0)),
//        TestScalarField(image.sliceTensor(0, 1)),
//        TestScalarField(image.sliceTensor(1, 0)),
//        TestScalarField(image.sliceTensor(1, 1))
//      )
//
//      val filteredMatrixField = matrixField convolve(kernel, BorderClamp)
//      val filteredScalarFields = Array[ScalarField](
//        scalarFields(0).convolve(kernel, BorderClamp),
//        scalarFields(1).convolve(kernel, BorderClamp),
//        scalarFields(2).convolve(kernel, BorderClamp),
//        scalarFields(3).convolve(kernel, BorderClamp)
//      )
//    }
//
//    import graph._
//    withRelease {
//      step
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
//              readScalar(filteredScalarFields(0)))
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
//              readScalar(filteredScalarFields(1)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
//              readScalar(filteredScalarFields(2)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
//              readScalar(filteredScalarFields(3)))
//    }
//  }

  /** Test 2D MatrixField convolve static 2D ScalarField using the BorderZero
    * border policy
    */
  test("matrix field convolve scalarfield, 2D, BorderZero") {
    // We assume that convolution on scalar fields works. Since convolution
    // is a linear operator, convolving a matrix field is equivalent to
    // convolving with a set of scalar fields, one scalar field for each
    // element of the matrix.
    val Rows = 4
    val Columns = 5
    val MatrixShape = Shape(2, 2)

    val image = RefMatrixField.random(Rows, Columns, MatrixShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel = ScalarField.random(3, 3)
      val matrixField = TestMatrixField(image)
      val scalarFields = Array[ScalarField](
        TestScalarField(image.sliceTensor(0, 0)),
        TestScalarField(image.sliceTensor(0, 1)),
        TestScalarField(image.sliceTensor(1, 0)),
        TestScalarField(image.sliceTensor(1, 1))
      )

      val filteredMatrixField = convolve(matrixField, kernel, BorderZero)
      val filteredScalarFields = Array[ScalarField](
        convolve(scalarFields(0), kernel, BorderZero),
        convolve(scalarFields(1), kernel, BorderZero),
        convolve(scalarFields(2), kernel, BorderZero),
        convolve(scalarFields(3), kernel, BorderZero)
      )

      probe(filteredMatrixField)
      probe(filteredScalarFields: _*)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
              readScalar(filteredScalarFields(0)))
      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
              readScalar(filteredScalarFields(1)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
              readScalar(filteredScalarFields(2)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
              readScalar(filteredScalarFields(3)))
    }
  }

  /** Test 2D MatrixField convolve static 2D MatrixField */
  test("matrix field convolve matrix field, 2D") {
    // We assume that convolution on scalar fields works. Since convolution
    // is a linear operator, convolving a matrix field is equivalent to
    // convolving with a set of scalar fields, one scalar field for each
    // element of the matrix.
    val Rows = 4
    val Columns = 5
    val MatrixShape = Shape(2, 2)

    val image = RefMatrixField.random(Rows, Columns, MatrixShape)
    val kernel = RefMatrixField.random(3, 3, MatrixShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {

      val kernelField = TestMatrixField(kernel)
      val kernelFields = Array[ScalarField](
        TestScalarField(kernel.sliceTensor(0, 0)),
        TestScalarField(kernel.sliceTensor(0, 1)),
        TestScalarField(kernel.sliceTensor(1, 0)),
        TestScalarField(kernel.sliceTensor(1, 1))
      )
      val imageField = TestMatrixField(image)
      val imageFields = Array[ScalarField](
        TestScalarField(image.sliceTensor(0, 0)),
        TestScalarField(image.sliceTensor(0, 1)),
        TestScalarField(image.sliceTensor(1, 0)),
        TestScalarField(image.sliceTensor(1, 1))
      )

      val filteredMatrixField = convolve(imageField, kernelField, BorderClamp)
      val filteredScalarFields = Array[ScalarField](
        convolve(imageFields(0), kernelFields(0), BorderClamp),
        convolve(imageFields(1), kernelFields(1), BorderClamp),
        convolve(imageFields(2), kernelFields(2), BorderClamp),
        convolve(imageFields(3), kernelFields(3), BorderClamp)
      )

      probe(filteredMatrixField)
      probe(filteredScalarFields: _*)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
              readScalar(filteredScalarFields(0)))
      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
              readScalar(filteredScalarFields(1)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
              readScalar(filteredScalarFields(2)))
      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
              readScalar(filteredScalarFields(3)))
    }
  }

  // This use of FFT-based convolution on MatrixFields is no longer supported.

//  /** Test 2D MatrixField convolve static 2D MatrixField using the FFT */
//  test("matrix field convolve matrix field, 2D, via FFT") {
//    val FilterSize = 17
//    val Rows = 4 * FilterSize
//    val Columns = 5 * FilterSize
//    val MatrixShape = Shape(2, 2)
//
//    val image = RefMatrixField.random(Rows, Columns, MatrixShape)
//    val kernel = RefMatrixField.random(FilterSize, FilterSize, MatrixShape)
//
//    val graph = new ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface2 {
//
//      val kernelField = TestMatrixField(kernel)
//      val kernelFields = Array[ScalarField](
//        TestScalarField(kernel.sliceTensor(0, 0)),
//        TestScalarField(kernel.sliceTensor(0, 1)),
//        TestScalarField(kernel.sliceTensor(1, 0)),
//        TestScalarField(kernel.sliceTensor(1, 1))
//      )
//      val imageField = TestMatrixField(image)
//      val imageFields = Array[ScalarField](
//        TestScalarField(image.sliceTensor(0, 0)),
//        TestScalarField(image.sliceTensor(0, 1)),
//        TestScalarField(image.sliceTensor(1, 0)),
//        TestScalarField(image.sliceTensor(1, 1))
//      )
//
//      val filteredMatrixField = imageField convolve(kernelField, BorderClamp)
//      val filteredScalarFields = Array[ScalarField](
//        imageFields(0).convolve(kernelFields(0), BorderClamp),
//        imageFields(1).convolve(kernelFields(1), BorderClamp),
//        imageFields(2).convolve(kernelFields(2), BorderClamp),
//        imageFields(3).convolve(kernelFields(3), BorderClamp)
//      )
//    }
//
//    import graph._
//    withRelease {
//      step
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
//              readScalar(filteredScalarFields(0)))
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
//              readScalar(filteredScalarFields(1)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
//              readScalar(filteredScalarFields(2)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
//              readScalar(filteredScalarFields(3)))
//    }
//  }

//  /** Test 2D MatrixField convolve dynamic 2D MatrixField using the FFT */
//  test("matrix field convolve dynamic matrix field, 2D, via FFT") {
//    val FilterSize = 21
//    val Rows = 4 * FilterSize
//    val Columns = 5 * FilterSize
//    val MatrixShape = Shape(2, 2)
//
//    val image = RefMatrixField.random(Rows, Columns, MatrixShape)
//    val kernel = RefMatrixField.random(FilterSize, FilterSize, MatrixShape)
//
//    val stepChange = 0.5f
//
//    val graph = new ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface2 {
//
//      val kernelField = TestMatrixField(kernel)
//      kernelField <== kernelField * stepChange
//      val kernelFields = Array[ScalarField](
//        TestScalarField(kernel.sliceTensor(0, 0)),
//        TestScalarField(kernel.sliceTensor(0, 1)),
//        TestScalarField(kernel.sliceTensor(1, 0)),
//        TestScalarField(kernel.sliceTensor(1, 1))
//      )
//      val imageField = TestMatrixField(image)
//      val imageFields = Array[ScalarField](
//        TestScalarField(image.sliceTensor(0, 0)),
//        TestScalarField(image.sliceTensor(0, 1)),
//        TestScalarField(image.sliceTensor(1, 0)),
//        TestScalarField(image.sliceTensor(1, 1))
//      )
//
//      val filteredMatrixField = imageField convolve(kernelField, BorderClamp)
//      val filteredScalarFields = Array[ScalarField](
//        imageFields(0).convolve(kernelFields(0), BorderClamp),
//        imageFields(1).convolve(kernelFields(1), BorderClamp),
//        imageFields(2).convolve(kernelFields(2), BorderClamp),
//        imageFields(3).convolve(kernelFields(3), BorderClamp)
//      )
//    }
//
//    import graph._
//    withRelease {
//      reset
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
//              readScalar(filteredScalarFields(0)))
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
//              readScalar(filteredScalarFields(1)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
//              readScalar(filteredScalarFields(2)))
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
//              readScalar(filteredScalarFields(3)))
//      step
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 0) ~==
//              readScalar(filteredScalarFields(0)) * stepChange)
//      require(readMatrix(filteredMatrixField).sliceTensor(0, 1) ~==
//              readScalar(filteredScalarFields(1)) * stepChange)
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 0) ~==
//              readScalar(filteredScalarFields(2)) * stepChange)
//      require(readMatrix(filteredMatrixField).sliceTensor(1, 1) ~==
//              readScalar(filteredScalarFields(3)) * stepChange)
//
//    }
//  }

  test("matrix field / solve") {
    val matrix = Matrix(
      Array(2f, 3f),
      Array(2f, 1f)
    )
    val invMatrix = matrix.invert
    val vector = new Vector(1.2f, 3.4f)
    val Rows = 17
    val Columns = 39
    val matrixField = RefMatrixField(Rows, Columns, (_, _) => matrix)
    val vectorField = RefVectorField(Rows, Columns, (_, _) => vector)

    val xExpected = invMatrix * vector

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // Solve Ax = b for x
      val A = TestMatrixField(matrixField)
      val b = TestVectorField(vectorField)
      val x = solve(A, b)
      probe(x)
    }


    import graph._
    withRelease {
      step
      val xx = readVector(x)
      for (row <- 0 until Rows) {
        for (col <- 0 until Columns) {
          require(xx.read(row, col) ~== xExpected, "row: " + row + ", col: " + col)
        }
      }
    }
  }

  /** Test the warp operator on a 2D MatrixField */
  test("matrix field / warp2D") {
    // For this test, we assume that translate has been tested for
    // scalar fields and vector fields
    val Rows = 4
    val Columns = 5
    val MatrixShape = Shape(2, 3)

    val input = RefMatrixField.random(Rows, Columns, MatrixShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputField = TestMatrixField(input)

      // Translation by a 0D vector field
      val vectorField0D =
        TestVectorField(RefVectorField(new Vector(0.5f, -1.5f)))
      val translated0D = warp(inputField, vectorField0D)
      val expected0D = matrixField(warp(matrixRow(inputField, 0), vectorField0D),
                                   warp(matrixRow(inputField, 1), vectorField0D))

      // Translation by a 2D vector field
      val vectorField2D =
        TestVectorField(RefVectorField.random(Rows, Columns, Shape(2)))
      val translated2D = warp(inputField, vectorField2D)
      val expected2D = matrixField(warp(matrixRow(inputField, 0), vectorField2D),
                                   warp(matrixRow(inputField, 1), vectorField2D))

      // Test implicit trimming that occurs when guide field is smaller than input

      val TrimmedRows = Rows - 1
      val TrimmedColumns = Columns - 3

      val trimmedGuide = trim(vectorField2D, Shape(TrimmedRows, TrimmedColumns))
      val translatedTrimmed = warp(inputField, trimmedGuide)
      val expectedTrimmed = matrixField(warp(matrixRow(inputField, 0), trimmedGuide),
        warp(matrixRow(inputField, 1), trimmedGuide))
      probe(translated0D, expected0D, translated2D, expected2D, translatedTrimmed, expectedTrimmed)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(translated0D) ~== readMatrix(expected0D))
      require(readMatrix(translated2D) ~== readMatrix(expected2D))
      require(readMatrix(translatedTrimmed) ~== readMatrix(expectedTrimmed))
    }
  }

  /** Test the subfield operator on a 2D MatrixField */
  test("matrix field / subfield") {
    // For this test, we assume that subfield has been tested for
    // scalar fields.
    val Rows = 4
    val Columns = 5
    val MatrixShape = Shape(2, 2)
    val BigMatrixShape = Shape(3, 2)

    val OutputRows = 3
    val OutputCols = 3
    val OutputShape = Shape(OutputRows, OutputCols)

    val input = RefMatrixField.random(Rows, Columns, MatrixShape)

    val bigInput = RefMatrixField.random(Rows, Columns, BigMatrixShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputField = TestMatrixField(input)

      // Translation by a 0D vector field
      val vectorField0D =
        TestVectorField(RefVectorField(new Vector(0.5f, -1.5f)))
      val translated0D = subfield(inputField, vectorField0D, OutputShape)
      val expected0D = matrixField(subfield(matrixRow(inputField, 0), vectorField0D, OutputShape),
                                   subfield(matrixRow(inputField, 1), vectorField0D, OutputShape)
                                 )

      // Repeat with a vector length that cannot be handled in SmallTensorAddressing mode.


      val bigInputField = TestMatrixField(bigInput)
      val bigTranslated0D = subfield(bigInputField, vectorField0D, OutputShape)
      val bigExpected0D = matrixField(subfield(matrixRow(bigInputField, 0), vectorField0D, OutputShape),
                                      subfield(matrixRow(bigInputField, 1), vectorField0D, OutputShape),
                                      subfield(matrixRow(bigInputField, 2), vectorField0D, OutputShape)
                                    )

      probe(translated0D, expected0D, bigTranslated0D, bigExpected0D)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(translated0D) ~== readMatrix(expected0D))
      require(readMatrix(bigTranslated0D) ~== readMatrix(bigExpected0D))
    }
  }

  /** Test the upsample operator on a 2D MatrixField */
  test("matrix field / upsample") {
    val data00 = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val data01 = data00 * 2f
    val data10 = data00 * 3f
    val data11 = data00 * 4f
    val input = RefMatrixField(3, 3,
                               (r, c) => Matrix(
                                 Array(
                                   Array(data00(r, c), data01(r, c)),
                                   Array(data10(r, c), data11(r, c))
                                 )
                               )
                             )
    val expectedData00 = Matrix(
      Array(
        Array( 1f, 0f, 7f, 0f, 5f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array(-2f, 0f, 0f, 0f, 4f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array( 4f, 0f,-7f, 0f, 3f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f)
      )
    )
    val expectedData01 = expectedData00 * 2f
    val expectedData10 = expectedData00 * 3f
    val expectedData11 = expectedData00 * 4f
    val expected = RefMatrixField(6, 6,
                                  (r, c) => Matrix(
                                    Array(
                                      Array(expectedData00(r, c), expectedData01(r, c)),
                                      Array(expectedData10(r, c), expectedData11(r, c))
                                    )
                                  )
                                )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestMatrixField(input)
      val outField = upsample(inField)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(outField) == expected)
    }
  }

  /** Test the supersample operator on a 2D MatrixField */
  test("matrix field / supersample") {
    val data00 = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val data01 = data00 * 2f
    val data10 = data00 * 3f
    val data11 = data00 * 4f
    val input = RefMatrixField(3, 3,
                               (r, c) => Matrix(
                                 Array(
                                   Array(data00(r, c), data01(r, c)),
                                   Array(data10(r, c), data11(r, c))
                                 )
                               )
                             )
    val expectedData00 = Matrix(
      Array(
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f)
      )
    )

    val expectedData01 = expectedData00 * 2f
    val expectedData10 = expectedData00 * 3f
    val expectedData11 = expectedData00 * 4f
    val expected = RefMatrixField(6, 6,
                                  (r, c) => Matrix(
                                    Array(
                                      Array(expectedData00(r, c), expectedData01(r, c)),
                                      Array(expectedData10(r, c), expectedData11(r, c))
                                    )
                                  )
                                )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestMatrixField(input)
      val outField = supersample(inField)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(outField) == expected)
    }
  }

  /** Test the trim operator on a 2D MatrixField */
  test("matrix field / trim") {

    val input00 = Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    )
    val input01 = Matrix(
      Array(20f, 21f, 22f, 23f, 24f),
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(30f, 31f, 32f, 33f, 34f),
      Array(10f, 11f, 12f, 13f, 14f)
    )
    val input10 = Matrix(
      Array(21f, 22f, 23f, 24f, 25f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(32f, 32f, 33f, 36f, 39f),
      Array(12f, 18f, 13f, 12f, 17f)
    )
    val input11 = Matrix(
      Array(15f, 16f, 17f, 18f, 19f),
      Array(10f, 55f, 12f, 13f, 14f),
      Array(32f, 32f, 33f, 36f, 39f),
      Array(12f, 18f, 13f, 12f, 17f)
    )
    val input = Array2D.rectangularize[Matrix](Array(input00, input01, input10, input11))
    val expected00 = Matrix(
      Array( 0f,  1f,  2f,  3f),
      Array(10f, 11f, 12f, 13f)
    )
    val expected01 = Matrix(
      Array(20f, 21f, 22f, 23f),
      Array( 0f,  1f,  2f,  3f)
    )
    val expected10 = Matrix(
      Array(21f, 22f, 23f, 24f),
      Array(10f, 11f, 12f, 13f)
    )
    val expected11 = Matrix(
      Array(15f, 16f, 17f, 18f),
      Array(10f, 55f, 12f, 13f)
    )
    val expected = Array2D.rectangularize[Matrix](Array(expected00, expected01, expected10, expected11))
    val expectedRef = RefMatrixField(2, 4, (r, c) =>  Matrix(2, 2, (rr, cc) => expected(rr,cc)(r,c)))
    val outShape = Shape(2, 4)
    val matrixShape = Shape(2,2)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestMatrixField(RefMatrixField(4, 5, (r, c) => Matrix(2, 2, (rr, cc) => input(rr,cc)(r,c))))
      val out = trim(in, outShape)
      val autoTrimmedOut = MatrixField(outShape, matrixShape)
      autoTrimmedOut <== in

      probe(out, autoTrimmedOut)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(out) == expectedRef)
      require(readMatrix(autoTrimmedOut) == expectedRef)
    }
  }
}
