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
import cogx.helper.MatrixFieldBuilderInterface
import cogx.helper.VectorFieldBuilderInterface
import cogx.api.ImplicitConversions

/** Test code for VectorFields.
 */


@RunWith(classOf[JUnitRunner])
class VectorFieldSpec extends FunSuite with MustMatchers 
                      with ImplicitConversions
                      with ScalarFieldBuilderInterface
                      with MatrixFieldBuilderInterface
                      with VectorFieldBuilderInterface {
  val VectorSize = 3
  val VectorShape = Shape(VectorSize)

  val Optimize = true

  /** Test combining a dynamic vector field and a constant. */
  test("vector field / constant") {

    def vecGreater(v: Vector, thresh: Float) = v.map(x => if (x > thresh) 1f else 0f)
    def vecGreaterEq(v: Vector, thresh: Float) = v.map(x => if (x >= thresh) 1f else 0f)
    def vecLess(v: Vector, thresh: Float) = v.map(x => if (x < thresh) 1f else 0f)
    def vecLessEq(v: Vector, thresh: Float) = v.map(x => if (x <= thresh) 1f else 0f)
    def vecEq(v: Vector, thresh: Float) = v.map(x => if (x == thresh) 1f else 0f)
    def vecNotEq(v: Vector, thresh: Float) = v.map(x => if (x != thresh) 1f else 0f)

    def doTest(vectorLen: Int): Unit = {
      val vectorShape = Shape(vectorLen)
      val Size = 5

      val R = 3.21f
      val initField = RefVectorField.random(Size, Size, vectorShape)

      // Tests that involve equality should involve a threshold that matches
      // at least one element.  Here are some elements:
      val elem1 = initField.read(0,0).read(0)
      val elem2 = initField.read(Size-1,0).read(0)
      val elem3 = initField.read(Size-1,Size-1).read(0)
      val elem4 = initField.read(0,0).read(vectorLen-1)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val a = TestVectorField(initField)
        val sum = a + R
        val diff = a - R
        val product = a * R
        val quotient = a / R
        val maxa = max(a, 0.5f)
        val mina = min(a, 0.5f)
        val power = pow(a, 3.0f)
        val pown = pow(a, 3)

        // Constant appears first in mutator expression

        val constSum = R + a
        val constDiff = R - a
        val constProduct = R * a
        val constQuotient = R / a

        val greater = a > 0.5f
        val greaterEq = a >= elem1
        val less = a < 0.5f
        val lessEq = a <= elem2
        val eq3 = a === elem3
        val nEq3 = a !=== elem4

        // Above tests are all on 2D inputs.  Test 0D, 1D and 3D:

        val a0D = TestVectorField(RefVectorField.random(vectorShape))
        val a1D = TestVectorField(RefVectorField.random(Size, vectorShape))
        val a3D = TestVectorField(RefVectorField.random(Size, Size, Size, vectorShape))

        val sum0D = a0D + R
        val sum1D = a1D + R
        val sum3D = a3D + R

        probe(sum, a, diff, product, quotient, maxa, mina, power, pown, constSum, constDiff, constProduct, constQuotient,
          greater, greaterEq, less, lessEq, eq3, nEq3, sum0D, a0D, sum1D, a1D, sum3D, a3D)
      }

      import graph._
      withRelease {
        step

        require(readVector(sum) == (readVector(graph.a) + R))
        require(readVector(diff) == (readVector(graph.a) - R))
        require(readVector(product) == (readVector(graph.a) * R))
        require(readVector(quotient) ~== (readVector(graph.a) / R))
        require(readVector(maxa) ~==
          readVector(graph.a).map(v => v.map(_ max 0.5f)))
        require(readVector(mina) ~==
          readVector(graph.a).map(v => v.map(_ min 0.5f)))
        require(readVector(power) ~== (readVector(graph.a) :* readVector(graph.a) :* readVector(graph.a)))
        require(readVector(pown) ~== (readVector(graph.a) :* readVector(graph.a) :* readVector(graph.a)))

        require(readVector(constSum) == (readVector(graph.a) + R))
        require(readVector(constDiff) == (-readVector(graph.a) + R))
        require(readVector(constProduct) == (readVector(graph.a) * R))
        require(readVector(constQuotient) ~== (readVector(graph.a).reciprocal * R))

        require(readVector(greater) == readVector(graph.a).map(vecGreater(_, 0.5f)))
        require(readVector(greaterEq) == readVector(graph.a).map(vecGreaterEq(_, elem1)))
        require(readVector(less) == readVector(graph.a).map(vecLess(_, 0.5f)))
        require(readVector(lessEq) == readVector(graph.a).map(vecLessEq(_, elem2)))
        require(readVector(eq3) == readVector(graph.a).map(vecEq(_, elem3)))
        require(readVector(nEq3) == readVector(graph.a).map(vecNotEq(_, elem4)))

        require(readVector(sum0D) == (readVector(a0D) + R))
        require(readVector(sum1D) == (readVector(a1D) + R))
        require(readVector(sum3D) == (readVector(a3D) + R))
      }

    }
    // The implementation of pown has length-specific code for all the "SmallTensor" types.
    // We test all these vector lengths, plus the non-SmallTensor value 5.
    val vectorLengths = Seq(2,3,4,5,8,16)
    for (vectorLength <- vectorLengths)
      doTest(vectorLength)


  }

  /** Test combining a dynamic vector field and a vector constant. */
  test("vector field / vector constant") {
    val Size = 5

    val R = Vector.random(VectorSize)
    val initField = RefVectorField.random(Size, Size, VectorShape)

    // Tests that involve equality should involve a threshold that matches
    // at least one element.  Here are some elements:
    val elem1 = initField.read(0,0)
    val elem2 = initField.read(Size-1,0)
    val elem3 = initField.read(Size-1,Size-1)
    val elem4 = initField.read(0,0)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestVectorField(initField)
      val sum = a + R
      val diff = a - R
      val product = a * R
      val quotient = a / R
      val maxa = max(a, R)
      val mina = min(a, R)

      // Constant appears first in mutator expression

      val constSum = R + a
      val constDiff = R - a
      val constProduct = R * a
      val constQuotient = R / a

      val greater = a > R
      val greaterEq = a >= elem1
      val less = a < R
      val lessEq = a <= elem2
      val eq3 = a === elem3
      val nEq3 = a !=== elem4

      // Above tests are all on 2D inputs.  Test 0D, 1D and 3D:

      val a0D = TestVectorField(RefVectorField.random(VectorShape))
      val a1D = TestVectorField(RefVectorField.random(Size, VectorShape))
      val a3D = TestVectorField(RefVectorField.random(Size, Size, Size, VectorShape))

      val sum0D = a0D + R
      val sum1D = a1D + R
      val sum3D = a3D + R

      probe(sum, a, diff, product, quotient, maxa, mina, constSum, constDiff, constProduct, constQuotient,
        greater, greaterEq, less, lessEq, eq3, nEq3, sum0D, a0D, sum1D, a1D, sum3D, a3D)
    }

    import graph._
    withRelease {
      step

      require(readVector(sum) == (readVector(graph.a) + R))
      require(readVector(diff) == (readVector(graph.a) - R))
      require(readVector(product) == (readVector(graph.a) :* R))
      require(readVector(quotient) ~== (readVector(graph.a) :/ R))
      require(readVector(maxa) ~==
              readVector(graph.a).map(vecMax(_, R)))
      require(readVector(mina) ~==
              readVector(graph.a).map(vecMin(_, R)))

      require(readVector(constSum) == (readVector(graph.a) + R))
      require(readVector(constDiff) == (-readVector(graph.a) + R))
      require(readVector(constProduct) == (readVector(graph.a) :* R))
      require(readVector(constQuotient) ~== (readVector(graph.a).reciprocal :* R))

      require(readVector(greater) == readVector(graph.a).map(vecGreater(_, R)))
      require(readVector(greaterEq) == readVector(graph.a).map(vecGreaterEq(_, elem1)))
      require(readVector(less) == readVector(graph.a).map(vecLess(_, R)))
      require(readVector(lessEq) == readVector(graph.a).map(vecLessEq(_, elem2)))
      require(readVector(eq3) == readVector(graph.a).map(vecEq(_, elem3)))
      require(readVector(nEq3) == readVector(graph.a).map(vecNotEq(_, elem4)))

      require(readVector(sum0D) == (readVector(a0D) + R))
      require(readVector(sum1D) == (readVector(a1D) + R))
      require(readVector(sum3D) == (readVector(a3D) + R))
    }

    def vecMax(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) >= thresh(i)) v(i) else thresh(i))
    def vecMin(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) <= thresh(i)) v(i) else thresh(i))
    def vecGreater(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) > thresh(i)) 1f else 0f)
    def vecGreaterEq(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) >= thresh(i)) 1f else 0f)
    def vecLess(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) < thresh(i)) 1f else 0f)
    def vecLessEq(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) <= thresh(i)) 1f else 0f)
    def vecEq(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) == thresh(i)) 1f else 0f)
    def vecNotEq(v: Vector, thresh: Vector) = Vector(thresh.length,
      (i) => if (v(i) != thresh(i)) 1f else 0f)

  }

  /** Test combining two dynamic vector fields. */
  test("vector field / field") {
    //val Size = 125
    val Size = 4
    val Constant = 0.123f
    val ConstantVector = Vector(1.53f, -2.38f, 0.81f)
    val BigConstantVector = Vector(1.03f, -2.68f, 10.81f, 7.4f, -9.23f)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestVectorField(RefVectorField.random(Size, Size, VectorShape))
      val b = TestVectorField(RefVectorField.random(Size, Size, VectorShape) + 0.1f)
      val s = TestScalarField(RefScalarField.random(Size, Size) + 0.1f)

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

      /** A 0-dimensional vector field, can be combined with any other. */
      val v = TestVectorField(RefVectorField(ConstantVector))
      val sum0DVector = a + v
      val diff0DVector = a - v
      val product0DVector = a * v
      val quotient0DVector = a / v

      /** Repeat vectorfield op 0Dvectorfield with big tensor fields */
      val BigVectorShape = Shape(5)
      val bigA = TestVectorField(RefVectorField.random(Size, Size, BigVectorShape))
      val bigV = TestVectorField(RefVectorField(BigConstantVector))
      val bigSum0DVector = bigA + bigV
      val bigDiff0DVector = bigA - bigV
      val bigProduct0DVector = bigA * bigV
      val bigQuotient0DVector = bigA / bigV

      /** Repeat 0D tests with the 0D field appearing as the first operand */

      val sum0D2 = c + a
      val diff0D2 = c - a
      val product0D2 = c * a
      val quotient0D2 = c / a

      /** A 0-dimensional vector field, can be combined with any other. */
      val sum0DVector2 = v + a
      val diff0DVector2 = v - a
      val product0DVector2 = v * a
      val quotient0DVector2 = v / a

      /** Repeat vectorfield op 0Dvectorfield with big tensor fields */
      val bigSum0DVector2 = bigV + bigA
      val bigDiff0DVector2 = bigV - bigA
      val bigProduct0DVector2 = bigV * bigA
      val bigQuotient0DVector2 = bigV / bigA

      probe(sum, a, b, diff, product, quotient, sumS, s, diffS, productS, quotientS, sSum, sDiff, sProduct, sQuotient,
        greater, greaterEq, less, lessEq, eq1, eq2, nEq1, nEq2, sum0D, diff0D, product0D, quotient0D, sum0DVector,
        diff0DVector, product0DVector, quotient0DVector, bigSum0DVector, bigA, bigDiff0DVector, bigProduct0DVector,
        bigQuotient0DVector, sum0D2, diff0D2, product0D2, quotient0D2, sum0DVector2, diff0DVector2, product0DVector2,
        quotient0DVector2, bigSum0DVector2, bigDiff0DVector2, bigProduct0DVector2, bigQuotient0DVector2)
    }

    import graph._
    withRelease {
      step

      require(readVector(sum) == (readVector(graph.a) + readVector(b)))
      require(readVector(diff) == (readVector(graph.a) - readVector(b)))
      require(readVector(product) == (readVector(graph.a) :* readVector(b)))
      require(readVector(quotient) ~== (readVector(graph.a) :/ readVector(b)))

      require(readVector(sumS) == (readVector(graph.a) + readScalar(s)))
      require(readVector(diffS) == (readVector(graph.a) - readScalar(s)))
      require(readVector(productS) == (readVector(graph.a) :* readScalar(s)))
      require(readVector(quotientS) ~== (readVector(graph.a) :/ readScalar(s)))


      require(readVector(sSum) == (readVector(graph.a) + readScalar(s)))
      require(readVector(sDiff) == (-readVector(graph.a) + readScalar(s)))
      require(readVector(sProduct) == (readVector(graph.a) :* readScalar(s)))
      require(readVector(sQuotient) ~== (readVector(graph.a).reciprocal :* readScalar(s)))

      val aa = readVector(graph.a)
      val bb = readVector(b)
      require(readVector(greater) == aa.combine(bb, greaterThan _))
      require(readVector(greaterEq) == aa.combine(bb, greaterThanEq _))
      require(readVector(less) == aa.combine(bb, lessThan _))
      require(readVector(lessEq) == aa.combine(bb, lessThanEq _))
      require(readVector(eq1) == aa.combine(aa, Eq _))
      require(readVector(eq2) == aa.combine(bb, Eq _))
      require(readVector(nEq1) == aa.combine(aa, notEq _))
      require(readVector(nEq2) == aa.combine(bb, notEq _))

      require(readVector(sum0D) == (readVector(graph.a) + Constant))
      require(readVector(diff0D) == (readVector(graph.a) - Constant))
      require(readVector(product0D) == (readVector(graph.a) * Constant))
      require(readVector(quotient0D) ~== (readVector(graph.a) / Constant))

      require(readVector(sum0DVector) == readVector(graph.a).map(_ + ConstantVector))
      require(readVector(diff0DVector) == readVector(graph.a).map(_ - ConstantVector))
      require(readVector(product0DVector) == readVector(graph.a).map(_ :* ConstantVector))
      require(readVector(quotient0DVector) ~== readVector(graph.a).map(_ :/ ConstantVector))

      require(readVector(bigSum0DVector) == readVector(bigA).map(_ + BigConstantVector))
      require(readVector(bigDiff0DVector) == readVector(bigA).map(_ - BigConstantVector))
      require(readVector(bigProduct0DVector) == readVector(bigA).map(_ :* BigConstantVector))
      require(readVector(bigQuotient0DVector) ~== readVector(bigA).map(_ :/ BigConstantVector))

      require(readVector(sum0D2) == (readVector(graph.a) + Constant))
      require(readVector(diff0D2) == (readVector(graph.a)*(-1f) + Constant))
      require(readVector(product0D2) == (readVector(graph.a) * Constant))
      require(readVector(quotient0D2) ~== (readVector(graph.a).reciprocal * Constant))

      require(readVector(sum0DVector2) == readVector(graph.a).map(ConstantVector + _))
      require(readVector(diff0DVector2) == readVector(graph.a).map(ConstantVector - _))
      require(readVector(product0DVector2) == readVector(graph.a).map(ConstantVector :* _))
      require(readVector(quotient0DVector2) ~== readVector(graph.a).map(ConstantVector :/ _))

      require(readVector(bigSum0DVector2) == readVector(bigA).map(BigConstantVector + _))
      require(readVector(bigDiff0DVector2) == readVector(bigA).map(BigConstantVector - _))
      require(readVector(bigProduct0DVector2) == readVector(bigA).map(BigConstantVector :* _))
      require(readVector(bigQuotient0DVector2) ~== readVector(bigA).map(BigConstantVector :/ _))
    }

    def greaterThan(a: Vector, b: Vector): Vector = compare(a, b, _ > _)
    def greaterThanEq(a: Vector, b: Vector): Vector = compare(a, b, _ >= _)
    def lessThan(a: Vector, b: Vector): Vector = compare(a, b, _ < _)
    def lessThanEq(a: Vector, b: Vector): Vector = compare(a, b, _ <= _)
    def Eq(a: Vector, b: Vector): Vector = compare(a, b, _ == _)
    def notEq(a: Vector, b: Vector): Vector = compare(a, b, _ != _)

    def compare(a: Vector, b: Vector, f: (Float, Float) => Boolean): Vector = {
      val result = new Vector(a.length)
      for (i <- 0 until result.length)
        result(i) = if (f(a(i), b(i))) 1f else 0f
      result
    }

  }

  /** Test applying unary operations on dynamic vector fields. */
  test("vector field / unary") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestVectorField(
        RefVectorField.random(Size, Size, VectorShape) - 0.5f)
      val b = TestVectorField(
        RefVectorField.random(Size, Size, VectorShape) + 0.5f)

      val aAbs = abs(a)
      val aAcos = acos(a)
      val aAsin = asin(a)
      val aCos = cos(a)
      val aCosh = cosh(a)
      val aExp = exp(a)
      val bLog = log(b)
      val aSignum = signum(a)
      val aSin = sin(a)
      val aSinh = sinh(a)
      val aSq = sq(a)
      val aSqrt = sqrt(b)
      val aTan = tan(a)
      val aTanh = tanh(a)

      val aNegative = -a

      probe(aAbs, a, aAcos, aAsin, aCos, aCosh, aExp, bLog, b, aSignum, aSin, aSinh, aSq, aSqrt, aTan, aTanh, aNegative)
    }
    //val Subsize = (Size + 1) / 2
    //val subsample = new DynamicVectorField(Subsize, Subsize, VectorShape) {
    //  this <== a.subsample
    //}

    import graph._
    withRelease {
      step

      require(readVector(aAbs) == readVector(graph.a).map(v => v.map(_.abs)))
      require(readVector(aAcos) ~== readVector(graph.a).map(v => v.map(e => math.acos(e).toFloat)))
      require(readVector(aAsin) ~== readVector(graph.a).map(v => v.map(e => math.asin(e).toFloat)))
      require(readVector(aCos) ~== readVector(graph.a).map(v => v.map(e => math.cos(e).toFloat)))
      require(readVector(aCosh) ~== readVector(graph.a).map(v => v.map(e => math.cosh(e).toFloat)))
      require(readVector(aExp) ~== readVector(graph.a).map(v => v.map(e => math.exp(e).toFloat)))
      require(readVector(bLog) ~== readVector(graph.b).map(v => v.map(e => math.log(e).toFloat)))
      require(readVector(aSignum) ~== readVector(graph.a).map(v => v.map(e =>
        if (e < 0) -1f else if (e > 0) 1f else 0f)))
      require(readVector(aSin) ~== readVector(graph.a).map(v => v.map(e => math.sin(e).toFloat)))
      require(readVector(aSinh) ~== readVector(graph.a).map(v => v.map(e => math.sinh(e).toFloat)))
      require(readVector(aSq) ~== readVector(graph.a).map(v => v.map(e => e * e)))
      require(readVector(aSqrt) ~== readVector(graph.b).map(v => v.map(e => math.sqrt(e).toFloat)))
      require(readVector(aTan) ~== readVector(graph.a).map(v => v.map(e => math.tan(e).toFloat)))
      require(readVector(aTanh) ~== readVector(graph.a).map(v => v.map(e => math.tanh(e).toFloat)))
      require(readVector(aNegative) == readVector(graph.a) * -1f)
    }
  }

  /** Test the shift and shiftCyclic operators. */
  test("vector field / shift") {
    val InRows = 9
    val InColumns = 22

    val rowShift = Array(3, -1, 2, 7, -5)
    val columnShift = Array(4, 6, 0, -8, -2)

    // Test vectors of length 1, 2, 3, 4, 5
    val NumTests = 5

    val inputImage = Array.tabulate(NumTests) { i =>
      RefVectorField(InRows, InColumns,
        (row, col) => new Vector(i+1).randomize)
    }
    // Use Matrix.shift() on tensorSlices to create expected result
    // Matix.shift uses opposite sense!
    val expectedImage = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        RefScalarField(inputImage(i).sliceTensor(j).toTensor[Matrix].shift(rowShift(i), columnShift(i)))
      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(InRows, InColumns,
          (row, col) => new Vector(first.read(row, col)))
      else
        first.stackTensor(rest : _*)
    }}
    // Use Matrix.shift() on tensorSlices to create expected result
    val expectedCyclicImage = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        RefScalarField(inputImage(i).sliceTensor(j).toTensor[Matrix].shiftCyclic(rowShift(i), columnShift(i)))
      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(InRows, InColumns,
          (row, col) => new Vector(first.read(row, col)))
      else
        first.stackTensor(rest : _*)
    }}

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = Array.tabulate(NumTests) { i => TestVectorField(inputImage(i)) }
      val shifted = Array.tabulate(NumTests) { i => shift(in(i), rowShift(i), columnShift(i)) }
      val shiftedCyclic = Array.tabulate(NumTests) { i => shiftCyclic(in(i), rowShift(i), columnShift(i)) }

      probe(shifted: _*)
      probe(shiftedCyclic: _*)
    }

    import graph._
    withRelease {
      step
      for (i <- 0 until NumTests) {
        require(readVector(shifted(i)) == expectedImage(i))
        require(readVector(shiftedCyclic(i)) == expectedCyclicImage(i))
      }
    }
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("vector field / fft") {
    def testShape(fieldShape: Shape, vectorSize: Int) {
      // Offsets added to help relative error of ~==
      val VectorShape = Shape(vectorSize)
      val field = RefVectorField.random(fieldShape, VectorShape) + 0.1f
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val space = TestVectorField(field)
        val recovered = realPart(fftInverse(fft(space)))

        probe(recovered)
      }

      import graph._
      withRelease {
        step
        require(readVector(recovered) ~== field)
      }
    }

    def test1D(columns: Int, vectorSize: Int) = testShape(Shape(columns), vectorSize)

    def test2D(rows: Int, columns: Int, vectorSize: Int) = testShape(Shape(rows, columns), vectorSize)

    def test3D(layers: Int, rows: Int, columns: Int, vectorSize: Int) = testShape(Shape(layers, rows, columns), vectorSize)

    def runtest(vectorSize: Int) {
      test1D(256, vectorSize)
      test2D(64, 4, vectorSize)
      test3D(8, 2, 32, vectorSize)
    }
    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("vector field / fftRI (split real/imaginary)") {
    def testShape(fieldShape: Shape, vectorSize: Int) {
      // Offsets added to help relative error of ~==
      val VectorShape = Shape(vectorSize)
      val field = RefVectorField.random(fieldShape, VectorShape) + 0.1f
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val space = TestVectorField(field)
        val (freqR, freqI) = fftRI(space)
        val recovered = fftInverseRI(freqR, freqI)._1

        probe(recovered)
      }

      import graph._
      withRelease {
        step
        require(readVector(recovered) ~== field)
      }
    }

    def test1D(columns: Int, vectorSize: Int) = testShape(Shape(columns), vectorSize)

    def test2D(rows: Int, columns: Int, vectorSize: Int) = testShape(Shape(rows, columns), vectorSize)

    def test3D(layers: Int, rows: Int, columns: Int, vectorSize: Int) = testShape(Shape(layers, rows, columns), vectorSize)

    def runtest(vectorSize: Int) {
      test1D(128, vectorSize)
      test2D(512, 2, vectorSize)
      test3D(1, 4, 16, vectorSize)
    }
    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  test("vector field / dot") {
    val Rows = 13
    val Columns = 37

    // Test vectors of length 1, 2, 3, 4, 5
    val NumTests = 5

    val fieldA = Array.tabulate(NumTests) { i =>
      RefVectorField(Rows, Columns,
                     (row, col) => new Vector(i+1).randomize)
                                         }
    val fieldB = Array.tabulate(NumTests) { i =>
      RefVectorField(Rows, Columns,
                     (row, col) => new Vector(i+1).randomize)
                                         }
    // The 2nd operand of 'dot' is allowed to be a 0D field
    val fieldB_0D = Array.tabulate(NumTests) { i =>
      RefVectorField(new Vector(i+1).randomize)
                                            }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val A = Array.tabulate(NumTests) { i => TestVectorField(fieldA(i)) }
      val B = Array.tabulate(NumTests) { i => TestVectorField(fieldB(i)) }
      val B_0D = Array.tabulate(NumTests) { i => TestVectorField(fieldB_0D(i)) }

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
        val a = readVector(A(i))
        val b = readVector(B(i))
        val c = readScalar(C(i))
        for (row <- 0 until Rows; col <- 0 until Columns) {
          val aVector = a.read(row, col)
          val bVector = b.read(row, col)
          val dotProduct = aVector dot bVector
          require(c.read(row, col) ~== dotProduct)
        }
      }
      // Check A dot B, where B is a 0D field
      for (i <- 0 until NumTests) {
        val a = readVector(A(i))
        val bVector = readVector(B_0D(i)).read()
        val c = readScalar(C_0D(i))
        for (row <- 0 until Rows; col <- 0 until Columns) {
          val aVector = a.read(row, col)
          val dotProduct = aVector dot bVector
          require(c.read(row, col) ~== dotProduct)
        }
      }
    }
  }

  test("vector field / cross dot") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s = TestScalarField(RefScalarField.random(VectorSize))
      val w = TestVectorField(RefVectorField.random(Size, Size, VectorShape))
      val z = crossDot(w, s)

      probe(w, s, z)
    }

    import graph._
    withRelease {
      step

      for (row <- 0 until Size; col <- 0 until Size) {
        val wVector = readVector(w).read(row, col)
        val sVector = readScalar(s).toTensor[Vector]
        val dotProduct = wVector dot sVector
        require(readScalar(z).read(row, col) ~== dotProduct)
      }
    }
  }

  test("vector field / reverse cross dot") {
    val Rows = 5
    val Columns = 7

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val z = TestScalarField(RefScalarField.random(Rows, Columns))
      val w = TestVectorField(RefVectorField.random(Rows, Columns, VectorShape))
      val x = reverseCrossDot(w, z)

      probe(w, z, x)

    }
    import graph._
    withRelease {
      step
      var sum = new Vector(VectorSize)
      for (r <- 0 until Rows; c <- 0 until Columns)
        sum += readVector(w).read(r, c) * readScalar(z).read(r, c)
      for (r <- 0 until VectorSize)
        require(readScalar(x).read(r) ~== sum(r))
    }
  }

  /** Test the expand operator. */
  test("vector field / expand border") {

    // 2D test parameters
    val InRows = 9
    val InColumns = 22
    val OutRows = 13
    val OutColumns = 37

    // 1D test parameters

    val InColumns_1D = 7
    val OutColumns_1D = 11

    // Test vectors of length 1, 2, 3, 4, 5
    val NumTests = 5

    val inputImage = Array.tabulate(NumTests) { i =>
      RefVectorField(InRows, InColumns,
                     (row, col) => new Vector(i+1).randomize)
                                             }
    // Use Matrix.expand() on tensorSlices to create expected result
    val expectedImage = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        RefScalarField(inputImage(i).sliceTensor(j).toTensor[Matrix].
          expand(OutRows, OutColumns, borderFill = true))
                                      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(OutRows, OutColumns,
                       (row, col) => new Vector(first.read(row, col)))
      else
        first.stackTensor(rest : _*)
    }}

    // 1D test inputs and expected outputs

    val inputImage_1D = Array.tabulate(NumTests) { i =>
      RefVectorField(InColumns_1D,
        (col) => new Vector(i+1).randomize)
    }
    // Use Vector.expand() on tensorSlices to create expected result
    val expectedImage_1D = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        RefScalarField(inputImage_1D(i).sliceTensor(j).toTensor[Vector].
          expand(OutColumns_1D, borderFill = true))
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
      val in = Array.tabulate(NumTests) { i => TestVectorField(inputImage(i)) }
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
        require(readVector(expanded(i)) == expectedImage(i))
      for (i <- 0 until NumTests)
        require(readVector(expanded_1D(i)) == expectedImage_1D(i))
    }
  }

  /** Test the (Int) operator. */
  test("vector field / slice") {
    val image = RefVectorField(3, 3, (row, col) => new Vector(Array[Float](row, col)))

    val expect0 = RefVectorField(3, (col) => new Vector(Array[Float](0, col)))
    val expect1 = RefVectorField(3, (col) => new Vector(Array[Float](1, col)))
    val expect2 = RefVectorField(3, (col) => new Vector(Array[Float](2, col)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestVectorField(image)
      val row0 = field1(0)
      val row1 = field1(1)
      val row2 = field1(2)

      probe(row0, row1, row2)
    }

    import graph._
    withRelease {
      step
      require(readVector(row0) == expect0)
      require(readVector(row1) == expect1)
      require(readVector(row2) == expect2)
    }
  }


  /** Test the (0D-ScalarField) operator (slice). */
  test("vector field / slice point") {

    // 1D input

    val rand = new cogx.utilities.Random
    val shape1D = Shape(17)
    val vectorShape = Shape(4)
    val image1D = RefVectorField.random(shape1D, vectorShape)
    // 2D input

    val shape2D = Shape(5, 7)
    val vectorShape2 = Shape(5)
    val image2D = RefVectorField.random(shape2D, vectorShape2)

    // 3D input

    val shape3D = Shape(3, 9, 11)
    val vectorShape3 = Shape(2)
    val image3D = RefVectorField.random(shape2D, vectorShape3)

    val sliceToExtract1D = shape1D(0) * rand.nextFloat
    val sliceToExtract2D = shape2D(0) * rand.nextFloat
    val sliceToExtract3D = shape3D(0) * rand.nextFloat

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1D = TestVectorField(image1D)
      val indexField1D = TestScalarField(RefScalarField(sliceToExtract1D))
      val slicedField0D = field1D(indexField1D)

      val field2D = TestVectorField(image2D)
      val indexField2D = TestScalarField(RefScalarField(sliceToExtract2D))
      val slicedField1D = field2D(indexField2D)

      val field3D = TestVectorField(image3D)
      val indexField3D = TestScalarField(RefScalarField(sliceToExtract3D))
      val slicedField2D = field3D(indexField3D)

      probe(slicedField0D, slicedField1D, slicedField2D)
    }

    import graph._
    withRelease {
      step
      require(readVector(slicedField0D) == image1D.slice(sliceToExtract1D.toInt))
      require(readVector(slicedField1D) == image2D.slice(sliceToExtract2D.toInt))
      require(readVector(slicedField2D) == image3D.slice(sliceToExtract3D.toInt))
    }
  }

  /** Test the stack operator. */
  test("vector field / stack") {
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


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f0 = TestVectorField(field0)
      val f1 = TestVectorField(field1)
      val f2 = TestVectorField(field2)
      val fArray = Array(f0, f1, f2)
      val stack0 = stack(f0, f1, f2)
      val stack1 = stack(fArray)

      probe(stack0, stack1)
    }

    import graph._
    withRelease {
      step
      require(readVector(stack0) == expected)
      require(readVector(stack1) == expected)
    }
  }

  /** Test the tensors(Int) operator. This should technically be in the
   * ScalarFieldGeneratorSpec, since the output is a ScalarField.  XXX */
  test("vector field / tensor slice") {
    val Rows = 3
    val Columns = 3
    val image = RefVectorField(Rows, Columns,
                               (row, col) => new Vector(Array[Float](row, col, row + col)))

    val expect0 = RefScalarField(Rows, Columns, (row, col) => row)
    val expect1 = RefScalarField(Rows, Columns, (row, col) => col)
    val expect2 = RefScalarField(Rows, Columns, (row, col) => row + col)

    val bigImage = RefVectorField(Rows, Columns,
                                  (row, col) => new Vector(Array[Float](row, col, row - col, row + col, row * col)))

    val bigExpect0 = RefScalarField(Rows, Columns, (row, col) => row)
    val bigExpect1 = RefScalarField(Rows, Columns, (row, col) => col)
    val bigExpect2 = RefScalarField(Rows, Columns, (row, col) => row - col)
    val bigExpect3 = RefScalarField(Rows, Columns, (row, col) => row + col)
    val bigExpect4 = RefScalarField(Rows, Columns, (row, col) => row * col)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field = TestVectorField(image)
      val slice0 = vectorElement(field, 0)
      val slice1 = vectorElement(field, 1)
      val slice2 = vectorElement(field, 2)
      val bigField = TestVectorField(bigImage)
      val bigSlice0 = vectorElement(bigField, 0)
      val bigSlice1 = vectorElement(bigField, 1)
      val bigSlice2 = vectorElement(bigField, 2)
      val bigSlice3 = vectorElement(bigField, 3)
      val bigSlice4 = vectorElement(bigField, 4)

      probe(slice0, slice1, slice2, bigSlice0, bigSlice1, bigSlice2, bigSlice3, bigSlice4)
    }

    import graph._
    withRelease {
      step

      require(readScalar(slice0) == expect0)
      require(readScalar(slice1) == expect1)
      require(readScalar(slice2) == expect2)

      require(readScalar(bigSlice0) == bigExpect0)
      require(readScalar(bigSlice1) == bigExpect1)
      require(readScalar(bigSlice2) == bigExpect2)
      require(readScalar(bigSlice3) == bigExpect3)
      require(readScalar(bigSlice4) == bigExpect4)
    }
  }

  /** Test the stackTensors operator. */
  test("vector field / tensor stack") {
    val Rows = 3
    val Columns = 3
    val VectorLength = 2
    val MatrixRows = 3
    val image0 = RefVectorField(Rows, Columns,
                                (row, col) => new Vector(Array[Float](row, col)))
    val image1 = RefVectorField(Rows, Columns,
                                (row, col) => new Vector(Array[Float](col, row)))
    val image2 = RefVectorField(Rows, Columns,
                                (row, col) => new Vector(Array[Float](col * row, row + 5)))
    val expected = RefMatrixField(Rows, Columns,
                                  (row, col) => Matrix(
                                    Array[Float](row, col),
                                    Array[Float](col, row),
                                    Array[Float](col * row, row + 5)
                                  )
                                )


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field0 = TestVectorField(image0)
      val field1 = TestVectorField(image1)
      val field2 = TestVectorField(image2)
      val stackedField = matrixField(field0, field1, field2)

      probe(stackedField)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(stackedField) == expected)
    }
  }

  /** Test the tensor reduction operators. */
  test("vector field / tensor reduce") {
    val Rows = 3
    val Columns = 3

    def expectedReducedSum(field: RefVectorField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toArray.reduceLeft(_ + _))
    def expectedReducedMin(field: RefVectorField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toArray.reduceLeft(_ min _))
    def expectedReducedMax(field: RefVectorField) = RefScalarField(Rows, Columns,
                                                                   (row, col) => field.read(row, col).toArray.reduceLeft(_ max _))

    // Test vectors of length 1, 2, 3, 4, 5

    val field = Array.tabulate(5) { i =>
      RefVectorField(Rows, Columns,
                     (row, col) => new Vector(i+1).randomize)
                                 }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v = Array.tabulate(5) { i => TestVectorField(field(i)) }
      val vSum = Array.tabulate(5) { i => reduceSum(v(i)) }
      val vMax = Array.tabulate(5) { i => reduceMax(v(i)) }
      val vMin = Array.tabulate(5) { i => reduceMin(v(i)) }

      // Concatenate all the arrays and then probe all the elements
      probe((vSum ++ vMax ++ vMin): _*)

    }
    import graph._
    withRelease {
      step
      for (i <- 0 until 5) {
        // GPU sum done as binary tree, not reduceLeft, so answer is approx.
        require(readScalar(vSum(i)) ~== expectedReducedSum(field(i)))
        require(readScalar(vMax(i)) == expectedReducedMax(field(i)))
        require(readScalar(vMin(i)) == expectedReducedMin(field(i)))
      }
    }
  }

  /** Test the tensor reduction operators. */
  test("vector field / tensor reduce sum precision") {
    // This tests Cog's ability to perform large reductions fairly precisely.
    // The test adds the value (2^12 + 1) to itself 2^13 times, so the result is:
    //
    // (2^12 + 1) * 2^13 = 2^25 + 2^13
    //
    // The value is representable by a single precision float with its 23-bit
    // mantissa exactly, but the result will not be obtained if a single accumulator
    // is used.
    val VectorLen = 8192
    val VectorVal = 4097L
    val ExpectedSum = VectorLen * VectorVal

    val graph = new ComputeGraph(Optimize) {
      val v = VectorField(Vector(VectorLen, (c) => VectorVal))
      val vSum = reduceSum(v)
      // Concatenate all the arrays and then probe all the elements
      probe(vSum)

    }
    import graph._
    withRelease {
      step
      val actualSum = read(vSum).asInstanceOf[ScalarFieldReader].read().toLong
      require(actualSum == ExpectedSum, s"Expecting tensor sum $ExpectedSum, saw $actualSum")
    }
  }
  /** Test the tensor reduction operators. */
  test("vector field / tensor block reduce sum precision") {
    // This tests Cog's ability to perform large reductions fairly precisely.
    // The test adds the value (2^12 + 1) to itself (2^13 + 2^3) times, so the result is:
    //
    // (2^12 + 1) * (2^13 + 2^3) = 2^25 + 2^15 + 2^13 + 2^3
    //
    // The value is representable by a single precision float with its 23-bit
    // mantissa exactly, but the result will not be obtained if a single accumulator
    // is used.
    val VectorLen = 8192 + 8
    val VectorVal = 4097L
    val OutputLen = 2
    val ExpectedSum = VectorLen * VectorVal

    val graph = new ComputeGraph(Optimize) {
      val v = VectorField(Vector(VectorLen*OutputLen, (c) => VectorVal))
      val vSum = blockReduceSum(v, VectorLen)
      // Concatenate all the arrays and then probe all the elements
      probe(vSum)

    }
    import graph._
    withRelease {
      step
      val reader = read(vSum).asInstanceOf[VectorFieldReader]
      val outVector = new Vector(OutputLen)
      reader.read(outVector)
      for (i <- 0 until OutputLen) {
        val actualSum = outVector(i).toLong
        require(actualSum == ExpectedSum, s"Expecting tensor sum $ExpectedSum, saw $actualSum")
      }
    }
  }

  /** Test the block tensor reduction operators. */
  test("vector field / block tensor reduce") {
    val Rows = 3
    val Columns = 3

    // Test reduction factors of 2, 3, 4 and 5

    val numTests = 4
    def outVLen(testNum: Int) = testNum + 2
    def factor(testNum: Int) = testNum + 2
    def inVLen(testNum: Int) = outVLen(testNum) * factor(testNum)

    def expectedBlockReducedSum(testNum: Int, field: RefVectorField) = RefVectorField(Rows, Columns,
      (row, col) => Vector(outVLen(testNum), i =>
        field.read(row, col).
        subvector(i*factor(testNum) until (i+1)*factor(testNum)).toArray.reduceLeft(_ + _)))
    def expectedBlockReducedMax(testNum: Int, field: RefVectorField) = RefVectorField(Rows, Columns,
      (row, col) => Vector(outVLen(testNum), i =>
        field.read(row, col).
        subvector(i*factor(testNum) until (i+1)*factor(testNum)).toArray.reduceLeft(_ max _)))
    def expectedBlockReducedMin(testNum: Int, field: RefVectorField) = RefVectorField(Rows, Columns,
      (row, col) => Vector(outVLen(testNum), i =>
        field.read(row, col).
        subvector(i*factor(testNum) until (i+1)*factor(testNum)).toArray.reduceLeft(_ min _)))

    val field = Array.tabulate(numTests) { i =>
      RefVectorField(Rows, Columns,
                     (row, col) => new Vector(inVLen(i)).randomize)
                                 }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v = Array.tabulate(numTests) { i => TestVectorField(field(i)) }
      val vSum = Array.tabulate(numTests) { i => blockReduceSum(v(i), factor(i)) }
      val vMax = Array.tabulate(numTests) { i => blockReduceMax(v(i), factor(i)) }
      val vMin = Array.tabulate(numTests) { i => blockReduceMin(v(i), factor(i)) }

      // Concatenate all the arrays and then probe all the elements
      probe((vSum ++ vMax ++ vMin): _*)
    }
    import graph._
    withRelease {
      step
      for (i <- 0 until numTests) {
        // GPU sum done as binary tree, not reduceLeft, so answer is approx.
        require(readVector(vSum(i)) ~== expectedBlockReducedSum(i, field(i)))
        require(readVector(vMax(i)) == expectedBlockReducedMax(i, field(i)))
        require(readVector(vMin(i)) == expectedBlockReducedMin(i, field(i)))
      }
    }
  }

  /** Test the fieldReduceSum operator. */
  test("vector field / field reduce sum") {
    val Rows = 3
    val Columns = 3
    val VectorLength = 5
    val field = RefVectorField(Rows, Columns,
                               (row, col) => new Vector(VectorLength).randomize)
    val sum = new Vector(VectorLength)
    for (r <- 0 until Rows; c <- 0 until Columns)
      sum += field.read(r, c)
    val expectedReducedSum = RefVectorField(sum)

    // Try another case: 0D field

    val VectorLength0 = 4
    val sum0 = new Vector(VectorLength0).randomize
    val field0 = RefVectorField(sum0)
    val expectedReducedSum0 = RefVectorField(sum0)

    val VectorLength0_2 = 5
    val sum0_2 = new Vector(VectorLength0_2).randomize
    val field0_2 = RefVectorField(sum0_2)
    val expectedReducedSum0_2 = RefVectorField(sum0_2)

    // Try another 2 cases: 1D fields

    // Vector fields of length 1 are important to check: they can exercise unique code paths
    val Rows1 = 511
    val VectorLength1 = 1
    val field1 = RefVectorField(Rows1,
                                (row) => new Vector(VectorLength1).randomize)
    val sum1 = new Vector(VectorLength1)
    for (r <- 0 until Rows1)
      sum1 += field1.read(r)
    val expectedReducedSum1 = RefVectorField(sum1)


    val Rows2 = 257
    val VectorLength2 = 13
    val field2 = RefVectorField(Rows2,
                                (row) => new Vector(VectorLength2).randomize)
    val sum2 = new Vector(VectorLength2)
    for (r <- 0 until Rows2)
      sum2 += field2.read(r)
    val expectedReducedSum2 = RefVectorField(sum2)

    // Try another case: 2D Tensor Field

    val Rows3 = 371
    val Columns3 = 113
    val VectorLength3 = 2
    val field3 = RefVectorField(Rows3, Columns3,
                                (row, col) => new Vector(VectorLength3).randomize)
    val sum3 = new Vector(VectorLength3)
    for (r <- 0 until Rows3; c <- 0 until Columns3)
      sum3 += field3.read(r, c)
    val expectedReducedSum3 = RefVectorField(sum3)

    // Try another case: 3D vector field

    val Layers4 = 5
    val Rows4 = 3
    val Columns4 = 155
    val VectorShape4 = Shape(4)
    val field4 = RefVectorField.random(Layers4, Rows4, Columns4, VectorShape4)

    val sum4 = new Vector(VectorShape4(0))
    for (l <- 0 until Layers4; r <- 0 until Rows4; c <- 0 until Columns4)
      sum4 += field4.read(l, r, c)
    val expectedReducedSum4 = RefVectorField(sum4)

    // Try another case: 3D Tensor Field

    val Layers5 = 7
    val Rows5 = 17
    val Columns5 = 17
    val VectorShape5 = Shape(3)
    val field5 = RefVectorField.random(Layers5, Rows5, Columns5, VectorShape5)

    val sum5 = new Vector(VectorShape5(0))
    for (l <- 0 until Layers5; r <- 0 until Rows5; c <- 0 until Columns5)
      sum5 += field5.read(l, r, c)
    val expectedReducedSum5 = RefVectorField(sum5)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v = TestVectorField(field)
      val vSum = fieldReduceSum(v)

      val v0 = TestVectorField(field0)
      val vSum0 = fieldReduceSum(v0)

      val v0_2 = TestVectorField(field0_2)
      val vSum0_2 = fieldReduceSum(v0_2)

      val v1 = TestVectorField(field1)
      val vSum1 = fieldReduceSum(v1)

      val v2 = TestVectorField(field2)
      val vSum2 = fieldReduceSum(v2)

      val v3 = TestVectorField(field3)
      val vSum3 = fieldReduceSum(v3)

      val input4 = TestVectorField(field4)
      val vSum4 = fieldReduceSum(input4)

      val input5 = TestVectorField(field5)
      val vSum5 = fieldReduceSum(input5)

      probe(vSum, vSum0, vSum0_2, vSum1, vSum2, vSum3, vSum4, vSum5)
    }

    import graph._
    withRelease {
      step
      require(readVector(vSum) ~== expectedReducedSum)
      require(readVector(vSum0) ~== expectedReducedSum0)
      require(readVector(vSum0_2) ~== expectedReducedSum0_2)
      require(readVector(vSum1) ~== expectedReducedSum1)
      require(readVector(vSum2) ~== expectedReducedSum2)
      require(readVector(vSum3) ~== expectedReducedSum3)
      require(readVector(vSum4) ~== expectedReducedSum4)
      require(readVector(vSum5) ~== expectedReducedSum5)
    }
  }

  /** Test the fieldReduceMax operator. */
  test("vector field / field reduce max") {
    val Rows = 3
    val Columns = 3
    val VectorLength = 5
    val field = RefVectorField(Rows, Columns,
                               (row, col) => new Vector(VectorLength).randomize)

    // Create a vector that is the component-wise maximum of two vectors
    def vectorMax(a: Vector, b: Vector) = Vector(a.length, (i) => math.max(a(i),b(i)))

    // Create a RefVectorField that is the fieldReduceMax of a RefVectorField
    def refVectorFieldMax(field: RefVectorField) = RefVectorField(field.reduce(vectorMax(_ , _)))

    val expectedReducedMax = refVectorFieldMax(field)

    // Try another case: 0D field

    val VectorLength0 = 4
    val max0 = new Vector(VectorLength0).randomize
    val field0 = RefVectorField(max0)
    val expectedReducedMax0 = RefVectorField(max0)

    val VectorLength0_2 = 5
    val max0_2 = new Vector(VectorLength0_2).randomize
    val field0_2 = RefVectorField(max0_2)
    val expectedReducedMax0_2 = RefVectorField(max0_2)

    // Try another 2 cases: 1D fields

    // Vector fields of length 1 are important to check: they can exercise unique code paths
    val Rows1 = 511
    val VectorLength1 = 1
    val field1 = RefVectorField(Rows1, (row) => new Vector(VectorLength1).randomize)
    val expectedReducedMax1 = refVectorFieldMax(field1)


    val Rows2 = 257
    val VectorLength2 = 13
    val field2 = RefVectorField(Rows2,
                                (row) => new Vector(VectorLength2).randomize)
    val expectedReducedMax2 = refVectorFieldMax(field2)

    // Try another case: 2D Tensor Field

    val Rows3 = 371
    val Columns3 = 113
    val VectorLength3 = 2
    val field3 = RefVectorField(Rows3, Columns3,
                                (row, col) => new Vector(VectorLength3).randomize)
    val expectedReducedMax3 = refVectorFieldMax(field3)

    // Try another case: 3D vector field

    val Layers4 = 5
    val Rows4 = 3
    val Columns4 = 155
    val VectorShape4 = Shape(4)
    val field4 = RefVectorField.random(Layers4, Rows4, Columns4, VectorShape4)

    val expectedReducedMax4 = refVectorFieldMax(field4)

    // Try another case: 3D Tensor Field

    val Layers5 = 7
    val Rows5 = 17
    val Columns5 = 17
    val VectorShape5 = Shape(3)
    val field5 = RefVectorField.random(Layers5, Rows5, Columns5, VectorShape5)

    val expectedReducedMax5 = refVectorFieldMax(field5)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v = TestVectorField(field)
      val vSum = fieldReduceMax(v)

      val v0 = TestVectorField(field0)
      val vSum0 = fieldReduceMax(v0)

      val v0_2 = TestVectorField(field0_2)
      val vSum0_2 = fieldReduceMax(v0_2)

      val v1 = TestVectorField(field1)
      val vSum1 = fieldReduceMax(v1)

      val v2 = TestVectorField(field2)
      val vSum2 = fieldReduceMax(v2)

      val v3 = TestVectorField(field3)
      val vSum3 = fieldReduceMax(v3)

      val input4 = TestVectorField(field4)
      val vSum4 = fieldReduceMax(input4)

      val input5 = TestVectorField(field5)
      val vSum5 = fieldReduceMax(input5)

      probe(vSum, vSum0, vSum0_2, vSum1, vSum2, vSum3, vSum4, vSum5)
    }

    import graph._
    withRelease {
      step
      require(readVector(vSum) ~== expectedReducedMax)
      require(readVector(vSum0) ~== expectedReducedMax0)
      require(readVector(vSum0_2) ~== expectedReducedMax0_2)
      require(readVector(vSum1) ~== expectedReducedMax1)
      require(readVector(vSum2) ~== expectedReducedMax2)
      require(readVector(vSum3) ~== expectedReducedMax3)
      require(readVector(vSum4) ~== expectedReducedMax4)
      require(readVector(vSum5) ~== expectedReducedMax5)
    }
  }

  /** Test the fieldReduceMin operator. */
  test("vector field / field reduce min") {
    val Rows = 3
    val Columns = 3
    val VectorLength = 5
    val field = RefVectorField(Rows, Columns,
                               (row, col) => new Vector(VectorLength).randomize)

    // Create a vector that is the component-wise minimum of two vectors
    def vectorMin(a: Vector, b: Vector) = Vector(a.length, (i) => math.min(a(i),b(i)))

    // Create a RefVectorField that is the fieldReduceMin of a RefVectorField
    def refVectorFieldMin(field: RefVectorField) = RefVectorField(field.reduce(vectorMin(_ , _)))

    val expectedReducedMin = refVectorFieldMin(field)

    // Try another case: 0D field

    val VectorLength0 = 4
    val min0 = new Vector(VectorLength0).randomize
    val field0 = RefVectorField(min0)
    val expectedReducedMin0 = RefVectorField(min0)

    val VectorLength0_2 = 5
    val min0_2 = new Vector(VectorLength0_2).randomize
    val field0_2 = RefVectorField(min0_2)
    val expectedReducedMin0_2 = RefVectorField(min0_2)

    // Try another 2 cases: 1D fields

    // Vector fields of length 1 are important to check: they can exercise unique code paths
    val Rows1 = 511
    val VectorLength1 = 1
    val field1 = RefVectorField(Rows1, (row) => new Vector(VectorLength1).randomize)
    val expectedReducedMin1 = refVectorFieldMin(field1)


    val Rows2 = 257
    val VectorLength2 = 13
    val field2 = RefVectorField(Rows2,
                                (row) => new Vector(VectorLength2).randomize)
    val expectedReducedMin2 = refVectorFieldMin(field2)

    // Try another case: 2D Tensor Field

    val Rows3 = 371
    val Columns3 = 113
    val VectorLength3 = 2
    val field3 = RefVectorField(Rows3, Columns3,
                                (row, col) => new Vector(VectorLength3).randomize)
    val expectedReducedMin3 = refVectorFieldMin(field3)

    // Try another case: 3D vector field

    val Layers4 = 5
    val Rows4 = 3
    val Columns4 = 155
    val VectorShape4 = Shape(4)
    val field4 = RefVectorField.random(Layers4, Rows4, Columns4, VectorShape4)

    val expectedReducedMin4 = refVectorFieldMin(field4)

    // Try another case: 3D Tensor Field

    val Layers5 = 7
    val Rows5 = 17
    val Columns5 = 17
    val VectorShape5 = Shape(3)
    val field5 = RefVectorField.random(Layers5, Rows5, Columns5, VectorShape5)

    val expectedReducedMin5 = refVectorFieldMin(field5)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v = TestVectorField(field)
      val vSum = fieldReduceMin(v)

      val v0 = TestVectorField(field0)
      val vSum0 = fieldReduceMin(v0)

      val v0_2 = TestVectorField(field0_2)
      val vSum0_2 = fieldReduceMin(v0_2)

      val v1 = TestVectorField(field1)
      val vSum1 = fieldReduceMin(v1)

      val v2 = TestVectorField(field2)
      val vSum2 = fieldReduceMin(v2)

      val v3 = TestVectorField(field3)
      val vSum3 = fieldReduceMin(v3)

      val input4 = TestVectorField(field4)
      val vSum4 = fieldReduceMin(input4)

      val input5 = TestVectorField(field5)
      val vSum5 = fieldReduceMin(input5)

      probe(vSum, vSum0, vSum0_2, vSum1, vSum2, vSum3, vSum4, vSum5)
    }

    import graph._
    withRelease {
      step
      require(readVector(vSum) ~== expectedReducedMin)
      require(readVector(vSum0) ~== expectedReducedMin0)
      require(readVector(vSum0_2) ~== expectedReducedMin0_2)
      require(readVector(vSum1) ~== expectedReducedMin1)
      require(readVector(vSum2) ~== expectedReducedMin2)
      require(readVector(vSum3) ~== expectedReducedMin3)
      require(readVector(vSum4) ~== expectedReducedMin4)
      require(readVector(vSum5) ~== expectedReducedMin5)
    }
  }

  /** Test the winnerTakeAll operator. */
  test("vector field / winner take all") {

    // 2D test parameters, will generate 2-kernel chained reduction
    val Rows = 11
    val Columns = 42

    // 1D test parameters

    val Columns_1D = 7

    // Test vectors of length 1, 2, 3, 4, 5
    val NumTests = 5

    val input = Array.tabulate(NumTests) { i =>
      RefVectorField(Rows, Columns,
        (row, col) => new Vector(i+1).randomize)
    }
    // Use RefScalarField.winnerTakeAll on tensorSlices to create expected result
    val expectedOutput = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate[RefScalarField](i+1) { j =>
        input(i).sliceTensor(j).winnerTakeAll
      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(Rows, Columns,
          (row, col) => new Vector(first.read(row, col)))
      else
        first.stackTensor(rest : _*)
    }}

    // 1D test inputs and expected outputs

    val input_1D = Array.tabulate(NumTests) { i =>
      RefVectorField(Columns_1D,
        (col) => new Vector(i+1).randomize)
    }
    // Use RefScalarField.winnerTakeAll on tensorSlices to create expected result
    val expectedOutput_1D = Array.tabulate(NumTests) { i => {
      val slices = Array.tabulate(i+1) { j =>
        input_1D(i).sliceTensor(j).winnerTakeAll
      }
      val first = slices(0)
      val rest = Array.tabulate(slices.length - 1) { i => slices(i+1) }
      if (i == 0)
        RefVectorField(Columns_1D,
          (col) => new Vector(first.read(col)))
      else
        first.stackTensor(rest : _*)
    }}

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = Array.tabulate(NumTests) { i => TestVectorField(input(i)) }
      val wta = Array.tabulate(NumTests) { i => winnerTakeAll(in(i)) }
      val in_1D = Array.tabulate(NumTests) { i => TestVectorField(input_1D(i)) }
      val wta_1D = Array.tabulate(NumTests) { i => winnerTakeAll(in_1D(i)) }

      probe((wta ++ wta_1D): _*)
    }

    import graph._
    withRelease {
      step
      for (i <- 0 until NumTests)
        require(readVector(wta(i)) == expectedOutput(i))
      for (i <- 0 until NumTests)
        require(readVector(wta_1D(i)) == expectedOutput_1D(i))
    }
  }

  /** Test the field reduction operators. Only sum defined for vectors */
  test("vector field / max position") {
    val Layers = 5
    val Rows = 4
    val Columns = 7

    // normL2 used to create functions that max-out at a specified point

    // 1D input field

    val correctWinner1 = new Vector(3f)
    val init1 = RefScalarField(Columns, (c) => 1.5f - (new Vector(c.toFloat) - correctWinner1).normL2)

    val correctWinner2 = new Vector(2f, 5f)
    val init2 = RefScalarField(Rows, Columns, (r, c) => 3.5f - (new Vector(r.toFloat, c.toFloat) - correctWinner2).normL2)

    val correctWinner3 = new Vector(3f, 2f, 6f)
    val init3 = RefScalarField(Layers, Rows, Columns, (l, r, c) => -(new Vector(l.toFloat, r.toFloat, c.toFloat) - correctWinner3).normL2)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input1 = TestScalarField(init1)
      val winner1 = maxPosition(input1)

      // 2D input field

      val input2 = TestScalarField(init2)
      val winner2 = maxPosition(input2)

      // 3D input field

      val input3 = TestScalarField(init3)
      val winner3 = maxPosition(input3)

      probe(winner1, winner2, winner3)
    }
    import graph._
    withRelease {
      step

      require(readVector(winner1).read() == correctWinner1)
      require(readVector(winner2).read() == correctWinner2)
      require(readVector(winner3).read() == correctWinner3)
    }
  }

  test("vector field / commutative") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field = TestVectorField(RefVectorField.random(10, 10, Shape(5)))
      val plus = field + 1
      val plusReverse = 1 + field
      val minus = 1 - field
      val minusReverse = -field + 1
      val times = 2 * field
      val timesReverse = field * 2

      probe(plus, plusReverse, minus, minusReverse, times, timesReverse)
    }

    import graph._
    withRelease {
      step
      require(readVector(plus) == readVector(plusReverse))
      require(readVector(minus) == readVector(minusReverse))
      require(readVector(times) == readVector(timesReverse))
    }
  }

  test("vector field / transpose") {
    // Try vectors of length 1, 4 and 5

    val field1 = RefVectorField.random(100, 100, Shape(1))
    val transposed1 = RefMatrixField(100, 100,
                                     (row, col) => field1.read(row, col).transpose)

    val field4 = RefVectorField.random(100, 100, Shape(4))
    val transposed4 = RefMatrixField(100, 100,
                                     (row, col) => field4.read(row, col).transpose)

    val field5 = RefVectorField.random(100, 100, Shape(5))
    val transposed5 = RefMatrixField(100, 100,
                                     (row, col) => field5.read(row, col).transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val v1 = TestVectorField(field1)
      val m1 = transposeVectors(v1)

      val v4= TestVectorField(field4)
      val m4 = transposeVectors(v4)

      val v5 = TestVectorField(field5)
      val m5 = transposeVectors(v5)

      probe(m1, m4, m5)
    }

    import graph._
    withRelease {
      step
      require(readMatrix(m1) == transposed1)
      require(readMatrix(m4) == transposed4)
      require(readMatrix(m5) == transposed5)
    }
  }

  /*
  test("vector field / push") {
    // 1D VectorField Stack, 0D VectorField Slice
    def point1D(col: Int) = new Vector(col, 2 * col + 1)
    val Columns_1D = 3
    val stack_1D = RefVectorField(Columns_1D, point1D _)
    val stack0_1D = RefVectorField(point1D(0))
    val stack1_1D = RefVectorField(point1D(1))
    val stack2_1D = RefVectorField(point1D(2))
    val slice_1D = RefVectorField(point1D(5))

    // 2D VectorField Stack, 1D VectorField Slice
    def point2D(row: Int, col: Int) = new Vector(row + col, row * col)
    val Rows_2D = 4
    val Columns_2D = 5
    val stack_2D = RefVectorField(Rows_2D, Columns_2D, point2D _)
    val stack0_2D = RefVectorField(Columns_2D, point2D(0, _))
    val stack1_2D = RefVectorField(Columns_2D, point2D(1, _))
    val stack2_2D = RefVectorField(Columns_2D, point2D(2, _))
    val stack3_2D = RefVectorField(Columns_2D, point2D(3, _))
    val slice_2D = RefVectorField(Columns_2D, point2D(5, _))

    // 3D VectorField Stack, 2D VectorField Slice
    def point3D(depth: Int, row: Int, col: Int) = new Vector(depth + row, row + col, row * col, col - depth, col * depth)
    val Depth_3D = 3
    val Rows_3D = 4
    val Columns_3D = 5
    val stack_3D = RefVectorField(Depth_3D, Rows_3D, Columns_3D, point3D _)
    val stack0_3D = RefVectorField(Rows_3D, Columns_3D, point3D(0, _, _))
    val stack1_3D = RefVectorField(Rows_3D, Columns_3D, point3D(1, _, _))
    val stack2_3D = RefVectorField(Rows_3D, Columns_3D, point3D(2, _, _))
    val slice_3D = RefVectorField(Rows_3D, Columns_3D, point3D(5, _, _))

    val graph = new ComputeGraph(Optimize) with RefTestInterface2 {
      val a_1D = TestVectorField(stack_1D)
      val b_1D = a_1D push TestVectorField(slice_1D)

      val a_2D = TestVectorField(stack_2D)
      val b_2D = a_2D push TestVectorField(slice_2D)

      val a_3D = TestVectorField(stack_3D)
      val b_3D = a_3D push TestVectorField(slice_3D)
    }

    import graph._
    withRelease {
      step

      // check 1D stack
      require(readVector(a_1D).slice(0) == stack0_1D)
      require(readVector(a_1D).slice(1) == stack1_1D)
      require(readVector(a_1D).slice(2) == stack2_1D)

      require(readVector(b_1D).slice(0) == slice_1D)
      require(readVector(b_1D).slice(1) == stack0_1D)
      require(readVector(b_1D).slice(2) == stack1_1D)

      // check 2D stack
      require(readVector(a_2D).slice(0) == stack0_2D)
      require(readVector(a_2D).slice(1) == stack1_2D)
      require(readVector(a_2D).slice(2) == stack2_2D)
      require(readVector(a_2D).slice(3) == stack3_2D)

      require(readVector(b_2D).slice(0) == slice_2D)
      require(readVector(b_2D).slice(1) == stack0_2D)
      require(readVector(b_2D).slice(2) == stack1_2D)
      require(readVector(b_2D).slice(3) == stack2_2D)

      // check 3D stack
      require(readVector(a_3D).slice(0) == stack0_3D)
      require(readVector(a_3D).slice(1) == stack1_3D)
      require(readVector(a_3D).slice(2) == stack2_3D)

      require(readVector(b_3D).slice(0) == slice_3D)
      require(readVector(b_3D).slice(1) == stack0_3D)
      require(readVector(b_3D).slice(2) == stack1_3D)
    }
  }
  */

  test("vector field / warp2D") {

    // For this test, we assume that warp has been tested for
    // scalar fields.
    val Rows = 4
    val Columns = 5
    val VectorLength = 3
    val VectorShape = Shape(VectorLength)

    val input = RefVectorField.random(Rows, Columns, VectorShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputField = TestVectorField(input)

      // Translation by a 0D vector field
      val vectorField0D =
        TestVectorField(RefVectorField(new Vector(0.5f, -1.5f)))
      val translated0D = warp(inputField, vectorField0D)
      val expected0D = vectorField(
        warp(vectorElement(inputField, 0), vectorField0D),
        warp(vectorElement(inputField, 1), vectorField0D),
        warp(vectorElement(inputField, 2), vectorField0D)
      )

      // Translation by a 2D vector field
      val vectorField2D =
        TestVectorField(RefVectorField.random(Rows, Columns, Shape(2)))
      val translated2D = warp(inputField, vectorField2D)
      val expected2D = vectorField(
        warp(vectorElement(inputField, 0), vectorField2D),
        warp(vectorElement(inputField, 1), vectorField2D),
        warp(vectorElement(inputField, 2), vectorField2D)
      )

      // Translation of a TensorField by a 2D vector field
      val inputTensor = RefVectorField.random(Rows, Columns, Shape(2))
      val inputTensorField = TestVectorField(inputTensor)
      val vectorField2D2 =
        TestVectorField(RefVectorField.random(Rows, Columns, Shape(2)))
      val translatedTensor2D = warp(inputTensorField, vectorField2D2)
      val expected2D2 = vectorField(
        warp(vectorElement(inputTensorField, 0), vectorField2D2),
        warp(vectorElement(inputTensorField, 1), vectorField2D2)
      )

      // Test implicit trimming that occurs when guide field is smaller than input

      val TrimmedRows = Rows - 1
      val TrimmedColumns = Columns - 2

      val trimmedGuide = trim(vectorField2D2, Shape(TrimmedRows, TrimmedColumns))
      val translatedTrimmed = warp(inputTensorField, trimmedGuide)
      val expectedTrimmed = vectorField(
        warp(vectorElement(inputTensorField, 0), trimmedGuide),
        warp(vectorElement(inputTensorField, 1), trimmedGuide)
      )
      probe(translated0D, expected0D, translated2D, expected2D, translatedTensor2D, expected2D2,
            translatedTrimmed, expectedTrimmed)
    }

    import graph._
    withRelease {
      step
      require(readVector(translated0D) ~== readVector(expected0D))
      require(readVector(translated2D) ~== readVector(expected2D))
      require(readVector(translatedTensor2D) ~== readVector(expected2D2))
      require(readVector(translatedTrimmed) ~== readVector(expectedTrimmed))
    }
  }

  test("vector field / subfield") {
    // For this test, we assume that subfield has been tested for
    // scalar fields.
    val Rows = 4
    val Columns = 5
    val VectorLength = 3
    val VectorShape = Shape(VectorLength)

    val OutputRows = 3
    val OutputCols = 3
    val OutputShape = Shape(OutputRows, OutputCols)

    val input = RefVectorField.random(Rows, Columns, VectorShape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputField = TestVectorField(input)

      // guide field is a 0D vector field
      val vectorField0D =
        TestVectorField(RefVectorField(new Vector(0.5f, -1.5f)))
      val windowed = subfield(inputField, vectorField0D, OutputShape)
      val expected = vectorField(
        subfield(vectorElement(inputField, 0), vectorField0D, OutputShape),
        subfield(vectorElement(inputField, 1), vectorField0D, OutputShape),
        subfield(vectorElement(inputField, 2), vectorField0D, OutputShape))

      // Repeat with a vector length that cannot be handled in SmallTensorAddressing mode.

      val BigTensorVectorLength = 5
      val BigVectorShape = Shape(BigTensorVectorLength)
      val bigInput = RefVectorField.random(Rows, Columns, BigVectorShape)
      val bigInputField = TestVectorField(bigInput)
      val bigWindowed = subfield(bigInputField, vectorField0D, OutputShape)
      val bigExpected = vectorField(
        subfield(vectorElement(bigInputField, 0), vectorField0D, OutputShape),
        subfield(vectorElement(bigInputField, 1), vectorField0D, OutputShape),
        subfield(vectorElement(bigInputField, 2), vectorField0D, OutputShape),
        subfield(vectorElement(bigInputField, 3), vectorField0D, OutputShape),
        subfield(vectorElement(bigInputField, 4), vectorField0D, OutputShape)
      )

      // Repeat with a 1D input field

      val OneDColumns = 7
      val oneDinput = RefVectorField.random(OneDColumns, VectorShape)
      val oneDinputField = TestVectorField(oneDinput)
      val OneDOutputCols = 5
      val oneDOutputShape = Shape(OneDOutputCols)

      val guideField0D =
        TestVectorField(RefVectorField(new Vector(2.5f)))
      val oneDWindowed = subfield(oneDinputField, guideField0D, oneDOutputShape)
      val oneDExpected0D = vectorField(
        subfield(vectorElement(oneDinputField, 0), guideField0D, oneDOutputShape),
        subfield(vectorElement(oneDinputField, 1), guideField0D, oneDOutputShape),
        subfield(vectorElement(oneDinputField, 2), guideField0D, oneDOutputShape))

      probe(windowed, expected, bigWindowed, bigExpected, oneDWindowed, oneDExpected0D)
    }

    import graph._
    withRelease {
      step
      require(readVector(windowed) ~== readVector(expected))
      require(readVector(bigWindowed) ~== readVector(bigExpected))
      require(readVector(oneDWindowed) ~== readVector(oneDExpected0D))
    }
  }

  test("vector field / subsample") {
    val m0 = Matrix(
      Array(1f, 2f, 3f, 4f),
      Array(5f, 6f, 7f, 8f),
      Array(0f, 2f, 4f, 6f)
    )
    val m1 = Matrix(
      Array(-1f, -2f, -3f, -4f),
      Array(-5f, -6f, -7f, -8f),
      Array( 0f, -2f, -4f, -6f)

    )
    val m2 = Matrix(
      Array(-11f, -12f, -13f, -14f),
      Array(-15f, -16f, -17f, -18f),
      Array( -10f, -12f, -14f, -16f)

    )
    val sub0 = Matrix(
      Array(1f, 3f),
      Array(0f, 4f)
    )
    val sub1 = Matrix(
      Array(-1f, -3f),
      Array( 0f, -4f)
    )
    val sub2 = Matrix(
      Array(-11f, -13f),
      Array( -10f, -14f)
    )
    val expect = RefVectorField(2, 2, (r, c) => new Vector(sub0(r, c), sub1(r, c)))
    val expect3 = RefVectorField(2, 2, (r, c) => new Vector(sub0(r, c), sub1(r, c), sub2(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val vField = TestVectorField(RefVectorField(3, 4, (r, c) => new Vector(m0(r, c), m1(r, c))))
      val vSub = downsample(vField, 2,0)
      val vField3 = TestVectorField(RefVectorField(3, 4, (r, c) => new Vector(m0(r, c), m1(r, c), m2(r, c))))
      val vSub3 = downsample(vField3, 2,0)
      probe(vSub, vSub3)
    }


    import graph._
    withRelease {
      step
      require(readVector(vSub) == expect)
      require(readVector(vSub3) == expect3)
    }
  }

  test("vector field / upsample") {
    val data0 = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val data1 = data0 * 2f
    val data2 = data1 * 3.5f
    val data3 = data2 * 7.1f
    val data4 = data3 * 1.9f
    val input = RefVectorField(3, 3,
                               (r, c) => new Vector(data0(r, c), data1(r, c)))
    val expectedData0 = Matrix(
      Array(
        Array( 1f, 0f, 7f, 0f, 5f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array(-2f, 0f, 0f, 0f, 4f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array( 4f, 0f,-7f, 0f, 3f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f)
      )
    )
    val expectedData1 = expectedData0 * 2f
    val expectedData2 = expectedData1 * 3.5f
    val expectedData3 = expectedData2 * 7.1f
    val expectedData4 = expectedData3 * 1.9f
    val expected = RefVectorField(6, 6,
                                  (r, c) => new Vector(expectedData0(r, c), expectedData1(r, c)))

    val input2 = RefVectorField(3, 3,
                                (r, c) => new Vector(data0(r, c), data1(r, c), data2(r,c)))
    val expected2 = RefVectorField(6, 6,
                                   (r, c) => new Vector(expectedData0(r, c), expectedData1(r, c), expectedData2(r, c)))

    val input3 = RefVectorField(3, 3,
                                (r, c) => new Vector(data0(r, c), data1(r, c), data2(r,c), data3(r,c), data4(r,c)))
    val expected3 = RefVectorField(6, 6,
                                   (r, c) => new Vector(expectedData0(r, c), expectedData1(r, c), expectedData2(r, c), expectedData3(r, c), expectedData4(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestVectorField(input)
      val outField = upsample(inField)

      // A second test where input isn't a TensorField

      val inField2 = TestVectorField(input2)
      val outField2 = upsample(inField2)

      // A third test where input is a BigTensorField
      val inField3 = TestVectorField(input3)
      val outField3 = upsample(inField3)

      probe(outField, outField2, outField3)
    }


    import graph._
    withRelease {
      step
      require(readVector(outField) == expected)
      require(readVector(outField2) == expected2)
      require(readVector(outField3) == expected3)
    }
  }

  test("vector field / supersample") {
    val data0 = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val data1 = data0 * 2f
    val input = RefVectorField(3, 3,
                               (r, c) => new Vector(data0(r, c), data1(r, c)))
    val expectedData0 = Matrix(
      Array(
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f)
      )
    )
    val expectedData1 = expectedData0 * 2f
    val expected = RefVectorField(6, 6,
                                  (r, c) => new Vector(expectedData0(r, c), expectedData1(r, c)))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestVectorField(input)
      val outField = supersample(inField)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readVector(outField) == expected)
    }
  }

  test("vector field / divergence") {
    val data0 = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val data1 = data0 * -2
    val div = data0.backwardDivergence(data1)
    val vectorField = RefVectorField(3, 3,
                                     (r, c) => new Vector(data0(r, c), data1(r, c)))

    val expectedDivergence = RefScalarField(3, 3,
                                            (r, c) => div(r, c))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestVectorField(vectorField)
      val divergence = backwardDivergence(input)
      probe(divergence)
    }

    import graph._
    withRelease {
      step
      require(readScalar(divergence) ~== expectedDivergence)
    }
  }

  test("vector field / trim") {

    val input0 = Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    )
    val input1 = Matrix(
      Array(20f, 21f, 22f, 23f, 24f),
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(30f, 31f, 32f, 33f, 34f),
      Array(10f, 11f, 12f, 13f, 14f)
    )
    val input2 = Matrix(
      Array(21f, 22f, 23f, 24f, 25f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(32f, 32f, 33f, 36f, 39f),
      Array(12f, 18f, 13f, 12f, 17f)
    )
    val expected0 = Matrix(
      Array( 0f,  1f,  2f,  3f),
      Array(10f, 11f, 12f, 13f)
    )
    val expected1 = Matrix(
      Array(20f, 21f, 22f, 23f),
      Array( 0f,  1f,  2f,  3f)
    )
    val expected2 = Matrix(
      Array(21f, 22f, 23f, 24f),
      Array(10f, 11f, 12f, 13f)
    )
    val expected = RefVectorField(2, 4, (r, c) => new Vector(expected0(r, c), expected1(r, c)))
    val outShape = Shape(2, 4)
    val vectorShape = Shape(2)

    val expectedB = RefVectorField(2, 4, (r, c) => new Vector(expected0(r, c), expected1(r, c), expected2(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestVectorField(RefVectorField(4, 5, (r, c) => new Vector(input0(r, c), input1(r, c))))
      val out = trim(in, outShape)
      val autoTrimmedOut = VectorField(outShape, vectorShape)
      autoTrimmedOut <== in

      // A second test where the field is not a tensor field (invokes different code
      // path in HyperKernel code generation.

      val inB = TestVectorField(RefVectorField(4, 5, (r, c) => new Vector(input0(r, c), input1(r, c), input2(r, c))))
      val outB = trim(inB, outShape)

      probe(out, autoTrimmedOut, outB)
    }
    import graph._
    withRelease {
      step
      require(readVector(out) == expected)
      require(readVector(autoTrimmedOut) == expected)
      require(readVector(outB) == expectedB)
    }
  }

  test("vector field / apply") {

    val input0 = Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    )
    val input1 = Matrix(
      Array(20f, 21f, 22f, 23f, 24f),
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(30f, 31f, 32f, 33f, 34f),
      Array(10f, 11f, 12f, 13f, 14f)
    )
    val input2 = Matrix(
      Array(21f, 22f, 23f, 24f, 25f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(32f, 32f, 33f, 36f, 39f),
      Array(12f, 18f, 13f, 12f, 17f)
    )
    val input3 = Matrix(
      Array(32f, 32f, 33f, 36f, 39f),
      Array(21f, 22f, 23f, 24f, 25f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(12f, 18f, 13f, 12f, 17f)
    )
    val input4 = Matrix(
      Array(21f, 22f, 23f, 24f, 25f),
      Array(32f, 32f, 33f, 36f, 39f),
      Array(12f, 18f, 13f, 12f, 17f),
      Array(10f, 11f, 12f, 13f, 14f)
    )

    val expected0 = Matrix(
      Array(12f, 13f),
      Array(22f, 23f)
    )
    val expected1 = Matrix(
      Array( 2f, 3f),
      Array(32f, 33f)
    )
    val expected2 = Matrix(
      Array(12f, 13f),
      Array(33f, 36f)
    )
    val expected3 = Matrix(
      Array(23f, 24f),
      Array(12f, 13f)
    )
    val expected4 = Matrix(
      Array(33f, 36f),
      Array(13f, 12f)
    )

    val expectedShort = RefVectorField(2, 2, (r, c) => new Vector(expected0(r, c), expected1(r, c), expected2(r,c)))
    val expectedLong = RefVectorField(2, 2, (r, c) => new Vector(expected0(r, c), expected1(r, c), expected2(r,c), expected3(r,c), expected4(r,c)))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inShort = TestVectorField(RefVectorField(4, 5, (r, c) => new Vector(input0(r, c), input1(r, c), input2(r,c))))
      val outShort = inShort(1 to 2, 2 to 3)
      val inLong = TestVectorField(RefVectorField(4, 5, (r, c) => new Vector(input0(r, c), input1(r, c), input2(r,c), input3(r,c), input4(r,c))))
      val outLong = inLong(1 to 2, 2 to 3)

      probe(outShort, outLong)
    }
    import graph._
    withRelease {
      step
      require(readVector(outShort) == expectedShort)

      readVector(outLong).print
      require(readVector(outLong) == expectedLong)
    }
  }

/*
  test("vector field / multiplex") {
    val Rows = 11
    val Columns = 13
    val VectorShape = Shape(5)

    val field0 = RefVectorField.random(Rows, Columns, VectorShape)
    val field1 = RefVectorField.random(Rows, Columns, VectorShape)
    val field2 = RefVectorField.random(Rows, Columns, VectorShape)
    val field3 = RefVectorField.random(Rows, Columns, VectorShape)
    val fields = Array(field0, field1, field2, field3)


    val graph = new ComputeGraph(Optimize) with RefTestInterface2 {
      val fieldArray = Array.tabulate(fields.length) {
        i => TestVectorField(fields(i))
      }

      val indexField = TestScalarField(RefScalarField(2f))

      val outArray = Array.tabulate(fields.length) {
        i => TestVectorField(fields(i) * 2)
      }

      // Test selection (reading)
      val selectArray = fieldArray.select(indexField)

      // Test writing
      outArray.insert(indexField) <== field0

    }

    import graph._
    withRelease {
      step

      require(readVector(selectArray) == field2)
      require(readVector(outArray(0)) == field0 * 2)
      require(readVector(outArray(1)) == field1 * 2)
      require(readVector(outArray(2)) == field0)
      require(readVector(outArray(3)) == field3 * 2)
    }
  }

  test("vector field / multiplex 2D") {
    val Rows = 11
    val Columns = 13
    val VectorShape = Shape(5)

    val field0 = RefVectorField.random(Rows, Columns, VectorShape)
    val field1 = RefVectorField.random(Rows, Columns, VectorShape)
    val field2 = RefVectorField.random(Rows, Columns, VectorShape)
    val field3 = RefVectorField.random(Rows, Columns, VectorShape)
    val field4 = RefVectorField.random(Rows, Columns, VectorShape)
    val field5 = RefVectorField.random(Rows, Columns, VectorShape)

    val fields = Array(
      Array(field0, field1, field2),
      Array(field3, field4, field5)
    )
    val fieldsRows = fields.length
    val fieldsColumns = fields(0).length


    val graph = new ComputeGraph(Optimize) with RefTestInterface2 {
      val fieldArray = Array.tabulate(fields.length, fields(0).length) {
        (i, j) => TestVectorField(fields(i)(j))
      }

      val indexField1 = TestScalarField(RefScalarField(1f))
      val indexField2 = TestScalarField(RefScalarField(0f))

      val outArray = Array.tabulate(fieldsRows, fieldsColumns) {
        (i, j) => TestVectorField(fields(i)(j) * 2)
      }

      // Test selection (reading)
      val selectArray = fieldArray.select(indexField1, indexField2)

      // Test writing
      outArray.insert(indexField1, indexField2) <== field0

    }
    import graph._
    withRelease {
      step

      require(readVector(selectArray) == field3)
      require(readVector(outArray(0)(0)) == field0 * 2)
      require(readVector(outArray(0)(1)) == field1 * 2)
      require(readVector(outArray(0)(2)) == field2 * 2)
      require(readVector(outArray(1)(0)) == field0)
      require(readVector(outArray(1)(1)) == field4 * 2)
      require(readVector(outArray(1)(2)) == field5 * 2)
    }
  }
  */
  test("vector field / non max supression") {
    val Rows = 4
    val Cols = 5

    // hit all 3 cases: border, corner, interior
    /*
    val input0 = Matrix(
      Array( 0f,  1f, 13f, 15f,  4f),
      Array(10f, 22f, 12f, 13f, 14f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array(30f, 16f, 19f, 33f, 34f)
    )
    val input1 = Matrix(
      Array(10f, 22f, 12f, 13f, 14f),
      Array(30f, 16f, 19f, 33f, 34f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array( 0f,  1f, 13f, 15f,  4f)
    )
    val input2 = Matrix(
      Array(10f, 22f, 12f, 13f, 14f),
      Array( 0f,  1f, 13f, 15f,  4f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array(30f, 16f, 19f, 33f, 34f)
    )
    val input3 = Matrix(
      Array(10f, 22f, 12f, 13f, 14f),
      Array( 0f, 22f, 13f, 15f,  4f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array(30f, 16f, 19f, 33f, 34f)
    )
    val input4= Matrix(
      Array(10f, 22f, 12f, 13f, 14f),
      Array( 0f,  1f, 13f, 15f,  4f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array(30f, 16f, 33f, 33f, 34f)
    )
    */

    val expectedOut0 = Matrix(
      Array( 0f,  0f,  0f, 15f,  0f),
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f,  0f,  0f, 34f)
    )
    val expectedOut1 = Matrix(
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f,  0f,  0f, 34f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f)
    )
    val expectedOut2 = Matrix(
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f,  0f,  0f, 34f)
    )
    val expectedOut3 = Matrix(
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f,  0f,  0f, 34f)
    )
    val expectedOut4 = Matrix(
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f, 33f,  0f, 34f)
    )

    val expectedOutput = RefVectorField(Rows, Cols,
          (r, c) => new Vector(expectedOut0(r, c),
                               expectedOut1(r, c), expectedOut2(r, c),
                               expectedOut3(r, c), expectedOut4(r, c)))

    val expectedOutput2 = RefVectorField(Rows, Cols,
          (r, c) => new Vector(expectedOut0(r, c),
                               expectedOut1(r, c), expectedOut2(r, c),
                               expectedOut3(r, c)))

   val graph = new ComputeGraph(Optimize) with RefTestInterface {
     val in = TestVectorField(expectedOutput)
     val out = nonMaximumSuppression(in)
     val in2 = TestVectorField(expectedOutput2)
     val out2 = nonMaximumSuppression(in2)

     probe(out, out2)
    }

    import graph._
    withRelease {
      step
      require(readVector(out) == expectedOutput)
      require(readVector(out2) == expectedOutput2)
    }
  }

  test("vector field / local max position") {
    val neighborhood = Matrix(
      Array(
        Array(1f, 1f, 0f),
        Array(1f, 1f, 1f),
        Array(0f, 1f, 1f)
      )
    )
    val input = RefScalarField(Matrix(
      Array(1.0f, 1.1f, 0.0f, 2.1f, 3.3f),
      Array(0.0f, 5.0f, 3.0f, 1.2f, 6.0f),
      Array(1.0f, 2.0f, 0.0f, 2.1f, 0.0f),
      Array(1.1f, 1.2f, 1.0f, 2.0f, 2.3f),
      Array(3.0f, 1.1f, 4.0f, 3.0f, 1.0f)
    ))
    val expectedOutRow = Matrix(
      Array( 1f,  1f,  1f,  1f,  1f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array( 0f, -1f, -1f, -1f, -1f),
      Array( 1f,  1f,  1f,  1f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f)
    )
    val expectedOutCol = Matrix(
      Array( 1f,  0f,  0f,  1f,  0f),
      Array( 1f,  0f, -1f,  1f,  0f),
      Array( 1f,  0f, -1f, -1f,  0f),
      Array( 0f,  1f,  0f,  0f,  0f),
      Array( 0f,  1f,  0f, -1f, -1f)
    )
    val expectedOutput = RefVectorField(5, 5,
                                        (r, c) => new Vector(expectedOutRow(r, c), expectedOutCol(r, c))
                                      )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = localMaxPosition(inField, neighborhood)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readVector(outField) == expectedOutput)
    }
  }

  test("vector field / local min position") {
    val neighborhood = Matrix(
      Array(
        Array(1f, 1f, 0f),
        Array(1f, 1f, 1f),
        Array(0f, 1f, 1f)
      )
    )
    val input = RefScalarField(Matrix(
      Array(1.0f, 1.1f, 0.0f, 2.1f, 3.3f),
      Array(0.0f, 5.0f, 3.0f, 1.2f, 6.0f),
      Array(1.0f, 2.0f, 0.1f, 2.1f, 0.2f),
      Array(1.1f, 1.2f, 1.0f, 2.0f, 2.3f),
      Array(3.0f, 1.3f, 4.0f, 3.0f, 0.9f)
    ))
    val expectedOutRow = Matrix(
      Array( 1f,  0f,  0f,  0f,  0f),
      Array( 0f,  0f, -1f, -1f,  1f),
      Array(-1f, -1f,  0f,  0f,  0f),
      Array(-1f, -1f, -1f, -1f, -1f),
      Array(-1f, -1f, -1f,  0f,  0f)
    )
    val expectedOutCol = Matrix(
      Array( 0f,  1f,  0f, -1f, -1f),
      Array( 0f, -1f,  0f, -1f,  0f),
      Array( 0f, -1f,  0f, -1f,  0f),
      Array( 0f, -1f,  0f, -1f,  0f),
      Array( 0f, -1f,  0f,  1f,  0f)
    )
    val expectedOutput = RefVectorField(5, 5,
                                        (r, c) => new Vector(expectedOutRow(r, c), expectedOutCol(r, c))
                                      )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = localMinPosition(inField, neighborhood)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readVector(outField) == expectedOutput)
    }
  }


  /** Test the subfields operator on a 2D MatrixField producing a field
    * of a smaller shape based on using only valid subfields.
    */
  test("vector field / small subfields") {
    val SubfieldSize = 3

    // Input scalar field
    val input = Matrix(
      Array(1f, 2f, 3f, 4f),
      Array(5f, 6f, 7f, 8f),
      Array(9f, 8f, 7f, 6f),
      Array(3f, 4f, 5f, 6f),
      Array(2f, 4f, 6f, 8f)
    )

    // Expected output subfields (layers of vector field)
    val out00 = Matrix(
      Array(1f, 2f, 3f),
      Array(5f, 6f, 7f),
      Array(9f, 8f, 7f)
    )
    val out01 = Matrix(
      Array(2f, 3f, 4f),
      Array(6f, 7f, 8f),
      Array(8f, 7f, 6f)
    )
    val out10 = Matrix(
      Array(5f, 6f, 7f),
      Array(9f, 8f, 7f),
      Array(3f, 4f, 5f)
    )
    val out11 = Matrix(
      Array(6f, 7f, 8f),
      Array(8f, 7f, 6f),
      Array(4f, 5f, 6f)
    )
    val out20 = Matrix(
      Array(9f, 8f, 7f),
      Array(3f, 4f, 5f),
      Array(2f, 4f, 6f)
    )
    val out21 = Matrix(
      Array(8f, 7f, 6f),
      Array(4f, 5f, 6f),
      Array(4f, 6f, 8f)
    )

    val expectedOutput = RefVectorField(SubfieldSize, SubfieldSize,
      (r, c) => new Vector(
        out00(r, c), out01(r, c),
        out10(r, c), out11(r, c),
        out20(r, c), out21(r, c)
      )
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(input)
      val out = subfields(in, SubfieldSize)

      probe(out)
    }

    import graph._
    withRelease {
      step
      require(readVector(out) == expectedOutput)
    }
  }

  /** Test the subfields operator on a 2D MatrixField producing a field
    * of a smaller shape based on using only valid subfields.
    */
  test("vector field / big subfields") {
    val Rows = 19
    val Columns = 21
    val SubfieldSize = 15
    val SubfieldRows = Rows - SubfieldSize + 1
    val SubfieldColumns = Columns - SubfieldSize + 1
    val Subfields = SubfieldRows * SubfieldColumns

    // Input scalar field
    val input = Matrix.random(Rows, Columns)

    // Subfields
    val subfieldsx = Array.tabulate(SubfieldRows, SubfieldColumns) {
      (r, c) =>
        new Matrix(SubfieldSize, SubfieldSize) {
          for (row <- 0 until rows; col <- 0 until columns) {
            this(row, col) = input(row + r, col + c)
          }
        }
    }
    val layers = new Array[Matrix](Subfields)

      var index = 0
      for (r <- 0 until SubfieldRows; c <- 0 until SubfieldColumns) {
        layers(index) = subfieldsx(r)(c)
        index += 1
      }


    // Subfields stacked into vector field (expected output)
    val expectedOut = RefVectorField(SubfieldSize, SubfieldSize,
      (r, c) => {
        val v = new Vector(Subfields)
        for (i <- 0 until Subfields)
          v(i) = layers(i)(r, c)
        v
      }
    )


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(input)
      val out = subfields(in, SubfieldSize)

      probe(out)
    }

    import graph._
    withRelease {
      step
      require(readVector(out) == expectedOut)
    }
  }
}
