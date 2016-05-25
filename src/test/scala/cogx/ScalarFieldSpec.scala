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

import cogx.api.{CogFunctionAPI, ImplicitConversions}

/** Test code for ScalarFields.
 */

@RunWith(classOf[JUnitRunner])
class ScalarFieldSpec
  extends FunSuite
  with MustMatchers
  with ImplicitConversions
  with ScalarFieldBuilderInterface
  with ComplexFieldBuilderInterface
  with MatrixFieldBuilderInterface
  with VectorFieldBuilderInterface
  with CogFunctionAPI
{
  val Optimize = true

  test("scalar field / constant") {
    val Size = 5
    val C = 1.234f
    val D = 3.579f
    val E = -5.123f
    val F = 7.384f
    val initField = RefScalarField.random(Size, Size)
    // Tests that involve equality should involve a threshold that matches
    // at least one element.  Here are some elements:
    val elem1 = initField.read(0, 0)
    val elem2 = initField.read(Size - 1, 0)
    val elem3 = initField.read(Size - 1, Size-1)
    val elem4 = initField.read(0, Size - 1)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(initField)
      val sum = a + C
      val sum2 = a + D + C
      val diff = a - C
      val product = a * C
      val quotient = a / C
      val mod = a % C
      val powa = pow(a, C)
      val powna = pow(a, 5)

      val greater = a > 0.5f
      val greaterEq = a >= elem1
      val less = a < 0.5f
      val lessEq = a <= elem2
      val equal3 = a === elem3
      val nEq3 = a !=== elem4

      val constSum = E + a
      val constSum2 = F + a + E
      val constDiff = E - a
      val constProduct = E * a
      val constQuotient = E / a

      probe(a, sum, sum2, diff, product, quotient, mod, powa, powna, greater, greaterEq, less, lessEq,
        equal3, nEq3, constSum, constSum2, constDiff, constProduct, constQuotient)
    }
    import graph._
    withRelease {
      step

      require(readScalar(sum) == readScalar(graph.a) + C)
      require(readScalar(sum2) == readScalar(graph.a) + D + C)
      require(readScalar(diff) == readScalar(graph.a) - C)
      require(readScalar(product) == readScalar(graph.a) * C)
      require(readScalar(quotient) ~== readScalar(graph.a) / C)
      require(readScalar(mod) ~== readScalar(graph.a) % C)
      require(readScalar(powa) ~== readScalar(graph.a).map(e => math.pow(e, C).toFloat))
      require(readScalar(powna) ~== readScalar(graph.a).map(e => math.pow(e, 5.0).toFloat))
      require(readScalar(greater) == readScalar(graph.a).map(e => if (e > 0.5f) 1f else 0f))
      require(readScalar(greaterEq) == readScalar(graph.a).map(e => if (e >= elem1) 1f else 0f))
      require(readScalar(less) == readScalar(graph.a).map(e => if (e < 0.5f) 1f else 0f))
      require(readScalar(lessEq) == readScalar(graph.a).map(e => if (e <= elem2) 1f else 0f))
      require(readScalar(equal3) == readScalar(graph.a).map(e => if (e == elem3) 1f else 0f))
      require(readScalar(nEq3) == readScalar(graph.a).map(e => if (e != elem4) 1f else 0f))
      require(readScalar(constSum) == readScalar(graph.a) + E)
      require(readScalar(constSum2) == readScalar(graph.a) + F + E)
      require(readScalar(constDiff) == -readScalar(graph.a) + E)
      require(readScalar(constProduct) == readScalar(graph.a) * E)
      require(readScalar(constQuotient) ~== readScalar(graph.a).map(E / _))
    }
  }

  /** Test the expand operator. */
  test("scalar field / expand border") {
    val image1 = RefScalarField(Matrix(
      Array(1f, 2f),
      Array(3f, 4f)
    ))
    val image2 = RefScalarField(Matrix(
      Array(1f, 2f, 2f, 1f),
      Array(3f, 4f, 4f, 3f),
      Array(3f, 4f, 4f, 3f),
      Array(1f, 2f, 2f, 1f)
    ))
    val image2ZeroFill = RefScalarField(Matrix(
      Array(1f, 2f, 0f, 0f),
      Array(3f, 4f, 0f, 0f),
      Array(0f, 0f, 0f, 0f),
      Array(0f, 0f, 0f, 0f)
    ))

    val image3 = RefScalarField(Matrix(
      Array(11f, 12f, 13f, 14f, 15f),
      Array(21f, 22f, 23f, 24f, 25f),
      Array(31f, 32f, 33f, 34f, 35f),
      Array(41f, 42f, 43f, 44f, 45f),
      Array(51f, 52f, 53f, 54f, 55f),
      Array(61f, 62f, 63f, 64f, 65f)
    ))

    val image3CyclicFill = RefScalarField(Matrix(
      Array(11f, 12f, 13f, 14f, 15f, 11f, 12f, 14f, 15f),
      Array(21f, 22f, 23f, 24f, 25f, 21f, 22f, 24f, 25f),
      Array(31f, 32f, 33f, 34f, 35f, 31f, 32f, 34f, 35f),
      Array(41f, 42f, 43f, 44f, 45f, 41f, 42f, 44f, 45f),
      Array(51f, 52f, 53f, 54f, 55f, 51f, 52f, 54f, 55f),
      Array(61f, 62f, 63f, 64f, 65f, 61f, 62f, 64f, 65f),
      Array(11f, 12f, 13f, 14f, 15f, 11f, 12f, 14f, 15f),
      Array(21f, 22f, 23f, 24f, 25f, 21f, 22f, 24f, 25f),
      Array(51f, 52f, 53f, 54f, 55f, 51f, 52f, 54f, 55f),
      Array(61f, 62f, 63f, 64f, 65f, 61f, 62f, 64f, 65f)
    ))

    // 1D tests sliced out of the above data

    val image1_1D =  image1.slice(0)
    val image2_1D = image2.slice(0)
    val image2ZeroFill_1D = image2ZeroFill.slice(0)

    val image3_1D = image3.slice(4)
    val image3CyclicFill_1D = image3CyclicFill.slice(4)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(image1)
      val in3 = TestScalarField(image3)
      val expanded = expand(in, BorderClamp, 4, 4)
      val expandedZeroFill = expand(in, BorderZero, 4, 4)
      val expandedCyclicFill = expand(in3, BorderCyclic, 10, 9)

      // 1D tests

      val in_1D = TestScalarField(image1_1D)
      val in3_1D = TestScalarField(image3_1D)
      val expanded_1D = expand(in_1D, BorderClamp, 4)
      val expandedZeroFill_1D = expand(in_1D, BorderZero, 4)
      val expandedCyclicFill_1D = expand(in3_1D, BorderCyclic, 9)

      probe(expanded, expandedZeroFill, expandedCyclicFill,
        expanded_1D, expandedZeroFill_1D, expandedCyclicFill_1D)
    }

    import graph._
    withRelease {
      step

      require(readScalar(expanded) == image2)
      require(readScalar(expandedZeroFill) == image2ZeroFill)
      require(readScalar(expandedCyclicFill) == image3CyclicFill)

      require(readScalar(expanded_1D) == image2_1D)
      require(readScalar(expandedZeroFill_1D) == image2ZeroFill_1D)
      require(readScalar(expandedCyclicFill_1D) == image3CyclicFill_1D)
    }
  }

  /** Test combining a DynamicScalarField with a static ScalarField.
    *
    * In Cog 3.X, a ScalarField was a different concept from a
    * DynamicScalarField with no mutator.  Since in Cog 4.X there is no
    * distinction, this test should be deleted or merged with the dynamic/dynamic
    * test.   XXX -RJC
    */
  test("scalar field / constant field") {
    val Size = 5
    val refC = RefScalarField.random(Size, Size)
    val refD = RefScalarField.random(Size, Size)
    val refInitField = RefScalarField.random(Size, Size)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val C = TestScalarField(refC)
      val D = TestScalarField(refD)
      val initField = TestScalarField(refInitField)  // A "scalarfield"
      val a = TestScalarField(refInitField)          // A "dynamic scalar field" with no mutator
      val sum = a + C
      val sum2 = (a + C) + C
      val sum3 = (a + C) + D
      val diff = a - C
      val eq1 = a === initField
      val eq2 = a === (initField - 1f)
      val nEq1 = a !=== initField
      val nEq2 = a !=== (initField - 1f)

      probe(a, sum, sum2, sum3, diff, eq1, eq2, nEq1, nEq2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(sum) == readScalar(graph.a) + refC)
      require(readScalar(sum2) == readScalar(graph.a) + refC + refC)
      require(readScalar(sum3) == readScalar(graph.a) + refC + refD)
      require(readScalar(diff) == readScalar(graph.a) - refC)
      require(readScalar(eq1) == readScalar(graph.a).combine(refInitField, (a, b) => if (a == b) 1f else 0f))
      require(readScalar(eq2) == readScalar(graph.a).combine(refInitField, (a, b) => if (a == (b - 1f)) 1f else 0f))
      require(readScalar(nEq1) == readScalar(graph.a).combine(refInitField, (a, b) => if (a != b) 1f else 0f))
      require(readScalar(nEq2) == readScalar(graph.a).combine(refInitField, (a, b) => if (a != (b - 1f)) 1f else 0f))
    }
  }

  /** Test combining two dynamic scalar fields. */
  test("scalar field / field") {
    val Size = 5
    val Constant = 3.456f

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size))
      val b = TestScalarField(RefScalarField.random(Size, Size))
      val d = TestScalarField(RefScalarField.random(Size, Size))

      val sum = a + b
      val diff = a - b
      val product = a * b
      val quotient = a / b
      val mod = a % b
      val multiPoint = a * d + b

      /** A 0-dimensional scalar field, can be combined with any other. */
      val c = TestScalarField(RefScalarField(Constant))

      val sum0D = a + c
      val diff0D = a - c
      val product0D = a * c
      val quotient0D = a / c
      val mod0D = a % c

      /** The 0D scalar field argument can appear first */
      val sum0D2 = c + a
      val diff0D2 = c - a
      val product0D2 = c * a
      val quotient0D2 = c / a

      val greater = a > b
      val greaterEq = a >= b
      val less = a < b
      val lessEq = a <= b
      val eq1 = a === a
      val eq2 = a === b
      val nEq1 = a !=== a
      val nEq2 = a !=== b

      probe(a, sum, diff, product, quotient, mod, multiPoint, d, sum0D, diff0D, product0D, quotient0D, mod0D, sum0D2,
        diff0D2, product0D2, quotient0D2, b, greater, greaterEq, less, lessEq, eq1, eq2, nEq1, nEq2)
    }

    import graph._
    withRelease {
      step

      require(readScalar(sum) == (readScalar(graph.a) + readScalar(b)))
      require(readScalar(diff) == (readScalar(graph.a) - readScalar(b)))
      require(readScalar(product) == (readScalar(graph.a) :* readScalar(b)))
      require(readScalar(quotient) ~== (readScalar(graph.a) :/ readScalar(b)))
      require(readScalar(mod) ~== (readScalar(graph.a) % readScalar(b)))
      require(readScalar(multiPoint) ~== ((readScalar(graph.a) :* readScalar(d)) + readScalar(b)))

      require(readScalar(sum0D) == (readScalar(graph.a) + Constant))
      require(readScalar(diff0D) == (readScalar(graph.a) - Constant))
      require(readScalar(product0D) == (readScalar(graph.a) * Constant))
      require(readScalar(quotient0D) ~== (readScalar(graph.a) / Constant))
      require(readScalar(mod0D) ~== (readScalar(graph.a) % Constant))

      require(readScalar(sum0D2) == (readScalar(graph.a) + Constant))
      require(readScalar(diff0D2) == (readScalar(graph.a)*(-1f) + Constant))
      require(readScalar(product0D2) == (readScalar(graph.a) * Constant))
      require(readScalar(quotient0D2) ~== (readScalar(graph.a).reciprocal * Constant))

      val aa = readScalar(graph.a)
      val bb = readScalar(b)
      require(readScalar(greater) == aa.combine(bb, (x, y) => if (x > y) 1f else 0f))
      require(readScalar(greaterEq) == aa.combine(bb, (x, y) => if (x >= y) 1f else 0f))
      require(readScalar(less) == aa.combine(bb, (x, y) => if (x < y) 1f else 0f))
      require(readScalar(lessEq) == aa.combine(bb, (x, y) => if (x <= y) 1f else 0f))
      require(readScalar(eq1) == aa.combine(aa, (x, y) => if (x == y) 1f else 0f))
      require(readScalar(eq2) == aa.combine(bb, (x, y) => if (x == y) 1f else 0f))
      require(readScalar(nEq1) == aa.combine(aa, (x, y) => if (x != y) 1f else 0f))
      require(readScalar(nEq2) == aa.combine(bb, (x, y) => if (x != y) 1f else 0f))
    }
  }

  /** Test applying unary operations on dynamic scalar fields. */
  test("scalar field / unary") {
    val Size = 5
    val Size1D = Size*Size
    val uniformVal = 1.0f
    val OneDSize = 1321
    val ThreeDShape = Shape(3,300,7)     // Layers,Rows,Columns

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val b = TestScalarField(RefScalarField.random(Size, Size) + 0.5f)
      val uniform = TestScalarField(RefScalarField(Size, Size) + uniformVal)
      // downRamp values are -1, -2, -3, ... so winnerTakeAll should have
      // at 1 at the 0th position (unless padding 0's are mistakenly considered)
      val downRamp = TestScalarField(RefScalarField(Size, (r) => -1f - r))
      val zeros = TestScalarField(RefScalarField(Size, Size))
      val ones = TestScalarField(RefScalarField(Size, Size) + 1.0f)
      val aAbs = abs(a)
      val aAcos = acos(a)
      val aAsin = asin(a)
      val aCos = cos(a)
      val aCosh = cosh(a)
      val aExp = exp(a)
      val bLog = log(b)
      val bNormalizeL1 = normalizeL1(b)
      val bNormalizeL2 = normalizeL2(b)
      // The following approach puts uniformVal into the padding as well, so as
      // to prove that normalizeL1 and normalizeL2 properly ignore it.
      val normalizeL1_uniform = normalizeL1(zeros+uniformVal)
      val normalizeL2_uniform = normalizeL2(zeros+uniformVal)
      val aSignum = signum(a)
      val aSin = sin(a)
      val aSinh = sinh(a)
      val aSq = sq(a)
      val bSqrt = sqrt(b)
      val aTan = tan(a)
      val aTanh = tanh(a)
      val aWinnerTakeAll = winnerTakeAll(a)
      val aWinnerTakeAllTake2 = winnerTakeAll(downRamp)
      val aNegative = -a

      val aDownsample = downsample(a)
      val aUpsample = upsample(a)

      //    1D Tests
      val a1D = TestScalarField(RefScalarField.random(Size1D) - 0.5f)

      val downsample1D = downsample(a1D)

      // Tests of different dimensions
      val input1D = TestScalarField(RefScalarField.random(OneDSize) - 0.5f)
      val WTA1D = winnerTakeAll(input1D)

      val input3D = TestScalarField(RefScalarField.random(ThreeDShape.toArray.toSeq: _*) - 0.5f)
      val WTA3D = winnerTakeAll(input3D)
      probe(a, aAbs, aAcos, aAsin, aCos, aCosh, aExp, bLog, bNormalizeL1, bNormalizeL2, b, normalizeL1_uniform,
        b, normalizeL2_uniform, uniform, aSignum, aSin, aSinh, aSq, bSqrt, aTan, aTanh, aWinnerTakeAll, WTA1D, input1D, WTA3D,
        input3D, aWinnerTakeAllTake2, aNegative, aDownsample, downsample1D, a1D, aUpsample)
    }
    //val median = a.median

    import graph._
    withRelease {
      step

      require(readScalar(aAbs) == readScalar(graph.a).map(_.abs))
      require(readScalar(aAcos) ~== readScalar(graph.a).map(e => math.acos(e).toFloat))
      require(readScalar(aAsin) ~== readScalar(graph.a).map(e => math.asin(e).toFloat))
      require(readScalar(aCos) ~== readScalar(graph.a).map(e => math.cos(e).toFloat))
      require(readScalar(aCosh) ~== readScalar(graph.a).map(e => math.cosh(e).toFloat))
      require(readScalar(aExp) ~== readScalar(graph.a).map(e => math.exp(e).toFloat))
      require(readScalar(bLog) ~== readScalar(b).map(e => math.log(e).toFloat))
      require(readScalar(bNormalizeL1) ~== readScalar(b) / readScalar(b).reduce(_ + _))
      require(readScalar(bNormalizeL2) ~==
              readScalar(b) / readScalar(b).map(x => x * x).reduce(_ + _))
      require(readScalar(normalizeL1_uniform) ~== readScalar(uniform) / (Size * Size * uniformVal))
      require(readScalar(normalizeL2_uniform) ~==
              readScalar(uniform) / (Size * Size * uniformVal * uniformVal))
      require(readScalar(aSignum) == readScalar(graph.a).map(e =>
        if (e < 0) -1f else if (e > 0) 1f else 0f))
      require(readScalar(aSin) ~== readScalar(graph.a).map(e => math.sin(e).toFloat))
      require(readScalar(aSinh) ~== readScalar(graph.a).map(e => math.sinh(e).toFloat))
      require(readScalar(aSq) == readScalar(graph.a).map(e => e * e))
      require(readScalar(bSqrt) ~== readScalar(graph.b).map(e => math.sqrt(e).toFloat))
      require(readScalar(aTan) ~== readScalar(graph.a).map(e => math.tan(e).toFloat))
      require(readScalar(aTanh) ~== readScalar(graph.a).map(e => math.tanh(e).toFloat))
      require(readScalar(aWinnerTakeAll) == readScalar(graph.a).winnerTakeAll)
      require(readScalar(WTA1D) == readScalar(input1D).winnerTakeAll)
      require(readScalar(WTA3D) == readScalar(input3D).winnerTakeAll)
      for (i <- 0 until Size)
        require(readScalar(aWinnerTakeAllTake2).read(i) == (if (i == 0) 1f else 0f))
      require(readScalar(aNegative) == readScalar(graph.a) * -1f)
      require(readScalar(aDownsample) == readScalar(graph.a).downsample())
      require(readScalar(downsample1D) == readScalar(a1D).downsample())
      require(readScalar(aUpsample) == readScalar(graph.a).upsample())
    }
  }

  /** Test combinations of two dynamic scalar fields. */
  test("scalar field / binary") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val b = TestScalarField(RefScalarField.random(Size, Size) + 0.5f)
      val maxab = max(a, b)
      val minab = min(a, b)
      val dotProd = dot(a, b)
      val aAtan2 = atan2(a, b)

      probe(a, maxab, b, minab, dotProd, aAtan2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(maxab) == readScalar(graph.a).combine(readScalar(b), (a, b) => a max b))
      require(readScalar(minab) == readScalar(graph.a).combine(readScalar(b), (a, b) => a min b))
      require(readScalar(dotProd) == (readScalar(graph.a) dot readScalar(b)))
      require(readScalar(aAtan2) ~== readScalar(graph.a).combine(readScalar(b),
        (a, b) => math.atan2(a, b).toFloat))
    }
  }

  /** Test combinations of a dynamic scalar fields and a constant. */
  test("scalar field / binary constant") {
    val Size = 5
    val b = 0.5f
    val bField = RefScalarField(Size, Size) + b
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val maxab = max(a, b)
      val minab = min(a, b)

      probe(a, maxab, minab)
    }
    import graph._
    withRelease {
      step
      require(readScalar(maxab) == readScalar(graph.a).combine(bField, (a, b) => a max b))
      require(readScalar(minab) == readScalar(graph.a).combine(bField, (a, b) => a min b))
    }
  }

  /** Test reductions of dynamic scalar field to a 0-D dynamic scalar field. */
  test("scalar field / reductions") {
    val Size = 25
    val BigFieldElementVal = 1.001f
    val bigSize =  100000
    val OneDSize = 1321
    val ThreeDShape = Shape(3,300,7)     // Layers,Rows,Columns

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val maxa = fieldReduceMax(a)
      val mina = fieldReduceMin(a)
      val sum = fieldReduceSum(a)
      val median = fieldReduceMedian(a)
      val bigField = TestScalarField(bigSize)
      val bigFieldSum = fieldReduceSum(bigField + BigFieldElementVal)

      // Tests of different dimensions
      val input0D = TestScalarField(RefScalarField.random() - 0.5f)
      val zeroD = fieldReduceSum(input0D)
      val zeroDMedian = fieldReduceMedian(input0D)
      val input1D = TestScalarField(RefScalarField.random(OneDSize) - 0.5f)
      val oneD = fieldReduceSum(input1D)
      val input3D = TestScalarField(RefScalarField.random(ThreeDShape.toArray.toSeq: _*) - 0.5f)
      val threeD = fieldReduceSum(input3D)

      probe(a, maxa, mina, sum, bigFieldSum, median, zeroD, input0D, zeroDMedian, oneD, input1D, threeD, input3D)
    }

    def calcMedian(a: RefScalarField): Float = {
      val unsortedArray: Array[Float] = a.data.toArray.map(_.read(0))
      require(unsortedArray.length > 0, "Invalid array for median calculation, length = " + unsortedArray.length)
      val sortedArray = unsortedArray.sortWith((e1,e2) => e1 < e2)
      val midpoint = unsortedArray.length/2        // = the higher-indexed of two middle elements for an even length array
      if (unsortedArray.length % 2 == 1) {
        sortedArray(midpoint)
      } else {
        (sortedArray(midpoint) + sortedArray(midpoint-1))/2.0f
      }
    }

    import graph._
    withRelease {
      step

      require(readScalar(maxa).read() == readScalar(graph.a).reduce(_ max _))
      require(readScalar(mina).read() == readScalar(graph.a).reduce(_ min _))
      require(readScalar(sum).read() ~== readScalar(graph.a).reduce(_ + _),
        "Expecting " + readScalar(graph.a).reduce(_ + _) + ", saw " + readScalar(sum).read())
      require(readScalar(bigFieldSum).read() == BigFieldElementVal * bigSize,
        "Expecting " + BigFieldElementVal * bigSize.toFloat + ", saw " + readScalar(bigFieldSum).read())
      require(readScalar(median).read() == calcMedian(readScalar(graph.a)))
      require(readScalar(zeroD).read() ~== readScalar(input0D).read(),
        "Expecting " + readScalar(input0D).read() + ", saw " + readScalar(zeroD).read())
      require(readScalar(zeroDMedian).read() ~== readScalar(input0D).read(),
        "Expecting " + readScalar(input0D).read() + ", saw " + readScalar(zeroDMedian).read())
      require(readScalar(oneD).read() ~== readScalar(input1D).reduce(_ + _),
        "Expecting " + readScalar(input1D).reduce(_ + _) + ", saw " + readScalar(oneD).read())
      require(readScalar(threeD).read() ~== readScalar(input3D).reduce(_ + _),
        "Expecting " + readScalar(input3D).reduce(_ + _) + ", saw " + readScalar(threeD).read())
    }
  }

  /** Test adding a complex constant to a dynamic scalar field. */
  test("scalar field / complex constant") {
    val C = Complex(1.2f, 3.4f)
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val sum = a + C
      val diff = a - C
      val product = a * C

      probe(a, sum, diff, product)
    }

    import graph._
    withRelease {
      step
      require(readComplex(sum) ~== (RefComplexField(readScalar(graph.a)) + C))
      require(readComplex(diff) ~== (RefComplexField(readScalar(graph.a)) - C))
      require(readComplex(product) ~== (RefComplexField(readScalar(graph.a)) * C))
    }
  }

  test("scalar field / shift") {
    val vector = new Vector(Array[Float](1, 2, 3, 4))
    val vectorShiftLeft = new Vector(Array[Float](2, 3, 4, 0))
    val vectorShiftRight = new Vector(Array[Float](0, 1, 2, 3))
    val vectorRotateLeft = new Vector(Array[Float](2, 3, 4, 1))
    val vectorRotateRight = new Vector(Array[Float](4, 1, 2, 3))

    val matrix = Matrix(
      Array[Float](1, 2, 3, 4),
      Array[Float](5, 6, 7, 8),
      Array[Float](9, 10, 11, 12),
      Array[Float](13, 14, 15, 16),
      Array[Float](17, 18, 19, 20)
    )
    val RowShift = -1
    val ColShift = 2
    val matrixRotate = Matrix(
      Array[Float](7, 8, 5, 6),
      Array[Float](11, 12, 9, 10),
      Array[Float](15, 16, 13, 14),
      Array[Float](19, 20, 17, 18),
      Array[Float](3, 4, 1, 2)
    )
    val matrixShift = Matrix(
      Array[Float](0, 0, 5, 6),
      Array[Float](0, 0, 9, 10),
      Array[Float](0, 0, 13, 14),
      Array[Float](0, 0, 17, 18),
      Array[Float](0, 0, 0, 0)
    )


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a1 = TestScalarField(RefScalarField(vector))
      val Size = a1.fieldType.fieldShape(0)
      val shift1Left = shift(a1, -1)
      val shift1Right = shift(a1, 1)
      val rotate1Left = shiftCyclic(a1, -1)
      val rotate1Right = shiftCyclic(a1, 1)

      val a2 = TestScalarField(RefScalarField(matrix))
      val Rows = a2.fieldType.fieldShape(0)
      val Columns = a2.fieldType.fieldShape(1)
      val shift2 = shift(a2, RowShift, ColShift)
      val rotate2 = shiftCyclic(a2, RowShift, ColShift)

      probe(shift1Left, shift1Right, rotate1Left, rotate1Right, shift2, rotate2)
    }
    import graph._
    withRelease {
      step

      require(readScalar(shift1Left) == RefScalarField(vectorShiftLeft))
      require(readScalar(shift1Right) == RefScalarField(vectorShiftRight))
      require(readScalar(rotate1Left) == RefScalarField(vectorRotateLeft))
      require(readScalar(rotate1Right) == RefScalarField(vectorRotateRight))
      require(readScalar(shift2) == RefScalarField(matrixShift))
      require(readScalar(rotate2) == RefScalarField(matrixRotate))
    }
  }

  /** Test combining real and complex dynamic scalar fields. */
  test("scalar field / complex field") {
    val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val r = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val i = TestScalarField(RefScalarField.random(Size, Size) - 0.5f)
      val c = TestComplexField(RefComplexField.random(Size, Size))

      val sum = r + c
      val diff = r - c
      val product = r * c
      val quotient = r / c
      val composition = complex (r, i)

      probe(sum, r, c, diff, product, quotient, composition, i)
    }

    import graph._
    withRelease {
      step

      require(readComplex(sum) == RefComplexField(readScalar(r)) + readComplex(c))
      require(readComplex(diff) == RefComplexField(readScalar(r)) - readComplex(c))
      require(readComplex(product) == RefComplexField(readScalar(r)) :* readComplex(c))
      require(readComplex(quotient) ~== RefComplexField(readScalar(r)) :/ readComplex(c))
      require(readComplex(composition) == RefComplexField(readScalar(r), readScalar(i)))
    }
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("scalar field / fft") {
    def testShape(fieldShape: Shape) {
      // Offsets added to help relative error of ~==
      val field = RefScalarField.random(fieldShape) + 0.1f
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val space = TestScalarField(field)
        val recovered = realPart(fftInverse(fft(space)))

        probe(recovered)
      }

      import graph._
      withRelease {
        step
        require(readScalar(recovered) ~== field)
      }
    }

    def test1D(columns: Int) = testShape(Shape(columns))

    def test2D(rows: Int, columns: Int) = testShape(Shape(rows, columns))

    def test3D(layers: Int, rows: Int, columns: Int) = testShape(Shape(layers, rows, columns))

    test1D(16)
    test2D(32, 32)
    test2D(16, 1)    // Our FFT doesn't handle this case, invokes reshape work-around
    test2D(1, 4)
    test3D(16, 16, 16)
    test3D(2, 4, 8)
    test3D(8, 16, 1) // Our FFT doesn't handle this case, invokes reshape work-around
    test3D(16, 1, 1) // Our FFT doesn't handle this case, invokes reshape work-around

    // We should probably check the fft against a known frequency-domain result
    // Next best thing is to prove that it works to perform convolution:
    // Finally, do a sanity check that fft canY be used for convolution

    val Size = 16
    val signal = RefScalarField(Size, c => c)
    // Small filter, already shifted
    val filter = RefScalarField(Size, c =>
      if (c == 0)
        1f
      else if (c == 1)
        0.25f
      else if (c == Size - 1)
        0.5f
      else
        0f
    )
    // Expected filtered result (scaling factors of 0.25 and 0.5 are flipped due to convolution)
    val expected = RefScalarField(Size, c => {
      c +
      0.25f * ((c - 1 + Size) % Size) +
      0.5f * ((c + 1 + Size) % Size)
    })

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val signalSpace = TestScalarField(signal)
      val filterSpace = TestScalarField(filter)
      val filteredFreq = fft(signalSpace) * fft(filterSpace)
      val filteredSpace = realPart(fftInverse(filteredFreq))

      probe(filteredSpace)
    }
    import graph._
    withRelease {
      step
      require(readScalar(filteredSpace) ~== expected)
    }
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("scalar field / fftRI (split real/imaginary)") {
    def testShape(fieldShape: Shape) {
      // Offsets added to help relative error of ~==
      val field = RefScalarField.random(fieldShape) + 0.1f
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val space = TestScalarField(field)
        val (freqR, freqI) = fftRI(space)
        val recovered = fftInverseRI(freqR, freqI)._1

        probe(recovered)
      }

      import graph._
      withRelease {
        step
        require(readScalar(recovered) ~== field)
      }
    }

    def test1D(columns: Int) = testShape(Shape(columns))

    def test2D(rows: Int, columns: Int) = testShape(Shape(rows, columns))

    def test3D(layers: Int, rows: Int, columns: Int) = testShape(Shape(layers, rows, columns))


    test1D(16)
    test2D(32, 32)
    test2D(16, 1)    // Our FFT doesn't handle this case, invokes reshape work-around
    test2D(1, 4)
    test3D(16, 16, 16)
    test3D(2, 4, 8)
    test3D(8, 16, 1) // Our FFT doesn't handle this case, invokes reshape work-around
    test3D(16, 1, 1) // Our FFT doesn't handle this case, invokes reshape work-around
  }

  /** Test dct and dctInverse. Note that this only checks inversion, not that
    * the dct produces the correct result. Should be fixed.
    */
  test("scalar field / dct and dctInverse") {
    val field256 = RefScalarField.random(256, 256)
    val field512 = RefScalarField.random(512, 512)
    val field1024 = RefScalarField.random(1024, 1024)
    val field2048 = RefScalarField.random(2048, 2048)

    // DCT currently not working for non-square inputs
//    val fieldNotSquare = RefScalarField.random(256, 512)

    // 1D
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val space256 = TestScalarField(field256)
      val recovered256 = dctInverse(dct(space256))

      val space512 = TestScalarField(field512)
      val recovered512 = dctInverse(dct(space512))

      val space1024 = TestScalarField(field1024)
      val recovered1024 = dctInverse(dct(space256))

      val space2048 = TestScalarField(field2048)
      val recovered2048 = dctInverse(dct(space2048))

//      val spaceNotSquare= TestScalarField(fieldNotSquare)
//      val recoveredNotSquare = dctInverse(dct(spaceNotSquare))

      probe(recovered256, recovered512, recovered1024, recovered2048)
    }

    import graph._
    withRelease {
      step

      require(readScalar(recovered256) ~== field256)
      require(readScalar(recovered512) ~== field512)
      // DCT not working yet for non-square fields
//      require(readScalar(recoveredNotSquare) ~== fieldNotSquare)
      // The dct doesn't have the numerical precision required to pass the
      // next two tests, so they are commented out.
      //require(readScalar(recovered1024) ~== field1024)
      //require(readScalar(recovered2048) ~== field2048)
    }
  }

  /** Test dctTransposed and dctInverseTransposed.
    * Note that this only checks inversion, not that the dct produces the
    * correct result. Should be fixed.
    */
  test("scalar field / dctTransposed and dctInverseTransposed") {
    val field256 = RefScalarField.random(256, 256)
    val field512 = RefScalarField.random(512, 512)
    val field1024 = RefScalarField.random(1024, 1024)
    val field2048 = RefScalarField.random(2048, 2048)

    // 1D
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val space256 = TestScalarField(field256)
      val recovered256 = dctInverseTransposed(dctTransposed(space256))

      val space512 = TestScalarField(field512)
      val recovered512 = dctInverseTransposed(dctTransposed(space512))

      val space1024 = TestScalarField(field1024)
      val recovered1024 = dctInverseTransposed(dctTransposed(space1024))

      val space2048 = TestScalarField(field2048)
      val recovered2048 = dctInverseTransposed(dctTransposed(space2048))

      probe(recovered256, recovered512, recovered1024, recovered2048)
    }

    import graph._
    withRelease {
      step

      require(readScalar(recovered256) ~== field256)
      require(readScalar(recovered512) ~== field512)
      // The dct doesn't have the numerical precision required to pass the
      // next two tests, so they are commented out.
      //require(readScalar(recovered1024) ~== field1024)
      //require(readScalar(recovered2048) ~== field2048)
    }
  }

  /** Test separable FFT: 2D only (for now)
    *
    * Note: these tests merely test that the inverse FFT undoes what the
    * forward FFT does, not that the FFT actually calculates the frequency-
    * space version of its input.   XXX
    */
  test("scalar field / separable fft") {
    val field2 = RefScalarField.random(32, 32)
    val big2 = RefScalarField.random(512, 512)

    // 2D
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val space2 = TestScalarField(field2)
      val recovered2 =
        realPart(fftInverseColumns(fftInverseRows(fftColumns(fftRows(space2)))))
      val bigSpace2 = TestScalarField(big2)
      val recoveredBig2 =
        realPart(fftInverseColumns(fftInverseRows(fftColumns(fftRows(bigSpace2)))))

      probe(recovered2, recoveredBig2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(recovered2) ~== field2)
      require(readScalar(recoveredBig2) ~== big2)
    }
  }

  /** Test the replicate operator. */
  test("scalar field / replicate") {
    val image1D = new Vector(Array[Float](-4, 5, 18, 11))
    val image2D = Matrix(
      Array(10f, 11f, 12f),
      Array(12f, 13f, 14f),
      Array(18f, 17f, 16f)
    )
    val Size = 4


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val matrixField = TestMatrixField(Size, Size, Shape(3, 3))
      val vectorField = TestVectorField(Size, Size, Shape(image1D.length))
      val scalarField2D = TestScalarField(RefScalarField(image2D))
      val rep = replicate(scalarField2D, matrixField.fieldShape)
      val scalarField1D = TestScalarField(RefScalarField(image1D))
      val rep2 = replicate(scalarField1D, vectorField.fieldShape)

      probe(rep, rep2)
    }

    import graph._
    withRelease {
      step

      val replicated = readMatrix(rep)
      for (row <- 0 until Size; col <- 0 until Size)
        require(replicated.read(row, col) == image2D)

      val replicated2 = readVector(rep2)
      for (row <- 0 until Size; col <- 0 until Size)
        require(replicated2.read(row, col) == image1D)
    }
  }

  /** Test the ^^ operator. */
  test("scalar field / outer product") {
    val vector1 = new Vector(Array[Float](1, 2, 3))
    val vector2 = new Vector(Array[Float](4, 5, 6))
    val expected = Matrix(
      Array(4f, 5f, 6f),
      Array(8f, 10f, 12f),
      Array(12f, 15f, 18f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(RefScalarField(vector1))
      val field2 = TestScalarField(RefScalarField(vector2))
      val outer = field1 ^ field2

      probe(outer)
    }

    import graph._
    withRelease {
      step
      require(readScalar(outer) == RefScalarField(expected))
    }
  }

  /** Test the () operator. */
  test("scalar field / apply") {
    val image = Matrix(
      Array(4f, 5f, 6f),
      Array(8f, 10f, 12f),
      Array(12f, 15f, 18f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(RefScalarField(image))
      val row0 = field1(0 to 0, 0 until 3)
      val upperLeft = field1(0 to 1, 0 to 1)
      val bottomRight = field1(1 to 2, 1 to 2)

      probe(row0, upperLeft, bottomRight)
    }

    import graph._
    withRelease {
      step
      require(readScalar(row0) == RefScalarField(Matrix(Array(4f, 5f, 6f))))
      require(readScalar(upperLeft) == RefScalarField(Matrix(Array(4f, 5f), Array(8f, 10f))))
      require(readScalar(bottomRight) == RefScalarField(Matrix(Array(10f, 12f), Array(15f, 18f))))
    }
  }

  /** Test the reshape operator. */
  test("scalar field / reshape") {
    val image1 = RefScalarField(Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f)
    ))
    val image2 = RefScalarField(Matrix(
      Array(1f, 2f),
      Array(3f, 4f),
      Array(5f, 6f)
    ))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(image1)
      val field2 = reshape(field1, 3, 2)

      probe(field2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(field2) == image2)
    }
  }

  /** Test the join operator. */
  /*
  test("scalar field / join") {
    val image1 = RefScalarField(Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f)
    ))
    val image2 = RefScalarField(Matrix(
      Array(7f, 8f, 9f)
    ))
    val joined = RefScalarField(Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f),
      Array(7f, 8f, 9f)
    ))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(image1)
      val field2 = TestScalarField(image2)
      val joinedField = field1.join(field2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(joinedField) == joined)
    }
  }
  */

  /** Test the (Int) operator. */
  test("scalar field / slice") {
    val image = Matrix(
      Array(4f, 5f, 6f),
      Array(8f, 10f, 12f),
      Array(12f, 15f, 18f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(RefScalarField(image))
      val row0 = field1(0)
      val row1 = field1(1)
      val row2 = field1(2)

      probe(row0, row1, row2)
    }

    import graph._
    withRelease {
      step
      require(readScalar(row0) == RefScalarField(new Vector(Array(4f, 5f, 6f))))
      require(readScalar(row1) == RefScalarField(new Vector(Array(8f, 10f, 12f))))
      require(readScalar(row2) == RefScalarField(new Vector(Array(12f, 15f, 18f))))
    }
  }

  /** Test the (0D-ScalarField) operator (slice). */
  test("scalar field / slice point") {
    // 1D input

    val rand = new cogx.utilities.Random
    val shape1D = Shape(17)
    val image1D = RefScalarField.random(shape1D)
    val sliceToExtract1D = shape1D(0) * rand.nextFloat
    val image = Matrix(
      Array(4f, 5f, 6f),
      Array(8f, 10f, 12f),
      Array(12f, 15f, 18f)
    )
    val shape3D = Shape(3, 5, 4)
    val image3D = RefScalarField.random(shape3D)
    val sliceToExtract3D = shape3D(0) * rand.nextFloat

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1D = TestScalarField(image1D)

      val indexField1D = TestScalarField(RefScalarField(sliceToExtract1D))
      val slicedField0D = field1D(indexField1D)

      // 2D input

      val index0 = TestScalarField(RefScalarField(0f))
      val index1 = TestScalarField(RefScalarField(1.1f))
      val index2 = TestScalarField(RefScalarField(2.4f))

      val field2D = TestScalarField(RefScalarField(image))

      val row0 = field2D(index0)
      val row1 = field2D(index1)
      val row2 = field2D(index2)

      // 3D input

      val field3D = TestScalarField(image3D)
      val indexField3D = TestScalarField(RefScalarField(sliceToExtract3D))
      val slicedField2D = field3D(indexField3D)

      probe(slicedField0D, row0, row1, row2, slicedField2D)
    }

    import graph._
    withRelease {
      step
      require(readScalar(slicedField0D) == image1D.slice(sliceToExtract1D.toInt))
      require(readScalar(row0) == RefScalarField(new Vector(Array(4f, 5f, 6f))))
      require(readScalar(row1) == RefScalarField(new Vector(Array(8f, 10f, 12f))))
      require(readScalar(row2) == RefScalarField(new Vector(Array(12f, 15f, 18f))))
      require(readScalar(slicedField2D) == image3D.slice(sliceToExtract3D.toInt))
    }
  }

  /** Test the stack operator. */
  test("scalar field / stack") {
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

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f0 = TestScalarField(field0)
      val f1 = TestScalarField(field1)
      val f2 = TestScalarField(field2)
      val fArray = Array(f0, f1, f2)
      val stack0 = stack(f0, f1, f2)
      val stack1 = stack(fArray)

      probe(stack0, stack1)
    }

    import graph._
    withRelease {
      step
      require(readScalar(stack0) == expected)
      require(readScalar(stack1) == expected)
    }
  }


  /** Test the stackTensors operator. */
  test("scalar field / tensor stack") {
    val Rows = 3
    val Columns = 3
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

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field0 = TestScalarField(image0)
      val field1 = TestScalarField(image1)
      val field2 = TestScalarField(image2)
      val field3 = TestScalarField(image3)
      val stackedField = vectorField(field0, field1, field2, field3)

      probe(stackedField)
    }

    import graph._
    withRelease {
      step
      require(readVector(stackedField) == expected)
    }
  }

  test("scalar field / median") {
    val oneElement = new Vector(Array[Float](1))                              // median = 1
    val twoSameElements = new Vector(Array[Float](2, 2))                      // median = 2
    val twoDiffElements = new Vector(Array[Float](1, 2))                      // median = (1+2)/2 = 1.5
    val oddDiffElements = new Vector(Array[Float](2, 3, 4, 0, 5, -3, 7))      // median = 3
    val oddSameElements = new Vector(Array[Float](2, -8, 2, 0, 12, -1, 6))    // median = 2 (duplicate irrelevant)
    val evenDiffElements = new Vector(Array[Float](2, 3, 4, 0, -1, 11, 6, -4)) // median = (2+3)/2 = 2.5
    val evenSameElements = new Vector(Array[Float](2, 3, 4, 0, -3, 3, 9, 14))  // median = (3+3)/3 = 3


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val oneElementField = TestScalarField(RefScalarField(oneElement))
      val oneElementMedian = fieldReduceMedian(oneElementField)

      val twoSameElementsField = TestScalarField(RefScalarField(twoSameElements))
      val twoSameElementsMedian = fieldReduceMedian(twoSameElementsField)

      val twoDiffElementsField = TestScalarField(RefScalarField(twoDiffElements))
      val twoDiffElementsMedian = fieldReduceMedian(twoDiffElementsField)

      val oddDiffElementsField = TestScalarField(RefScalarField(oddDiffElements))
      val oddDiffElementsMedian = fieldReduceMedian(oddDiffElementsField)

      val oddSameElementsField = TestScalarField(RefScalarField(oddSameElements))
      val oddSameElementsMedian = fieldReduceMedian(oddSameElementsField)

      val evenDiffElementsField = TestScalarField(RefScalarField(evenDiffElements))
      val evenDiffElementsMedian = fieldReduceMedian(evenDiffElementsField)

      val evenSameElementsField = TestScalarField(RefScalarField(evenSameElements))
      val evenSameElementsMedian = fieldReduceMedian(evenSameElementsField)

      probe(oneElementMedian, twoSameElementsMedian, twoDiffElementsMedian,
        oddDiffElementsMedian, oddSameElementsMedian, evenDiffElementsMedian, evenSameElementsMedian)
    }

    import graph._
    withRelease {
      step

      require(readScalar(oneElementMedian) == RefScalarField(1.0f))
      require(readScalar(twoSameElementsMedian) == RefScalarField(2.0f))
      require(readScalar(twoDiffElementsMedian) == RefScalarField(1.5f))
      require(readScalar(oddDiffElementsMedian) == RefScalarField(3.0f))
      require(readScalar(oddSameElementsMedian) == RefScalarField(2.0f))
      require(readScalar(evenDiffElementsMedian) == RefScalarField(2.5f))
      require(readScalar(evenSameElementsMedian) == RefScalarField(3.0f))
    }
  }

  // Test (Field, scalar) commutativity
  test("scalar field / commutative") {

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field = TestScalarField(RefScalarField.random(10, 10))
      val plus = field + 1
      val plusReverse = 1 + field
      val minus = 1 - field
      val minusReverse = -field + 1
      val times = 2 * field
      val timesReverse = field * 2

      // Test (Field, Field) commutativity where one field is 0-dimensional
      val field0D = TestScalarField(RefScalarField(1.234f))
      val plus0D = field + field0D
      val plusReverse0D = field0D + field

      val minus0D = field - field0D
      val minusReverse0D = field0D - field
      val times0D = field * field0D
      val timesReverse0D = field0D * field

      probe(plus, plusReverse, minus, minusReverse, times, timesReverse,
        plus0D, plusReverse0D, minus0D, minusReverse0D, times0D, timesReverse0D)
    }

    import graph._
    withRelease {
      step

      require(readScalar(plus) == readScalar(plusReverse))
      require(readScalar(minus) == readScalar(minusReverse))
      require(readScalar(times) == readScalar(timesReverse))

      require(readScalar(plus0D) == readScalar(plusReverse0D))
      require(readScalar(minus0D) == -readScalar(minusReverse0D))
      require(readScalar(times0D) == readScalar(timesReverse0D))
    }
  }

  /*
  test("scalar field / push") {

    // 1D RefScalarField Stack, 0D RefScalarField Slice
    def point1D(col: Int): Float = col
    val Columns_1D = 3
    val stack_1D = RefScalarField(Columns_1D, point1D _)
    val stack0_1D = RefScalarField(0f)
    val stack1_1D = RefScalarField(1f)
    val stack2_1D = RefScalarField(2f)
    val slice_1D = RefScalarField(5f)

    // 2D RefScalarField Stack, 1D RefScalarField Slice
    def point2D(row: Int, col: Int): Float =
      row * 10 + col
    val Rows_2D = 4
    val Columns_2D = 5
    val stack_2D = RefScalarField(Rows_2D, Columns_2D, point2D _)
    val stack0_2D = RefScalarField(Columns_2D, point2D(0, _))
    val stack1_2D = RefScalarField(Columns_2D, point2D(1, _))
    val stack2_2D = RefScalarField(Columns_2D, point2D(2, _))
    val stack3_2D = RefScalarField(Columns_2D, point2D(3, _))
    val slice_2D = RefScalarField(Columns_2D, point2D(5, _))

    // 3D RefScalarField Stack, 2D RefScalarField Slice
    def point3D(depth: Int, row: Int, col: Int): Float =
      depth * 100 + row * 10 + col
    val Depth_3D = 3
    val Rows_3D = 4
    val Columns_3D = 5
    val stack_3D = RefScalarField(Depth_3D, Rows_3D, Columns_3D, point3D _)
    val stack0_3D = RefScalarField(Rows_3D, Columns_3D, point3D(0, _, _))
    val stack1_3D = RefScalarField(Rows_3D, Columns_3D, point3D(1, _, _))
    val stack2_3D = RefScalarField(Rows_3D, Columns_3D, point3D(2, _, _))
    val slice_3D = RefScalarField(Rows_3D, Columns_3D, point3D(5, _, _))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // 1D RefScalarField Stack, 0D RefScalarField Slice
      val a_1D = TestScalarField(stack_1D)
      val b_1D = a_1D.push(TestScalarField(slice_1D))

      // 2D RefScalarField Stack, 1D RefScalarField Slice
      val a_2D = TestScalarField(stack_2D)
      val b_2D = a_2D.push(TestScalarField(slice_2D))

      // 3D RefScalarField Stack, 2D RefScalarField Slice
      val a_3D = TestScalarField(stack_3D)
      val b_3D = a_3D.push(TestScalarField(slice_3D))
    }

    import graph._
    withRelease {
      step

      // check 1D stack
      require(readScalar(a_1D).slice(0) == stack0_1D)
      require(readScalar(a_1D).slice(1) == stack1_1D)
      require(readScalar(a_1D).slice(2) == stack2_1D)
      require(readScalar(b_1D).slice(0) == slice_1D)
      require(readScalar(b_1D).slice(1) == stack0_1D)
      require(readScalar(b_1D).slice(2) == stack1_1D)

      // check 2D stack
      require(readScalar(a_2D).slice(0) == stack0_2D)
      require(readScalar(a_2D).slice(1) == stack1_2D)
      require(readScalar(a_2D).slice(2) == stack2_2D)
      require(readScalar(a_2D).slice(3) == stack3_2D)
      require(readScalar(b_2D).slice(0) == slice_2D)
      require(readScalar(b_2D).slice(1) == stack0_2D)
      require(readScalar(b_2D).slice(2) == stack1_2D)
      require(readScalar(b_2D).slice(3) == stack2_2D)

      // check 3D stack
      require(readScalar(a_3D).slice(0) == stack0_3D)
      require(readScalar(a_3D).slice(1) == stack1_3D)
      require(readScalar(a_3D).slice(2) == stack2_3D)
      require(readScalar(b_3D).slice(0) == slice_3D)
      require(readScalar(b_3D).slice(1) == stack0_3D)
      require(readScalar(b_3D).slice(2) == stack1_3D)
    }
  }
  */

  test("scalar field / warp 2D") {
    val Rows = 4
    val Columns = 5
    val input = RefScalarField(Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    ))

    // Not used, but provided here to help visualize border clamping
    /*
    val borderClampedInput = RefScalarField(Matrix(
      Array(0f,  0f,  1f,  2f,  3f,  4f,  4f),
      Array(0f,  0f,  1f,  2f,  3f,  4f,  4f),
      Array(10f, 10f, 11f, 12f, 13f, 14f, 14f),
      Array(20f, 20f, 21f, 22f, 23f, 24f, 24f),
      Array(30f, 30f, 31f, 32f, 33f, 34f, 34f),
      Array(30f, 30f, 31f, 32f, 33f, 34f, 34f)
    ))
    */
    // Translated by (-0.5, -0.5)
    val borderClampExpectedNegative = RefScalarField(Matrix(
      Array( 5.5f,  6.5f,  7.5f,  8.5f,   9.0f),
      Array(15.5f, 16.5f, 17.5f, 18.5f,  19.0f),
      Array(25.5f, 26.5f, 27.5f, 28.5f,  29.0f),
      Array(30.5f, 31.5f, 32.5f, 33.5f,  34.0f)
    ))
    // Translated by (1.5, 0.5)   (row-delta, column-delta)
    val borderClampExpectedPositive = RefScalarField(Matrix(
      Array( 0.0f,  0.5f,  1.5f,  2.5f,  3.5f),
      Array( 0.0f,  0.5f,  1.5f,  2.5f,  3.5f),
      Array( 5.0f,  5.5f,  6.5f,  7.5f,  8.5f),
      Array(15.0f, 15.5f, 16.5f, 17.5f, 18.5f)
    ))

    // Translated by (0, 1.0)   (row-delta, column-delta)
    val borderZeroExpectedPositive = RefScalarField(Matrix(
      Array( 0f,  0f,  1f,  2f,  3f),
      Array( 0f, 10f, 11f, 12f, 13f),
      Array( 0f, 20f, 21f, 22f, 23f),
      Array( 0f, 30f, 31f, 32f, 33f)
    ))

    // Translation by a 2D vector field (with varying vectors).
    def vectorAt(row: Int, col: Int) = new Vector(row / 4f, -col / 4f)


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarField = TestScalarField(input)

      // Translation by a 0D scalar field
      val posVectorField0D =
        TestVectorField(RefVectorField(new Vector(1.5f, 0.5f)))
      val negVectorField0D =
        TestVectorField(RefVectorField(new Vector(-0.5f, -0.5f)))
      val shiftRightField0D =
        TestVectorField(RefVectorField(new Vector(0f, 1f)))
      val translatedPos0D = warp(scalarField, posVectorField0D)
      val translatedNeg0D = warp(scalarField, negVectorField0D)
      val translatedRight0D = warp(scalarField, shiftRightField0D, BorderZero)

      // Translation by a 2D vector field (with same vector used
      // throughout, identical to the positive case above).
      val posVectorField2D =
        TestVectorField(RefVectorField(Rows, Columns, (r, c) => new Vector(1.5f, 0.5f)))
      val translatedPos2D = warp(scalarField, posVectorField2D)

      val vectorField2D = TestVectorField(RefVectorField(Rows, Columns, vectorAt _))

      // Since the 0D translations have now been tested, when can use those to
      // help us test translation over a variable field.
      val multiVectorField = Array.ofDim[VectorField](Rows, Columns)
      val multiTranslated = Array.ofDim[ScalarField](Rows, Columns)
      for (row <- 0 until Rows; col <- 0 until Columns) {
        multiVectorField(row)(col) =
          TestVectorField(RefVectorField(Rows, Columns,
            (_, _) => vectorAt(row, col)))
        multiTranslated(row)(col) = warp(scalarField, multiVectorField(row)(col))
      }
      val translated = warp(scalarField, vectorField2D)

      // Test implicit trimming that occurs when guide field is smaller than input

      val TrimmedRows = Rows - 2
      val TrimmedColumns = Columns - 1

      val trimmedGuide = trim(vectorField2D, Shape(TrimmedRows, TrimmedColumns))
      val translatedTrimmed = warp(scalarField, trimmedGuide)

      probe(translatedPos0D, translatedNeg0D, translatedPos2D, translated, translatedRight0D, translatedTrimmed)
      multiTranslated.foreach(fieldArray => fieldArray.foreach(probe(_)))
    }

    import graph._
    withRelease {
      step

      require(readScalar(translatedPos0D) == borderClampExpectedPositive)
      require(readScalar(translatedNeg0D) == borderClampExpectedNegative)
      require(readScalar(translatedPos2D) == borderClampExpectedPositive)
      require(readScalar(translatedRight0D) == borderZeroExpectedPositive)
      for (row <- 0 until Rows; col <- 0 until Columns) {
        require(readScalar(translated).read(row, col) ==
                readScalar(multiTranslated(row)(col)).read(row, col))
      }
      for (row <- 0 until TrimmedRows; col <- 0 until TrimmedColumns) {
        require(readScalar(translatedTrimmed).read(row, col) ==
                readScalar(multiTranslated(row)(col)).read(row, col))
      }
    }
  }

  test("scalar field / subfield") {
    val OutputShape = Shape(2,3)
    val input = RefScalarField(Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    ))

    val lastInputRow = RefScalarField(Vector(
      30f, 31f, 32f, 33f, 34f
    ))

    // Not used, but provided here to help visualize border clamping
    /*
    val borderClampedInput = RefScalarField(Matrix(
      Array(0f,  0f,  1f,  2f,  3f,  4f,  4f),
      Array(0f,  0f,  1f,  2f,  3f,  4f,  4f),
      Array(10f, 10f, 11f, 12f, 13f, 14f, 14f),
      Array(20f, 20f, 21f, 22f, 23f, 24f, 24f),
      Array(30f, 30f, 31f, 32f, 33f, 34f, 34f),
      Array(30f, 30f, 31f, 32f, 33f, 34f, 34f)
    ))
    */

    // Not used, but provided here to help visualize cyclic border handling
    /*
    val borderCyclicInput = RefScalarField(Matrix(
      Array(34f, 30f, 31f, 32f, 33f, 34f, 30f),
      Array(4f,  0f,  1f,  2f,  3f,  4f,  0f),
      Array(14f, 10f, 11f, 12f, 13f, 14f, 10f),
      Array(24f, 20f, 21f, 22f, 23f, 24f, 20f),
      Array(34f, 30f, 31f, 32f, 33f, 34f, 30f),
      Array(4f,  0f,  1f,  2f,  3f,  4f,  0f)
    ))
    */

    // Subfield starting at (0.5, 0.5), Shape(2,3)
    val borderClampExpected1 = RefScalarField(Matrix(
      Array( 5.5f,  6.5f,  7.5f),
      Array(15.5f, 16.5f, 17.5f)
    ))
    // Subfield starting at (-1.5, -0.5)   (row-delta, column-delta)
    val borderClampExpected2 = RefScalarField(Matrix(
      Array( 0.0f,  0.5f,  1.5f),
      Array( 0.0f,  0.5f,  1.5f)
    ))

    // Subfield starting at (3.0, 2.0)   (row-delta, column-delta)
    // Tests corner case of BorderZero processing with elements at edge of valid data.
    val borderZeroExpected = RefScalarField(Matrix(
      Array( 32f,  33f,  34f),
      Array(  0f,   0f,   0f)
    ))

    val borderCyclicExpected3 = RefScalarField(Vector(
      31.75f, 32.75f, 33.75f, 31f
    ))

    val Shape1D = Shape(4)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarField = TestScalarField(input)

      // Translation by a 0D vector field
      val inBoundsUpperLeft =
        TestVectorField(RefVectorField(new Vector(0.5f, 0.5f)))
      val inBoundsWindow = subfield(scalarField, inBoundsUpperLeft, OutputShape)

      val outOfBoundsUpperLeft =
        TestVectorField(RefVectorField(new Vector(-1.5f, -0.5f)))
      val outOfBoundsWindow = subfield(scalarField, outOfBoundsUpperLeft, OutputShape)

      val borderZeroLowerRight =
        TestVectorField(RefVectorField(new Vector(3f, 2f)))
      val outOfBoundsLowerRight = subfield(scalarField, borderZeroLowerRight, OutputShape, BorderZero)


      // Test of 1D subfield()

      val Left = TestVectorField(RefVectorField(new Vector(1.75f)))
      val scalarField1D = TestScalarField(lastInputRow)

      val window1D = subfield(scalarField1D, Left, Shape1D, BorderCyclic)

      probe(inBoundsWindow, outOfBoundsWindow, outOfBoundsLowerRight, window1D)
    }

    import graph._
    withRelease {
      step

      require(readScalar(inBoundsWindow) == borderClampExpected1)
      require(readScalar(outOfBoundsWindow) == borderClampExpected2)
      require(readScalar(outOfBoundsLowerRight) == borderZeroExpected)
      require(readScalar(window1D) == borderCyclicExpected3)
    }
  }

  /**
   * Verify that looped constructs cause OpenCL buffer sharing yet
   * still work correctly.
   */
  test("scalar field / iteration") {
    val input = RefScalarField.random(16, 16) + 1

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(input)
      val out = realPart(fftInverse(fft(fftInverse(fft(fftInverse(fft(in)))))))

      probe(out)
    }
    import graph._
    withRelease {
      step
      require(readScalar(out) ~== input)
    }
  }

  test("scalar field / transpose") {
    val Rows = 61
    val Columns = 88
    val data = Matrix.random(Rows, Columns)

    val field = RefScalarField(data)
    val expectedTransposed = RefScalarField(data.transpose)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestScalarField(field)
      val transposed = transpose(input)

      probe(transposed)
    }

    import graph._
    withRelease {
      step
      require(readScalar(transposed) ~== expectedTransposed)
    }
  }

  test("scalar field / forward gradient") {
    val data = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val forwardColDeriv = Matrix(
      Array(
        Array(  6f, -2f,  0f),
        Array(  2f,  4f,  0f),
        Array(  0f,  0f,  0f)
      )
    )
    val forwardRowDeriv = Matrix(
      Array(
        Array( -3f, -7f,  0f),
        Array(  6f, -7f,  0f),
        Array(  0f,  0f,  0f)
      )
    )
    val expectedGradient = RefVectorField(3, 3,
      (r, c) => new Vector(forwardRowDeriv(r, c), forwardColDeriv(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestScalarField(RefScalarField(data))
      val gradient = forwardGradient(input)

      probe(gradient)
    }

    import graph._
    withRelease {
      step
      require(readVector(gradient) ~== expectedGradient)
    }
  }

  test("scalar field / backward gradient") {
    val data = Matrix(
      Array(
        Array( 1f,  7f,  5f),
        Array(-2f,  0f,  4f),
        Array( 4f, -7f,  3f)
      )
    )
    val backwardColDeriv = Matrix(
      Array(
        Array(  0f,   0f,  0f),
        Array(  0f,   2f,  4f),
        Array(  0f, -11f, 10f)
      )
    )
    val backwardRowDeriv = Matrix(
      Array(
        Array(  0f,  0f,  0f),
        Array(  0f, -7f, -1f),
        Array(  0f, -7f, -1f)
      )
    )
    val expectedGradient = RefVectorField(3, 3,
      (r, c) => new Vector(backwardRowDeriv(r, c), backwardColDeriv(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestScalarField(RefScalarField(data))
      val gradient = backwardGradient(input)

      probe(gradient)
    }

    import graph._
    withRelease {
      step
      println("actual")
      readVector(gradient).print
      println("expected")
      expectedGradient.print
      require(readVector(gradient) ~== expectedGradient)
    }
  }

  test("scalar field / central gradient") {
    val data = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    )
    val centralColDeriv = Matrix(
      Array(
        Array( 3.0f,  2.0f, -1.0f),
        Array( 1.0f,  3.0f,  2.0f),
        Array(-5.5f, -0.5f,  5.0f)
      )
    )
    val centralRowDeriv = Matrix(
      Array(
        Array( -1.5f, -3.5f, -0.5f),
        Array(  1.5f, -7.0f, -1.0f),
        Array(  3.0f, -3.5f, -0.5f)
      )
    )
    val expectedGradient = RefVectorField(3, 3,
                                          (r, c) => new Vector(centralRowDeriv(r, c), centralColDeriv(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestScalarField(RefScalarField(data))
      val gradient = centralGradient(input)

      probe(gradient)
    }

    import graph._
    withRelease {
      step
      require(readVector(gradient) ~== expectedGradient)
    }
  }


  test("scalar field / median filter") {
    val data = Matrix(
      Array(
        Array( 1f,  7f,  5f,  2f),
        Array(-2f,  0f,  4f, -9f),
        Array( 4f, -7f,  3f,  7f),
        Array( 2f , 1f,  3f,  5f)
      )
    )
    val expectedResult = RefScalarField(Matrix(
      Array(
        Array( 1f,  4f,  4f,  2f),
        Array( 1f,  3f,  3f,  3f),
        Array( 1f,  2f,  3f,  4f),
        Array( 2f , 2f,  3f,  5f)
      )
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val input = TestScalarField(RefScalarField(data))
      val filtered = medianFilter(input)

      probe(filtered)
    }

    import graph._
    withRelease {
      step
      require(readScalar(filtered) ~== expectedResult)
    }
  }

  test("scalar field / upsample") {
    val input = RefScalarField(Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array(
        Array( 1f, 0f, 7f, 0f, 5f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array(-2f, 0f, 0f, 0f, 4f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array( 4f, 0f,-7f, 0f, 3f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f)
      )
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = upsample(inField)

      probe(outField)
    }


    import graph._
    withRelease {
      step
      require(readScalar(outField) == expectedOutput)
    }
  }

  test("scalar field / flip") {
    val input = RefScalarField(Matrix(
      Array(11f, 12f, 13f, 14f, 15f),
      Array(21f, 22f, 23f, 24f, 25f),
      Array(31f, 32f, 33f, 34f, 35f),
      Array(41f, 42f, 43f, 44f, 45f)
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array(45f, 44f, 43f, 42f, 41f),
      Array(35f, 34f, 33f, 32f, 31f),
      Array(25f, 24f, 23f, 22f, 21f),
      Array(15f, 14f, 13f, 12f, 11f)
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = flip(inField)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readScalar(outField) == expectedOutput)
    }
  }

  test("scalar field / supersample") {
    val input = RefScalarField(Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(-2f, 0f, 4f),
        Array(4f, -7f, 3f)
      )
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array(
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array( 1f, 1f, 7f, 7f, 5f, 5f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array(-2f,-2f, 0f, 0f, 4f, 4f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f),
        Array( 4f, 4f,-7f,-7f, 3f, 3f)
      )
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = supersample(inField)

      probe(outField)
    }


    import graph._
    withRelease {
      step
      require(readScalar(outField) == expectedOutput)
    }
  }

  /** Not done yet. */
  test("scalar field / adjoint") {
    val Rows = 5
    val Columns = 5
    val m = Matrix.random(Rows, Columns)
    val v0 = Matrix.random(Rows, Columns)
    val v1 = Matrix.random(Rows, Columns)

    val gradientM: RefVectorField = {
      val (v0, v1) = m.forwardGradient
      RefVectorField(Rows, Columns, (r, c) => new Vector(v0(r, c), v1(r, c)))
    }

    val divergenceV0V1 = RefScalarField(v0.backwardDivergence(v1))


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s = TestScalarField(RefScalarField(m))
      val v = TestVectorField(RefVectorField(Rows, Columns, (r, c) => new Vector(v0(r, c), v1(r, c))))

      val sGradient = forwardGradient(s)

      val vDivergence = backwardDivergence(v)

      // Dot product of scalar field and negative vector field divergence
      // This also tests parsers ability to ignore extra parentheses
      val sv = fieldReduceSum(-dot(s, backwardDivergence(v)))

      // Dot product of vector field and scalar field gradient
      val vs = fieldReduceSum(dot(v, forwardGradient(s)))
      probe(sGradient, vDivergence, sv, vs)
    }

    //------------------------------------------------------------------------

    import graph._
    withRelease {
      step

      //println("input scalar field")
      //m.print
      //println("expected gradient")
      //gradientM.print
      //println("actual gradient")
      //read(sGradient).print
      require(readVector(sGradient) ~== gradientM)

      //println("input vector field")
      //v0.print
      //v1.print
      //println("expected divergence")
      //divergenceV0V1.print
      //println("actual divergence")
      //read(vDivergence).print
      require(readScalar(vDivergence) ~== divergenceV0V1)

      //printf("dot product results: %f %f\n", Fyx, yFx)
      //println("Dot product test")
      //read(sv).print
      //read(vs).print
      require(readScalar(sv) ~== readScalar(vs), "operators are not mutually adjoint")
    }
  }

  test("scalar field / trim") {
    val input = RefScalarField(Matrix(
      Array( 0f,  1f,  2f,  3f,  4f),
      Array(10f, 11f, 12f, 13f, 14f),
      Array(20f, 21f, 22f, 23f, 24f),
      Array(30f, 31f, 32f, 33f, 34f)
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array( 0f,  1f,  2f,  3f),
      Array(10f, 11f, 12f, 13f)
    ))
    val outShape = Shape(2, 4)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(input)
      val out = trim(in, outShape)
      val autoTrimmedOut = ScalarField(outShape)
      autoTrimmedOut <== in

      probe(out, autoTrimmedOut)
    }

    import graph._
    withRelease {
      step

      require(readScalar(out) == expectedOutput)
      require(readScalar(autoTrimmedOut) == expectedOutput)
    }
  }

  test("scalar field / non max supression") {
    // hit all 3 cases: border, corner, interior
    val input = RefScalarField(Matrix(
      Array( 0f,  1f, 13f, 15f,  4f),
      Array(10f, 22f, 12f, 13f, 14f),
      Array(20f, 20f, 22f, 20f, 19f),
      Array(30f, 16f, 19f, 33f, 34f)
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array( 0f,  0f,  0f, 15f,  0f),
      Array( 0f, 22f,  0f,  0f,  0f),
      Array( 0f,  0f,  0f,  0f,  0f),
      Array(30f,  0f,  0f,  0f, 34f)
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestScalarField(input)
      val out = nonMaximumSuppression(in)

      probe(out)
    }

    import graph._
    withRelease {
      step
      require(readScalar(out) == expectedOutput)
    }
  }

  test("scalar field / tensor dot op") {
    val Rows = 11
    val Columns = 13
    val vectorShape = Shape(17)
    val field1 = RefVectorField.random(Rows, Columns, vectorShape)
    val field2 = RefVectorField.random(Rows, Columns, vectorShape)
    val expectedResult = field1 dot field2

    val field0D = RefVectorField.random(vectorShape)
    //val field0D = VectorField(new Vector(1f, 2f, 3f, 4f, 5f))
    val expectedResult0D = field1.dot(field0D)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val dynField1 = TestVectorField(field1)
      val dynField2 = TestVectorField(field2)
      val dynField0D = TestVectorField(field0D)

      val out = dot(dynField1, dynField2)
      val out0D = dot(dynField1, dynField0D)

      probe(out, out0D)
    }

    import graph._
    withRelease {
      step

      /*
       println("field1")
       field1.print
       println("field0D")
       field0D.print
       println("expected:")
       expectedResult0D.print
       println("actual:")
       read(out0D).print
       */
      require(readScalar(out) ~== expectedResult)
      require(readScalar(out0D) ~== expectedResult0D)
    }
  }
 /*
  test("scalar field / multiplex") {
    val Rows = 11
    val Columns = 13

    val field0 = RefScalarField.random(Rows, Columns)
    val field1 = RefScalarField.random(Rows, Columns)
    val field2 = RefScalarField.random(Rows, Columns)
    val field3 = RefScalarField.random(Rows, Columns)
    val fields = Array(field0, field1, field2, field3)


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val fieldArray = Array.tabulate(fields.length) {
        i => TestScalarField(fields(i))
      }
      val indexField = TestScalarField(RefScalarField(2f))
      val outArray = Array.tabulate(fields.length) {
        i => TestScalarField(fields(i) * 2)
      }
      // Test selection (reading)
      val selectArray = fieldArray.select(indexField)
      // Test writing
      outArray.insert(indexField) = field0
    }

    import graph._
    withRelease {
      step

      require(readScalar(selectArray) == field2)
      require(readScalar(outArray(0)) == field0 * 2)
      require(readScalar(outArray(1)) == field1 * 2)
      require(readScalar(outArray(2)) == field0)
      require(readScalar(outArray(3)) == field3 * 2)
    }
  }

  test("scalar field / multiplex 2D") {
    val Rows = 11
    val Columns = 13

    val field0 = RefScalarField.random(Rows, Columns)
    val field1 = RefScalarField.random(Rows, Columns)
    val field2 = RefScalarField.random(Rows, Columns)
    val field3 = RefScalarField.random(Rows, Columns)
    val field4 = RefScalarField.random(Rows, Columns)
    val field5 = RefScalarField.random(Rows, Columns)

    val fields = Array(
      Array(field0, field1, field2),
      Array(field3, field4, field5)
    )
    val fieldsRows = fields.length
    val fieldsColumns = fields(0).length


    val graph = new ComputeGraph(Optimize) {
      val fieldArray = Array.tabulate(fields.length, fields(0).length) {
        (i, j) => TestScalarField(fields(i)(j))
      }

      val indexField1 = TestScalarField(RefScalarField(1f))
      val indexField2 = TestScalarField(RefScalarField(0f))

      val outArray = Array.tabulate(fieldsRows, fieldsColumns) {
        (i, j) => TestScalarField(fields(i)(j) * 2)
      }

      // Test selection (reading)
      val selectArray = fieldArray.select(indexField1, indexField2)

      // Test writing
      outArray.insert(indexField1, indexField2) <== field0

    }

    import graph._
    withRelease {
      step

      require(readScalar(selectArray) == field3)
      require(readScalar(outArray(0)(0)) == field0 * 2)
      require(readScalar(outArray(0)(1)) == field1 * 2)
      require(readScalar(outArray(0)(2)) == field2 * 2)
      require(readScalar(outArray(1)(0)) == field0)
      require(readScalar(outArray(1)(1)) == field4 * 2)
      require(readScalar(outArray(1)(2)) == field5 * 2)
    }
  }
  */

  test("scalar field / local max") {
    val neighborhood = Matrix(
      Array(
        Array(1f, 1f, 0f),
        Array(1f, 1f, 0f),
        Array(0f, 0f, 0f)
      )
    )
    val input = RefScalarField(Matrix(
      Array(1f, 1f, 0f, 2f, 3f),
      Array(0f, 5f, 3f, 1f, 6f),
      Array(1f, 2f, 0f, 2f, 0f),
      Array(1f, 1f, 1f, 2f, 2f),
      Array(3f, 1f, 4f, 3f, 1f)
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array(1f, 1f, 1f, 2f, 3f),
      Array(1f, 5f, 5f, 3f, 6f),
      Array(1f, 5f, 5f, 3f, 6f),
      Array(1f, 2f, 2f, 2f, 2f),
      Array(3f, 3f, 4f, 4f, 3f)
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = localMax(inField, neighborhood)

      probe(outField)
    }
    import graph._
    withRelease {
      step
      require(readScalar(outField) == expectedOutput)
    }
  }

  test("scalar field / local min") {
    val neighborhood = Matrix(
      Array(
        Array(1f, 1f, 0f),
        Array(1f, 1f, 0f),
        Array(0f, 0f, 0f)
      )
    )
    val input = RefScalarField(Matrix(
      Array(1f, 1f, 0f, 2f, 3f),
      Array(0f, 5f, 3f, 1f, 6f),
      Array(1f, 2f, 0f, 2f, 0f),
      Array(1f, 1f, 1f, 2f, 2f),
      Array(3f, 1f, 4f, 3f, 1f)
    ))
    val expectedOutput = RefScalarField(Matrix(
      Array(1f, 1f, 0f, 0f, 2f),
      Array(0f, 0f, 0f, 0f, 1f),
      Array(0f, 0f, 0f, 0f, 0f),
      Array(1f, 1f, 0f, 0f, 0f),
      Array(1f, 1f, 1f, 1f, 1f)
    ))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestScalarField(input)
      val outField = localMin(inField, neighborhood)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readScalar(outField) == expectedOutput)
    }
  }

  /** test a fork in a chain of piped fields */
  test("scalar field / forked recurrences") {
    val shape = Shape(10, 10)
    val refA = RefScalarField.random(shape)
    val refD = RefScalarField.random(shape)
    val refE = RefScalarField.random(shape)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val A = probe(TestScalarField(refA), "")
      val B = probe(ScalarField(shape), "")
      val C = probe(ScalarField(shape), "")
      B <== A
      C <== A

      val D = probe(TestScalarField(refD), "")
      val E = probe(TestScalarField(refE), "")
      val F = probe(ScalarField(shape), "")
      val G = probe(ScalarField(shape), "")
      val H = probe(ScalarField(shape), "")
      F <== D + E
      G <== D + E
      H <== D + E + 0f   // Threw exception only with CogDebuggerApp

      probe(B, C, F, G, H)
    }

    import graph._
    withRelease {
      step
      require(readScalar(B) == refA)
      require(readScalar(C) == refA)
      require(readScalar(F) ~== refD + refE)
      require(readScalar(G) ~== refD + refE)
      require(readScalar(H) ~== refD + refE)
    }
  }

}

