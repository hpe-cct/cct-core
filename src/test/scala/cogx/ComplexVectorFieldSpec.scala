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
import cogx.reference.{RefTestInterface, RefVectorField, RefComplexField}
import cogx.helper.ScalarFieldBuilderInterface
import cogx.helper.ComplexFieldBuilderInterface
import cogx.helper.MatrixFieldBuilderInterface
import cogx.helper.VectorFieldBuilderInterface
import cogx.api.{CogFunctionAPI, ImplicitConversions}
import cogx.utilities.Array2D

/** Test code.
 *
 * @author Greg Snider and Dick Carter
 */

@RunWith(classOf[JUnitRunner])
class ComplexVectorFieldSpec
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

  // To speed the longer running convolve tests, only two vector sizes are tried.
  // We pick randomly from the "small tensor" lengths, namely 2, 3 or 4, and
  // then also from the "big tensor lengths", namely >= 5 (but not 8 or 16)
  // which might be either.

  /** Test combining a dynamic scalar field and a constant. */
  test("complex vector field / constant") {
    def runtest(vectorSize: Int) {
      val Size = 5
      val fieldShape = Shape(Size, Size)
      val vectorShape = Shape(vectorSize)
      val C = Complex(1.234f, -0.0567f)
      val R = 3.21f

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val a = ComplexVectorField.random(fieldShape, vectorShape) + Complex(0.5f, 0.5f)
        val sum = a + C
        val diff = a - C
        val product = a * C
        val quotient = a / C
        val sumR = a + R
        val diffR = a - R
        val productR = a * R
        val quotientR = a / R

        // Constant appears first in the mutator expression

        val constSum = C + a
        val constDiff = C - a
        val constProduct = C * a
        val constQuotient = C / a
        val constSumR = R + a
        val constDiffR = R - a
        val constProductR = R * a
        val constQuotientR = R / a

        probe(a, sum, diff, product, quotient, sumR, diffR, productR, quotientR,
          constSum, constDiff, constProduct, constQuotient, constSumR, constDiffR, constProductR, constQuotientR)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(sum) ==
                  (readComplexPlane(i)(graph.a) + C))
          require(readComplexPlane(i)(diff) ==
                  (readComplexPlane(i)(graph.a) - C))
          require(readComplexPlane(i)(product) ~==
                  (readComplexPlane(i)(graph.a) * C))
          require(readComplexPlane(i)(quotient) ~==
                  (readComplexPlane(i)(graph.a) / C))
          require(readComplexPlane(i)(sumR) ==
                  (readComplexPlane(i)(graph.a) + R))
          require(readComplexPlane(i)(diffR) ==
                  (readComplexPlane(i)(graph.a) - R))
          require(readComplexPlane(i)(productR) ~==
                  (readComplexPlane(i)(graph.a) * R))
          require(readComplexPlane(i)(quotientR) ~==
                  (readComplexPlane(i)(graph.a) / R))

          require(readComplexPlane(i)(constSum) ==
                  (readComplexPlane(i)(graph.a) + C))
          require(readComplexPlane(i)(constDiff) ==
                  (-readComplexPlane(i)(graph.a) + C))
          require(readComplexPlane(i)(constProduct) ~==
                  (readComplexPlane(i)(graph.a) * C))
          require(readComplexPlane(i)(constQuotient) ~==
                  (readComplexPlane(i)(graph.a).reciprocal * C))
          require(readComplexPlane(i)(constSumR) ==
                  (readComplexPlane(i)(graph.a) + R))
          require(readComplexPlane(i)(constDiffR) ==
                  (-readComplexPlane(i)(graph.a) + R))
          require(readComplexPlane(i)(constProductR) ~==
                  (readComplexPlane(i)(graph.a) * R))
          require(readComplexPlane(i)(constQuotientR) ~==
                  (readComplexPlane(i)(graph.a).reciprocal * R))
        }
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))

  }

  /** Test combining a dynamic scalar field and a constant. */
  test("complex vector field / vector constant") {
    def runtest(vectorSize: Int) {
      val Size = 5
      val fieldShape = Shape(Size, Size)
      val vectorShape = Shape(vectorSize)
      val C = ComplexVector.random(vectorSize)
      val R = Vector.random(vectorSize)

      // "nice" inputs are designed to prevent the result value real and
      // imaginary components from being near 0, as those can fail the high
      // approximate-equals test due to high relative error.

      val niceC = ComplexVector.random(vectorSize) * 0.1f + Complex(1.0f, 0.1f)
      val niceR = Vector.random(vectorSize) + 0.1f

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val a = ComplexVectorField.random(fieldShape, vectorShape) + Complex(0.5f, 0.5f)
        val sum = a + C
        val diff = a - C
        val product = a * niceC
        val quotient = a / niceC
        val sumR = a + R
        val diffR = a - R
        val productR = a * niceR
        val quotientR = a / niceR

        // Constant appears first in the mutator expression

        val constSum = C + a
        val constDiff = C - a
        val constProduct = niceC * a
        val constQuotient = niceC / a
        val constSumR = R + a
        val constDiffR = R - a
        val constProductR = niceR * a
        val constQuotientR = niceR / a

        probe(a, sum, diff, product, quotient, sumR, diffR, productR, quotientR,
          constSum, constDiff, constProduct, constQuotient, constSumR, constDiffR, constProductR, constQuotientR)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(sum) ==
                  (readComplexPlane(i)(graph.a) + C(i)))
          require(readComplexPlane(i)(diff) ==
                  (readComplexPlane(i)(graph.a) - C(i)))
          require(readComplexPlane(i)(product) ~==
                  (readComplexPlane(i)(graph.a) * niceC(i)))
          require(readComplexPlane(i)(quotient) ~==
                  (readComplexPlane(i)(graph.a) / niceC(i)))
          require(readComplexPlane(i)(sumR) ==
                  (readComplexPlane(i)(graph.a) + R(i)))
          require(readComplexPlane(i)(diffR) ==
                  (readComplexPlane(i)(graph.a) - R(i)))
          require(readComplexPlane(i)(productR) ~==
                  (readComplexPlane(i)(graph.a) * niceR(i)))
          require(readComplexPlane(i)(quotientR) ~==
                  (readComplexPlane(i)(graph.a) / niceR(i)))

          require(readComplexPlane(i)(constSum) ==
                  (readComplexPlane(i)(graph.a) + C(i)))
          require(readComplexPlane(i)(constDiff) ==
                  (-readComplexPlane(i)(graph.a) + C(i)))
          require(readComplexPlane(i)(constProduct) ~==
                  (readComplexPlane(i)(graph.a) * niceC(i)))
          require(readComplexPlane(i)(constQuotient) ~==
                  (readComplexPlane(i)(graph.a).reciprocal * niceC(i)))
          require(readComplexPlane(i)(constSumR) ==
                  (readComplexPlane(i)(graph.a) + R(i)))
          require(readComplexPlane(i)(constDiffR) ==
                  (-readComplexPlane(i)(graph.a) + R(i)))
          require(readComplexPlane(i)(constProductR) ~==
                  (readComplexPlane(i)(graph.a) * niceR(i)))
          require(readComplexPlane(i)(constQuotientR) ~==
                  (readComplexPlane(i)(graph.a).reciprocal * niceR(i)))
        }
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))

  }

  /** Test combining two dynamic scalar fields. */
  test("complex vector field / field") {
    def runtest(vectorSize: Int) {
      val Size = 5
      val fieldShape = Shape(Size, Size)
      val vectorShape = Shape(vectorSize)

      val Constant = Complex(3.456f, -0.04987f)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val a = ComplexVectorField.random(fieldShape, vectorShape)
        val b = ComplexVectorField.random(fieldShape, vectorShape)
        val c = ComplexVectorField.random(fieldShape, vectorShape)

        // "nice" inputs are designed to prevent the result value real and
        // imaginary components from being near 0, as those can fail the high
        // approximate-equals test due to high relative error.

        val niceA = ComplexVectorField.random(fieldShape, vectorShape) + Complex(0.5f, 0.5f)
        val niceB = ComplexVectorField.random(fieldShape, vectorShape) * 0.1f + Complex(1.0f, 0.1f)
        val niceC = ComplexVectorField.random(fieldShape, vectorShape) * 0.1f + Complex(1.0f, 0.1f)

        val sum = a + b
        val diff = a - b
        val product = niceA * niceB
        val quotient = niceA / niceB
        val multiPoint = niceA * niceC + b

        /** A 0-dimensional scalar field, can be combined with any other. */

        val c0D = TestComplexField(RefComplexField(Constant))

        val sum0D = a + c0D
        val diff0D = a - c0D
        val product0D = a * c0D
        val quotient0D = a / c0D

        /** The 0D field can appear as the first operand */

        val sum0D2 = c0D + a
        val diff0D2 = c0D - a
        val product0D2 = c0D * a
        val quotient0D2 = c0D / a


        probe(sum, a, b, diff, product, niceA, niceB, quotient, multiPoint, niceC, sum0D, diff0D, product0D, quotient0D,
          sum0D2, diff0D2, product0D2, quotient0D2)
      }

      import graph._
      withRelease {
        step

        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(sum) ==
                  (readComplexPlane(i)(graph.a) + readComplexPlane(i)(b)))
          require(readComplexPlane(i)(diff) ==
                  (readComplexPlane(i)(graph.a) - readComplexPlane(i)(b)))
          require(readComplexPlane(i)(product) ~==
                  (readComplexPlane(i)(niceA) :* readComplexPlane(i)(niceB)))
          require(readComplexPlane(i)(quotient) ~==
                  (readComplexPlane(i)(niceA) :/ readComplexPlane(i)(niceB)))
          require(readComplexPlane(i)(multiPoint) ~==
                  ((readComplexPlane(i)(niceA) :* readComplexPlane(i)(niceC)) +
                          readComplexPlane(i)(b)))

          require(readComplexPlane(i)(sum0D) ==
                  (readComplexPlane(i)(graph.a) + Constant))
          require(readComplexPlane(i)(diff0D) ==
                  (readComplexPlane(i)(graph.a) - Constant))
          require(readComplexPlane(i)(product0D) ~==
                  (readComplexPlane(i)(graph.a) * Constant))
          require(readComplexPlane(i)(quotient0D) ~==
                  (readComplexPlane(i)(graph.a) / Constant))

          require(readComplexPlane(i)(sum0D2) ==
                  (readComplexPlane(i)(graph.a) + Constant))
          require(readComplexPlane(i)(diff0D2) ==
                  (readComplexPlane(i)(graph.a) * (-1f) + Constant))
          require(readComplexPlane(i)(product0D2) ~==
                  (readComplexPlane(i)(graph.a) * Constant))
          require(readComplexPlane(i)(quotient0D2) ~==
                  (readComplexPlane(i)(graph.a).reciprocal * Constant))
        }
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test applying unary operations on dynamic scalar fields. */
  test("complex vector field / unary") {
    def runtest(vectorSize: Int) {
      val Size = 5
      val fieldShape = Shape(Size, Size)
      val vectorShape = Shape(vectorSize)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val a = ComplexVectorField.random(fieldShape, vectorShape)
        val b = ComplexVectorField.random(fieldShape, vectorShape)
        // exp test gets its own tweeked input, since values near the origin
        // may have a large relative error when exponentiated
        val expIn = ComplexVectorField.random(fieldShape, vectorShape) +
                Complex(0.01f, 0.01f)
        val phase_ = phase(a)
        val magnitude_ = magnitude(a)
        val realPartA = realPart(a)
        val imagPartA = imaginaryPart(a)
        val expp = exp(expIn)
        val conj = conjugate(a)
        val negative = -a
        val mergerTest = realPart(b) + imaginaryPart(b)

        probe(phase_, a, magnitude_, realPartA, imagPartA, expp, expIn, conj, negative, mergerTest, b)
      }
      //val Subsize = (Size + 1) / 2
      //val subsample = a.subsample

      import graph._
      withRelease {
        step

        for (i <- 0 until vectorSize) {
          require(readVector(phase_).sliceTensor(i) ~==
                  readComplexPlane(i)(graph.a).mapToReal(_.phase))
          require(readVector(magnitude_).sliceTensor(i) ~==
                  readComplexPlane(i)(graph.a).mapToReal(_.magnitude))
          require(readVector(graph.realPartA).sliceTensor(i) ==
                  readComplexPlane(i)(graph.a).realPart)
          require(readVector(imagPartA).sliceTensor(i) ==
                  readComplexPlane(i)(graph.a).imaginaryPart)
          require(readComplexPlane(i)(graph.expp) ~==
                  readComplexPlane(i)(graph.expIn).map(_.exp))
          require(readComplexPlane(i)(conj) ==
                  readComplexPlane(i)(graph.a).map(_.conjugate))
          require(readComplexPlane(i)(negative) ==
                  readComplexPlane(i)(graph.a) * -1f)
          require(readVector(mergerTest).sliceTensor(i) ==
                  readComplexPlane(i)(b).realPart +
                          readComplexPlane(i)(b).imaginaryPart)
        }
      }
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
  test("complex vector field / fft") {
    def runtest1D(columns: Int, vectorSize: Int) {
      val fieldVals = Array.tabulate[ComplexVector](columns) {
        (c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(columns,
        (c) => fieldVals(c).real)
      val fieldImaginary = RefVectorField(columns,
        (c) => fieldVals(c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 1D
        val space = ComplexVectorField(columns, c => fieldVals(c))
        val recovered = fftInverse(fft(space))

        probe(recovered)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(recovered) ~== fieldReal)
        require(readImaginaryVector(recovered) ~== fieldImaginary)
      }
    }

    def runtest2D(rows: Int, columns: Int, vectorSize: Int) {
      val fieldVals = Array2D.tabulate[ComplexVector](rows, columns) {
        (r,c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(rows, columns,
        (r,c) => fieldVals(r,c).real)
      val fieldImaginary = RefVectorField(rows, columns,
        (r,c) => fieldVals(r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 2D
        val space = ComplexVectorField(rows, columns, (r,c) => fieldVals(r,c))
        val recovered = fftInverse(fft(space))

        probe(recovered)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(recovered) ~== fieldReal)
        require(readImaginaryVector(recovered) ~== fieldImaginary)
      }
    }


    def runtest3D(layers: Int, rows: Int, columns: Int, vectorSize: Int) {
      val fieldVals = Array.tabulate[ComplexVector](layers, rows, columns) {
        (l, r,c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(layers, rows, columns,
        (l,r,c) => fieldVals(l)(r)(c).real)
      val fieldImaginary = RefVectorField(layers, rows, columns,
        (l,r,c) => fieldVals(l)(r)(c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 3D
        val space = ComplexVectorField(layers, rows, columns, (l,r,c) => fieldVals(l)(r)(c))
        val recovered = fftInverse(fft(space))

        probe(recovered)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(recovered) ~== fieldReal)
        require(readImaginaryVector(recovered) ~== fieldImaginary)
      }
    }


    def runtest(vectorSize: Int) {
      runtest1D(16, vectorSize)
      runtest2D(8, 32, vectorSize)
      runtest2D(2, 1, vectorSize)
      runtest3D(4, 1, 64, vectorSize)
    }

    val vectorSizes = Seq(2,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("complex vector field / fftRI (split real/imaginary)") {
    def runtest1D(columns: Int, vectorSize: Int) {
      val fieldVals = Array.tabulate[ComplexVector](columns) {
        (c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(columns,
        (c) => fieldVals(c).real)
      val fieldImaginary = RefVectorField(columns,
        (c) => fieldVals(c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 1D
        val space = ComplexVectorField(columns, c => fieldVals(c))
        val spaceR = TestVectorField(fieldReal)
        val spaceI = TestVectorField(fieldImaginary)
        val (freqR, freqI) = fftRI(spaceR, spaceI)
        val (recoveredR, recoveredI) = fftInverseRI(freqR, freqI)
        val expected = fftInverse(fft(space))
        val recovered = fftInverse(fft(space))

        probe(expected, recoveredR, recoveredI)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(expected) ~== fieldReal)
        require(readImaginaryVector(expected) ~== fieldImaginary)
        require(readVector(recoveredR) ~== fieldReal)
        require(readVector(recoveredI) ~== fieldImaginary)
      }
    }

    def runtest2D(rows: Int, columns: Int, vectorSize: Int) {
      val fieldVals = Array2D.tabulate[ComplexVector](rows, columns) {
        (r,c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(rows, columns,
        (r,c) => fieldVals(r,c).real)
      val fieldImaginary = RefVectorField(rows, columns,
        (r,c) => fieldVals(r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 2D
        val space = ComplexVectorField(rows, columns, (r,c) => fieldVals(r,c))
        val spaceR = TestVectorField(fieldReal)
        val spaceI = TestVectorField(fieldImaginary)
        val (freqR, freqI) = fftRI(spaceR, spaceI)
        val (recoveredR, recoveredI) = fftInverseRI(freqR, freqI)
        val expected = fftInverse(fft(space))
        val recovered = fftInverse(fft(space))

        probe(expected, recoveredR, recoveredI)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(expected) ~== fieldReal)
        require(readImaginaryVector(expected) ~== fieldImaginary)
        require(readVector(recoveredR) ~== fieldReal)
        require(readVector(recoveredI) ~== fieldImaginary)
      }
    }

    def runtest3D(layers: Int, rows: Int, columns: Int, vectorSize: Int) {
      val fieldVals = Array.tabulate[ComplexVector](layers, rows, columns) {
        (l, r,c) => ComplexVector.random(vectorSize) + Complex(0.1f, 0.1f)
      }
      val fieldReal = RefVectorField(layers, rows, columns,
        (l,r,c) => fieldVals(l)(r)(c).real)
      val fieldImaginary = RefVectorField(layers, rows, columns,
        (l,r,c) => fieldVals(l)(r)(c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 3D
        val space = ComplexVectorField(layers, rows, columns, (l,r,c) => fieldVals(l)(r)(c))
        val spaceR = TestVectorField(fieldReal)
        val spaceI = TestVectorField(fieldImaginary)
        val (freqR, freqI) = fftRI(spaceR, spaceI)
        val (recoveredR, recoveredI) = fftInverseRI(freqR, freqI)
        val expected = fftInverse(fft(space))
        val recovered = fftInverse(fft(space))

        probe(expected, recoveredR, recoveredI)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(expected) ~== fieldReal)
        require(readImaginaryVector(expected) ~== fieldImaginary)
        require(readVector(recoveredR) ~== fieldReal)
        require(readVector(recoveredI) ~== fieldImaginary)
      }
    }

    def runtest(vectorSize: Int) {
      runtest1D(1024, vectorSize)
      runtest2D(2, 512, vectorSize)
      runtest2D(1, 1, vectorSize)
      runtest3D(4, 2, 8, vectorSize)
    }

    val vectorSizes = Seq(2,5)

    vectorSizes.foreach(t => runtest(t))
  }

  test("complex vector field / real-convolve-complex") {
    def runtest(vectorSize: Int) {
      val ImageSize = 21
      val FilterSize = 5

      val imageVals = Array2D.tabulate[Vector](ImageSize, ImageSize) {
        (r,c) => Vector.random(vectorSize)
      }
      val filterVals = Array2D.tabulate[ComplexVector](FilterSize, FilterSize) {
        (r,c) => ComplexVector.random(vectorSize)
      }

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inputRealField = VectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c))
        val filterRealField = VectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c).real)
        val filterImagField = VectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c).imaginary)
        val realReal = convolve(inputRealField, filterRealField, BorderClamp)
        val realImag = convolve(inputRealField, filterImagField, BorderClamp)
        val expected = complex(realReal, realImag)

        val filterComplex = ComplexVectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c))

        val outField = convolve(inputRealField, filterComplex, BorderClamp)

        probe(expected, outField)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(expected) ~== readComplexPlane(i)(outField))
        }
      }
    }
    val vectorSizes = Seq(3,7)

    vectorSizes.foreach(t => runtest(t))
  }

  test("complex vector field / complex-convolve-real") {
    def runtest(vectorSize: Int) {
      val ImageSize = 21
      val FilterSize = 5

      val imageVals = Array2D.tabulate[ComplexVector](ImageSize, ImageSize) {
        (r,c) => ComplexVector.random(vectorSize)
      }
      val filterVals = Array2D.tabulate[Vector](FilterSize, FilterSize) {
        (r,c) => Vector.random(vectorSize)
      }

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inputRealField = VectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c).real)
        val inputImagField = VectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c).imaginary)
        val filterRealField = VectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c))
        val realReal = convolve(inputRealField, filterRealField, BorderClamp)
        val imagReal = convolve(inputImagField, filterRealField, BorderClamp)
        val expected = complex(realReal, imagReal)

        val inputComplex = ComplexVectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c))

        val outField = convolve(inputComplex, filterRealField, BorderClamp)

        probe(expected, outField)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(expected) ~== readComplexPlane(i)(outField))
        }
      }
    }
    val vectorSizes = Seq(4,6)

    vectorSizes.foreach(t => runtest(t))
  }

  test("complex vector field / complex-convolve-complex") {
    def runtest(vectorSize: Int) {
      val ImageSize = 21
      val FilterSize = 5

      // Offsets are added to help with relative error in ~== .  If the result
      // of the convolution is near 0, then this can be a problem.
      val imageVals = Array2D.tabulate[ComplexVector](ImageSize, ImageSize) {
        (r,c) => ComplexVector.random(vectorSize) + Complex(0.5f, 0.1f)
      }
      val filterVals = Array2D.tabulate[ComplexVector](FilterSize, FilterSize) {
        (r,c) => ComplexVector.random(vectorSize) / (FilterSize*FilterSize) + Complex(0.1f, 0.01f)
      }

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inputRealField = VectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c).real)
        val inputImagField = VectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c).imaginary)
        val filterRealField = VectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c).real)
        val filterImagField = VectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c).imaginary)
        val realReal = convolve(inputRealField, filterRealField, BorderClamp)
        val realImag = convolve(inputRealField, filterImagField, BorderClamp)
        val imagReal = convolve(inputImagField, filterRealField, BorderClamp)
        val imagImag = convolve(inputImagField, filterImagField, BorderClamp)
        val expected = complex(realReal - imagImag, realImag + imagReal)

        val inputComplex = ComplexVectorField(ImageSize, ImageSize, (r,c) => imageVals(r,c))
        val filterComplex = ComplexVectorField(FilterSize, FilterSize, (r,c) => filterVals(r,c))

        val outField = convolve(inputComplex, filterComplex, BorderClamp)

        probe(expected, outField)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize) {
          if (!(readComplexPlane(i)(expected) ~== readComplexPlane(i)(outField))) {
            println("******************************* planes = " + vectorSize + ", plane = " + i)
            readComplexPlane(i)(expected).print
            println("*******************************")
            readComplexPlane(i)(outField).print
          }
          require(readComplexPlane(i)(expected) ~== readComplexPlane(i)(outField))
        }
      }
    }
    val vectorSizes = Seq(2,9)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the (Int) operator. */
  test("complex vector field / slice") {
    def runtest(vectorSize: Int) {
      val Size = 5

      def fieldVal(row: Int, col: Int) = ComplexVector(vectorSize,
        i => Complex(row + i * 10, col - i * 10))

      val inputReal = RefVectorField(Size, Size,
        (r, c) => fieldVal(r,c).real)

      val inputImaginary = RefVectorField(Size, Size,
        (r, c) => fieldVal(r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val field1 = ComplexVectorField(Size, Size, (r,c) => fieldVal(r,c))
        val row0 = field1(0)
        val row1 = field1(1)
        val row2 = field1(2)

        probe(row0, row1, row2)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(row0) == inputReal.slice(0))
        require(readRealVector(row1) == inputReal.slice(1))
        require(readRealVector(row2) == inputReal.slice(2))

        require(readImaginaryVector(row0) == inputImaginary.slice(0))
        require(readImaginaryVector(row1) == inputImaginary.slice(1))
        require(readImaginaryVector(row2) == inputImaginary.slice(2))

      }

    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the (dynamic Int) operator. */
  test("complex vector field / slice point") {
    def runtest(vectorSize: Int) {
      val Size = 5

      def fieldVal(row: Int, col: Int) = ComplexVector(vectorSize,
        i => Complex(row + i * 10, col - i * 10))

      val inputReal = RefVectorField(Size, Size,
        (r, c) => fieldVal(r,c).real)

      val inputImaginary = RefVectorField(Size, Size,
        (r, c) => fieldVal(r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val index0 = ScalarField(0.5f)
        val index1 = index0 + 1.1f
        val index2 = index1 + 1.1f
        val field1 = ComplexVectorField(Size, Size, (r,c) => fieldVal(r,c))
        val row0 = field1(index0)
        val row1 = field1(index1)
        val row2 = field1(index2)

        probe(row0, row1, row2)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(row0) == inputReal.slice(0))
        require(readRealVector(row1) == inputReal.slice(1))
        require(readRealVector(row2) == inputReal.slice(2))

        require(readImaginaryVector(row0) == inputImaginary.slice(0))
        require(readImaginaryVector(row1) == inputImaginary.slice(1))
        require(readImaginaryVector(row2) == inputImaginary.slice(2))

      }

    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the stack operator. */
  test("complex vector field / stack") {
    def runtest(vectorSize: Int) {
      def fieldVal(layer: Int, row: Int, col: Int) = ComplexVector(vectorSize,
        i => Complex(layer + row * 10 + i * 100, layer - col * 10 - i * 100))

      val Layers = 3
      val Rows = 5
      val Columns = 7

      val outputReal = RefVectorField(Layers, Rows, Columns,
        (l,r,c) => fieldVal(l,r,c).real)

      val outputImaginary = RefVectorField(Layers, Rows, Columns,
        (l,r,c) => fieldVal(l,r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val f0 = ComplexVectorField(Rows, Columns, (r, c) => fieldVal(0, r, c))
        val f1 = ComplexVectorField(Rows, Columns, (r, c) => fieldVal(1, r, c))
        val f2 = ComplexVectorField(Rows, Columns, (r, c) => fieldVal(2, r, c))
        val fArray = Array(f0, f1, f2)

        val stack0 = stack(f0, f1, f2)
        val stack1 = stack(fArray)

        probe(stack0, stack1)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(stack0) == outputReal)
        require(readRealVector(stack1) == outputReal)
        require(readImaginaryVector(stack0) == outputImaginary)
        require(readImaginaryVector(stack1) == outputImaginary)
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }


  /** Test the trim and cog-private trimToReal operators. */
  test("complex vector field / trim") {
    def runtest(vectorSize: Int) {
      val InRows = 7
      val InCols = 6
      val trimmedRows = 5
      val trimmedCols = 4

      val fieldVals = Array2D.tabulate[ComplexVector](InRows, InCols) {
        (r,c) => ComplexVector.random(vectorSize)
      }

      val outShape = Shape(trimmedRows, trimmedCols)


      val outputReal = RefVectorField(trimmedRows, trimmedCols,
        (r,c) => fieldVals(r,c).real)

      val outputImaginary = RefVectorField(trimmedRows, trimmedCols,
        (r,c) => fieldVals(r,c).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        // 3 copies of input to keep outputs from being optimized away
        // (no probe support yet).
        val inDynamicField =
          ComplexVectorField(InRows, InCols, (r,c) => fieldVals(r,c))
        val inDynamicField2 =
          ComplexVectorField(InRows, InCols, (r,c) => fieldVals(r,c))
        val inDynamicField3 =
          ComplexVectorField(InRows, InCols, (r,c) => fieldVals(r,c))

        val trimmedComplexDynamicField = trim(inDynamicField, outShape)
        val autoTrimmedOut = ComplexVectorField(outShape, Shape(vectorSize))
        autoTrimmedOut <== inDynamicField2

        // Support for "trimToReal" is only within cog package for this testing.
        val trimmedRealDynamicField =
          trimToReal(inDynamicField3, Shape(trimmedRows, trimmedCols))

        probe(trimmedComplexDynamicField, autoTrimmedOut, trimmedRealDynamicField)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(trimmedComplexDynamicField) == outputReal)
        require(readImaginaryVector(trimmedComplexDynamicField) == outputImaginary)

        require(readRealVector(autoTrimmedOut) == outputReal)
        require(readImaginaryVector(autoTrimmedOut) == outputImaginary)

        require(readVector(trimmedRealDynamicField) == outputReal)
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the expand operator. */
  test("complex vector field / expand border") {
    def runtest(vectorSize: Int) {
      val InRows = 13
      val InCols = 21
      val ExpandedRows = 18
      val ExpandedCols = 33

      val inFieldVals = Array2D.tabulate[ComplexVector](InRows, InCols) {
        (r,c) => ComplexVector.random(vectorSize)
      }

      /** Perform zero-border padding */
      val zero = new ComplexVector(vectorSize)
      def outFieldValsBorderZero(row: Int, col: Int) = {
        if (row < 0 || row >= InRows || col < 0 || col >= InCols)
          zero
        else
          inFieldVals(row, col)
      }
      /** Perform nearest-border padding */
      def outFieldValsBorderClamp(row: Int, col: Int) = {
        val rowApron = (ExpandedRows - InRows)/2
        val inRangeRow =
          if (row >= InRows + rowApron)
            0
          else if (row >= InRows)
            InRows - 1
          else
            row
        val colApron = (ExpandedCols - InCols)/2
        val inRangeCol =
          if (col >= InCols + colApron)
            0
          else if (col >= InCols)
            InCols - 1
          else
            col
        inFieldVals(inRangeRow, inRangeCol)
      }


      val outputBorderZeroReal = RefVectorField(ExpandedRows, ExpandedCols,
        (r,c) => outFieldValsBorderZero(r,c).real)
      val outputBorderZeroImaginary = RefVectorField(ExpandedRows, ExpandedCols,
        (r,c) => outFieldValsBorderZero(r,c).imaginary)

      val outputBorderClampReal = RefVectorField(ExpandedRows, ExpandedCols,
        (r,c) => outFieldValsBorderClamp(r,c).real)
      val outputBorderClampImaginary = RefVectorField(ExpandedRows, ExpandedCols,
        (r,c) => outFieldValsBorderClamp(r,c).imaginary)

      val zeroField = RefVectorField(ExpandedRows, ExpandedCols, Shape(vectorSize))

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inField =
          ComplexVectorField(InRows, InCols, (r,c) => inFieldVals(r,c))
        val inFieldRealPart =
          VectorField(InRows, InCols, (r,c) => inFieldVals(r,c).real)
        val expandedField =
          expand(inField, BorderClamp, ExpandedRows, ExpandedCols)
        val expandedZeroFilledField =
          expand(inField, BorderZero, ExpandedRows, ExpandedCols)
        // Support for "realToComplexExpandBorderFill" is only within the cog
        // package for this testing.
        val expandedFromRealField =
          realToComplexExpandBorderFill(inFieldRealPart, ExpandedRows, ExpandedCols)

        probe(expandedField, expandedZeroFilledField, expandedFromRealField)
      }

      import graph._
      withRelease {
        step

        require(readRealVector(expandedField) ==
                outputBorderClampReal)
        require(readImaginaryVector(expandedField) ==
                outputBorderClampImaginary)

        require(readRealVector(expandedZeroFilledField) ==
                outputBorderZeroReal)
        require(readImaginaryVector(expandedZeroFilledField) ==
                outputBorderZeroImaginary)

        require(readRealVector(expandedFromRealField) ==
                outputBorderClampReal)
        require(readImaginaryVector(expandedFromRealField) ==
                zeroField)

      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the tensors(Int) operator. This should technically be in the
   * ScalarFieldGeneratorSpec, since the output is a ScalarField.  XXX */
  test("complex vector field / tensor slice") {
    def runtest(vectorSize: Int) {
      val Rows = 3
      val Columns = 3

      def fieldVal(row: Int, col: Int) = ComplexVector(vectorSize,
        i => Complex(row + i * 10, col - i * 10))

      val expect = Array.tabulate(vectorSize) { i =>
        RefComplexField(Rows, Columns, (row, col) => fieldVal(row, col)(i))
      }

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val input = ComplexVectorField(Rows, Columns, (r,c) => fieldVal(r,c))
        val slices = Array.tabulate(vectorSize) { i => vectorElement(input, i) }

        probe(slices: _*)
      }

      import graph._
      withRelease {
        step

        for (i <- 0 until vectorSize)
          require(readComplex(slices(i)) == expect(i))
      }

    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the tensor reduction operators. */
  test("complex vector field / tensor reduce sum") {
    def runtest(vectorSize: Int) {
      val Rows = 3
      val Columns = 3

      val fieldShape = Shape(Rows, Columns)
      val vectorShape = Shape(vectorSize)

      val realPart = RefVectorField.random(fieldShape, vectorShape)
      val imaginaryPart = RefVectorField.random(fieldShape, vectorShape)

      val expectedReducedSum =
        RefComplexField(Rows, Columns,
        (row, col) =>
          Complex(realPart.read(row, col).toArray.reduceLeft(_ + _),
             imaginaryPart.read(row, col).toArray.reduceLeft(_ + _)
        ))

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val realField = TestVectorField(realPart)
        val imaginaryField = TestVectorField(imaginaryPart)
        val input = complex(realField, imaginaryField)
        val output = reduceSum(input)

        probe(output)
      }
      import graph._
      withRelease {
        step
        require(readComplex(output) ~== expectedReducedSum)
      }
    }

    val vectorSizes = Seq(1,2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test the block tensor reduction operators. */
  test("complex vector field / block tensor reduce") {
    def runtest(vectorSize: Int) {
      val Rows = 3
      val Columns = 3

      // Test reduction factors of 2, 3, 4 and 5

      val factor = vectorSize
      val inVectorSize = vectorSize * factor

      val fieldShape = Shape(Rows, Columns)
      val inVectorShape = Shape(inVectorSize)

      val realPart = RefVectorField.random(fieldShape, inVectorShape)
      val imaginaryPart = RefVectorField.random(fieldShape, inVectorShape)

      def expectedBlockReducedSum(plane: Int) = RefComplexField(Rows, Columns,
        (row, col) => Complex(
          realPart.read(row, col).
                  subvector(plane*factor until (plane+1)*factor).toArray.reduceLeft(_ + _),
          imaginaryPart.read(row, col).
                  subvector(plane*factor until (plane+1)*factor).toArray.reduceLeft(_ + _)
        )
      )

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val realField = TestVectorField(realPart)
        val imaginaryField = TestVectorField(imaginaryPart)
        val input = complex(realField, imaginaryField)
        val output = blockReduceSum(input, factor)

        probe(output)
      }

      import graph._
      withRelease {
        step
        for (i <- 0 until vectorSize)
          require(readComplexPlane(i)(output) ~== expectedBlockReducedSum(i))
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }


  /** Test commutative property of some operators on complex vector fields */
  test("complex vector field / commutative") {
    def runtest(vectorSize: Int) {
      val Size = 10
      val fieldShape = Shape(Size, Size)
      val vectorShape = Shape(vectorSize)
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val field = ComplexVectorField.random(fieldShape, vectorShape)
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
        for (i <- 0 until vectorSize) {
          require(readComplexPlane(i)(plus) ==
                  readComplexPlane(i)(plusReverse))
          require(readComplexPlane(i)(minus) ==
                  readComplexPlane(i)(minusReverse))
          require(readComplexPlane(i)(times) ==
                  readComplexPlane(i)(timesReverse))
        }
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  /** Test construction of complex vector fields from vector fields. */
  test("complex vector field / construction") {
    val rand = new Random()
    def runtest(vectorSize: Int) {
      // Test construction from magnitude and phase parts

      // Generate a distribution of phases with some randomness, but stay away
      // from phases close to the vertical and horizontal, as they may cause
      // the test to fail because of poor relative error in the real/imag parts.
      val NumAngles = 360
      val fieldShape = Shape(NumAngles)
      val vectorShape = Shape(vectorSize)

      val initMagnitudes = Array.tabulate[Vector](NumAngles) {
        (col) => Vector(vectorSize,
          i => 0.001f  + rand.nextFloat
        )
      }
      val initPhases = Array.tabulate[Vector](NumAngles) {
        (col) => Vector(vectorSize,
          i => math.Pi.toFloat * 2f *
                  (col * vectorSize + i) / vectorSize / (NumAngles + 1) +
                   0.001f  + rand.nextFloat * 0.0001f
        )
      }

      val outputReal = RefVectorField(NumAngles,
        (c) => ComplexVector.polar(initMagnitudes(c), initPhases(c)).real)

      val outputImaginary = RefVectorField(NumAngles,
        (c) => ComplexVector.polar(initMagnitudes(c), initPhases(c)).imaginary)

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val realPart = VectorField.random(fieldShape, vectorShape)
        val imaginaryPart = VectorField.random(fieldShape, vectorShape)
        val complex1 = complex(realPart, imaginaryPart)
        val magnitude = VectorField(NumAngles, i => initMagnitudes(i))
        val phase = VectorField(NumAngles, i => initPhases(i))
        val complex2 = polarComplex(magnitude, phase)

        probe(complex1, realPart, imaginaryPart, complex2)
      }

      import graph._
      withRelease {
        step
        require(readRealVector(complex1) ==
                readVector(graph.realPart))
        require(readImaginaryVector(complex1) ==
                readVector(graph.imaginaryPart))

        require(readRealVector(complex2) ~== outputReal)
        require(readImaginaryVector(complex2) ~== outputImaginary)
      }
    }

    val vectorSizes = Seq(2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }

  test("reshape a 2d complex field to a 0d complex vector field and vice versa"){
    val Rows = 4
    val Cols = 3
    val VectorSize = Rows * Cols
    val aRef = Array.tabulate(Rows, Cols)((r,c) => Complex(2*(r*Cols+c), 2*(r*Cols+c)+1))
    val bRef = Array.tabulate(VectorSize)(i => Complex(2*i, 2*i+1))

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ComplexField(Rows,Cols,(r,c)=>aRef(r)(c))
      val bIn = ComplexVectorField(ComplexVector(VectorSize,(i)=>bRef(i)))

      val aReshaped = reshape(aIn, Shape(), Shape(VectorSize))
      val bReshaped = reshape(bIn, Shape(Rows,Cols), Shape())

      probe(bReshaped, aReshaped)
    }

    import graph._
    withRelease {
      step
      require(readComplex(bReshaped) == readComplex(aIn))
      for (i <- 0 until VectorSize) {
        require(readComplexPlane(i)(aReshaped) ==
          readComplexPlane(i)(bIn))
      }
    }
  }

}
