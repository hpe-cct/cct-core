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

/** Test code.
 *
 * @author Greg Snider
 */

@RunWith(classOf[JUnitRunner])
class ComplexFieldSpec
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

  /** Test combining a dynamic scalar field and a constant. */
  test("complex field / constant") {
     val Size = 5
    val C = Complex(1.234f, -0.036912f)
    val R = 3.21f

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestComplexField(RefComplexField.random(Size, Size)) + Complex(0.5f, 0.5f)
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

      require(readComplex(sum) == (readComplex(graph.a) + C))
      require(readComplex(diff) == (readComplex(graph.a) - C))
      require(readComplex(product) ~== (readComplex(graph.a) * C))
      require(readComplex(quotient) ~== (readComplex(graph.a) / C))
      require(readComplex(sumR) == (readComplex(graph.a) + R))
      require(readComplex(diffR) == (readComplex(graph.a) - R))
      require(readComplex(productR) ~== (readComplex(graph.a) * R))
      require(readComplex(quotientR) ~== (readComplex(graph.a) / R))

      require(readComplex(constSum) == (readComplex(graph.a) + C))
      require(readComplex(constDiff) == (-readComplex(graph.a) + C))
      require(readComplex(constProduct) ~== (readComplex(graph.a) * C))
      require(readComplex(constQuotient) ~== (readComplex(graph.a).reciprocal * C))
      require(readComplex(constSumR) == (readComplex(graph.a) + R))
      require(readComplex(constDiffR) == (-readComplex(graph.a) + R))
      require(readComplex(constProductR) ~== (readComplex(graph.a) * R))
      require(readComplex(constQuotientR) ~== (readComplex(graph.a).reciprocal * R))
    }
  }

  /** Test combining two dynamic scalar fields. */
  test("complex field / field") {
     val Size = 5

    val Constant = Complex(3.456f, -0.51234f)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestComplexField(RefComplexField.random(Size, Size))
      val b = TestComplexField(RefComplexField.random(Size, Size))
      val c = TestComplexField(RefComplexField.random(Size, Size))

      // "nice" inputs are designed to prevent the result value real and
      // imaginary components from being near 0, as those can fail the high
      // approximate-equals test due to high relative error.

      val niceA = TestComplexField(RefComplexField.random(Size, Size))
      val niceB = TestComplexField(RefComplexField.random(Size, Size))
      val niceC = TestComplexField(RefComplexField.random(Size, Size))

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

      require(readComplex(sum) == (readComplex(graph.a) + readComplex(b)))
      require(readComplex(diff) == (readComplex(graph.a) - readComplex(b)))
      require(readComplex(product) ~== (readComplex(niceA) :* readComplex(niceB)))
      require(readComplex(quotient) ~== (readComplex(niceA) :/ readComplex(niceB)))
      require(readComplex(multiPoint) ~== ((readComplex(niceA) :* readComplex(niceC)) + readComplex(b)))

      require(readComplex(sum0D) == (readComplex(graph.a) + Constant))
      require(readComplex(diff0D) == (readComplex(graph.a) - Constant))
      require(readComplex(product0D) ~== (readComplex(graph.a) * Constant))
      require(readComplex(quotient0D) ~== (readComplex(graph.a) / Constant))

      require(readComplex(sum0D2) == (readComplex(graph.a) + Constant))
      require(readComplex(diff0D2) == (readComplex(graph.a) * (-1f) + Constant))
      require(readComplex(product0D2) ~== (readComplex(graph.a) * Constant))
      require(readComplex(quotient0D2) ~== (readComplex(graph.a).reciprocal * Constant))
    }
  }

  /** Test applying unary operations on dynamic scalar fields. */
  test("complex field / unary") {
     val Size = 5

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestComplexField(RefComplexField.random(Size, Size))
      val b = TestComplexField(RefComplexField.random(Size, Size))
      // exp test gets its own tweeked input, since values near the origin
      // may have a large relative error when exponentiated
      val expIn = TestComplexField(RefComplexField.random(Size, Size)) +
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

      require(readScalar(phase_) ~== readComplex(graph.a).mapToReal(_.phase))
      require(readScalar(magnitude_) ~== readComplex(graph.a).mapToReal(_.magnitude))
      require(readScalar(graph.realPartA) == readComplex(graph.a).realPart)
      require(readScalar(imagPartA) == readComplex(graph.a).imaginaryPart)
      require(readComplex(graph.expp) ~== readComplex(expIn).map(_.exp))
      require(readComplex(conj) == readComplex(graph.a).map(_.conjugate))
      require(readComplex(negative) == readComplex(graph.a) * -1f)
      require(readScalar(mergerTest) == readComplex(b).realPart + readComplex(b).imaginaryPart)
    }
  }

  test("complex field / complex-convolve-real") {
     val imageReal = Matrix(
      Array(0f, 0f, 0f),
      Array(1f, 1f, 1f),
      Array(2f, 2f, 2f)
    )
    val imageImaginary = Matrix(
      Array(0f, 1f, 2f),
      Array(0f, 1f, 2f),
      Array(0f, 1f, 2f)
    )
    val image = new ComplexMatrix(imageReal, imageImaginary)
    // Simple filter takes average of 4 neighbors
    val filter = Matrix(
      Array(0.00f, 0.25f, 0.00f),
      Array(0.25f, 0.00f, 0.25f),
      Array(0.00f, 0.25f, 0.00f)
    )

    val expectedRealBorderConvolve = Matrix(
      Array(0.25f, 0.25f, 0.25f),
      Array(1.00f, 1.00f, 1.00f),
      Array(1.75f, 1.75f, 1.75f)
    )

    val expectedImaginaryBorderConvolve = Matrix(
      Array(0.25f, 1.00f, 1.75f),
      Array(0.25f, 1.00f, 1.75f),
      Array(0.25f, 1.00f, 1.75f)
    )

    val expectedBorderConvolve =
      new ComplexMatrix(expectedRealBorderConvolve, expectedImaginaryBorderConvolve)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestComplexField(RefComplexField(image))
      val expected = convolve(field1, TestScalarField(filter), BorderClamp)

      probe(expected)
    }

    import graph._
    withRelease {
      step
      require(readComplex(expected) ~== RefComplexField(expectedBorderConvolve))
    }
  }

  test("complex field / real-convolve-complex") {
    val image = Matrix(
      Array(1f, 2f, 3f),
      Array(4f, 5f, 6f),
      Array(7f, 8f, 9f)
    )
    val filterReal = Matrix(
      Array(0.0f, 0.0f, 0.0f),
      Array(0.5f, 0.0f, 0.5f),
      Array(0.0f, 0.0f, 0.0f)
    )
    val filterImaginary = Matrix(
      Array(0.0f, 0.5f, 0.0f),
      Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.5f, 0.0f)
    )
    val filter = new ComplexMatrix(filterReal, filterImaginary)

    val expectedRealBorderConvolve = Matrix(
      Array(1.5f, 2.0f, 2.5f),
      Array(4.5f, 5.0f, 5.5f),
      Array(7.5f, 8.0f, 8.5f)
    )

    val expectedImaginaryBorderConvolve = Matrix(
      Array(2.5f, 3.5f, 4.5f),
      Array(4.0f, 5.0f, 6.0f),
      Array(5.5f, 6.5f, 7.5f)
    )

    val expectedBorderConvolve =
      new ComplexMatrix(expectedRealBorderConvolve, expectedImaginaryBorderConvolve)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestScalarField(RefScalarField(image))
      val expected =
        convolve(field1, TestComplexField(RefComplexField(filter)), BorderClamp)

      probe(expected)
    }

    import graph._
    withRelease {
      step
      require(readComplex(expected) ~== RefComplexField(expectedBorderConvolve))
    }
  }

  test("complex field / complex-convolve-complex") {
    val InputSize = 5
    val inputReal = Matrix.random(InputSize, InputSize)
    val inputImag = Matrix.random(InputSize, InputSize)

    val filterReal = Matrix(
      Array(0.0f, 0.0f, 0.0f),
      Array(0.5f, 0.0f, 0.5f),
      Array(0.0f, 0.0f, 0.0f)
    )
    val filterImag = Matrix(
      Array(0.0f, 0.5f, 0.0f),
      Array(0.0f, 0.0f, 0.0f),
      Array(0.0f, 0.5f, 0.0f)
    )
    val input = new ComplexMatrix(inputReal, inputImag)
    val filter = new ComplexMatrix(filterReal, filterImag)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputRealField = TestScalarField(inputReal)
      val inputImagField = TestScalarField(inputImag)
      val filterRealField = TestScalarField(filterReal)
      val filterImagField = TestScalarField(filterImag)
      val realReal = convolve(inputRealField, filterRealField, BorderClamp)
      val realImag = convolve(inputRealField, filterImagField, BorderClamp)
      val imagReal = convolve(inputImagField, filterRealField, BorderClamp)
      val imagImag = convolve(inputImagField, filterImagField, BorderClamp)
      val expected = complex(realReal - imagImag, realImag + imagReal)

      val inputComplex = TestComplexField(RefComplexField(input))
      val filterComplex = TestComplexField(RefComplexField(filter))

      val outField = convolve(inputComplex, filterComplex, BorderClamp)

      probe(expected, outField)
    }

    import graph._
    withRelease {
      step
      require(readComplex(expected) ~== readComplex(outField))
    }
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("complex field / fft") {
    // Offsets added to help relative error of ~==
    val field1 = RefComplexField.random(16) + Complex(0.1f, 0.1f)
    val field2 = RefComplexField.random(32, 32) + Complex(0.1f, 0.1f)
    val field3 = RefComplexField.random(16, 16, 16) + Complex(0.1f, 0.1f)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // 1D
      val space1 = TestComplexField(field1)
      val recovered1 = fftInverse(fft(space1))

      // 2D
      val space2 = TestComplexField(field2)
      val recovered2 = fftInverse(fft(space2))

      // 3D
      val space3 = TestComplexField(field3)
      val recovered3 = fftInverse(fft(space3))

      probe(recovered1, recovered2, recovered3)
    }

    import graph._
    withRelease {
      step

      require(readComplex(recovered1) ~== field1)
      require(readComplex(recovered2) ~== field2)
      require(readComplex(recovered3) ~== field3)
    }
  }

  /** Test FFT: 1D, 2D, 3D.
   *
   * Note: these tests merely test that the inverse FFT undoes what the
   * forward FFT does, not that the FFT actually calculates the frequency-
   * space version of its input.   XXX
   * */
  test("complex field / fftRI (split real/imaginary)") {
    // Offsets added to help relative error of ~==
    val field1 = RefComplexField.random(16) + Complex(0.1f, 0.1f)
    val field2 = RefComplexField.random(32, 32) + Complex(0.1f, 0.1f)
    val field3 = RefComplexField.random(16, 16, 16) + Complex(0.1f, 0.1f)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // 1D
      val space1 = TestComplexField(field1)
      val spaceR1 = realPart(space1)
      val spaceI1 = imaginaryPart(space1)
      val (freqR1, freqI1) = fftRI(spaceR1, spaceI1)
      val (recoveredR1, recoveredI1) = fftInverseRI(freqR1, freqI1)
      val recovered1 = complex(recoveredR1, recoveredI1)
      val expected1 = fftInverse(fft(space1))

      // 2D
      val space2 = TestComplexField(field2)
      val spaceR2 = realPart(space2)
      val spaceI2 = imaginaryPart(space2)
      val (freqR2, freqI2) = fftRI(spaceR2, spaceI2)
      val (recoveredR2, recoveredI2) = fftInverseRI(freqR2, freqI2)
      val recovered2 = complex(recoveredR2, recoveredI2)
      val expected2 = fftInverse(fft(space2))

      // 3D
      val space3 = TestComplexField(field3)
      val spaceR3 = realPart(space3)
      val spaceI3 = imaginaryPart(space3)
      val (freqR3, freqI3) = fftRI(spaceR3, spaceI3)
      val (recoveredR3, recoveredI3) = fftInverseRI(freqR3, freqI3)
      val recovered3 = complex(recoveredR3, recoveredI3)
      val expected3 = fftInverse(fft(space3))

      probe(recovered1, expected1, recovered2, expected2, recovered3, expected3)
    }

    import graph._
    withRelease {
      step

      require(readComplex(recovered1) ~== field1)
      require(readComplex(recovered1) ~== readComplex(expected1))
      require(readComplex(recovered2) ~== field2)
      require(readComplex(recovered2) ~== readComplex(expected2))
      require(readComplex(recovered3) ~== field3)
      require(readComplex(recovered3) ~== readComplex(expected3))
    }
  }

  /** Test the reshape operator. */
  test("complex field / reshape") {

    val InRows = 10
    val InCols = 12
    val InElements = InRows * InCols
    val OutRows = 6
    val OutCols = InElements / OutRows


    val matrix = ComplexMatrix.random(InRows, InCols)
    val reshapedMatrix = ComplexMatrix(OutRows, OutCols, (r,c) => {
      val elementIndex = r * OutCols + c
      val inRow = elementIndex / InCols
      val inCol = elementIndex % InCols
      matrix(inRow, inCol)
    })

    val field1 = RefComplexField(InRows, InCols)
    val field2 = RefComplexField(OutRows, OutCols, (r,c) => {
      val elementIndex = r * OutCols + c
      val inRow = elementIndex / InCols
      val inCol = elementIndex % InCols
      field1.read(inRow, inCol)
    })

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in = TestComplexField(field1)
      val reshaped = reshape(in, OutRows, OutCols)

      probe(reshaped)
    }

    import graph._
    withRelease {
      step
      require(readComplex(reshaped) == field2)
    }
  }

  /** Test the (Int) operator. */
  test("complex field / slice") {
    val image = new ComplexMatrix(3, 3,
                                  (row, col) => Complex(row, col))
    val expect0 = RefComplexField(3, (col) => Complex(0, col))
    val expect1 = RefComplexField(3, (col) => Complex(1, col))
    val expect2 = RefComplexField(3, (col) => Complex(2, col))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1 = TestComplexField(RefComplexField(image))
      val row0 = field1(0)
      val row1 = field1(1)
      val row2 = field1(2)

      probe(row0, row1, row2)
    }

    import graph._
    withRelease {
      step
      require(readComplex(row0) == expect0)
      require(readComplex(row1) == expect1)
      require(readComplex(row2) == expect2)
    }

  }

  /** Test the (0D-ScalarField) operator (slice). */
  test("complex field / slice point") {
    // 1D input

    val rand = new cogx.utilities.Random
    val shape1D = Shape(17)
    val image1D = RefComplexField.random(shape1D)
    val sliceToExtract1D = shape1D(0) * rand.nextFloat

    val row0 = Array(4f, 5f, 6f)
    val row1 = Array(8f, 10f, 12f)
    val row2 = Array(12f, 15f, 18f)
    val realPart = Matrix(row0, row1, row2)
    val imaginaryPart = realPart * -1f
    val image = new ComplexMatrix(realPart, imaginaryPart)

    val shape3D = Shape(3, 5, 4)
    val image3D = RefComplexField.random(shape3D)
    val sliceToExtract3D = shape3D(0) * rand.nextFloat

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field1D = TestComplexField(image1D)

      val indexField1D = TestScalarField(RefScalarField(sliceToExtract1D))
      val slicedField0D = field1D(indexField1D)

      // 2D input

      val index0 = TestScalarField(RefScalarField(0f))
      val index1 = TestScalarField(RefScalarField(1.1f))
      val index2 = TestScalarField(RefScalarField(2.4f))

      val field2D = TestComplexField(RefComplexField(image))

      val row0Field = field2D(index0)
      val row1Field = field2D(index1)
      val row2Field = field2D(index2)

      // 3D input

      val field3D = TestComplexField(image3D)
      val indexField3D = TestScalarField(RefScalarField(sliceToExtract3D))
      val slicedField2D = field3D(indexField3D)

      probe(slicedField0D, row0Field, row1Field, row2Field, slicedField2D)
    }

    import graph._
    withRelease {
      step
      require(readComplex(slicedField0D) == image1D.slice(sliceToExtract1D.toInt))
      require(readComplex(row0Field) == RefComplexField(new ComplexVector(new Vector(row0), new Vector(row0) * -1f)))
      require(readComplex(row1Field) == RefComplexField(new ComplexVector(new Vector(row1), new Vector(row1) * -1f)))
      require(readComplex(row2Field) == RefComplexField(new ComplexVector(new Vector(row2), new Vector(row2) * -1f)))
      require(readComplex(slicedField2D) == image3D.slice(sliceToExtract3D.toInt))
    }
  }

  /** Test the stack operator. */
  test("complex field / stack") {
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

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f0 = TestComplexField(field0)
      val f1 = TestComplexField(field1)
      val f2 = TestComplexField(field2)
      val fArray = Array(f0, f1, f2)

      val stack0 = stack(f0, f1, f2)
      val stack1 = stack(fArray)

      probe(stack0, stack1)
    }

    import graph._
    withRelease {
      step
      require(readComplex(stack0) == expected)
      require(readComplex(stack1) == expected)
    }
  }

  /** Test the stackTensors operator. */
  test("complex field / tensor stack") {
    def runtest(vectorSize: Int) {
      val Rows = 5
      val Columns = 7
      val planes = Array.tabulate(vectorSize) { i =>
        RefComplexField(Rows, Columns,
          (row, col) => Complex(i * 100 + row * 10 + col,
                                i * 100 - row * 10 - col))
      }

      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inputs = Array.tabulate(vectorSize) { i => TestComplexField(planes(i))}
        val stackedField = complexVectorField(inputs: _*)

        probe(stackedField)
      }

      import graph._
    withRelease {
        step
        for (i <- 0 until vectorSize)
          require(readComplexPlane(i)(stackedField) == planes(i))
      }
    }

    val vectorSizes = Seq(1,2,3,4,5)

    vectorSizes.foreach(t => runtest(t))
  }


  /** Test the trim and cog-private trimToReal operators. */
  test("complex field / trim") {
    val InRows = 7
    val InCols = 6
    val trimmedRows = 5
    val trimmedCols = 4

    val inField = RefComplexField.random(InRows, InCols)
    val expected = RefComplexField(trimmedRows, trimmedCols,
                                   (row, col) => inField.read(row, col)
                                 )
    val outShape = Shape(trimmedRows, trimmedCols)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // 3 copies of input to keep outputs from being optimized away
      // (no probe support yet).
      val inDynamicField = TestComplexField(inField)
      val inDynamicField2 = TestComplexField(inField)
      val inDynamicField3 = TestComplexField(inField)

      val trimmedComplexDynamicField = trim(inDynamicField, outShape)
      val autoTrimmedOut = ComplexField(outShape)
      autoTrimmedOut <== inDynamicField2

      // Support for "trimToReal" is only within cog package for this testing.
      val trimmedRealDynamicField = trimToReal(inDynamicField3, Shape(trimmedRows, trimmedCols))

      probe(trimmedComplexDynamicField, autoTrimmedOut, trimmedRealDynamicField)
    }

    import graph._
    withRelease {
      step
      require(readComplex(trimmedComplexDynamicField) == expected)
      require(readComplex(autoTrimmedOut) == expected)
      require(readScalar(trimmedRealDynamicField) == expected.realPart)
    }
  }

  /** Test the expand operator. */
  test("complex field / expand border") {
    val InRows = 13
    val InCols = 21
    val expandedRows = 18
    val expandedCols = 33

    val inField = RefComplexField.random(InRows, InCols)

    // Test ComplexExpandBorderKernel

    val expectedMatrix =
      inField.toComplexMatrix.expand(expandedRows, expandedCols, borderFill = true)
    val expectedField = RefComplexField(expectedMatrix)

    // Test ComplexExpandBorderKernel - zero fill

    val expectedZeroFillMatrix =
      inField.toComplexMatrix.expand(expandedRows, expandedCols, borderFill = false)
    val expectedZeroFillField = RefComplexField(expectedZeroFillMatrix)

    // Test RealToComplexExpandBorderKernel

    val realMatrix = new Matrix(InRows, InCols).randomize
    val realField = RefScalarField(realMatrix)

    val expectedRealMatrix = realMatrix.expand(expandedRows, expandedCols, borderFill = true)
    val allZeroMatrix = new Matrix(expandedRows, expandedCols)
    val expectedComplexMatrix = new ComplexMatrix(expectedRealMatrix, allZeroMatrix)
    val expectedFieldFromReal = RefComplexField(expectedComplexMatrix)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inDynamicField = TestComplexField(inField)
      val inDynamicFieldRealPart = TestScalarField(inField.realPart)
      val expandedComplexDynamicField =
        expand(inDynamicField, BorderClamp, expandedRows, expandedCols)
      val expandedZeroFilledField =
        expand(inDynamicField, BorderZero, expandedRows, expandedCols)
      val realDynamicField = TestScalarField(realField)
      // Support for "realToComplexExpandBorderFill" is only within cog package for this testing.
      val expandedFromRealDynamicField =
        realToComplexExpandBorderFill(realDynamicField, expandedRows, expandedCols)

      probe(expandedComplexDynamicField, expandedZeroFilledField, expandedFromRealDynamicField)
    }

    import graph._
    withRelease {
      step
      require(readComplex(expandedComplexDynamicField) == expectedField)
      require(readComplex(expandedZeroFilledField) == expectedZeroFillField)
      require(readComplex(expandedFromRealDynamicField) == expectedFieldFromReal)
    }
  }

  test("complex field / commutative") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field = TestComplexField(RefComplexField.random(10, 10))
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
      require(readComplex(plus) == readComplex(plusReverse))
      require(readComplex(minus) == readComplex(minusReverse))
      require(readComplex(times) == readComplex(timesReverse))
    }
  }

  /** Test construction of complex fields from scalar fields. */
  test("complex field / construction") {
    // Test construction from real and imaginary parts
    val Size = 5

    // Test construction from magnitude and phase parts

    // Generate a distribution of phases with some randomness, but stay away
    // from phases close to the vertical and horizontal, as they may cause
    // the test to fail because of poor relative error in the real/imag parts.
    val NumAngles = 360
    val initPhase = RefScalarField(NumAngles, i => math.Pi.toFloat * 2f * i / (NumAngles + 1)) +
    0.001f  + RefScalarField.random(NumAngles) * 0.0001f

    // Add offset to avoid near-zero magnitudes
    val initMagnitude = RefScalarField.random(NumAngles) + 0.001f

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val realPart = TestScalarField(RefScalarField.random(Size, Size))
      val imaginaryPart = TestScalarField(RefScalarField.random(Size, Size))
      val complex1 = complex(realPart, imaginaryPart)
      val phase_ = TestScalarField(initPhase)
      val magnitude_ = TestScalarField(initMagnitude)
      val complex2 = polarComplex(magnitude_, phase_)

      probe(complex1, realPart, imaginaryPart, complex2, magnitude_, phase_)
    }

    import graph._
    withRelease {
      step
      require(readComplex(complex1) == RefComplexField(readScalar(graph.realPart), readScalar(graph.imaginaryPart)))
      require(readComplex(complex2) ~== RefComplexField.polar(readScalar(magnitude_), readScalar(phase_)))
    }
  }


  /** test a fork in a chain of piped fields */
  test("complex field / forked recurrences") {
    val Size = 10
    val shape = Shape(Size, Size)
    val refA = RefComplexField.random(Size, Size)
    val refD = RefComplexField.random(Size, Size)
    val refE = RefComplexField.random(Size, Size)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val A = probe(TestComplexField(refA), "")
      val B = probe(ComplexField(shape), "")
      val C = probe(ComplexField(shape), "")
      B <== A
      C <== A

      val D = probe(TestComplexField(refD), "")
      val E = probe(TestComplexField(refE), "")
      val F = probe(ComplexField(shape), "")
      val G = probe(ComplexField(shape), "")
      val H = probe(ComplexField(shape), "")
      F <== D + E
      G <== D + E
      H <== D + E + 0f    // Threw exception only with CogDebuggerApp

      probe(B, C, F, G, H)
    }

    import graph._
    withRelease {
      step
      require(readComplex(B) == refA)
      require(readComplex(C) == refA)
      require(readComplex(F) ~== refD + refE)
      require(readComplex(G) ~== refD + refE)
      require(readComplex(H) ~== refD + refE)
    }
  }

}
