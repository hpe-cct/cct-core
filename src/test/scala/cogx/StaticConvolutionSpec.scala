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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import cogx.platform.opencl.OpenCLPlatform
import cogx.reference._
import cogx.helper.{ComplexFieldBuilderInterface, MatrixFieldBuilderInterface, VectorFieldBuilderInterface, ScalarFieldBuilderInterface}
import cogx.utilities.Array2D

@RunWith(classOf[JUnitRunner])
class StaticConvolutionSpec extends FunSuite
                            with ScalarFieldBuilderInterface
                            with VectorFieldBuilderInterface
                            with MatrixFieldBuilderInterface
                            with ComplexFieldBuilderInterface
{
  val Optimize = true

  // pseudo Gaussian
  val kernelMatrix = Matrix(
    Array(0.25f, 0.50f, 0.25f),
    Array(0.50f, 1.00f, 0.50f),
    Array(0.25f, 0.50f, 0.25f)
  )

  val complexMatrix = new ComplexMatrix(kernelMatrix, -kernelMatrix)

  val imageMatrix = Matrix(
    Array(0f, 1f, 2f, 3f),
    Array(2f, 3f, 4f, 5f),
    Array(8f, 7f, 6f, 4f),
    Array(5f, 4f, 9f, 8f)
  )

  /**
    * Test convolution of real dynamic scalar fields. Note that convolution
    * is currently defined with border fill, but normalized convolution may
    * actually be much more robust with minimal cost. These tests will change
    * if we switch to normalized convolution.
    *
    * TODO: Reorganize convolution tests.  There is no longer a
    * StaticConvolutionGenerator and some dynamic filter tests have crept
    * into this file.
    */
  test("scalar convolve scalar, 1D") {
    val borderConvolve1D = new Vector(Array(1.25f, 2.0f, 3.0f, 3.75f))
    val cyclicConvolve1D = new Vector(Array(2.00f, 2.0f, 3.0f, 3.0f))
    val zeroConvolve1D   = new Vector(Array(1.00f, 2.0f, 3.0f, 2.75f))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel1D = TestScalarField(new Vector(Array(0.25f, 0.5f, 0.25f)))
      val image1D = RefScalarField(new Vector(Array(1f, 2f, 3f, 4f)))

      val smooth1D = convolve(TestScalarField(image1D), kernel1D, BorderClamp)
      val smoothCyclic1D = convolve(TestScalarField(image1D), kernel1D, BorderCyclic)
      val smoothZero1D = convolve(TestScalarField(image1D), kernel1D, BorderZero)

      probe(smooth1D, smoothCyclic1D, smoothZero1D)
    }

    import graph._
    withRelease {
      step
      require(readScalar(smooth1D) ~== RefScalarField(borderConvolve1D))
      require(readScalar(smoothCyclic1D) ~== RefScalarField(cyclicConvolve1D))
      require(readScalar(smoothZero1D) ~== RefScalarField(zeroConvolve1D))
    }
  }

  /** Test convolution of a 2D scalar field with a 2D scalar field filter.
    */
  test("scalar convolve scalar, 2D") {

    val borderConvolve = Matrix(
      Array( 3.0f,  6.0f, 10.0f,  13.0f),
      Array(12.5f, 14.0f, 15.75f, 16.75f),
      Array(22.5f, 22.5f, 23.0f,  22.0f),
      Array(22.0f, 23.5f, 28.25f, 29.25f)
    )

    val cyclicConvolve = Matrix(
      Array(10.5f,  10.5f, 15.5f,  15.5f),
      Array(13.75f, 14.0f, 15.75f, 15.50f),
      Array(22.0f,  22.5f, 23.0f,  22.5f),
      Array(18.75f, 19.0f, 22.75f, 22.5f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel = TestScalarField(kernelMatrix)
      val src = TestScalarField(imageMatrix)
      val smooth = convolve(src, kernel, BorderClamp)
      val smoothCyclic = convolve(src, kernel, BorderCyclic)
      val complexSmooth =
        convolve(src, TestComplexField(RefComplexField(complexMatrix)), BorderClamp)

      probe(smooth, smoothCyclic, complexSmooth)
    }

    import graph._
    withRelease {
      step
      require(readScalar(smooth) ~== RefScalarField(borderConvolve))
      require(readScalar(smoothCyclic) ~== RefScalarField(cyclicConvolve))
      require(readComplex(complexSmooth).realPart ~== RefScalarField(borderConvolve))
      require(readComplex(complexSmooth).imaginaryPart ~== RefScalarField(-borderConvolve))
    }
  }

  /** Test convolution of a 2D scalar field with a 2D scalar field filter, forcing
    * use of the FFT and updating the filter dynamically.
    */
  test("scalar convolve dynamic scalar, 2D, by FFT ") {

    val borderConvolve = Matrix(
      Array( 3.0f,  6.0f, 10.0f,  13.0f),
      Array(12.5f, 14.0f, 15.75f, 16.75f),
      Array(22.5f, 22.5f, 23.0f,  22.0f),
      Array(22.0f, 23.5f, 28.25f, 29.25f)
    )

    val cyclicConvolve = Matrix(
      Array(10.5f,  10.5f, 15.5f,  15.5f),
      Array(13.75f, 14.0f, 15.75f, 15.50f),
      Array(22.0f,  22.5f, 23.0f,  22.5f),
      Array(18.75f, 19.0f, 22.75f, 22.5f)
    )

    val graph = new ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface {
      val kernel = TestScalarField(kernelMatrix)
      kernel <== kernel * 0.5f
      val complexKernel = TestComplexField(RefComplexField(complexMatrix))
      complexKernel <== complexKernel * 0.5f
      val src = TestScalarField(imageMatrix)
      val smooth = convolve(src, kernel, BorderClamp)
      val smoothCyclic = convolve(src, kernel, BorderCyclic)
      val complexSmooth =
        convolve(src, complexKernel, BorderClamp)

      probe(smooth, smoothCyclic, complexSmooth)
    }

    import graph._
    withRelease {
      reset
      require(readScalar(smooth) ~== RefScalarField(borderConvolve))
      require(readScalar(smoothCyclic) ~== RefScalarField(cyclicConvolve))
      require(readComplex(complexSmooth).realPart ~== RefScalarField(borderConvolve))
      require(readComplex(complexSmooth).imaginaryPart ~== RefScalarField(-borderConvolve))
      step
      require(readScalar(smooth) ~== RefScalarField(borderConvolve) * 0.5f)
      require(readScalar(smoothCyclic) ~== RefScalarField(cyclicConvolve) * 0.5f)
      require(readComplex(complexSmooth).realPart ~== RefScalarField(borderConvolve) * 0.5f)
      require(readComplex(complexSmooth).imaginaryPart ~== RefScalarField(-borderConvolve) * 0.5f)
      step
      require(readScalar(smooth) ~== RefScalarField(borderConvolve) * 0.25f)
      require(readScalar(smoothCyclic) ~== RefScalarField(cyclicConvolve) * 0.25f)
      require(readComplex(complexSmooth).realPart ~== RefScalarField(borderConvolve) * 0.25f)
      require(readComplex(complexSmooth).imaginaryPart ~== RefScalarField(-borderConvolve) * 0.25f)
    }
  }

  /** Test convolution of a 2D scalar field with a 2D scalar field filter
    * using the BorderZero border policy.
    */
  test("scalar convolve scalar, 2D, BorderZero") {
    val zeroConvolve = Matrix(
      Array( 2.25f,  5.0f,  8.0f,  7.5f),
      Array( 9.5f,  14.0f, 15.75f, 12.5f),
      Array(16.75f, 22.5f, 23.0f,  16.75f),
      Array(12.75f, 18.0f, 20.75f, 16.0f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel = TestScalarField(kernelMatrix)
      val src = TestScalarField(imageMatrix)
      val smooth = convolve(src, kernel, BorderZero)

      probe(smooth)
    }
    import graph._
    withRelease {
      step
      require(readScalar(smooth) ~== RefScalarField(zeroConvolve))
    }
  }

  /** Test cross-correlation of a 2D scalar field with a 2D scalar field filter.
    */
  test("scalar cross-correlate scalar, 2D") {
    val kernelData = Matrix(
      Array(1f, 2f, 0f),
      Array(0f, 3f, 0f),
      Array(0f, 0f, 0f)
    )

    val borderCrossCorrelate = Matrix(
      Array( 0f,  5f, 11f, 17f),
      Array( 6f, 11f, 17f, 23f),
      Array(30f, 29f, 29f, 26f),
      Array(39f, 34f, 46f, 38f)
    )

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val kernel = TestScalarField(kernelData)
      val src = TestScalarField(imageMatrix)
      val smooth = crossCorrelate(src, kernel, BorderClamp)

      probe(smooth)
    }

    import graph._
    withRelease {
      step
      require(readScalar(smooth) ~== RefScalarField(borderCrossCorrelate))
    }
  }

  /** Test convolution of a scalar field with a "big vector" and "small vector"
    * vector field filter bank.
    */
  test("scalar convolve vector, 2D") {
    class TestScalarConvolveVector2D(rows: Int, columns: Int, bigVector: Boolean) extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val src = TestScalarField(RefScalarField.random(rows, columns))
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(src, kernels(i), BorderClamp)
      }

      val kernelIn = vectorField(kernels)
      val vectorOut = convolve(src, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize) {
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        }
      }
    }

    new TestScalarConvolveVector2D(3, 4, bigVector = false)
    new TestScalarConvolveVector2D(3, 4, bigVector = true)
    // Large inputs trigger the placement of the filter in constant memory,
    // a unique code path in the ConvolveHyperKernel
    new TestScalarConvolveVector2D(256, 256, bigVector = false)
    new TestScalarConvolveVector2D(256, 256, bigVector = true)
  }

  /** Test convolution of scalar fields with a vector field filter with a
    * large filter diameter to trigger FFT use.
    */
  test("scalar convolve vector, 2D by FFT") {
    class TestScalarConvolveVector2D(bigVector: Boolean)
            extends ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface {
      val FilterSize = 15

      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3 * FilterSize
      val Columns = 4 * FilterSize

      val kernelMatrix = Matrix(FilterSize, FilterSize, (r,c) => (r + c) / 10f)

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      // Adding a bias reduces the relative error, helping this test to pass on GPUs with low-end FP support.
      val src = TestScalarField(RefScalarField.random(Rows, Columns)) + 1.0f
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(src, kernels(i), BorderClamp)
      }

      val kernelIn = vectorField(kernels)
      val vectorOut = convolve(src, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestScalarConvolveVector2D(bigVector = false)
    new TestScalarConvolveVector2D(bigVector = true)
  }

  /** Test convolution of scalar fields with a vector field filter with a
    * large filter diameter to trigger FFT use.
    */
  test("scalar convolve dynamic vector, 2D by FFT") {
    class TestScalarConvolveVector2D(bigVector: Boolean)
            extends ComputeGraph(Optimize, fftUse = UseFFTAlways)  with RefTestInterface{
      val FilterSize = 17

      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3 * FilterSize
      val Columns = 4 * FilterSize

      val kernelMatrix = Matrix(FilterSize, FilterSize, (r,c) => (r + c) / 10f)

      val stepChange = 0.75f

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      // Adding a bias reduces the relative error, helping this test to pass on GPUs with low-end FP support.
      val src = TestScalarField(RefScalarField.random(Rows, Columns)) + 1.0f
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(src, kernels(i), BorderClamp)
      }

      val kernelIn = VectorField(FilterSize, FilterSize, (r, c) =>
        Vector(VectorSize, (i) => kernelMatrix(r,c) + i/10f)
      )
      kernelIn <== kernelIn * stepChange
      val vectorOut = convolve(src, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        reset
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) * stepChange ~== readScalar(slicedVectorOut(i)))
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) * stepChange * stepChange ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestScalarConvolveVector2D(bigVector = false)
    new TestScalarConvolveVector2D(bigVector = true)
  }

  /** Test convolution of "big vector" and "small vector" vector fields with
    * a scalar field filter.
    */
  test("vector convolve scalar, 2D") {
    class TestVectorConvolve2D(rows: Int, columns: Int, bigVector: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(rows, columns))
      }
      val expectedSlicesOut =
        slices.map(slice => convolve(slice, kernel, BorderClamp))

      val vectorIn = vectorField(slices)
      val vectorOut = convolve(vectorIn,kernel, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolve2D(3, 4, bigVector = false)
    new TestVectorConvolve2D(3, 4, bigVector = true)
    // Large inputs trigger the placement of the filter in constant memory,
    // a unique code path in the ConvolveHyperKernel
    new TestVectorConvolve2D(256, 256, bigVector = false)
    new TestVectorConvolve2D(256, 256, bigVector = true)
  }

  /** Test convolution of "big vector" and "small vector" vector fields with
    * a scalar field filter, using the FFT.
    */
  test("vector convolve scalar, 2D, via FFT") {
    class TestVectorConvolve2D(bigVector: Boolean)
            extends ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface {
      val FilterSize = 19

      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3 * FilterSize
      val Columns = 4 * FilterSize

      val kernelMatrix = Matrix(FilterSize, FilterSize, (r,c) => (r + c) / 10f)

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut =
        slices.map(slice => convolve(slice, kernel, BorderClamp))

      val vectorIn = vectorField(slices)
      val vectorOut = convolve(vectorIn, kernel, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolve2D(bigVector = false)
    new TestVectorConvolve2D(bigVector = true)
  }

  /** Test convolution of "big vector" and "small vector" vector fields with
    * a vector field filter.
    */
  test("vector convolve vector, 2D") {
    class TestVectorConvolve2D(rows: Int, columns: Int, bigVector: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(rows, columns))
      }
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(slices(i), kernels(i), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolve(vectorIn, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolve2D(3, 4, bigVector = false)
    new TestVectorConvolve2D(3, 4, bigVector = true)
    // Large inputs trigger the placement of the filter in constant memory,
    // a unique code path in the ConvolveHyperKernel
    new TestVectorConvolve2D(256, 256, bigVector = false)
    new TestVectorConvolve2D(256, 256, bigVector = true)
  }

  /** Test frame back projection of a vector field image with a vector field filter.
    */
  test("vector backProjectFrame vector, 2D") {
    class TestVectorConvolveFrame2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)
      val Rows = 3
      val Columns = 4

      val kernels = Array.tabulate(numOutputPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i =>
          val inputIndex = i % numImagePlanes
          val kernelIndex = inputIndex * (numOutputPlanes / numImagePlanes) +
                            i / numImagePlanes
          convolve(slices(inputIndex), kernels(kernelIndex), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrame(vectorIn, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFrame2D(3, 12)
    new TestVectorConvolveFrame2D(2, 2)
    new TestVectorConvolveFrame2D(1, 4) // degenerate frame case
    new TestVectorConvolveFrame2D(6, 6) // degenerate frame case
  }

  /** Test frame back projection of a batched vector field image with a vector field filter.
    */
  test("batched vector backProjectFrame vector, 2D") {
    class TestVectorConvolveFrame2D(planesPerLogicalImage: Int, planesPerLogicalOutput: Int, batchSize: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(planesPerLogicalOutput % planesPerLogicalImage == 0)
      val totalFilterPlanes = planesPerLogicalOutput
      val numLogicalFilters = totalFilterPlanes / planesPerLogicalImage
      val Rows = 3
      val Columns = 4

      val kernels = Array.tabulate(planesPerLogicalOutput) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        i =>
          val imageIndex = i / planesPerLogicalOutput
          val thisImageOutputIndex = i % planesPerLogicalOutput
          val imagePlaneIndex = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage

          val kernelIndex = numLogicalFilters * (thisImageOutputIndex % planesPerLogicalImage) + (thisImageOutputIndex / planesPerLogicalImage)
          convolve(slices(imagePlaneIndex), kernels(kernelIndex), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrame(vectorIn, kernelIn, BorderClamp, batchSize = batchSize)
      val slicedVectorOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until planesPerLogicalOutput * batchSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFrame2D(2, 6, 2)
    new TestVectorConvolveFrame2D(3, 12, 2)
    new TestVectorConvolveFrame2D(2, 2, 3)
    new TestVectorConvolveFrame2D(1, 4, 4) // degenerate frame case
    new TestVectorConvolveFrame2D(6, 6, 5) // degenerate frame case
  }

  /** Test frame projection of a vector field image with a vector field filter with bundled block reduce.
    */
  test("vector backProjectFrameBlockReduceSum vector, 2D") {
    class TestVectorBackProjectFrameBlockReduceSum2D(outRows: Int, outColumns: Int, numImagePlanes: Int, numFilterPlanes: Int, borderPolicy: BorderPolicy = BorderClamp)
      extends ComputeGraph(Optimize) with RefTestInterface {
      require(numFilterPlanes % numImagePlanes == 0)
      val numOutputPlanes = numFilterPlanes / numImagePlanes

      val filterRows = kernelMatrix.rows
      val filterColumns = kernelMatrix.columns

      val inputRows = borderPolicy match {
        case BorderValid => outRows + filterRows - 1
        case BorderFull => outRows - filterRows + 1
        case BorderClamp => outRows
        case BorderZero => outRows
        case BorderCyclic => outRows
      }

      val inputColumns = borderPolicy match {
        case BorderValid => outColumns + filterColumns - 1
        case BorderFull => outColumns - filterColumns + 1
        case BorderClamp => outColumns
        case BorderZero => outColumns
        case BorderCyclic => outColumns
      }

      def outIndexToFilterIndex(i: Int) = (i % numImagePlanes) * (numFilterPlanes / numImagePlanes) + i / numImagePlanes

      val kernels = Array.tabulate(numFilterPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(inputRows, inputColumns))
      }
      val convolvedSlices = Array.tabulate(numFilterPlanes) {
        i => convolve(slices(i % numImagePlanes), kernels(outIndexToFilterIndex(i)), borderPolicy)
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => (i*numImagePlanes until (i+1)*numImagePlanes).map(convolvedSlices(_)).reduceLeft(_ + _)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrameBlockReduceSum(vectorIn, kernelIn, borderPolicy)
      // The following 2 fields check the operation of projectFrameBlockReduceSum invoked as a user would (relying on
      // the optimizer to swap in the kernel).  The use of flip() in creating vectorOut3 exists to prevent this
      // optimization and to foil common subexpression elimination on the common input potentially.
      val vectorOut2 = blockReduceSum(backProjectFrame(vectorIn, kernelIn, borderPolicy), numImagePlanes)

      val vectorOut3 = blockReduceSum(flip(flip(backProjectFrame(flip(flip(vectorIn)), kernelIn, borderPolicy))), numImagePlanes)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) =>
          if (vectorOut.tensorShape.dimensions == 0)
            vectorOut
          else
            vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)
      probe(vectorOut2, vectorOut3)

      withRelease {
        step

        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        if (numOutputPlanes == 1)
          require(readScalar(vectorOut2) ~== readScalar(vectorOut3))
        else
          require(readVector(vectorOut2) ~== readVector(vectorOut3))

      }
    }

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 10)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 24)
    // The following tests have big fields to invoke multi-plane approach in kernel
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 16)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 14)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 12)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 10)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 8)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 6)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 2)
    // degenerate frame cases
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 5)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 1)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 2, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 3)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 4, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 5)

    // Test Tiled convolve approach, enabled within an optimizer when borderPolicy = BorderValid

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9, BorderValid)

  }
  /** Test frame projection of a vector field image with a vector field filter with bundled block reduce.
    */
  test("vector backProjectFrameBlockReduceSum vector with upsampling, 2D") {
    class TestVectorBackProjectFrameBlockReduceSum2D(outRows: Int, outColumns: Int, numImagePlanes: Int, numFilterPlanes: Int, borderPolicy: BorderPolicy = BorderFull, upsamplingFactor: Int = 2)
      extends ComputeGraph(Optimize) with RefTestInterface {
      require(numFilterPlanes % numImagePlanes == 0)
      val numOutputPlanes = numFilterPlanes / numImagePlanes

      val filterRows = kernelMatrix.rows
      val filterColumns = kernelMatrix.columns

      val inputRows = borderPolicy match {
        case BorderValid => outRows * upsamplingFactor + filterRows - 1
        case BorderFull => outRows * upsamplingFactor- filterRows + 1
        case BorderClamp => outRows* upsamplingFactor
        case BorderZero => outRows* upsamplingFactor
        case BorderCyclic => outRows* upsamplingFactor
      }

      val inputColumns = borderPolicy match {
        case BorderValid => outColumns * upsamplingFactor + filterColumns - 1
        case BorderFull => outColumns * upsamplingFactor- filterColumns + 1
        case BorderClamp => outColumns * upsamplingFactor
        case BorderZero => outColumns * upsamplingFactor
        case BorderCyclic => outColumns * upsamplingFactor
      }

      def outIndexToFilterIndex(i: Int) = (i % numImagePlanes) * (numFilterPlanes / numImagePlanes) + i / numImagePlanes

      val kernels = Array.tabulate(numFilterPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(inputRows, inputColumns))
      }
      val convolvedSlices = Array.tabulate(numFilterPlanes) {
        i => convolve(slices(i % numImagePlanes), kernels(outIndexToFilterIndex(i)), borderPolicy, UpsampleInputConvolution(upsamplingFactor))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => (i*numImagePlanes until (i+1)*numImagePlanes).map(convolvedSlices(_)).reduceLeft(_ + _)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrameBlockReduceSum(vectorIn, kernelIn, borderPolicy, UpsampleInputConvolution(upsamplingFactor))
      // The following 2 fields check the operation of projectFrameBlockReduceSum invoked as a user would (relying on
      // the optimizer to swap in the kernel).  The use of flip() in creating vectorOut3 exists to prevent this
      // optimization and to foil common subexpression elimination on the common input potentially.
      val vectorOut2 = blockReduceSum(backProjectFrame(vectorIn, kernelIn, borderPolicy, UpsampleInputConvolution(upsamplingFactor)), numImagePlanes)

      val vectorOut3 = blockReduceSum(flip(flip(backProjectFrame(flip(flip(vectorIn)), kernelIn, borderPolicy, UpsampleInputConvolution(upsamplingFactor)))), numImagePlanes)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) =>
          if (vectorOut.tensorShape.dimensions == 0)
            vectorOut
          else
            vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)
      probe(vectorOut2, vectorOut3)

      withRelease {
        step

        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        if (numOutputPlanes == 1)
          require(readScalar(vectorOut2) ~== readScalar(vectorOut3))
        else
          require(readVector(vectorOut2) ~== readVector(vectorOut3))

      }
    }

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 10)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 24)
    // The following tests have big fields to invoke multi-plane approach in kernel
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 16)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 14)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 12)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 10)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 8)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 6)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 2)
    // degenerate frame cases
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 5)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 1)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 2, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 3)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 4, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 5)

    // Test Tiled convolve approach, enabled within an optimizer when borderPolicy = BorderValid

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9, BorderValid)

  }
  /** Test frame projection of a batched vector field image with a vector field filter with bundled block reduce.
    */
  test("batched vector backProjectFrameBlockReduceSum vector, 2D") {
    class TestVectorBackProjectFrameBlockReduceSum2D(outRows: Int, outColumns: Int, planesPerLogicalImage: Int, totalFilterPlanes: Int, batchSize: Int, borderPolicy: BorderPolicy = BorderClamp)
      extends ComputeGraph(Optimize) with RefTestInterface {
      require(totalFilterPlanes % planesPerLogicalImage == 0)
      val numLogicalFilters = totalFilterPlanes / planesPerLogicalImage
      val planesPerLogicalOutput = numLogicalFilters
      val numOutputPlanes = batchSize * numLogicalFilters

      val filterRows = kernelMatrix.rows
      val filterColumns = kernelMatrix.columns

      val inputRows = borderPolicy match {
        case BorderValid => outRows + filterRows - 1
        case BorderFull => outRows - filterRows + 1
        case BorderClamp => outRows
        case BorderZero => outRows
        case BorderCyclic => outRows
      }

      val inputColumns = borderPolicy match {
        case BorderValid => outColumns + filterColumns - 1
        case BorderFull => outColumns - filterColumns + 1
        case BorderClamp => outColumns
        case BorderZero => outColumns
        case BorderCyclic => outColumns
      }

      val kernels = Array.tabulate(totalFilterPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        (i) => TestScalarField(RefScalarField.random(inputRows, inputColumns))
      }
      val convolvedSlices = Array.tabulate(totalFilterPlanes * batchSize) {
        i =>
          val imageIndex = i / totalFilterPlanes
          val thisImageOutputIndex = i % totalFilterPlanes
          val imagePlaneIndex = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage

          val kernelIndex = numLogicalFilters * (thisImageOutputIndex % planesPerLogicalImage) + (thisImageOutputIndex / planesPerLogicalImage)
          convolve(slices(imagePlaneIndex), kernels(kernelIndex), borderPolicy)
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i =>
          (i*planesPerLogicalImage until (i+1)*planesPerLogicalImage).map(convolvedSlices(_)).reduceLeft(_ + _)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrameBlockReduceSum(vectorIn, kernelIn, borderPolicy, batchSize = batchSize)
      // The following 2 fields check the operation of projectFrameBlockReduceSum invoked as a user would (relying on
      // the optimizer to swap in the kernel).  The use of flip() in creating vectorOut3 exists to prevent this
      // optimization and to foil common subexpression elimination on the common input potentially.
      val vectorOut2 = blockReduceSum(backProjectFrame(vectorIn, kernelIn, borderPolicy, batchSize = batchSize), planesPerLogicalImage)

      val vectorOut3 = blockReduceSum(flip(flip(backProjectFrame(flip(flip(vectorIn)), kernelIn, borderPolicy, batchSize = batchSize))), planesPerLogicalImage)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) =>
          if (vectorOut.tensorShape.dimensions == 0)
            vectorOut
          else
            vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)
      probe(vectorOut2, vectorOut3)

      withRelease {
        step

        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        if (numOutputPlanes == 1)
          require(readScalar(vectorOut2) ~== readScalar(vectorOut3))
        else
          require(readVector(vectorOut2) ~== readVector(vectorOut3))

      }
    }

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 2, 6, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 10, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 24, 3)
    // The following tests have big fields to invoke multi-plane approach in kernel
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 16, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 14, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 12, 8)
    new TestVectorBackProjectFrameBlockReduceSum2D(64, 64, 2, 10, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 8, 8)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 6, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 4, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(96, 96, 2, 2, 3)
    // degenerate frame cases
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 5, 4)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 1, 1, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 2, 2, 3)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 3, 2)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 4, 4, 8)
    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 5, 5, 4)

    // Test Tiled convolve approach, enabled within an optimizer when borderPolicy = BorderValid

    new TestVectorBackProjectFrameBlockReduceSum2D(3, 4, 3, 9, 8, BorderValid)

  }
//
  /** Test frame back projection of a vector field image with a vector field filter
    * with integrated downsampling.
    */
  test("vector backProjectFrame vector, 2D with downsampling") {
    class TestVectorConvolveFrame2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)

      val ScaleFactor = 2
      val Rows = 6
      val Columns = 8

      val kernels = Array.tabulate(numOutputPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i =>
          val inputIndex = i % numImagePlanes
          val kernelIndex = inputIndex * (numOutputPlanes / numImagePlanes) +
                  i / numImagePlanes
          downsample(convolve(slices(inputIndex), kernels(kernelIndex), BorderClamp), 2)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrame(vectorIn, kernelIn, BorderClamp, DownsampleOutputConvolution(ScaleFactor))
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFrame2D(3, 12)
    new TestVectorConvolveFrame2D(2, 2)
    new TestVectorConvolveFrame2D(1, 4) // degenerate frame case
    new TestVectorConvolveFrame2D(6, 6) // degenerate frame case
  }
  /** Test frame back projection of a vector field image with a vector field filter
    * with integrated downsampling.
    */
  test("vector backProjectFrame vector, 2D with upsampling") {
    class TestVectorConvolveFrame2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)

      val ScaleFactor = 2
      val Rows = 6
      val Columns = 8

      val kernels = Array.tabulate(numOutputPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i =>
          val inputIndex = i % numImagePlanes
          val kernelIndex = inputIndex * (numOutputPlanes / numImagePlanes) +
                  i / numImagePlanes
          convolve(upsample(slices(inputIndex)), kernels(kernelIndex), BorderFull)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = backProjectFrame(vectorIn, kernelIn, BorderFull, UpsampleInputConvolution(ScaleFactor))
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFrame2D(3, 12)
    new TestVectorConvolveFrame2D(2, 2)
    new TestVectorConvolveFrame2D(1, 4) // degenerate frame case
    new TestVectorConvolveFrame2D(6, 6) // degenerate frame case
  }

  /** Test filterAdjoint convolution of a scalar field image with a scalar field
    * representation, with downsampling and without. Input data is constant
    * so this just makes sure that the right number of elements are summed.
    */
  test("scalar convolveFilterAdjoint scalar, 2D to small field with downsample") {

    class TestScalarConvolveScalarToSmall(inputSize: Int, filterSize: Int, downSampleFactor: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {

      val OutputSize = math.ceil((inputSize - filterSize + 1) / downSampleFactor).toInt

      val InputElementVal = 0.5f

      val input = ScalarField(inputSize, inputSize, (r,c) => InputElementVal)
      val filter = ScalarField(filterSize, filterSize, (r,c) => 1.0f)
      val output =
        if (downSampleFactor > 1)
          convolveFilterAdjoint(input, filter, BorderValid, DownsampleOutputConvolution(downSampleFactor))
        else
          convolveFilterAdjoint(input, filter, BorderValid)
      val expected = TestScalarField(Matrix(OutputSize, OutputSize,
        (r,c) => filterSize * filterSize * InputElementVal))

      probe(output, expected)

      withRelease {
        step
//        readScalar(output).print
//        println("----------------------------------------------------------")
//        readScalar(expected).print
        require(readScalar(output) ~== readScalar(expected))
      }
    }
    for (outputSize <- 1 until 10) {
      val filterSize = 16
      val inputSize = outputSize + filterSize - 1
      new TestScalarConvolveScalarToSmall(inputSize, filterSize, 1)
    }
    new TestScalarConvolveScalarToSmall(137, 128, 1)
    new TestScalarConvolveScalarToSmall(1033, 1000, 1)
    new TestScalarConvolveScalarToSmall(27, 16, 2)
    new TestScalarConvolveScalarToSmall(32, 24, 3)
  }

  /** Test filterAdjoint convolution of a scalar field image with a scalar field
    * representation, with upsampling. Input data is constant
    * so this just makes sure that the right number of elements are summed.
    */
  test("scalar convolveFilterAdjoint scalar, 2D to small field with upsample") {

    class TestScalarConvolveScalarToSmall(inputSize: Int, filterSize: Int, upSampleFactor: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val OutputSize = inputSize - filterSize * upSampleFactor + 1

      val InputElementVal = 0.5f

      val input = ScalarField(inputSize, inputSize, (r,c) => InputElementVal)
      val filter = ScalarField(filterSize, filterSize, (r,c) => 1.0f)
      val output =
        if (upSampleFactor > 1)
          convolveFilterAdjoint(input, filter, BorderValid, UpsampleInputConvolution(upSampleFactor))
        else
          convolveFilterAdjoint(input, filter, BorderValid)
      val expected = TestScalarField(Matrix(OutputSize, OutputSize,
        (r,c) => filterSize * filterSize * InputElementVal))

      probe(output, expected)

      withRelease {
        step
        require(readScalar(output) ~== readScalar(expected))
      }
    }

    // Lowering the local worksize by specifying a big input (> 48KBytes) stresses
    // the pipeline control logic of the ConvolveToSmallFieldHyperKernel.
    class TestReducedLocalWorkSize(inputSize: Int, filterSize: Int, upSampleFactor: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val OutputSize = inputSize - filterSize * upSampleFactor + 1

      val InputElementVal = 0.5f

      val input = ScalarField(inputSize, inputSize, (r,c) =>
        if (r < upSampleFactor && c < upSampleFactor) InputElementVal else 0f)
      val filter = ScalarField(filterSize, filterSize, (r,c) => 1.0f)
      val output =
        if (upSampleFactor > 1)
          convolveFilterAdjoint(input, filter, BorderValid, UpsampleInputConvolution(upSampleFactor))
        else
          convolveFilterAdjoint(input, filter, BorderValid)
      val expected = TestScalarField(Matrix(OutputSize, OutputSize,
        (r,c) => if (r == 0 && c == 0) InputElementVal else 0f))

      probe(output, expected)

      withRelease {
        step
        require(readScalar(output) ~== readScalar(expected))
      }
    }

    new TestScalarConvolveScalarToSmall(137, 64, 2)
    new TestScalarConvolveScalarToSmall(35, 10, 3)
    new TestScalarConvolveScalarToSmall(210, 50, 4)
    new TestReducedLocalWorkSize(360, 50, 7)
  }

  /** Test filterAdjoint convolution of a vector field image with a vector field representation.
    */
  test("scalar convolveFilterAdjoint scalar, checks proper upsampling / flip ordering.") {

    class TestScalarConvolveScalarToSmall(inputSize: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val filterSize = 1
      val upSampleFactor = inputSize
      val OutputSize = inputSize - filterSize * upSampleFactor + 1

      val input = ScalarField(inputSize, inputSize, (r,c) => inputSize * r + c + 1)
      val filter = ScalarField(filterSize, filterSize, (r,c) => 1.0f)

      val convolveOutput =
        if (upSampleFactor > 1)
          convolveFilterAdjoint(input, filter, BorderValid, UpsampleInputConvolution(upSampleFactor))
        else
          convolveFilterAdjoint(input, filter, BorderValid)

      val crossCorrelateOutput =
        if (upSampleFactor > 1)
          crossCorrelateFilterAdjoint(input, filter, BorderValid, UpsampleInputConvolution(upSampleFactor))
        else
          crossCorrelateFilterAdjoint(input, filter, BorderValid)

      val convolveExpected = TestScalarField(Matrix(OutputSize, OutputSize,
        (r,c) => inputSize * inputSize))
      val crossCorrelateExpected = TestScalarField(Matrix(OutputSize, OutputSize,
        (r,c) => 1.0f))

      probe(convolveOutput, convolveExpected, crossCorrelateOutput, crossCorrelateExpected)

      withRelease {
        step
        require(readScalar(convolveOutput) ~== readScalar(convolveExpected))
        require(readScalar(crossCorrelateOutput) ~== readScalar(crossCorrelateExpected))
      }
    }

    new TestScalarConvolveScalarToSmall(2)
    new TestScalarConvolveScalarToSmall(3)
    new TestScalarConvolveScalarToSmall(5)
  }

  /** Test filterAdjoint convolution of a vector field image with a vector field representation.
    */
  test("vector convolveFilterAdjoint vector, 2D") {
    class TestVectorConvolveFilterAdjoint2D(numImagePlanes: Int, numOutputPlanes: Int,
              InputRows: Int, InputColumns: Int, OutputSize: Int, randomData: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)
      val numKernelPlanes = numOutputPlanes / numImagePlanes

      val KernelRows = InputRows - OutputSize + 1
      val KernelColumns = InputColumns - OutputSize + 1

      val kernels = Array.tabulate(numKernelPlanes) {
        i =>
          if (randomData)
            TestScalarField(Matrix.random(KernelRows, KernelColumns))
          else
            TestScalarField(Matrix(KernelRows, KernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => convolve(slices(i % numImagePlanes), kernels(i / numImagePlanes), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolveFilterAdjoint(vectorIn, kernelIn, BorderValid)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFilterAdjoint2D(2, 6, 63, 67, 17, false)
    new TestVectorConvolveFilterAdjoint2D(5, 20, 63, 67, 17, false)
    new TestVectorConvolveFilterAdjoint2D(3, 12, 15, 27, 5, false)
    new TestVectorConvolveFilterAdjoint2D(2, 4, 15, 27, 5, false)
    new TestVectorConvolveFilterAdjoint2D(1, 4, 15, 27, 5, false) // degenerate frame case
    new TestVectorConvolveFilterAdjoint2D(6, 6, 15, 27, 5, false) // degenerate frame case
    // Leave the next test out, since it requires >32K of local memory, which is the guaranteed minimum.
//    new TestVectorConvolveFilterAdjoint2D(5, 20, 81, 79, 7, true)
    new TestVectorConvolveFilterAdjoint2D(3, 12, 64, 64, 31, true)
    new TestVectorConvolveFilterAdjoint2D(2, 4, 15, 27, 6, true)
  }

  /** Test filterAdjoint convolution of a batched vector field image with a vector field representation.
    */
  test("batched vector convolveFilterAdjoint vector, 2D") {
    class TestVectorConvolveFilterAdjoint2D(planesPerLogicalImage: Int, planesPerLogicalOutput: Int,
              InputRows: Int, InputColumns: Int, OutputSize: Int, randomData: Boolean, batchSize: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(planesPerLogicalOutput % planesPerLogicalImage == 0)
      val planesPerLogicalFilter = planesPerLogicalOutput / planesPerLogicalImage

      val kernelRows = InputRows - OutputSize + 1
      val kernelColumns = InputColumns - OutputSize + 1

      val kernels = Array.tabulate(planesPerLogicalFilter * batchSize) {
        i =>
          if (randomData)
            TestScalarField(Matrix.random(kernelRows, kernelColumns))
          else
            TestScalarField(Matrix(kernelRows, kernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        i =>
          // Which image within the batch
          val imageIndex = i % batchSize
          val thisImageOutputIndex = i / batchSize
          val in1Index = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage
          val in2Index = imageIndex * planesPerLogicalFilter + thisImageOutputIndex / planesPerLogicalImage
          convolve(slices(in1Index), kernels(in2Index), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolveFilterAdjoint(vectorIn, kernelIn, BorderValid, batchSize = batchSize)
      val slicedVectorOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until planesPerLogicalOutput*batchSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

      new TestVectorConvolveFilterAdjoint2D(2, 6, 63, 67, 17, false, 2)
      new TestVectorConvolveFilterAdjoint2D(5, 20, 63, 67, 17, false, 2)
      new TestVectorConvolveFilterAdjoint2D(3, 12, 15, 27, 5, false, 3)
      new TestVectorConvolveFilterAdjoint2D(2, 4, 15, 27, 5, false, 2)
      new TestVectorConvolveFilterAdjoint2D(1, 4, 15, 27, 5, false, 7) // degenerate frame case
      new TestVectorConvolveFilterAdjoint2D(6, 6, 15, 27, 5, false, 8) // degenerate frame case
      // Leave the next test out, since it requires >32K of local memory, which is the guaranteed minimum.
      //    new TestVectorConvolveFilterAdjoint2D(5, 20, 81, 79, 7, true)
      new TestVectorConvolveFilterAdjoint2D(3, 12, 64, 64, 31, true, 2)
      new TestVectorConvolveFilterAdjoint2D(2, 4, 15, 27, 6, true, 3)
  }

  /** Test filterAdjoint convolution of a vector field image with a vector field representation.
    */
  test("vector convolveFilterAdjoint vector, 2D with upsampling") {
    class TestVectorConvolveFilterAdjoint2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)

      val UpSampleFactor = 2
      val numKernelPlanes = numOutputPlanes / numImagePlanes

      val OutputSize = 5

      val InputRows = 16
      val InputColumns = 18

      val KernelRows = (InputRows - OutputSize + 1) / UpSampleFactor
      val KernelColumns = (InputColumns - OutputSize + 1)  / UpSampleFactor

      val kernels = Array.tabulate(numKernelPlanes) {
        i => TestScalarField(Matrix(KernelRows, KernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => convolve(slices(i % numImagePlanes), upsample(kernels(i / numImagePlanes), UpSampleFactor), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolveFilterAdjoint(vectorIn, kernelIn,
        BorderValid, UpsampleInputConvolution(UpSampleFactor))
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFilterAdjoint2D(3, 12)
    new TestVectorConvolveFilterAdjoint2D(2, 4)
    new TestVectorConvolveFilterAdjoint2D(1, 4) // degenerate frame case
    new TestVectorConvolveFilterAdjoint2D(6, 6) // degenerate frame case
  }

  /** Test filterAdjoint convolution of a batched vector field image with a vector field filter, with upsampling.
    */
  test("batched vector convolveFilterAdjoint vector, 2D with upsampling") {
    class TestVectorConvolveFilterAdjoint2D(planesPerLogicalImage: Int, planesPerLogicalOutput: Int,
                                            InputRows: Int, InputColumns: Int, OutputSize: Int, randomData: Boolean, batchSize: Int, upsampleFactor: Int)
      extends ComputeGraph(Optimize) with RefTestInterface {
      require(planesPerLogicalOutput % planesPerLogicalImage == 0)
      val planesPerLogicalFilter = planesPerLogicalOutput / planesPerLogicalImage

      val KernelRows = (InputRows - OutputSize + 1) / upsampleFactor
      val KernelColumns = (InputColumns - OutputSize + 1) / upsampleFactor

      require(KernelRows > 0)
      require(KernelColumns > 0)

      val kernels = Array.tabulate(planesPerLogicalFilter * batchSize) {
        i =>
          if (randomData)
            TestScalarField(Matrix.random(KernelRows, KernelColumns))
          else
            TestScalarField(Matrix(KernelRows, KernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        //          (i) => TestScalarField(RefScalarField(InputRows, InputColumns, (r,c) => i + 1f))
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        i =>
          // Which image within the batch
          val imageIndex = i % batchSize
          val thisImageOutputIndex = i / batchSize
          val in1Index = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage
          val in2Index = imageIndex * planesPerLogicalFilter + thisImageOutputIndex / planesPerLogicalImage
          convolve(slices(in1Index), upsample(kernels(in2Index), upsampleFactor), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolveFilterAdjoint(vectorIn, kernelIn, BorderValid, UpsampleInputConvolution(upsampleFactor), batchSize = batchSize)
      val slicedVectorOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until planesPerLogicalOutput*batchSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolveFilterAdjoint2D(2, 6, 64, 72, 17, false, 2, 2)
    new TestVectorConvolveFilterAdjoint2D(5, 20, 70, 79, 17, false, 2, 3)
    new TestVectorConvolveFilterAdjoint2D(3, 12, 16, 28, 5, false, 3, 2)
    new TestVectorConvolveFilterAdjoint2D(2, 4, 16, 28, 5, false, 2, 2)
    new TestVectorConvolveFilterAdjoint2D(1, 4, 16, 28, 5, false, 7, 2) // degenerate frame case
    new TestVectorConvolveFilterAdjoint2D(6, 6, 16, 28, 5, false, 8, 2) // degenerate frame case
    // Leave the next test out, since it requires >32K of local memory, which is the guaranteed minimum.
    //    new TestVectorConvolveFilterAdjoint2D(5, 20, 81, 79, 7, true)
    new TestVectorConvolveFilterAdjoint2D(3, 12, 64, 64, 31, true, 2, 2)
    new TestVectorConvolveFilterAdjoint2D(2, 4, 15, 27, 6, true, 3, 2)
    new TestVectorConvolveFilterAdjoint2D(2, 4, 70, 70, 11, true, 3, 4)
  }


  /** Test convolution of vector fields with a vector field filter with a
    * large filter diameter to trigger FFT use.
    */
  test("vector convolve vector, 2D by FFT") {
    class TestVectorConvolve2D(bigVector: Boolean)
            extends ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface {
      val FilterSize = 21

      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3 * FilterSize
      val Columns = 4 * FilterSize

      val kernelMatrix = Matrix(FilterSize, FilterSize, (r,c) => (r + c) / 10f)

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      // Adding a bias reduces the relative error, helping this test to pass on GPUs with low-end FP support.
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns)) + 1.0f
      }
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(slices(i), kernels(i), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = convolve(vectorIn, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolve2D(bigVector = false)
    new TestVectorConvolve2D(bigVector = true)
  }

  /** Test convolution of vector fields with a vector field filter with a
    * large filter diameter to trigger FFT use.
    */
  test("vector convolve dynamic vector, 2D by FFT") {
    class TestVectorConvolve2D(bigVector: Boolean)
            extends ComputeGraph(Optimize, fftUse = UseFFTAlways) with RefTestInterface {
      val FilterSize = 23

      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3 * FilterSize
      val Columns = 4 * FilterSize

      val kernelMatrix = Matrix(FilterSize, FilterSize, (r,c) => (r + c) / 10f)

      val stepChange = 0.75f

      val kernels = Array.tabulate(VectorSize) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      // Adding a bias reduces the relative error, helping this test to pass on GPUs with low-end FP support.
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns)) + 1.0f
      }
      val expectedSlicesOut = Array.tabulate(VectorSize) {
        i => convolve(slices(i), kernels(i), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = VectorField(FilterSize, FilterSize, (r, c) =>
        Vector(VectorSize, (i) => kernelMatrix(r,c) + i/10f)
      )
      kernelIn <== kernelIn * stepChange
      val vectorOut = convolve(vectorIn, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        reset
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) * stepChange ~== readScalar(slicedVectorOut(i)))
        step
        for (i <- 0 until VectorSize)
          require(readScalar(expectedSlicesOut(i)) * stepChange * stepChange ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorConvolve2D(bigVector = false)
    new TestVectorConvolve2D(bigVector = true)
  }


  /** Test convolution of "big vector" and "small vector" vector fields with
    * a scalar field filter, using the BorderZero border policy.
    */
  test("vector convolve scalar, 2D, BorderZero") {
    class TestVectorConvolveZero2D(bigVector: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2
      val Rows = 4
      val Columns = 4

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut =
        slices.map(slice => convolve(slice, kernel, BorderZero))

      val vectorIn = vectorField(slices)
      val vectorOut = convolve(vectorIn, kernel, BorderZero)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize) {
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        }
      }
    }

    new TestVectorConvolveZero2D(bigVector = false)
    new TestVectorConvolveZero2D(bigVector = true)
  }

  /** Test cross-correlation of "big vector" and "small vector" vector fields with
    * a vector field filter.
    */
  test("vector cross-correlate vector, 2D") {
    class TestVectorCrossCorrelate2D(bigVector: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3
      val Columns = 4

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut =
        slices.map(slice => crossCorrelate(slice, kernel, BorderClamp))

      val vectorIn = vectorField(slices)
      val vectorOut = crossCorrelate(vectorIn, kernel, BorderClamp)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize) {
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        }
      }
    }

    new TestVectorCrossCorrelate2D(bigVector = false)
    new TestVectorCrossCorrelate2D(bigVector = true)
  }


  /** Test frame projection of a vector field image with a vector field filter.
    */
  test("vector projectFrame vector, 2D") {
    class TestVectorCrossCorrelateFrame2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)
      val Rows = 3
      val Columns = 4

      val kernels = Array.tabulate(numOutputPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => crossCorrelate(slices(i % numImagePlanes), kernels(i), BorderClamp)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = projectFrame(vectorIn, kernelIn, BorderClamp)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorCrossCorrelateFrame2D(3, 9)
    new TestVectorCrossCorrelateFrame2D(5, 10)
    new TestVectorCrossCorrelateFrame2D(1, 5) // degenerate frame case
    new TestVectorCrossCorrelateFrame2D(4, 4) // degenerate frame case
  }

  /** Test frame projection of a vector batched field image with a vector field filter.
    */
  test("batched vector projectFrame vector, 2D") {
    class TestBatchedVectorCrossCorrelateFrame2D(planesPerLogicalImage: Int, planesPerLogicalOutput: Int, batchSize: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(planesPerLogicalOutput % planesPerLogicalImage == 0)
      val Rows = 3
      val Columns = 4

      val kernels = Array.tabulate(planesPerLogicalOutput) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        i => {
          val imageIndex = i / planesPerLogicalOutput
          val thisImageOutputIndex = i % planesPerLogicalOutput
          val imagePlaneIndex = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage
          val filterPlaneIndex = i % planesPerLogicalOutput
          crossCorrelate(slices(imagePlaneIndex), kernels(filterPlaneIndex), BorderClamp)
        }
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = projectFrame(vectorIn, kernelIn, BorderClamp, batchSize = batchSize)
      val slicedVectorOut = Array.tabulate(planesPerLogicalOutput * batchSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until planesPerLogicalOutput * batchSize) {
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        }
      }
    }

    new TestBatchedVectorCrossCorrelateFrame2D(2, 6, 2)
    new TestBatchedVectorCrossCorrelateFrame2D(3, 9, 2)
    new TestBatchedVectorCrossCorrelateFrame2D(5, 10, 5)
    new TestBatchedVectorCrossCorrelateFrame2D(1, 5, 3) // degenerate frame case
    new TestBatchedVectorCrossCorrelateFrame2D(4, 4, 4) // degenerate frame case
  }

  /** Test frame projection of a vector field image with a vector field filter with bundled block reduce.
    */
  test("vector projectFrameBlockReduceSum vector, 2D") {
    class TestVectorProjectFrameBlockReduceSum2D(outRows: Int, outColumns: Int, numImagePlanes: Int, numFilterPlanes: Int, borderPolicy: BorderPolicy = BorderClamp)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numFilterPlanes % numImagePlanes == 0)
      val numOutputPlanes = numFilterPlanes / numImagePlanes

      val filterRows = kernelMatrix.rows
      val filterColumns = kernelMatrix.columns

      val inputRows = borderPolicy match {
        case BorderValid => outRows + filterRows - 1
        case BorderFull => outRows - filterRows + 1
        case BorderClamp => outRows
        case BorderZero => outRows
        case BorderCyclic => outRows
      }

      val inputColumns = borderPolicy match {
        case BorderValid => outColumns + filterColumns - 1
        case BorderFull => outColumns - filterColumns + 1
        case BorderClamp => outColumns
        case BorderZero => outColumns
        case BorderCyclic => outColumns
      }

      val kernels = Array.tabulate(numFilterPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(inputRows, inputColumns))
      }
      val convolvedSlices = Array.tabulate(numFilterPlanes) {
        i => crossCorrelate(slices(i % numImagePlanes), kernels(i), borderPolicy)
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => (i*numImagePlanes until (i+1)*numImagePlanes).map(convolvedSlices(_)).reduceLeft(_ + _)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = projectFrameBlockReduceSum(vectorIn, kernelIn, borderPolicy)
      // The following 2 fields check the operation of projectFrameBlockReduceSum invoked as a user would (relying on
      // the optimizer to swap in the kernel).  The use of flip() in creating vectorOut3 exists to prevent this
      // optimization and to foil common subexpression elimination on the common input potentially.
      val vectorOut2 = blockReduceSum(projectFrame(vectorIn, kernelIn, borderPolicy), numImagePlanes)

      val vectorOut3 = blockReduceSum(flip(flip(projectFrame(flip(flip(vectorIn)), kernelIn, borderPolicy))), numImagePlanes)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) =>
          if (vectorOut.tensorShape.dimensions == 0)
            vectorOut
          else
            vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)
      probe(vectorOut2, vectorOut3)

      withRelease {
        step

        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        if (numOutputPlanes == 1)
          require(readScalar(vectorOut2) ~== readScalar(vectorOut3))
        else
          require(readVector(vectorOut2) ~== readVector(vectorOut3))
      }
    }

    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 9)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 5, 10)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 24)
    // The following tests have big fields to invoke multi-plane approach in kernel
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 16)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 14)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 12)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 10)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 8)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 6)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 4)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 2)
    // degenerate frame cases
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 1, 5)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 1, 1)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 2, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 3)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 4, 4)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 5, 5)


    // Test Tiled convolve approach, enabled within an optimizer when borderPolicy = BorderValid

    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 9, BorderValid)

  }
  /** Test frame projection of a vector field image with a vector field filter with bundled block reduce.
    */
  test("batched vector projectFrameBlockReduceSum vector, 2D") {
    class TestVectorProjectFrameBlockReduceSum2D(outRows: Int, outColumns: Int, planesPerLogicalImage: Int, totalFilterPlanes: Int, batchSize: Int, borderPolicy: BorderPolicy = BorderClamp)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(totalFilterPlanes % planesPerLogicalImage == 0)
      val numLogicalFilters = totalFilterPlanes / planesPerLogicalImage
      val numOutputPlanes = batchSize * numLogicalFilters

      val filterRows = kernelMatrix.rows
      val filterColumns = kernelMatrix.columns

      val inputRows = borderPolicy match {
        case BorderValid => outRows + filterRows - 1
        case BorderFull => outRows - filterRows + 1
        case BorderClamp => outRows
        case BorderZero => outRows
        case BorderCyclic => outRows
      }

      val inputColumns = borderPolicy match {
        case BorderValid => outColumns + filterColumns - 1
        case BorderFull => outColumns - filterColumns + 1
        case BorderClamp => outColumns
        case BorderZero => outColumns
        case BorderCyclic => outColumns
      }

      val kernels = Array.tabulate(totalFilterPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(planesPerLogicalImage * batchSize) {
        (i) => TestScalarField(RefScalarField.random(inputRows, inputColumns))
      }
      val convolvedSlices = Array.tabulate(totalFilterPlanes * batchSize) {
        i => {
          val imageIndex = i / totalFilterPlanes
          val thisImageOutputIndex = i % totalFilterPlanes
          val imagePlaneIndex = imageIndex * planesPerLogicalImage + thisImageOutputIndex % planesPerLogicalImage
          val filterPlaneIndex = i % totalFilterPlanes
          crossCorrelate(slices(imagePlaneIndex), kernels(filterPlaneIndex), borderPolicy)
        }
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => (i*planesPerLogicalImage until (i+1)*planesPerLogicalImage).map(convolvedSlices(_)).reduceLeft(_ + _)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = projectFrameBlockReduceSum(vectorIn, kernelIn, borderPolicy, batchSize = batchSize)
      // The following 2 fields check the operation of projectFrameBlockReduceSum invoked as a user would (relying on
      // the optimizer to swap in the kernel).  The use of flip() in creating vectorOut3 exists to prevent this
      // optimization and to foil common subexpression elimination on the common input potentially.
      val vectorOut2 = blockReduceSum(projectFrame(vectorIn, kernelIn, borderPolicy, batchSize = batchSize), planesPerLogicalImage)

      val vectorOut3 = blockReduceSum(flip(flip(projectFrame(flip(flip(vectorIn)), kernelIn, borderPolicy, batchSize = batchSize))), planesPerLogicalImage)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) =>
          if (vectorOut.tensorShape.dimensions == 0)
            vectorOut
          else
            vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)
      probe(vectorOut2, vectorOut3)

      withRelease {
        step

        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        if (numOutputPlanes == 1)
          require(readScalar(vectorOut2) ~== readScalar(vectorOut3))
        else
          require(readVector(vectorOut2) ~== readVector(vectorOut3))
      }
    }

    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 2, 6, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 9, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 5, 10, 3)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 24, 4)
    // The following tests have big fields to invoke multi-plane approach in kernel
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 16, 5)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 14, 2)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 12, 3)
    new TestVectorProjectFrameBlockReduceSum2D(64, 64, 2, 10, 4)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 8, 5)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 6, 6)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 4, 7)
    new TestVectorProjectFrameBlockReduceSum2D(96, 96, 2, 2, 2)
    // degenerate frame cases
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 1, 5, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 1, 1, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 2, 2, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 3, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 4, 4, 2)
    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 5, 5, 2)

    // Test Tiled convolve approach, enabled within an optimizer when borderPolicy = BorderValid

    new TestVectorProjectFrameBlockReduceSum2D(3, 4, 3, 9, 2, BorderValid)

  }

  /** Test frame projection of a vector field image with a vector field filter
    * with integrated downsampling.
    */
  test("vector projectFrame vector, 2D with downsampling") {
    class TestVectorCrossCorrelateFrame2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)

      val ScaleFactor = 2
      val Rows = 32
      val Columns = 40

      val kernels = Array.tabulate(numOutputPlanes) {
        i => TestScalarField(kernelMatrix + i/10f)
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => downsample(crossCorrelate(slices(i % numImagePlanes), kernels(i), BorderClamp), 2)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = projectFrame(vectorIn, kernelIn, BorderClamp, DownsampleOutputConvolution(ScaleFactor))
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorCrossCorrelateFrame2D(3, 9)
    new TestVectorCrossCorrelateFrame2D(5, 10)
    new TestVectorCrossCorrelateFrame2D(1, 5) // degenerate frame case
    new TestVectorCrossCorrelateFrame2D(4, 4) // degenerate frame case
  }

  /** Test filterAdjoint convolution of a vector field image with a vector field representation.
    */
  test("vector crossCorrelateFilterAdjoint vector, 2D") {
    class TestVectorCrossCorrelateFilterAdjoint2D(numImagePlanes: Int, numOutputPlanes: Int,
              InputRows: Int, InputColumns: Int, OutputSize: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)
      val numKernelPlanes = numOutputPlanes / numImagePlanes

      val KernelRows = InputRows - OutputSize + 1
      val KernelColumns = InputColumns - OutputSize + 1

      val kernels = Array.tabulate(numKernelPlanes) {
        i => TestScalarField(Matrix(KernelRows, KernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => crossCorrelate(slices(i % numImagePlanes), kernels(i / numImagePlanes), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = crossCorrelateFilterAdjoint(vectorIn, kernelIn, BorderValid)
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorCrossCorrelateFilterAdjoint2D(3, 12, 71, 23, 7)
    new TestVectorCrossCorrelateFilterAdjoint2D(2, 4, 19, 17, 7)
    new TestVectorCrossCorrelateFilterAdjoint2D(1, 4, 31, 23, 12) // degenerate frame case
    new TestVectorCrossCorrelateFilterAdjoint2D(6, 6, 23, 29, 7) // degenerate frame case
  }

  /** Test filterAdjoint convolution of a vector field image with a vector field representation.
    */
  test("vector crossCorrelateFilterAdjoint vector, 2D with upsampling") {
    class TestVectorCrossCorrelateFilterAdjoint2D(numImagePlanes: Int, numOutputPlanes: Int)
            extends ComputeGraph(Optimize) with RefTestInterface {
      require(numOutputPlanes % numImagePlanes == 0)

      val UpSampleFactor = 2
      val numKernelPlanes = numOutputPlanes / numImagePlanes

      val OutputSize = 5

      val InputRows = 16
      val InputColumns = 18

      val KernelRows = (InputRows - OutputSize + 1) / UpSampleFactor
      val KernelColumns = (InputColumns - OutputSize + 1)  / UpSampleFactor

      val kernels = Array.tabulate(numKernelPlanes) {
        i => TestScalarField(Matrix(KernelRows, KernelColumns, (r,c) => 10*r + c + i/10f))
      }
      val slices = Array.tabulate(numImagePlanes) {
        (i) => TestScalarField(RefScalarField.random(InputRows, InputColumns))
      }
      val expectedSlicesOut = Array.tabulate(numOutputPlanes) {
        i => crossCorrelate(slices(i % numImagePlanes), upsample(kernels(i / numImagePlanes), UpSampleFactor), BorderValid)
      }

      val vectorIn = vectorField(slices)
      val kernelIn = vectorField(kernels)
      val vectorOut = crossCorrelateFilterAdjoint(vectorIn, kernelIn,
        BorderValid, UpsampleInputConvolution(UpSampleFactor))
      val slicedVectorOut = Array.tabulate(numOutputPlanes) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until numOutputPlanes)
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
      }
    }

    new TestVectorCrossCorrelateFilterAdjoint2D(3, 12)
    new TestVectorCrossCorrelateFilterAdjoint2D(2, 4)
    new TestVectorCrossCorrelateFilterAdjoint2D(1, 4) // degenerate frame case
    new TestVectorCrossCorrelateFilterAdjoint2D(6, 6) // degenerate frame case
  }

  /** Test cross-correlation of "big vector" and "small vector" vector fields with
    * a vector field filter, using the BorderZero border policy.
    */
  test("vector cross-correlate vector, 2D, BorderZero") {
    class TestVectorCrossCorrelateZero2D(bigVector: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val VectorSize = if (bigVector) 10 else 2
      val Rows = 3
      val Columns = 4

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(VectorSize) {
        (i) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut =
        slices.map(slice => crossCorrelate(slice, kernel, BorderZero))

      val vectorIn = vectorField(slices)
      val vectorOut = crossCorrelate(vectorIn, kernel, BorderZero)
      val slicedVectorOut = Array.tabulate(VectorSize) {
        (i) => vectorElement(vectorOut, i)
      }

      probe((expectedSlicesOut ++ slicedVectorOut): _*)

      withRelease {
        step
        for (i <- 0 until VectorSize) {
          require(readScalar(expectedSlicesOut(i)) ~== readScalar(slicedVectorOut(i)))
        }
      }
    }

    new TestVectorCrossCorrelateZero2D(bigVector = false)
    new TestVectorCrossCorrelateZero2D(bigVector = true)
  }

  /** Test convolution of "big matrix" and "small matrix" matrix fields with
    * a scalar field filter.
    */
  test("matrix convolve scalar, 2D") {
    class TestMatrixConvolve2D(bigMatrix: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val MatrixSize = if (bigMatrix) 4 else 2
      val Rows = 3
      val Columns = 4

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => convolve(slices(i)(j), kernel, BorderClamp)
      }

      val matrixIn = matrixField(slices)
      val matrixOut = convolve(matrixIn, kernel, BorderClamp)
      val slicedVectorOut = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => vectorElement(matrixRow(matrixOut, i), j)
      }

      expectedSlicesOut.foreach(fieldArray => fieldArray.foreach(probe(_)))
      slicedVectorOut.foreach(fieldArray => fieldArray.foreach(probe(_)))

      withRelease {
        step
        for (i <- 0 until MatrixSize; j <- 0 until MatrixSize) {
          require(readScalar(expectedSlicesOut(i)(j)) ~== readScalar(slicedVectorOut(i)(j)))
        }
      }
    }

    new TestMatrixConvolve2D(bigMatrix = false)
    new TestMatrixConvolve2D(bigMatrix = true)
  }

  /** Test cross-correlation of "big matrix" and "small matrix" matrix fields with
    * a scalar field filter.
    */
  test("matrix cross-correlate scalar, 2D") {
    class TestMatrixCrossCorrelate2D(bigMatrix: Boolean)
            extends ComputeGraph(Optimize) with RefTestInterface {
      val MatrixSize = if (bigMatrix) 4 else 2
      val Rows = 3
      val Columns = 4

      val kernel = TestScalarField(kernelMatrix)
      val slices = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => TestScalarField(RefScalarField.random(Rows, Columns))
      }
      val expectedSlicesOut = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => crossCorrelate(slices(i)(j), kernel, BorderClamp)
      }

      val matrixIn = matrixField(slices)
      val matrixOut = crossCorrelate(matrixIn, kernel, BorderClamp)
      val slicedVectorOut = Array.tabulate(MatrixSize, MatrixSize) {
        (i, j) => vectorElement(matrixRow(matrixOut, i), j)
      }

      expectedSlicesOut.foreach(fieldArray => fieldArray.foreach(probe(_)))
      slicedVectorOut.foreach(fieldArray => fieldArray.foreach(probe(_)))

      withRelease {
        step
        for (i <- 0 until MatrixSize; j <- 0 until MatrixSize) {
          require(readScalar(expectedSlicesOut(i)(j)) ~== readScalar(slicedVectorOut(i)(j)))
        }
      }
    }

    new TestMatrixCrossCorrelate2D(bigMatrix = false)
    new TestMatrixCrossCorrelate2D(bigMatrix = true)
  }

  /** Test convolution of a scalar field with a matrix field.  This has an
    * interpretation unlike the regular convolution: each filter is stored as
    * the entire tensor at a field point, rather than being a "tensor slice"
    * of the field.
    */
  test("scalar-matrix convolution") {

    val Rows = 4
    val Columns = 5
    val FilterRows = 2
    val FilterColumns = 3
    val FilterSize = 3
    val scalarField = RefScalarField.random(Rows, Columns)
    val matrices = Array2D.tabulate(FilterRows, FilterColumns) {
      (r, c) => Matrix.random(FilterSize, FilterSize)
    }
    val matrixField = RefMatrixField(FilterRows, FilterColumns,
      (r, c) => matrices(r, c))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val dynamicScalar = TestScalarField(scalarField)
      val dynamicMatrix = TestMatrixField(matrixField)

      val dynamicConvolved = Array2D.tabulate[ScalarField](FilterRows, FilterColumns) {
        (r, c) => convolve(dynamicScalar, TestScalarField(matrices(r, c).flip), BorderClamp)
      }

      val expected = vectorField(dynamicConvolved.toArray)

      val dynamicResult = crossCorrelate(dynamicScalar, dynamicMatrix, BorderClamp)

      probe(dynamicResult, expected)
    }

    import graph._
    withRelease {
      step
      require(readVector(dynamicResult) ~== readVector(expected))
    }
  }

  /** This problem case, discovered and distilled down by Matthew Pickett, generated the exception:
    *   "unknown cause: code -9999 [error: unknown]" from the OpenCL subsystem.
    *
    * The underlying bug was a convolution kernel that read outside the bounds of its input in BorderValid mode (reading
    * data into the input tile that was then never used to produce an output).  Of note, the input size here is 1MByte,
    * which is thought to be NVidia's TLB size, so the error was probably caused by a TLB exception.
    */
  test("BorderValid convolution input bounds") {
    class Test(vecLen: Int) {
      val cg = new ComputeGraph{
        val inputSize = 32
        val filterSize = 5

        // works with any vecLen != 256
        val a = VectorField.random(Shape(inputSize,inputSize), Shape(vecLen))
        val b = VectorField.random(Shape(filterSize,filterSize), Shape(vecLen))
        val c = convolve(a, b, BorderValid)
        probe(c)
      }
      import cg._
      withRelease(
        step(1)
      )
    }
    new Test(256)
  }

}
