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

package cogx.compiler.codegenerator.opencl.generator

import cogx.platform.types._
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.algebra.real.Logarithm
import java.io._
import cogx.compiler.parser.op.ConvolveOp
import cogx.utilities.InterpolatedFunction2D

/**
 * User: Dick Carter
 * Date: 10/3/13
 */
private[cogx]
object FFTConvolutionUtils extends Logarithm {

  /** Calculate the expanded FFT shape given the 2 inputs (the "image" and
    * "filter" by Cog convention).  FFTRows, FFTColumns must be a power-of-2 and
    * allow for any aprons required by the BorderPolicy.
    */
  def fftShape2D(imageType: FieldType, filterType: FieldType, op: ConvolveOp): Shape = {
    val imageRows = imageType.rows
    val imageColumns = imageType.columns
    require(imageType.fieldShape.dimensions == 2)

    val filterRows = filterType.rows
    val filterColumns = filterType.columns

    require(isComplexField(imageType) || isRealField(imageType))
    require(filterRows % 2 == 1, "Convolution kernel must have odd size.")
    require(filterColumns % 2 == 1, "Convolution kernel must have odd size.")

    // Compute size of FFT image. Note that we use 2 * apronSize
    // to accomodate border fill. For zero fill, 1 * apronSize is
    // sufficient.  For cyclic fill, if the image is a power of 2, technically
    // no border is required; however, if any expansion is performed, it
    // had better be at least 2 * apronSize.

    def fieldRowStride(rows: Int, columns: Int) = {
      val dummyType = new FieldType(Shape(rows, columns), Shape(), imageType.elementType)
      new FieldMemoryLayoutImpl(dummyType).fieldRowStride
    }

    val rowApronSize = filterRows / 2
    val columnApronSize = filterColumns / 2

    val fftRows = op.borderPolicy match {
      case BorderClamp => roundUpPowerOf2(imageRows + 2 * rowApronSize)
      case BorderZero => roundUpPowerOf2(imageRows + rowApronSize)
      case BorderCyclic =>
        if (!isPowerOf2(imageRows))
          roundUpPowerOf2(imageRows + 2 * rowApronSize)
        else
          imageRows
      case _ => throw new RuntimeException("Valid convolution not implemented here")
    }

    /* the expanded FFT columns, but ignoring minimum required by column padding */
    val minFftColumns = op.borderPolicy match {
      case BorderClamp => roundUpPowerOf2(imageColumns + 2 * columnApronSize)
      case BorderZero => roundUpPowerOf2(imageColumns + columnApronSize)
      case BorderCyclic =>
        if (!isPowerOf2(imageColumns) ||
            fieldRowStride(fftRows, imageColumns) > imageColumns)
          roundUpPowerOf2(imageColumns + 2 * columnApronSize)
        else
          imageColumns
      case _ => throw new RuntimeException("Valid convolution not implemented here")
    }
    // We don't want to alter the FFT kernel to be aware of the padding
    // we've inserted into the memory layout.  Thus, we expand the image
    // to be not only a power-of-2, but also a multiple of the MemoryBlockSize
    // (e.g. 16 or 32). This way, there is effectively no padding (other than
    //  border-fill) from the FFT kernel's perspective.  -RJC

    val fftColumns = fieldRowStride(fftRows, minFftColumns)
    require(isPowerOf2(fftColumns))
    val expandedShape = Shape(fftRows, fftColumns)
    expandedShape
  }

  /** Based on estimates, is fft-based convolution faster than standard convolution. */
  def fftConvolutionIsFaster(imageType: FieldType, filterType: FieldType, op: ConvolveOp, filterIsDynamic: Boolean): Boolean = {
    /** Print out comparision of FFT and std. convolve times */
    val Verbose = false
    /** Use the table-lookup approach from a file of convolution regression results */
    val EnableMeasurementBasedPolicy = true

    /** Threshold above which to use FFT with the old simple policy */
    val BigFilterPoints = 13 * 13
    /** Old, simple approach looked only at the filter size for choosing FFT */
    def simplePolicy() =
      filterType.fieldShape.points > BigFilterPoints * BigFilterPoints

    if (!EnableMeasurementBasedPolicy) {
      // Old policy was to use FFT for filter sizes above a certain threshold,
      // and standard convolution otherwise, irrespective of image size.
      simplePolicy()
    }
    else {
      val expandedSize = fftShape2D(imageType, filterType, op)
      val fftRows = expandedSize(0)
      val fftColumns = expandedSize(1)
      val fftCycleTime =
        stats.fftConvolveCycletime(fftRows, fftColumns, filterIsDynamic)
      val convolveCycleTime =
        stats.stdConvolveCycletime(imageType.fieldShape, filterType.fieldShape, filterIsDynamic)
      val fftIsFaster = fftCycleTime < convolveCycleTime

      if (Verbose) {
        if (fftIsFaster) {
          println("******** Chosing FFT time " + fftCycleTime +
                  " over slower convolve time " + convolveCycleTime)
        }
        else
          println("******** Chosing convolve time " + convolveCycleTime +
                  " over slower FFT time " + fftCycleTime)
      }

      fftIsFaster
    }
  }

  /** Convolution regression statistics access class, based on either a
    * locally generated resource: "compiler/convolutionStatsLocal.ser"
    * or a default resource: "compiler/convolutionStats.ser"
    */
  lazy val stats =
    try {
      new ConvolutionStats(
        "compiler/convolutionStatsLocal.ser")
    } catch {
      case x: FileNotFoundException =>
        try {
          new ConvolutionStats(
            "compiler/convolutionStats.ser")
        } catch {
          case _: FileNotFoundException =>
            throw new RuntimeException("Compiler error: missing resource " +
              "'compiler/convolutionStats.ser'")
        }
    }

  /** Reads in convolution statistics data and then looks-up or interpolates
    * between data values to estimate the runtime of standard and fft-based
    * convolution.
    */
  class ConvolutionStats(resource: String) extends Logarithm {
    val in = try {
      // First try to find the resource in the classpath.
      new ObjectInputStream(this.getClass.getClassLoader.getResourceAsStream(resource))
    } catch {
      case _: NullPointerException =>
        // If it isn't there, search the filesystem.
        val file = {
          // IntelliJ 14 now launches JUNIT tests from $PROJECT_DIR$/.idea/modules,
          // not from $PROJECT_DIR$ as before.
          val firstChoice = new File("src/main/resources/" + resource)
          if (firstChoice.exists)
            firstChoice
          else
            new File("../../src/main/resources/" + resource)
        }
        new ObjectInputStream(
          new BufferedInputStream(new FileInputStream(file)))
    }

    // Serialization of Scala's Tuple3 broke in going from Scala 2.10 to Scala 2.11
    // Now we store the convolution statistics only using Java's Array[Float]
    def loadTuple3Array(in: ObjectInputStream): Array[Tuple3[Float, Float, Float]] = {
      val flattened = in.readObject.asInstanceOf[Array[Float]]
      val inSize = flattened.length
      require(inSize % 3 == 0, "Unexpected array length in compiler tuning file.")
      val outArray = Array.tabulate(inSize/3){ i => (flattened(3*i), flattened(3*i+1),flattened(3*i+2))}
      outArray
    }

    val staticFilterConvolutionResults = loadTuple3Array(in)
    val staticFilterFFTResults = loadTuple3Array(in)
    val dynamidFilterFFTResults = loadTuple3Array(in)

    in.close()

    /** Std. convolution, f(imagePoints, filterPoints) -> cycleTime */
    val staticFilterConvolution =
      new InterpolatedFunction2D(staticFilterConvolutionResults)
    /** FFT-based static-filter convolve: f(imageRows, imageCols) -> cycleTime */
    val staticFilterFFT =
      new InterpolatedFunction2D(staticFilterFFTResults)
    /** FFT-based dynamic-filter convolve: f(imageRows, imageCols) -> cycleTime */
    val dynamicFilterFFT =
      new InterpolatedFunction2D(dynamidFilterFFTResults)

    /** The cycleTime of the given fft, looked-up from regression test results */
    def fftConvolveCycletime(fftRows: Int, fftColumns: Int, filterIsDynamic: Boolean): Double = {
      if (filterIsDynamic)
        dynamicFilterFFT.f(fftRows, fftColumns)
      else
        staticFilterFFT.f(fftRows, fftColumns)
    }

    /** Estimate the cycleTime of the given standard convolution via look-up and re-shaping */
    def stdConvolveCycletime(imageShape: Shape, filterShape: Shape, filterIsDynamic: Boolean): Double = {
      val SingleKernelTimeUsec = 20.0

      val imagePoints = imageShape.points
      val filterPoints = filterShape.points

      // The tests of the fft convolution with a dynamic filter use a short-
      // running kernel to update the filter.  To accurately compare against those
      // times, we add in a small amount to the static-filter std. convolve stats.

      val cycleTime = staticFilterConvolution.f(imagePoints, filterPoints) +
              ( if (filterIsDynamic) SingleKernelTimeUsec else 0.0f )
      cycleTime
    }
  }

}
