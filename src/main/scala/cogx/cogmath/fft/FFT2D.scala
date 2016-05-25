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

package cogx.cogmath.fft

import cogx.cogmath.algebra.real.Logarithm
import cogx.cogmath.algebra.complex.{Complex, ComplexArray, ComplexMatrix}

/** Class which performs a 2D FFT (and its inverse) on 2-dimensional, complex
  * data.
  *
  * The number of rows and columns in the data must each be a power of 2.
  *
  * The FFT2D class instances hold the state of the input in a functional form,
  * deferring instantiation of the input until it is needed by an output
  * element request.  The output is returned in a functional form.
  *
  */
private [cogx] class FFT2D(rows: Int, columns: Int, f: (Int, Int) => Complex) {
  // Input ComplexMatrix is discarded after the transformation is applied.
  lazy val transformed = {
    val input = ComplexMatrix(rows, columns, f)
    FFT2D.transform(input)
  }
  lazy val inverseTransformed = {
    val input = ComplexMatrix(rows, columns, f)
    FFT2D.inverseTransform(input)
  }
  def transform: (Int, Int) => Complex = transformed.apply
  def inverseTransform: (Int, Int) => Complex = inverseTransformed.apply
}

/** Object which performs a 2D FFT (and its inverse) on 2-dimensional, complex
  * data.
  *
  * The number of rows and columns in the data must each be a power of 2.
  *
  */
object FFT2D extends Logarithm {

  /** Take the 2D FFT of "matrix". */
  def transform(matrix: ComplexMatrix): ComplexMatrix = {
    val rowVectors = matrix.toRowVectors
    transform(rowVectors)
    new ComplexMatrix(rowVectors)
  }

  /** Take the 2D FFT of input data described functionally, returning
   * the transform also as a function */
  def transform(rows: Int, columns: Int,
                f: (Int, Int) => Complex): (Int, Int) => Complex = {
    new FFT2D(rows, columns, f).transform
  }

  /** Take the inverse 2D FFT of "matrix". */
  def inverseTransform(matrix: ComplexMatrix): ComplexMatrix = {
    val rowVectors = matrix.toRowVectors
    inverseTransform(rowVectors)
    new ComplexMatrix(rowVectors)
  }

  /** Take the inverse 2D FFT of input data described functionally, returning
   * the inverse transform also as a function */
  def inverseTransform(rows: Int, columns: Int,
                f: (Int, Int) => Complex): (Int, Int) => Complex = {
    new FFT2D(rows, columns, f).inverseTransform
  }
  /** Do an in-place 2D FFT of "rowVectors" (emulating a complex matrix). */
  private def transform(rowVectors: Array[ComplexArray]) {
    val rows = rowVectors.length
    val columns = rowVectors(0).length
    require(isPowerOf2(rows) && isPowerOf2(columns),
      "FFT is only supported for field dimensions which are powers of 2")
    val columnVectors = new Array[ComplexArray](columns)
    for (col <- 0 until columns)
      columnVectors(col) = new ComplexArray(rows)

    // Transform the rows.
    val rowFFT = FFT(columns)
    for (r <- 0 until rows)
      rowFFT.transform(rowVectors(r))

    // Transpose and transform the rows (really the columns)
    transpose(rowVectors, columnVectors)
    val colFFT = FFT(rows)
    for (c <- 0 until columns)
      colFFT.transform(columnVectors(c))

    // Transpose to get back to rows
    transpose(columnVectors, rowVectors)
  }


  /** Do an in-place 2D inverse FFT of "rowVectors" (a complex matrix). */
  private def inverseTransform(rowVectors: Array[ComplexArray]) {
    val rows = rowVectors.length
    val columns = rowVectors(0).length
    require(isPowerOf2(rows) && isPowerOf2(columns))
    val columnVectors = new Array[ComplexArray](columns)
    for (col <- 0 until columns)
      columnVectors(col) = new ComplexArray(rows)

    // Transform the rows.
    val rowFFT = FFT(columns)
    for (r <- 0 until rows)
      rowFFT.inverseTransform(rowVectors(r))

    // Transpose and transform the rows (really the columns)
    transpose(rowVectors, columnVectors)
    val colFFT = FFT(rows)
    for (c <- 0 until columns)
      colFFT.inverseTransform(columnVectors(c))

    // Transpose to get back to rows
    transpose(columnVectors, rowVectors)
  }


  // Transpose row vectors to column vectors.
  private def transpose(from: Array[ComplexArray], to: Array[ComplexArray]) {
    val rows = from.length
    val columns = from(0).length
    for (row <- 0 until rows; col <- 0 until columns)
      to(col)(row) = from(row)(col)
  }
}
