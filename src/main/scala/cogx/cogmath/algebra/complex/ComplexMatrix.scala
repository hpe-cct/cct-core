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

package cogx.cogmath.algebra.complex

import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Matrix
import java.util.Random


/** An immutable matrix of complex numbers.
  *
  * @author Greg Snider
  */

@SerialVersionUID(4152277281786508448L)
class ComplexMatrix(val rows: Int, val columns: Int,
                    private val data: Array[Array[Complex]])
        extends Serializable
{
  /** Create a `rows` x `columns` complex matrix with elements defined by `f`.*/
  def this(rows: Int, columns: Int, f: (Int, Int) => Complex) = {
    this(rows, columns, {
      val newData = Array.ofDim[Complex](rows, columns)
      for (row <- 0 until rows; col <- 0 until columns)
        newData(row)(col) = f(row, col)
      newData
    })
  }

  /** Create a `rows` x `columns` complex matrix filled with zeros.*/
  def this(rows: Int, columns: Int) =
    this(rows, columns, Array.tabulate[Complex](rows, columns) {
      (row, col) => new Complex(0, 0)
    })

  /** Coerce a (mutable) real Matrix to an immutable ComplexMatrix. */
  def this(matrix: Matrix) =
    this(matrix.rows, matrix.columns, {
      val newData = Array.ofDim[Complex](matrix.rows, matrix.columns)
      for (row <- 0 until matrix.rows; col <- 0 until matrix.columns)
        newData(row)(col) = new Complex(matrix(row, col), 0)
      newData
    })

  /** Create a ComplexMatrix from a `real` and `imaginary` Matrices. */
  def this(real: Matrix, imaginary: Matrix) =
    this(real.rows, real.columns, {
      val newData = Array.ofDim[Complex](real.rows, real.columns)
      for (row <- 0 until real.rows; col <- 0 until real.columns)
        newData(row)(col) = new Complex(real(row, col), imaginary(row, col))
      newData
    })

  /** Create a ComplexMatrix from a 2D array of complex elements. */
  def this(data: Array[Array[Complex]]) =
    this(data.length, data(0).length, {
      val rows = data.length
      val columns = data(0).length
      val newData = Array.ofDim[Complex](rows, columns)
      for (row <- 0 until rows; col <- 0 until columns)
        newData(row)(col) = data(row)(col)
      newData
    })

  /** Create a ComplexMatrix from an array of "rows" data. */
  def this(rows: Array[ComplexArray]) =
     this(rows.length, rows(0).length, {
       val data = Array.ofDim[Complex](rows.length, rows(0).length)
       for (row <- 0 until rows.length; col <- 0 until rows(0).length) {
         data(row)(col) = rows(row)(col)
       }
       data
     })

  val shape = Shape(rows, columns)

  /** Tensor accessor. */
  protected[cogx] def read(index: Int) = data(index)

  /** Read element (`row`, `column`) of this complex matrix (rarely used). */
  def apply(row: Int, column: Int): Complex = {
    data(row)(column)
  }

  /** Peek at the matrix data, flattened to a ComplexArray. */
//  private[cogx] def flattened: ComplexArray = data

  /** Get the real part of the matrix. */
  def realPart: Matrix = {
    val real = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      real(row, col) = data(row)(col).real
    real
  }

  /** Get the imaginary part of the matrix. */
  def imaginaryPart: Matrix = {
    val imaginary = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      imaginary(row, col) = data(row)(col).real
    imaginary
  }

  /** Internal-only method for writing an element in `this`. */
  def update(row: Int, column: Int, value: Complex) {
    data(row)(column) = value
  }

  def +(d: Float) = map(_ + d)

  def +(d: Complex) = map(_ + d)

  def -(d: Float) = map(_ - d)

  def -(d: Complex) = map(_ - d)

  def *(d: Float) = map(_ * d)

  def *(c: Complex) = map(_ * c)

  def +(that: ComplexMatrix): ComplexMatrix = combine(that, _ + _)

  def -(that: ComplexMatrix): ComplexMatrix = combine(that, _ - _)

  /** Element-wise multiplication of two complex matrices. */
  def :*(that: ComplexMatrix): ComplexMatrix = combine(that, _ * _)

  /** Element-wise division of two complex matrices. */
  def :/(that: ComplexMatrix): ComplexMatrix = combine(that, _ / _)


  def combine(that: ComplexMatrix, f: (Complex, Complex) => Complex): ComplexMatrix = {
    require(this.rows == that.rows && this.columns == that.columns)
    val combo = new ComplexMatrix(this.rows, this.columns)
    for (row <- 0 until rows; col <- 0 until columns)
      combo.data(row)(col) = f(this.data(row)(col), that.data(row)(col))
    combo
  }

  /** Get the data in the tensor, flattened to a linear array. */
  protected def getData = data

  /** Expand the matrix, optionally extending the border into the expanded
    * region. This operation is a key part of the FFT. The new matrix is of
    * size `bigRows` x `bigColumns` and element (0, 0) of this is anchored at
    * (0, 0) in the larger matrix. If `borderFill` is true, then the four
    * edges of the matrix are extended evenly in all directions, as though
    * the bigger matrix were actually a torus with opposite edges touching.
    */
  def expand(bigRows: Int, bigColumns: Int, borderFill: Boolean):ComplexMatrix =
  {
    require(bigRows >= rows && bigColumns >= columns)
    val big = new ComplexMatrix(bigRows, bigColumns)

    // Copy `this` into big
    for (row <- 0 until rows; col <- 0 until columns)
      big(row, col) = apply(row, col)

    // Now copy the border along the edges if requested
    if (borderFill) {
      val nearBottomApronSize = (bigRows - rows) / 2
      val farBottomApronSize = (bigRows - rows) - nearBottomApronSize
      val nearRightApronSize = (bigColumns - columns) / 2
      val farRightApronSize = (bigColumns - columns) - nearRightApronSize
      // far right edge
      for (col <- big.columns - farRightApronSize until big.columns)
        for (row <- 0 until rows)
          big(row, col) = apply(row, 0)
      // near right edge
      for (col <- columns until (columns + nearRightApronSize))
        for (row <- 0 until rows)
          big(row, col) = apply(row, columns - 1)
      //  far bottom edge, copied from expanded matrix 'big' along entire row
      for (row <- big.rows - farBottomApronSize until big.rows)
        for (col <- 0 until bigColumns)
          big(row, col) = big(0, col)
      // near bottom edge, copied from expanded matrix 'big' along entire row
      for (row <- rows until rows + nearBottomApronSize)
        for (col <- 0 until bigColumns)
          big(row, col) = big(rows - 1, col)
    }
    big
  }

  def upsample: ComplexMatrix = {
    val big = new ComplexMatrix(rows * 2, columns * 2)
    for (row <- 0 until rows; col <- 0 until columns)
      big(2 * row, 2 * col) = this(row, col)
    big
  }

  /**
    * Trim `this` to a `smallRows` x `smallColumns` complex matrix.
    */
  def trim(smallRows: Int, smallColumns: Int): ComplexMatrix = {
    require(smallRows <= rows && smallColumns <= columns)
    val small = new ComplexMatrix(smallRows, smallColumns)
    for (row <- 0 until smallRows; col <- 0 until smallColumns)
      small(row, col) = apply(row, col)
    small
  }

 /** Shift a matrix with zero fill on the edges.
    *
    * @param shiftRows Number of rows to shift `this` down.
    * @param shiftColumns Number of columns to shift `this` to the right.
    * @return Shifted complex matrix, zero-filling on edges.
    */
  def shift(shiftRows: Int, shiftColumns: Int): ComplexMatrix = {
     val Zero = new Complex(0, 0)

    val shifted = new ComplexMatrix(this.rows, this.columns)
    for (row <- 0 until this.rows; col <- 0 until this.columns) {
      val sRow = row - shiftRows
      val sCol = col - shiftColumns
      val pixel =
        if (sRow >= 0 && sRow < this.rows && sCol >= 0 && sCol < this.columns)
          this(sRow, sCol)
        else
          Zero
      shifted(row, col) = pixel
    }
    shifted
  }

  /**
   * Shifts the elements of a matrix cyclicly as though it were a torus, wrapping
   * around the left side to the right side, and the top to the bottom. The
   * rotation is specified by the tuple("deltaRows", "deltaColumns"). For
   * example, the tuple value (2, 3) would cause the element at location (0, 0)
   * to be moved to location (2, 3). In a 5 x 5 matrix, the same tuple would
   * cause the element at (4, 4) to be moved to (1, 2), wrapping around the
   * torus. Returns the cyclicly shifted matrix.
   */
  def shiftCyclic(deltaRows: Int, deltaColumns: Int): ComplexMatrix = {
    new ComplexMatrix(rows, columns, (r: Int, c: Int) => {
      // Works even if % of negative returns negative
      val sourceRow = (((r - deltaRows) % rows) + rows) % rows
      val sourceCol = (((c - deltaColumns) % columns) + columns) % columns
        apply(sourceRow, sourceCol)
    })
  }

  /** Transpose `this`. */
  def transpose: ComplexMatrix = {
    val self = this
    new ComplexMatrix(self.columns, self.rows,
      (row: Int, col: Int) => self(col, row))
  }

  def map(f: Complex => Complex): ComplexMatrix = {
    val result = Array.ofDim[Complex](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      result(row)(col) = f(data(row)(col))
    new ComplexMatrix(rows, columns, result)
  }

  /** Break `this` into an array of row vectors. */
  def toRowVectors: Array[ComplexArray] = {
    Array.tabulate[ComplexArray](rows) {
      row => {
        val array = new ComplexArray(columns)
        for (col <- 0 until columns)
          array(col) = data(row)(col)
        array
      }
    }
  }

  /** Compare `this` and `that` for approximate equality. */
  def ~==(that: ComplexMatrix): Boolean = {
    require(this.rows == that.rows && this.columns == that.columns)
    for (row <- 0 until rows; col <- 0 until columns)
      if (!(this.data(row)(col) ~== that.data(row)(col)))
        return false
    true
  }

  /** Print out the matrix for debugging. */
  def print {
    println("ComplexMatrix:")
    for (row <- 0 until rows) {
      for (col <- 0 until columns)
        printf("%s  ", apply(row, col).toString)
      println
    }
  }

  override def toString: String = "ComplexMatrix(" + rows + "," + columns + ")"
}

object ComplexMatrix {

  /** Create a `rows` x `columns` ComplexMatrix initialized by `f`. */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Complex) =
    new ComplexMatrix(rows, columns, f)


  /** Create a `rows` x `columns` ComplexMatrix filled with random numbers. */
  def random(rows: Int, columns: Int): ComplexMatrix = {
    val random = new Random
    val data = Array.ofDim[Complex](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      data(row)(col) = new Complex(random.nextFloat, random.nextFloat)
    new ComplexMatrix(rows, columns, data)
  }

  /** Create a `rows` x `columns` ComplexMatrix filled from an Iterable[Complex]. */
  def apply(rows: Int, columns: Int, values: Iterable[Complex]): ComplexMatrix = {
    val data = Array.ofDim[Complex](rows, columns)
    val iter = values.toIterator
    for (row <- 0 until rows; col <- 0 until columns)
      data(row)(col) = iter.next
    new ComplexMatrix(rows, columns, data)
  }
}
