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

package cogx.cogmath.algebra.real

import cogx.cogmath.geometry.Shape
import cogx.utilities.Random
import PoorFloat._


/** A matrix.
  * <p>
  * In the following descriptions, "v" represents a vector, "s" a scalar, "m"
  * a matrix, "b" a boolean, "i" and integer, "d" a double, "r" a range of
  * integers (e.g. "1 to 5" or "3 until 9").
  * <p>
  * The update operations, such as "+=", update the elements of the matrix in
  * place.
  *
  * {{{
  * Constructors:
  *   Matrix(i, i)
  *   Matrix(i, i, Array[d])
  *   copy
  *
  * Dimensions:
  *   rows               => i        (number of rows in matrix)
  *   columns            => i        (number of columns in matrix)
  *   size               => i        (total # elements in matrix)
  *
  * Linear element access:
  *   apply(i)           => s        (access element linearly)
  *   update(i, s)                   (write element i with value s)
  *
  * 2D element access:
  *   apply(i, i)        => s        (access element by 2D coordinates)
  *   update(i, i, s)                (write element (i, i) with value s)
  *
  * Submatrix multi-element reads:
  *   row(i)             => m        (extract a copy of a row)
  *   column(i)          => m        (extract a copy of a column)
  *   submatrix(r, r)    => m        (extract a subregion of a matrix)
  *   reshape(i, i)      => m        (reshape the matrix)
  *
  * Matrix operations:
  *   map(f: d => d)     => m        (map a Matrix by applying f to each element)
  *   mapSelf(f: d => d)			    (use f to map each element in place)
  *   reduce((d, d)=> d) => d        (reduce a Matrix to a scalar)
  *   randomize                      (initialize to random values in [0, 1))
  *
  * Matrix / Scalar operations:
  *   m + s   => m          (add scalar to each element to produce a new matrix)
  *   m += s                (add scalar to all elements of matrix in-place)
  *   m - s   => m
  *   m -= s
  *   m * s   => m
  *   m *= s
  *   m / s   => m
  *   m /= s
  *   -m      => m          (map each element to new matrix my multiplying by -1)
  *   m := s                (assign scalar to all elements of the matrix)
  *
  * Matrix / Matrix operations:
  *   m + m          => m
  *   m += m
  *   m - m          => m
  *   m -= m
  *   m :* m         => m   (element-wise multiplication)
  *   m :*= m               (element-wise multiplication in-place)
  *   m :/ m         => m   (element-wise right division)
  *   m :/= n               (element-wise right division in-place)
  *   m :\ m         => m   (element-wise left division)
  *   m :\= m               (element-wise left division in-place)
  *   m1 === m2		=> b   (m1 and m2 have same shape, identical elements)
  *   m1 !=== m2     => b
  *   m1 ~== m2
  *   m1 !~== m2
  *   m1 := m2			   (assign elements of m2 to m1)
  *
  * Matrix multiplication:
  *   m * m          => m   (matrix multiplication)
  *
  * Matrix decompositions:
  *   cholesky       => m         (Cholesky decomposition)
  *   eigen          => (m, m)    (eigenvector matrix, diag eigenvalue matrix)
  *   lu             => (m, m)    (L matrix, U matrix)
  *   qr             => (m, m)    (Q matrix, R matrix)
  *   svd            => (m, m, m) (U matrix, S matrix, V matrix)
  *
  * Miscellaneous cogx.utilities:
  *   abs            => m   (absolute value of each element)
  *   sgn            => m   (sign of each element: -1, 0, 1)
  *   rectify        => m   (clip negative elements to 0)
  *   normalizeRows         (normalize rows using L2 norm)
  *   normalizeColumns      (normalize columns using L2 norm)
  *   print                 (print a matrix for debugging)
  *
  * }}}
  *
  * @param rows The number of rows in the matrix.
  * @param columns The number of columns in the matrix.
  * @param data Storage for matrix elements; length must be equal to
  *        rows * columns.
  *
  * @author Greg Snider
  */
@SerialVersionUID(-4510500996822152778L)
class Matrix(val rows: Int, val columns: Int, private[cogx] val data: Array[Float])
        extends Tensor
        with Serializable
{
  val size = rows * columns

  /** Shape of the matrix. */
  def shape = Shape(rows, columns)

  /** Tensor accessor. */
  def read(index: Int) = data(index)

  /** Constructor that allocates memory for Matrix (most common). */
  def this(rows: Int, columns: Int) = 
    this(rows, columns, new Array[Float](rows * columns))

  /** Create a Matrix from a 2-D tensor. */
  def this(tensor: Tensor) = this(tensor.shape(0), tensor.shape(1), {
    require(tensor.shape.dimensions == 2)
    require(tensor.length == tensor.shape.points)
    val newData = new Array[Float](tensor.shape.points)
    for (i <- 0 until tensor.length)
      newData(i) = tensor.read(i)
    newData
  })

  /** Make a copy of a Matrix. */
  def copy: Matrix = {
    val t = new Matrix(rows, columns, new Array[Float](size))
    for (i <- 0 until size)
      t(i) = this(i)
    t
  } 
  
  /** Linear element access */
  def apply(index: Int) = data(index)
  /** Modify an element given its linear position. */
  def update(index: Int, value: Float) {data(index) = value}
  
  /** 2D coordinates element access */
  def apply(row: Int, column: Int) = data(row * columns + column)
  /** Modify an element given its 2D coordinates. */
  def update(row: Int, column: Int, value: Float) {
    data(row * columns + column) = value
  }

  /** Synonym for submatrix.
    *
    * @param rows Range of rows to extract.
    * @param columns Range of columns to extract.
    * @return The specified submatrix.
    */
  def apply(rows: Range, columns: Range): Matrix = {
    submatrix(rows, columns)
  }

  /** Update a subset of a matrix.
    *
    * @param rowRange Range of rows to update.
    * @param columnRange Range of columns to update.
    * @param values The new values to be written in the submatrix specified
    *        by `rows` and `columns`. This must have the exact same shape
    *        as (rows, columns), or it must be a 1 x 1 matrix, in which case
    *        it is treated like a scalar.
    */
  def update(rowRange: Range, columnRange: Range, values: Matrix) {
    if (values.rows == 1 && values.columns == 1) {
      val scalar = values(0, 0)
      // We treat the values matrix like a scalar
      for (row <- rowRange; col <- columnRange)
        this(row, col) = scalar
    } else {
      // Not a scalar
      require(rowRange.size == values.rows, columnRange.size == values.columns)
      var valueRow = 0
      for (row <- rowRange) {
        var valueColumn = 0
        for (col <- columnRange) {
          this(row, col) = values(valueRow, valueColumn)
          valueColumn += 1
        }
        valueRow += 1
      }
    }
  }

  /** Write the row "row" with the data in "vector". */
  def update(row: Int, vector: Vector) {
    require(columns == vector.length)
    val offset = row * columns
    Array.copy(vector.data, 0, this.data, offset, vector.length)
  }
  
  // Submatrix multi-element reads:
  def row(rowIndex: Int) = submatrix(rowIndex to rowIndex, 0 until columns)
  def column(colIndex: Int) = submatrix(0 until rows, colIndex to colIndex)
  def submatrix(rowRange: Range, columnRange: Range): Matrix = {
    val sub = new Matrix(rowRange.length, columnRange.length)
    for (row <- 0 until sub.rows; col <- 0 until sub.columns)
      sub(row, col) = this(row + rowRange.start, col + columnRange.start)
    sub
  }
  def reshape(newRows: Int, newColumns: Int): Matrix = {
    require(newRows * newColumns == rows * columns)
    new Matrix(newRows, newColumns, data.map(e => e))
  }

  /** Flatten the rows of this matrix to a vector. */
  def toVector =  new Vector(data)

  /** View the matrix as a flat array of data. */
  def asArray  = data
  
  // Matrix operations:
  def map(f: Float => Float) = new Matrix(rows, columns, data.map(f))
  def mapSelf(f: Float => Float) {
    (0 until size).foreach(i => data(i) = f(data(i)))
  }

  def randomize = {
    for (i <- 0 until size)
      data(i) = Matrix.rand.nextFloat
    this
  }
  
  def transpose: Matrix = {
    val t = new Matrix(columns, rows)
    for (row <- 0 until rows; col <- 0 until columns)
      t(col, row) = this(row, col)
    t
  }
  
  // Matrix / Scalar operations:
  def +(scalar: Float) = map(_ + scalar)
  def +=(scalar: Float) {mapSelf(_ + scalar)}
  def -(scalar: Float) = map(_ - scalar)
  def -=(scalar: Float) {mapSelf(_ - scalar)}
  def *(scalar: Float) = map(_ * scalar)
  def *=(scalar: Float) {mapSelf(_ * scalar)}
  def /(scalar: Float) = map(_ / scalar)
  def /=(scalar: Float) {mapSelf(_ / scalar)}
  def ^(scalar: Float) = map( math.pow(_,scalar).toFloat )
  def ^=(scalar: Float) {mapSelf( math.pow(_,scalar).toFloat )}
  def unary_- = map(_ * -1)
  def reciprocal = map(1f / _)
  def :=(scalar: Float) {mapSelf(e => scalar)}

  // Boolean comparisons
  def >(v: Float) = map(x => if (x > v) 1f else 0f)
  def >=(v: Float) = map(x => if (x >= v) 1f else 0f)
  def <(v: Float) = map(x => if (x < v) 1f else 0f)
  def <=(v: Float) = map(x => if (x <= v) 1f else 0f)
  
  // Matrix / Matrix operations:
  def +(that: Matrix) = this.combine(that, _ + _)
  def +=(that: Matrix) {this.combineSelf(that, _ + _)}
  def -(that: Matrix) = this.combine(that, _ - _)
  def -=(that: Matrix) {this.combineSelf(that, _ - _)}
  def :*(that: Matrix) = this.combine(that, _ * _)
  def :*=(that: Matrix) {this.combineSelf(that, _ * _)}
  def :/(that: Matrix) = this.combine(that, _ / _)
  def :/=(that: Matrix) {this.combineSelf(that, _ / _)}
  def :\(that: Matrix) = this.combine(that, (e1, e2) => e2 / e1)
  def :\=(that: Matrix) {this.combineSelf(that, (e1, e2) => e2 / e1)}
  def :^(that: Matrix) = this.combine(that, math.pow(_,_).toFloat )
  def :^=(that: Matrix) {this.combineSelf(that, math.pow(_,_).toFloat )}
  def max(that: Matrix) = this.combine(that, _ max _)
  def min(that: Matrix) = this.combine(that, _ min _)

  def ===(that: Matrix) = compare(that, _ == _)
  def !===(that: Matrix) = !(this === that)
  def ~==(that: Matrix) = compare(that, (x, y) => x ~== y)
  def !~==(that: Matrix) = !(this ~== that)
  def :=(that: Matrix) {this.combineSelf(that, (e1, e2) => e2)}
  
  // Matrix multiplication:
  def *(that: Matrix): Matrix = {
    require(this.columns == that.rows,
      "Matrix * incompatibility: " + this.shape + " * " + that.shape)
    val product = new Matrix(this.rows, that.columns)
    var thisIndex = 0
    var thatIndex = 0
    val thatStride = that.columns
    for (r <- 0 until product.rows) {
      for (c <- 0 until product.columns) {
        thisIndex = r * this.columns
        thatIndex = c
        var dotProduct = 0.0f
        for (i <- 0 until this.columns) {
          dotProduct += this(thisIndex) * that(thatIndex)
          thisIndex += 1
          thatIndex += thatStride
        }
        product(r, c) += dotProduct
      }
    }
    product
  }

  /** Dot product of "this" and "that" */
  def dot(that: Matrix): Float = {
    (data zip that.data).map(v => v._1 * v._2).reduceLeft(_ + _)
  }

  /** Reduce the matrix to a scalar. */
  def reduce(f: (Float, Float) => Float): Float = {
    data.reduceLeft(f)
  }

  /** Matrix multiplication. */
  def *(that: Vector): Vector = {
    require(this.columns == that.length)
    val product = new Vector(this.rows)
    var thisIndex = 0
    for (r <- 0 until product.length) {
      thisIndex = r * this.columns
      var dotProduct = 0.0f
      for (i <- 0 until this.columns) {
        dotProduct += this(thisIndex) * that(i)
        thisIndex += 1
      }
      product(r) = dotProduct
    }
    product
  }

  /** Get the data in the tensor, flattened to a linear array. */
  protected[cogx] def getData = data

  // Matrix decompositions:
  def cholesky: Matrix = 
    Matrix(new Jama.CholeskyDecomposition(Matrix.toJamaMatrix(this)).getL)
  def eigen: (Matrix, Matrix) = {
    val decomp = new Jama.EigenvalueDecomposition(Matrix.toJamaMatrix(this))
    (Matrix(decomp.getV), Matrix(decomp.getD))
  }
  def lu: (Matrix, Matrix) = {
    val decomp = new Jama.LUDecomposition(Matrix.toJamaMatrix(this))
    (Matrix(decomp.getL), Matrix(decomp.getU))
  }
  def qr: (Matrix, Matrix) = {
    val decomp = new Jama.QRDecomposition(Matrix.toJamaMatrix(this))
    (Matrix(decomp.getQ), Matrix(decomp.getR))
  }
  def svd: (Matrix, Matrix, Matrix) = {
    val decomp =  new Jama.SingularValueDecomposition(Matrix.toJamaMatrix(this))
    val u = decomp.getU
    val s = decomp.getS
    val v = decomp.getV
    (Matrix(u), Matrix(s), Matrix(v))
  }

  /** Attempt to separate this matrix into an outer product of two vectors.
    * Relies on a tolerance, here taken to be the machine epsilon.
    *
    * @return Some((verticalVector, horizontalVector)) if separable, otherwise
    *         None.
    */
  def separate: Option[(Vector, Vector)] = {
    require(rows == columns)
    val (u, s, v) = svd
    //val tolerance = 1.0e-20 * rows * columns
    val tolerance = epsilon
    val nonzeroSingularValues =
      s.map(math.abs(_)).map(e => if (e > tolerance) 1f else 0f).reduce(_ + _).toInt
    if (nonzeroSingularValues == 1) {
      val scale = scala.math.sqrt(s(0, 0)).toFloat
      val verticalVector = u.transpose.row(0).toVector * scale
      val horizontalVector = v.transpose.row(0).toVector * scale
      Some((verticalVector, horizontalVector))
    } else
      None
  }

  def invert: Matrix = {
    Matrix(Matrix.toJamaMatrix(this).inverse)
  }

  def forwardGradient: (Matrix, Matrix) = {
    val v0 = new Matrix(rows, columns)
    val v1 = new Matrix(rows, columns)
    for (row <- 0 until rows - 1) {
      for (col <- 0 until columns - 1) {
        v1(row, col) += this(row, col + 1) - this(row, col)
        v0(row, col) += this(row + 1, col) - this(row, col)
      }
    }
    (v0, v1)
  }

  def backwardDivergence(v1: Matrix): Matrix = {
    val v0 = this
    require(v1.shape == v0.shape)
    val m = new Matrix(rows, columns)
    for (row <- 0 until rows - 1) {
      for (col <- 0 until columns - 1) {
        // Differs from GEE book since they use (columns, rows), so must
        // flip role of v0 and v1
        m(row, col + 1) += v1(row, col)
        m(row + 1, col) += v0(row, col)
        m(row, col) -= v0(row, col) + v1(row, col)
      }
    }
    -m
  }


  // Miscellaneous cogx.utilities:
  def abs = map(_.abs)
  def sgn = map(e => {if (e > 0) 1.0f else if (e < 0) -1.0f else 0.0f})
  def rectify = map(_ max 0)
  def normalizeRows {
    for (r <- 0 until rows) {
      var sumOfSq = 0.0f
      for (c <- 0 until columns)
        sumOfSq += this(r, c) * this(r, c)
      val l2Norm = math.sqrt(sumOfSq).toFloat
      for (c <- 0 until columns)
        this(r, c) /= l2Norm
    }
  }
  def normalizeColumns {
    for (c <- 0 until columns) {
      var sumOfSq = 0.0f
      for (r <- 0 until rows)
        sumOfSq += this(r, c) * this(r, c)
      val l2Norm = math.sqrt(sumOfSq).toFloat
      for (r <- 0 until rows)
        this(r, c) /= l2Norm
    }
  }
  def print {
    for (row <- 0 until rows) {
      for (col <- 0 until columns)
        printf(" %9.4f", this(row, col))
      println
    }
    println
  }
  def compactString: String = {
    val s = new StringBuilder("Matrix[%d, %d](".format(rows, columns))

    for (row <- 0 until rows) {
      s ++= "("

      for (col <- 0 until columns){
        if (col < columns-1)
          s ++= "%.3f, ".format(this(row, col))
        else
          s ++= "%.3f".format(this(row, col))
      }

      if (row < rows-1)
        s ++= "), "
      else
        s ++= ")"
    }
    s ++= ")"

    s.toString
  }
  
  /** Combine "this" and "that" by using "f" on corresponding elements. */
  private def combine(that: Matrix, f:(Float, Float) => Float): Matrix = {
    require(this.shape == that.shape)
    val result = new Array[Float](this.size)
    for (i <- 0 until data.length)
      result(i) = f(this(i), that(i))
    new Matrix(rows, columns, result)
  }

  /** Compare "this" and "that" using "f" on corresponding elements. "This" and
    * "that" must be the same shape and corresponding elements must all satisfy
    * "f" to return true, otherwise this returns false.
    */
  private def compare(that: Matrix, f: (Float, Float) => Boolean): Boolean = {
    if (!(this.shape == that.shape))
      return false
    for (i <- 0 until data.length)
      if (!f(this(i), that(i)))
        return false
    true
  }
  
  /** Combine "that" into "this" by using "f" on corresponding elements. */
  private def combineSelf(that: Matrix, f:(Float, Float) => Float) {
    require(this.shape == that.shape)
    for (i <- 0 until data.length)
      this(i) = f(this(i), that(i))
  }

  /** Get the size of the matrix as a string. */
  def sizeString = "(" + rows + " x " + columns + ")"

  /** Shift a matrix with zero fill on the edges.
    *
    * @param shiftRows Number of rows to shift `this` down.
    * @param shiftColumns Number of columns to shift `this` to the right.
    * @return Shifted matrix, zero-filling on edges.
    */
  def shift(shiftRows: Int, shiftColumns: Int): Matrix = {
    val shifted = new Matrix(this.rows, this.columns)
    for (row <- 0 until this.rows; col <- 0 until this.columns) {
      val sRow = row - shiftRows
      val sCol = col - shiftColumns
      val pixel =
        if (sRow >= 0 && sRow < this.rows && sCol >= 0 && sCol < this.columns)
          this(sRow, sCol)
        else
          0f
      shifted(row, col) = pixel
    }
    shifted
  }

  /** Shifts the elements of a matrix cyclicly as though it were a torus, wrapping
    * around the left side to the right side, and the top to the bottom. The
    * rotation is specified by the tuple("deltaRows", "deltaColumns"). For
    * example, the tuple value (2, 3) would cause the element at location (0, 0)
    * to be moved to location (2, 3). In a 5 x 5 matrix, the same tuple would
    * cause the element at (4, 4) to be moved to (1, 2), wrapping around the
    * torus. Returns the cyclicly shifted matrix.
    */
  def shiftCyclic(deltaRows: Int, deltaColumns: Int): Matrix = {
    val result = new Matrix(rows, columns)
    var rowShift = deltaRows
    while (rowShift < 0)
      rowShift += rows
    var colShift = deltaColumns
    while (colShift < 0)
      colShift += columns
    for (row <- 0 until rows; col <- 0 until columns)
      result((row + rowShift) % rows, (col + colShift) % columns) =
        this(row, col)
    result
  }

  /** Shift "this" matrix down by "shift" rows and right by "shift" columns,
    * expand to a "bigRows" x "bigColumns" complex matrix, padding with zeros
    * everywhere an element is not defined by "this".
    */
  def shiftAndExpand(shift: Int, bigRows: Int, bigColumns: Int): Matrix =
  {
    require(bigRows + shift >= rows && bigColumns + shift >= columns)
    val big = new Matrix(bigRows, bigColumns)
    for (row <- 0 until rows) {
      val fromIndex = row * columns
      val toIndex = (row + shift) * bigColumns + shift
      Array.copy(data, fromIndex, big.data, toIndex, columns)
    }
    big
  }

  /** Expand the matrix, optionally extending the border into the expanded
    * region. This operation is a key part of the FFT. The new matrix is of
    * size "bigRows" x "bigColumns" and element (0, 0) of this is anchored at
    * (0, 0) in the larger matrix. If "borderFill" is true, then the four
    * edges of the matrix are extended evenly in all directions, as though
    * the bigger matrix were actually a torus with opposite edges touching.
    */
  def expand(bigRows: Int, bigColumns: Int, borderFill: Boolean): Matrix =
  {
    require(bigRows >= rows && bigColumns >= columns)
    val big = new Matrix(bigRows, bigColumns)

    // Copy "this" into big
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

  /** Trim "this" to a "smallRows" x "smallColumns" matrix. */
  def trim(smallRows: Int, smallColumns: Int): Matrix = {
    require(smallRows <= rows && smallColumns <= columns)
    val small = new Matrix(smallRows, smallColumns)
    for (row <- 0 until smallRows; col <- 0 until smallColumns)
      small(row, col) = apply(row, col)
    small
  }

  /** Flip the matrix left-to-right and top-to-bottom. This is useful for
    * creating correlation matrices that are implemented using convolution.
    */
  def flip: Matrix = {
    val result = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      result(rows - row - 1, columns - col - 1) = this(row, col)
    result
  }

  /** Compute the Moore-Penrose pseudo inverse. */
  def pseudoInverse: Matrix = {
    // There's a bug in the Jama library for SVD where it sometimes fails on
    // matrices with fewer rows than columns. In that case we tranpose the
    // matrix first, pseudo-invert it, and transpose again. There are no
    // plans to fix Jama, so we hack...
    val flip = rows < columns
    val matrix = if (flip) this.transpose else this

    val (u, s, v) = matrix.svd
    val tolerance = epsilon * (rows max columns) * s.abs.reduce(_ max _)

    // Take the pseudo-inverse of s.
    val inverseS = s.transpose
    val diagonalSize = inverseS.rows min inverseS.columns
    for (d <- 0 until diagonalSize)
      if (inverseS(d, d) > tolerance)
        inverseS(d, d) = 1f / inverseS(d, d)
    val inverse: Matrix = v * inverseS * u.transpose
    if (flip)
      inverse.transpose
    else
      inverse
  }

  /** The distance from 1.0f to the next larger float. */
  private val epsilon: Float = math.ulp(1.0f)

  /** Find the rank of a matrix.
    *
    * @return Rank of the matrix.
    */
  def rank: Int = {
    val (u, s, v) = this.svd
    val tolerance = epsilon * (rows max columns) * s.abs.reduce(_ max _)
    val rank = s.map(x => if (x > tolerance) 1f else 0f).reduce(_ + _)
    rank.toInt
  }

  /** Convolve this matrix with another. Useful for convolving two filters.
    * Works only with square, odd-sized filters.
    *
    * @param that The matrix to be convolved with this.
    * @return The convolution of the filters, larger than either.
    */
  def convolve(that: Matrix): Matrix = {
    require(this.rows == this.columns, "matrix must be square")
    require(that.rows == that.columns, "matrix must be square")
    require(this.rows % 2 == 1, "matrix must be of odd size")
    require(that.rows % 2 == 1, "matrix must be of odd size")
    require(that.rows <= this.rows, "that matrix cannot be larger than this")

    // Flip "that" for convolution. "That" is regarded as the filter.
    val thatFlipped = new Matrix(that.rows, that.columns)
    for (r <- 0 until that.rows; c <- 0 until that.columns)
      thatFlipped(r, c) = that(that.rows - r - 1, that.columns - c - 1)

    // Expand "this", border filled with zeroes, to make computation easier.
    val expandedHalo = thatFlipped.rows - 1
    val expandedSize = this.rows + 2 * expandedHalo
    val resultHalo = thatFlipped.rows / 2
    val resultSize = this.rows + 2 * resultHalo
    val expanded = new Matrix(expandedSize, expandedSize)
    for (r <- 0 until this.rows; c <- 0 until this.columns)
      expanded(r + expandedHalo, c + expandedHalo) = this(r, c)

    // Do the convolution.
    val result = new Matrix(resultSize, resultSize)
    for (r <- 0 until resultSize; c <- 0 until resultSize) {
      var sum = 0f
      for (thatRow <- 0 until that.rows; thatCol <- 0 until that.columns) {
        sum += thatFlipped(thatRow, thatCol) * expanded(r + thatRow, c + thatCol)
      }
      result(r, c) = sum
    }
    result
  }
}

/**
  * Factory methods for Matrix.
  */
object Matrix {
  private lazy val rand = new Random

  /** Create a Matrix from an array of row data. */
  def apply(rowArray: Array[Float]*): Matrix = Matrix.apply(rowArray.toArray)
    
  /** Create a Matrix from a 2D array. */
  def apply(rowArray: Array[Array[Float]]): Matrix = {
    val rows = rowArray.length
    val columns = rowArray(0).length
    for (row <- rowArray)
      require(row.length == columns)
    val data = new Array[Float](columns * rows)
    var index = 0
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        data(index) = rowArray(row)(col)
        index += 1
      }
    }
    new Matrix(rows, columns, data)
  }

  /** Create a "rows" x "columns" matrix initialized by "f". */
  def apply(rows: Int, columns: Int, f: (Int, Int) => Float): Matrix = {
    val matrix = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      matrix(row, col) = f(row, col)
    matrix
  }

  /** Create a square diagonal matrix with specified "diagonal" values. */
  def diagonal(vector: Vector): Matrix = {
    new Matrix(vector.length, vector.length) {
      for (row <- 0 until rows)
        this(row, row) = vector(row)
    }
  }

  /** Create a "rows" x "rows" square identity matrix. */
  def identity(rows: Int): Matrix = {
    new Matrix(rows, rows) {
      for (row <- 0 until rows)
        this(row, row) = 1f
    }
  }

  /** Create a random "rows" x "columns" matrix. */
  def random(rows: Int, columns: Int): Matrix = {
    val random = new Random
    new Matrix(rows, columns) {
      for (row <- 0 until rows; col <- 0 until columns)
        this(row, col) = random.nextFloat
    }
  }

  /** Create a Jama matrix containing the same values as "this". */
  private[algebra] def toJamaMatrix(m: Matrix): Jama.Matrix = {
    val array = new Array[Array[Double]](m.rows)
    for (r <- 0 until m.rows)
      array(r) = new Array[Double](m.columns)
    var index = 0
    for (r <- 0 until m.rows; c <- 0 until m.columns) {
      array(r)(c) = m(index)
      index += 1
    }
    val jamaMatrix = new Jama.Matrix(array)
    require(jamaMatrix.getRowDimension == m.rows)
    require(jamaMatrix.getColumnDimension == m.columns)
    require(jamaMatrix.getArray.length == m.rows)
    require(jamaMatrix.getArray()(0).length == m.columns)

    jamaMatrix
  }
  
  /** Create a Matrix containing the same values as a Jama matrix. */
  private[algebra] def apply(j: Jama.Matrix): Matrix = {
    // There appears to be a bug in the Jama library, making the
    // getRowDimension and getColumnDimension unreliable. So we look at
    // the actual stored arrays to get that information.
    //require(j.getArray.length == j.getRowDimension)
    //require(j.getArray()(0).length == j.getColumnDimension)

    val jamaArray = j.getArray
    val rows = jamaArray.length
    val columns = jamaArray(0).length

    val array = new Array[Float](rows * columns)
    var index = 0
    for (r <- 0 until rows; c <- 0 until columns) {
      array(index) = jamaArray(r)(c).toFloat
      index += 1
    }
    new Matrix(rows, columns, array)
  }
}
