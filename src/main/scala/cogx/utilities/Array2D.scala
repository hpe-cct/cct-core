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

package cogx.utilities

/** A two-dimensional array that supplies access to elements using a multi-
  * dimensional notation.
  *
  * @author Greg Snider
  */
@SerialVersionUID(7876724120625306643L)
private [utilities] class Array2D[T](val rows: Int, val columns: Int)(implicit m: Manifest[T])
        extends Iterable[T] with Serializable
{
  private val rowData = new Array[Array[T]](rows)
  for (r <- 0 until rowData.length)
    rowData(r) = new Array[T](columns)
  override val size = rows * columns

  /** Read element at ("row", "col"). */
  def apply(row: Int, col: Int): T = rowData(row)(col)

  /** Assign "data" to element at ("row", "col"). */
  def update(row: Int, col: Int, value: T) {rowData(row)(col) = value}

  /** Get an iterator over all elements of the array. */
  override def iterator = new Iterator[T] {
    private var row, col = 0
    def hasNext = row < rows && col < columns
    def next = {
      val nextElement = rowData(row)(col)
      col += 1
      if (col >= columns) {
        col = 0
        row += 1
      }
      nextElement
    }
  }

  /** Map each element in array to another type. */
  def map[R](f: (T) => R)(implicit m: Manifest[R]): Array2D[R] = {
    val result = new Array2D[R](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      result(row, col) = f(this(row, col))
    result
  }

  /** Reduce 2D array to a single value. */
// Scala 2.9 introduced TraversableOnce.reduce, making reduce unnecessary here.
// def reduce(f: (T, T) => T): T = {
//    var reduction = this(0, 0)
//    for (row <- 0 until rows; col <- 0 until columns)
//      if (!(row == 0 && col == 0))
//        reduction = f(reduction, this(row, col))
//    reduction
//  }

  /** Extract subarray of size "width" X "height" at ("startX", "startY) */
  def subarray(startRow: Int, startCol: Int, rows: Int, cols: Int): Array2D[T] = {
    val sub = new Array2D[T](rows, cols)
    for (row <- 0 until rows; col <- 0 until cols)
      sub(row, col) = this(row + startRow, col + startCol)
    sub
  }

  /** Extract subarray of size "width" X "height" at ("startX", "startY) */
  def subarray(rowRange: Range, columnRange: Range): Array2D[T] = {
    val sub = new Array2D[T](rowRange.length, columnRange.length)
    for (row <- 0 until sub.rows; col <- 0 until sub.columns)
      sub(row, col) = this(row + rowRange.start, col + columnRange.start)
    sub
  }

  /** Find and return member that has a maximum value for "function()". */
  def argmax(function: (T) => Float): T = {
    var maxValue = function(this(0, 0))
    var largest: T = this(0, 0)
    for (row <- 0 until rows; col <- 0 until columns) {
      val element = this(row, col)
      val value = function(element)
      if (value > maxValue) {
        maxValue = value
        largest = element
      }
    }
    largest
  }

  /** Concatenate rows to form a flat array. */
  def flatten(implicit m: Manifest[T]): Array[T] = {
    val flat = new Array[T](rows * columns)
    var index = 0
    for (row <- 0 until rows; col <- 0 until columns) {
      flat(index) = this(row, col)
      index += 1
    }
    flat
  }

  /** Return the "index" column as an array. */
  def column(index: Int)(implicit m: Manifest[T]): Array[T] =
    (0 until rows).map(r => this(r, index)).toArray

  /** Return the "index" row as an array. */
  def row(index: Int)(implicit m: Manifest[T]): Array[T] =
    (0 until columns).map(c => this(index, c)).toArray

  /** Compare "this" and "that" for equality. */
  def ===(that: Array2D[T]): Boolean = {
    if (this.rows != that.rows || this.columns != that.columns)
      return false
    for (row <- 0 until rows; col <- 0 until columns)
      if (this(row, col) != that(row, col))
        return false
    true
  }
}

/** Factory for creating Array2Ds.
  *
  * @author Greg Snider
  */
private [cogx] object Array2D {

  /** Create an Array2D, specifying each element with a function. This
    * parallels the functionality of Array.tabulate.
    *
    * @param rows Rows in array.
    * @param columns Columns in array.
    * @param f Function which generates an element for each (row, column)
    * @tparam T Type of object held in the array.
    * @return The 2D array.
    */
  def tabulate[T](rows: Int, columns: Int)(f:(Int, Int) => T)
                 (implicit m: Manifest[T]): Array2D[T] =
  {
    val array = new Array2D[T](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns) {
      array(row, col) = f(row, col)
    }
    array
  }

  /** Create a 2D array from a sequence of arrays of "T". */
  def apply[T](rowData: Array[T]*)(implicit m: Manifest[T]): Array2D[T] = {
    val rows = rowData.length
    val columns = rowData(0).length
    tabulate[T](rows, columns)((row, col) => rowData(row)(col))
  }

  /** Convert the 1D array to the most square 2D array possible. */
  def rectangularize[T](array: Array[T])(implicit m: Manifest[T]): Array2D[T] = {
    var rows = scala.math.sqrt(array.length).toInt
    var columns = array.length / rows
    while (rows * columns != array.length) {
      rows -= 1
      columns = array.length / rows
    }
    require(rows > 0 && rows * columns == array.length)
    val array2D = new Array2D[T](rows, columns)
    var index = 0
    for (row <- 0 until rows; column <- 0 until columns) {
      array2D(row, column) = array(index)
      index += 1
    }
    array2D
  }
}
