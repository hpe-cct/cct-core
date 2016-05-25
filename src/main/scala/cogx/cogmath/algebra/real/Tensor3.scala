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


/** An order-3 Tensor
  *
  * This is used primarily by the 3D FFT.
  *
  * @author Greg Snider
  */

class Tensor3(val layers: Int, val rows: Int, val columns: Int) {
  private val data = Array.ofDim[Float](layers, rows, columns)

  /** Read location ("slice", "row", "column"). */
  def apply(layer: Int, row: Int, column: Int): Float = data(layer)(row)(column)

  /** Write location ("layer", "row", "column") with "value". */
  def update(layer: Int, row: Int, column: Int, value: Float) {
    data(layer)(row)(column) = value
  }

  /** Make an exact copy of "this". */
  def copy: Tensor3 = {
    val replicant = new Tensor3(layers, rows, columns)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      replicant(layer, row, col) = this(layer, row, col)
    replicant
  }

  /** Map each element of "this" using "f". */
  def map(f: (Float) => Float): Tensor3 = {
    val result = new Tensor3(layers, rows, columns)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      result(layer, row, col) = f(this(layer, row, col))
    result
  }

  /** Flip the matrix across each axis, needed for convolution. */
  def flip: Tensor3 = {
    val result = new Tensor3(layers, rows, columns)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      result(layer, row, col) =
              this(layers - layer - 1, rows - row - 1, columns - col - 1)
    result
  }

  /** View the matrix as though it were a 2D array of arrays oriented along the
    * specified "axis" (0 => layer axis, 1 => row axis, 2 => col axis).
    *
    * These
    * arrays are both readable and writable and access the elements in this
    * matrix.
    */
  def asArrays(axis: Int): Array[Array[VirtualArray]] = {
    val m = this
    val arrays = axis match {
      // layer axis
      case 0 =>
        Array.tabulate(rows, columns) {
          (row: Int, col: Int) => new VirtualArray {
            def length = layers
            def apply(layer: Int) = m(layer, row, col)
            def update(layer: Int, value: Float) {m(layer, row, col) = value}
          }
        }

      // row axis
      case 1 =>
        Array.tabulate(layers, columns) {
          (layer: Int, col: Int) => new VirtualArray {
            def length = rows
            def apply(row: Int) = m(layer, row, col)
            def update(row: Int, value: Float) {m(layer, row, col) = value}
          }
        }

      // column axis
      case 2 =>
        Array.tabulate(layers, rows) {
          (layer: Int, row: Int) => new VirtualArray {
            def length = columns
            def apply(col: Int) = m(layer, row, col)
            def update(col: Int, value: Float) {m(layer, row, col) = value}
          }
        }

      case _ => throw new RuntimeException("Illegal axis.")
    }
    arrays
  }

  /** Print out a matrix 3d for debugging. */
  def print {
    for (layer <- 0 until layers) {
      for (row <- 0 until rows) {
        for (col <- 0 until columns)
          printf(" %8.3f", this(layer, row, col))
        println
      }
      println
    }
    println
  }

  /** Expand the matrix, optionally extending the border into the expanded
    * region.
    *
    * This operation is a key part of the FFT. The new matrix is of
    * size "bigRows" x "bigColumns" and element (0, 0) of this is anchored at
    * (0, 0) in the larger matrix. If "borderFill" is true, then the four
    * edges of the matrix are extended evenly in all directions, as though
    * the bigger matrix were actually a torus with opposite edges touching.
    */
  def expand(bigShape: Shape, borderFill: Boolean = false): Tensor3 = {
    require(!borderFill, "BorderFill not yet supported for Matrix3D.")
    require(bigShape(0) >= layers)
    require(bigShape(1) >= rows)
    require(bigShape(2) >= columns)
    val big = new Tensor3(bigShape(0), bigShape(1), bigShape(2))
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
      big(layer, row, col) = this(layer, row, col)
    big
  }

  /** Shift the values in "this" as though "this" were a torus with wrap around
    * at the edges, top to bottom, left to right. The shifting is negative, so
    * the value in this at location ("deltaRows", "deltaColumns") gets shifted
    * to location (0, 0) in the result.
    */
  def shift(deltaSlice: Int, deltaRow: Int, deltaCol: Int): Tensor3 = {
    require(deltaSlice >= 0 && deltaSlice < layers)
    require(deltaRow >= 0 && deltaRow < rows)
    require(deltaCol >= 0 && deltaCol < columns)

    val shifted = new Tensor3(layers, rows, columns)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns) {
      var fromSlice = (layer + deltaSlice) % layers
      var fromRow = (row + deltaRow) % rows
      var fromCol = (col + deltaCol) % columns
      shifted(layer, row, col) = this(fromSlice, fromRow, fromCol)
    }
    shifted
  }
}

/**
 * Factory methods for Tensor3.
 */
object Tensor3 {
  /** Create a random "layers" x "rows" x "columns" matrix. */
  def random(layers: Int, rows: Int, columns: Int): Tensor3 = {
    val random = new Random
    new Tensor3(layers, rows, columns) {
      for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns)
        this(layer, row, col) = random.nextFloat
    }
  }
}
