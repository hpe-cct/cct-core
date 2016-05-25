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

import cogx.cogmath.algebra.real.Tensor3
import cogx.cogmath.geometry.Shape


/** An order-3 complex tensor.
  *
  * @author Greg Snider
  */

class ComplexTensor3(val real: Tensor3, val imaginary: Tensor3) {
  require(real.layers == imaginary.layers)
  require(real.rows == imaginary.rows)
  require(real.columns == imaginary.columns)
  val layers = real.layers
  val rows = real.rows
  val columns = real.columns

  /** Create a complex matrix 3d from a real matrix 3d. */
  def this(real: Tensor3) =
    this(real, new Tensor3(real.layers, real.rows, real.columns))

  /** Create a zero-filled complex matrix 3d. */
  def this(layers: Int, rows: Int, columns: Int) =
    this(new Tensor3(layers, rows, columns), new Tensor3(layers, rows, columns))

  def copy = new ComplexTensor3(real.copy, imaginary.copy)

  def map(f: (Complex) => Complex): ComplexTensor3 = {
    val result = new ComplexTensor3(layers, rows, columns)
    for (layer <- 0 until layers; row <- 0 until rows; col <- 0 until columns) {
      val input = Complex(real(layer, row, col), imaginary(layer, row, col))
      result(layer, row, col) = f(input)
    }
    result
  }

  def flip = new ComplexTensor3(real.flip, imaginary.flip)

  /** Read location (`layer`, `row`, `col`). */
  def apply(layer: Int, row: Int, col: Int) =
    Complex(real(layer, row, col), imaginary(layer, row, col))

  /** Write location (`layer`, `row`, `col`) with `value`. */
  def update(layer: Int, row: Int, col: Int, value: Complex) {
    real(layer, row, col) = value.real
    imaginary(layer, row, col) = value.imaginary
  }

  /** Expand the matrix, optionally extending the border into the expanded
    * region.
    *
    * This operation is a key part of the FFT. The new matrix is of
    * shape `bigShape` and element (0, 0) of this is anchored at
    * (0, 0) in the larger matrix. If "borderFill" is true, then the four
    * edges of the matrix are extended evenly in all directions, as though
    * the bigger matrix were actually a torus with opposite edges touching.
    */
  def expand(bigShape: Shape, borderFill: Boolean = false): ComplexTensor3 = {
    require(!borderFill, "BorderFill not yet supported for ComplexMatrix3D.")
    new ComplexTensor3(real.expand(bigShape), imaginary.expand(bigShape))
  }

  /** Shift the values in "this" as though "this" were a torus with wrap around
    * at the edges, top to bottom, left to right.
    *
    * The shifting is negative, so
    * the value in this at location (`deltaRows`, `deltaColumns`) gets shifted
    * to location (0, 0) in the result.
    */
  def shift(deltaX: Int, deltaY: Int, deltaZ: Int): ComplexTensor3 = {
    new ComplexTensor3(real.shift(deltaX, deltaY, deltaZ),
      imaginary.shift(deltaX, deltaY, deltaZ))
  }

  def print {
    println("ComplexMatrix3D real:")
    real.print
    println("ComplexMatrix3D imaginary:")
    imaginary.print
  }
}