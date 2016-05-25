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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import java.util.Random
import cogx.cogmath.geometry.Shape

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class Tensor3Spec extends FunSuite with MustMatchers {

  test("AsArrays") {
    val Slices = 3
    val Rows = 4
    val Columns = 5
    val matrix = new Tensor3(Slices, Rows, Columns) {
      for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
        this(layer, row, col) = value(layer, row, col)
    }
    val layerView = matrix.asArrays(0)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val array = layerView(row)(col)
      for (layer <- 0 until Slices)
        array(layer) *= 2
      for (layer <- 0 until Slices) {
        require(array(layer) == matrix(layer, row, col))
        require(array(layer) == 2 * value(layer, row, col))
      }
    }
    val rowView = matrix.asArrays(1)
    for (layer <- 0 until Slices; col <- 0 until Columns) {
      val array = rowView(layer)(col)
      for (row <- 0 until Rows)
        array(row) *= 2
      for (row <- 0 until Rows) {
        require(array(row) == matrix(layer, row, col))
        require(array(row) == 4 * value(layer, row, col))
      }
    }
    val colView = matrix.asArrays(2)
    for (layer <- 0 until Slices; row <- 0 until Rows) {
      val array = colView(layer)(row)
      for (col <- 0 until Columns)
        array(col) *= 2
      for (col <- 0 until Columns) {
        require(array(col) == matrix(layer, row, col))
        require(array(col) == 8 * value(layer, row, col))
      }
    }
  }

  private def value(layer: Int, row: Int, col: Int): Float =
    layer * 100 + row * 10 + col

  test("Expand") {
    val rand = new Random
    val Slices = 3
    val Rows = 4
    val Columns = 5
    val small = new Tensor3(Slices, Rows, Columns)
    for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
      small(layer, row, col) = rand.nextFloat
    val BigSlices = 11
    val BigRows = 13
    val BigColumns = 17
    val big = small.expand(Shape(BigSlices, BigRows, BigColumns))
    for (layer <- 0 until BigSlices; row <- 0 until BigRows; col <- 0 until BigColumns) {
      if (layer < Slices && row < Rows && col < Columns)
        require(big(layer, row, col) == small(layer, row, col))
      else
        require(big(layer, row, col) == 0)
    }
  }

  test("Shift") {
    val rand = new Random
    val Slices = 3
    val Rows = 4
    val Columns = 5
    val matrix = new Tensor3(Slices, Rows, Columns)
    for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
      matrix(layer, row, col) = rand.nextFloat
    val SliceShift = 1
    val RowShift = 2
    val ColShift = 3
    val shifted = matrix.shift(SliceShift, RowShift, ColShift)
    for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
      require(shifted(layer, row, col) ==
              matrix((layer + SliceShift) % Slices,
                (row + RowShift) % Rows,
                (col + ColShift) % Columns))
  }

  test("Flip") {
    val rand = new Random
    val Slices = 3
    val Rows = 4
    val Columns = 5
    val matrix = new Tensor3(Slices, Rows, Columns)
    for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
      matrix(layer, row, col) = rand.nextFloat
    val flipped = matrix.flip
    val flippedTwice = flipped.flip
    for (layer <- 0 until Slices; row <- 0 until Rows; col <- 0 until Columns)
      require(matrix(layer, row, col) == flippedTwice(layer, row, col))
  }
}