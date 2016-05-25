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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import java.util.Random
import cogx.cogmath.algebra.real.Matrix


/** Test code for ComplexMatrix.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComplexMatrixSpec extends FunSuite with MustMatchers {
  private lazy val random = new Random
  private val Zero = Complex(0, 0)

  test("ZeroConstructor") {
    val Rows = 7
    val Columns = 11
    val matrix = new ComplexMatrix(Rows, Columns)
    require(matrix.rows == Rows && matrix.columns == Columns)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(matrix(row, col) == Zero)
  }

  test("FunctionalConstructor") {
    val Rows = 7
    val Columns = 11
    val matrix = new ComplexMatrix(Rows, Columns,
      (row: Int, col: Int) => Complex(row, col))
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(matrix(row, col) == Complex(row, col))
  }

  test("MatrixConstructor") {
    val Rows = 7
    val Columns = 11
    val matrix = new Matrix(Rows, Columns)
    for (row <- 0 until Rows; col <- 0 until Columns)
      matrix(row, col) = 100 * row + col
    val complexMatrix = new ComplexMatrix(matrix)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(complexMatrix(row, col) == Complex(100 * row + col, 0))
  }

  test("ArrayArrayConstructor") {
    val array2D = Array(
      Array(Complex(0, 0), Complex(0, 1), Complex(0, 2)),
      Array(Complex(1, 0), Complex(1, 1), Complex(1, 2))
    )
    val matrix = new ComplexMatrix(array2D)
    for (row <- 0 until 2; col <- 0 until 3)
      require(matrix(row, col) == Complex(row, col))
  }

  test("ElementWiseMultiply") {
    val Rows = 7
    val Columns = 11
    val m1 = makeRandom(Rows, Columns)
    val m2 = makeRandom(Rows, Columns)
    val product = m1 :* m2
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(product(row, col) ~== m1(row, col) * m2(row, col),
        "at (" + row + ", " + col + ") " + product(row, col) + " " + m1(row, col) + " " + m2(row, col) +
                " " + m1(row, col) * m2(row, col))
  }

  test("Expand") {
    val Rows = 6
    val Columns = 11
    val BigRows = 10
    val BigColumns = 16
    val m = makeRandom(Rows, Columns)
    // Zero fill:
    val big = m.expand(BigRows, BigColumns, false)
    for (row <- 0 until BigRows; col <- 0 until BigColumns) {
      if (row >= Rows || col >= Columns)
        require(big(row, col) == Zero)
      else
        require(big(row, col) == m(row, col))
    }
    // Border fill:
    val big2 = m.expand(BigRows, BigColumns, true)
    val apronSize = 2
    for (row <- 0 until BigRows; col <- 0 until BigColumns) {
      if (row < Rows && col < Columns)
        require(big2(row, col) == m(row, col))
      // Far bottom
      if (row >= BigRows - apronSize && col < Columns)
        require(big2(row, col) == m(0, col))
      // Near bottom
      if (row > Rows && row < Rows + apronSize && col < Columns)
        require(big2(row, col) == m(Rows - 1, col))
      // Far right
      if (row < Rows && col >= BigColumns - apronSize)
        require(big2(row, col) == m(row, 0))
      // Near right
      if (row < Rows && col >= Columns && col < Columns + apronSize)
        require(big2(row, col) == m(row, Columns - 1))
      // m(0, 0)
      if (row >= BigRows - apronSize && col >= BigColumns - apronSize)
        require(big2(row, col) == m(0, 0))
      // m(Rows - 1, Columns - 1)
      if (row >= Rows && row < Rows + apronSize && col >= Columns &&
              col < Columns + apronSize)
        require(big2(row, col) == m(Rows - 1, Columns - 1))
      // m(Rows - 1, 0)
      if (row >= Rows && row < Rows + apronSize &&
              col >= BigColumns - apronSize)
        require(big2(row, col) == m(Rows - 1, 0))
      // m(0, Columns - 1)
      if (row >= BigRows - apronSize && col >= Columns &&
              col < Columns + apronSize)
        require(big2(row, col) == m(0, Columns - 1))
    }
  }

  test("Trim") {
    val Rows = 13
    val Columns = 17
    val SmallRows = 5
    val SmallColumns = 8
    val m = makeRandom(Rows, Columns)
    val small = m.trim(SmallRows, SmallColumns)
    require(small.rows == SmallRows)
    require(small.columns == SmallColumns)
    for (row <- 0 until SmallRows; col <- 0 until SmallColumns)
      require(m(row, col) == small(row, col))
  }

  test("Shift") {
    val Rows = 13
    val Columns = 17
    val RowShift = 2
    val ColumnShift = 3
    val Zero = Complex(0,0)
    val m = makeRandom(Rows, Columns)

    val shifted = m.shift(RowShift, ColumnShift)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val r = row - RowShift
      val c = col - ColumnShift

      if (r < 0 || r >= Rows || c < 0 || c >= Columns)
        require(shifted(row, col) == Zero)
      else
        require(shifted(row, col) == m(r, c))
    }
  }

  test("ShiftCyclic") {
    val Rows = 13
    val Columns = 17
    // Test a shift up and to the left as FFT generally requires
    val RowShift = -2
    val ColumnShift = -3
    val m = makeRandom(Rows, Columns)
    val shifted = m.shiftCyclic(RowShift, ColumnShift)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      var r = row + RowShift
      if (r < 0)
        r += Rows
      var c = col + ColumnShift
      if (c < 0)
        c += Columns
      require(shifted(r, c) == m(row, col))
    }
  }

  private def makeRandom(rows: Int, columns: Int) =
    new ComplexMatrix(rows, columns,
      (r: Int, c: Int) => Complex(random.nextFloat, random.nextFloat))
}