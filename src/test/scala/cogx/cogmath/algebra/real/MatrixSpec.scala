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

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class MatrixSpec extends FunSuite with MustMatchers {

  test("Dimensions") {
    val t = new Matrix(2, 3)
    require(t.rows == 2)
    require(t.columns == 3)
    require(t.size == 2 * 3)
    //require(t.shape == (2, 3))
    require(t.shape(0) == 2 && t.shape(1) == 3)
  }

  test("ElementAccess") {
    // apply, update
    val Rows = 2
    val Columns = 3
    val t = new Matrix(Rows, Columns)
    for (i <- 0 until t.size)
      t(i) = i * i
    for (i <- 0 until t.size)
      require(t(i) == i * i)
    // 2D apply, update
    for (row <- 0 until Rows)
      for (col <- 0 until Columns)
        t(row, col) = row + 10 * col
    for (row <- 0 until Rows)
      for (col <- 0 until Columns)
        require(t(row, col) == row + 10 * col)
  }

  test("Subtensors") {
    val m = Matrix(
      Array(2, 1, 2f),
      Array(3, 5, 7f)
    )
    require(m.row(0) === Matrix(Array(2, 1, 2f)))
    require(m.row(1) === Matrix(Array(3, 5, 7f)))
    require(m.column(0) === Matrix(Array(2, 3f)).transpose)
    require(m.column(1) === Matrix(Array(1, 5f)).transpose)
    require(m.column(2) === Matrix(Array(2, 7f)).transpose)
    require(m(1, 1) == 5)
    m(1, 1) = 19
    require(m(1, 1) == 19)
    m(1, 1) = 5
    val s = m.submatrix(0 to 1, 1 to 2)
    val expected = Matrix(
      Array(1, 2f),
      Array(5, 7f)
    )
    require(s === expected)
  }

  test("MatrixOperations") {
    // reduce
    val Rows = 13
    val Columns = 17
    val t1 = new Matrix(Rows, Columns)
    for (i <- 0 until t1.size)
      t1(i) = i + 1
    val sum = t1.reduce(_ + _)
    require(sum == (t1.size * (t1.size + 1)) / 2)
    // random
    t1.randomize
    val average = t1.reduce(_ + _) / t1.size
    require(average > 0.4 && average < 0.6)
    // map
    val t2 = t1.map(2 * _)
    for (i <- 0 until t1.size)
      require(t2(i) == 2 * t1(i))
    // mapSelf
    t1.mapSelf(2 * _)
    for (i <- 0 until t1.size)
      require(t2(i) == t1(i))
    // transpose
    val m = Matrix(
      Array(2, 11, 13f),
      Array(3,  5,  7f)
    )
    val mt = Matrix(
      Array(2,  3f),
      Array(11, 5f),
      Array(13, 7f)
    )
    require(m.transpose === mt)
  }

  test("MatrixScalarOperations") {
    val Rows = 13
    val Columns = 17
    val t1 = new Matrix(Rows, Columns)
    t1.randomize
    // t := s
    t1 := 3
    for (i <- 0 until t1.size)
      require(t1(i) == 3)
    // t + s
    // t - s
    // t * s
    // t / s
    // t ^ s
    val t2 = t1 + 2
    val t3 = t1 - 2
    val t4 = t1 * 2
    val t5 = t1 / 2
    val t6 = t1 ^ 2
    for (i <- 0 until t1.size) {
      require(t2(i) == 3 + 2)
      require(t3(i) == 3 - 2)
      require(t4(i) == 3 * 2)
      require(t5(i) == 3.0 / 2)
      require(t6(i) == math.pow(3, 2).toFloat)
    }
    // t += s
    // t -= s
    // t *= s
    // t /= s
    // t ^= s
    t2 += 7
    t3 -= 11
    t4 *= 13
    t5 /= 17
    t6 ^= 4
    for (i <- 0 until t1.size) {
      require(t2(i) == 3 + 2 + 7)
      require(t3(i) == 3 - 2 - 11)
      require(t4(i) == 3 * 2 * 13)
      require(t5(i) == 3f / 2 / 17)
      require(t6(i) == math.pow(math.pow(3,2),4).toFloat)
    }
    // -t
    t1.randomize
    val tm = -t1
    for (i <- 0 until t1.size)
      require(tm(i) == -t1(i))
  }

  test("MatrixMatrixOperations") {
    // :=
    val Rows = 11
    val Columns = 13
    val t1 = new Matrix(Rows, Columns)
    t1.randomize
    val t2 = new Matrix(Rows, Columns)
    val t3 = new Matrix(Rows, Columns)
    t2 := t1
    for (i <- 0 until t1.size)
      require(t2(i) == t2(i))
    // ===
    // !===
    // ~==
    // !~==
    require(t1 === t2)
    require(t1 ~== t2)
    t1(0) = 1234
    require(t1 !=== t2)
    require(t1 !~== t2)
    // +
    // +=
    t1.randomize
    t2.randomize
    val sum = t1 + t2
    for (i <- 0 until t1.size)
      require(sum(i) == t1(i) + t2(i))
    t1 += t2
    require(t1 === sum)
    // -
    // -=
    t1.randomize
    t2.randomize
    val diff = t1 - t2
    for (i <- 0 until t1.size)
      require(diff(i) == t1(i) - t2(i))
    t1 -= t2
    require(t1 === diff)
    // :*
    // :*=
    t1.randomize
    t2.randomize
    val prod = t1 :* t2
    for (i <- 0 until t1.size)
      require(prod(i) == t1(i) * t2(i))
    t1 :*= t2
    require(t1 === prod)
    // :/
    // :/=
    t1.randomize
    t2.randomize
    val div = t1 :/ t2
    for (i <- 0 until t1.size)
      require(div(i) == t1(i) / t2(i))
    t1 :/= t2
    require(t1 === div)
    // :\
    // :\=
    t1.randomize
    t2.randomize
    val rdiv = t1 :\ t2
    for (i <- 0 until t1.size)
      require(rdiv(i) == t2(i) / t1(i))
    t1 :\= t2
    require(t1 === rdiv)
    // :^
    // :^=
    t1.randomize
    t2.randomize
    val rexp = t1 :^ t2
    for (i <- 0 until t1.size)
      require(rexp(i) == math.pow(t1(i) , t2(i)).toFloat )
    t1 :^= t2
    require(t1 === rexp)
  }

  test("MatrixMultiplication") {
    val m = Matrix(
      Array(2, 1, 2f),
      Array(3, 5, 7f)
    )
    val n = Matrix(
      Array(1, 2, 3f),
      Array(1, 1, 4f)
    )
    val nt = Matrix(
      Array(1, 1f),
      Array(2, 1f),
      Array(3, 4f)
    )
    val v = Matrix(
      Array(2f),
      Array(3f),
      Array(5f)
    )
    val mTimesNT = Matrix(
      Array(10, 11f),
      Array(34, 36f)
    )
    require(m * nt === mTimesNT)
    val mTimesV = Matrix(
      Array[Float](2*2 + 1*3 + 2*5),
      Array[Float](3*2 + 5*3 + 7*5)
    )
    require(m * v === mTimesV)
    val v2 = new Vector(2f, 3, 5)
    val expect = new Vector(2.00f*2 + 1*3 + 2*5, 3.0f*2 + 5*3 + 7*5)
    require(m * v2 === expect)
  }

  test("Copy") {
    val n = Matrix(
      Array(1, 2, 3f),
      Array(1, 1, 4f)
    )
    val c = n.copy
    require(c === n)
    c(0) = 999
    require(c !=== n)
  }

  test("RowWrite") {
    //println("testRowWrite")
    val matrix = Matrix(
      Array(1, 2, 3f),
      Array(4, 5, 6f),
      Array(7, 8, 9f),
      Array(3, 2, 1f)
    )
    //matrix.print
    matrix(1) = new Vector(11, 22, 33f)
    //matrix.print
    matrix(2) = new Vector (99, 98, 97f)
    //matrix.print
  }

  test("Shift") {
    val Rows = 13
    val Columns = 17
    val RowShift = 2
    val ColumnShift = 3
    val m = new Matrix(Rows, Columns)
    m.randomize
    val shifted = m.shift(RowShift, ColumnShift)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val r = row - RowShift
      val c = col - ColumnShift

      if (r < 0 || r >= Rows || c < 0 || c >= Columns)
        require(shifted(row, col) == 0f)
      else
        require(shifted(row, col) == m(r, c))
    }
  }

  test("ShiftCyclic") {
    val Rows = 6
    val Columns = 11
    val RowShift = 2
    val ColShift = 3
    val m = new Matrix(Rows, Columns)
    m.randomize
    val rotated = m.shiftCyclic(RowShift, ColShift)
    for (row <- 0 until Rows; col <- 0 until Columns) {
      val toRow = (row + RowShift) % Rows
      val toCol = (col + ColShift) % Columns
      require(rotated(toRow, toCol) == m(row, col))
    }
  }

  test("ShiftAndExpand") {
    val Rows = 6
    val Columns = 11
    val Shift = 2
    val BigRows = 10
    val BigColumns = 16
    val m = new Matrix(Rows, Columns)
    m.randomize
    val big = m.shiftAndExpand(Shift, BigRows, BigColumns)
    for (row <- 0 until BigRows; col <- 0 until BigColumns) {
      if (row >= Shift && row < Shift + Rows && col >= Shift && col < Shift + Columns)
        require(big(row, col) == m(row - Shift, col - Shift))
      else
        require(big(row, col) == 0, "row " + row + ", col " + col)
    }
  }

  test("Range apply and update") {
    val matrix = Matrix(
      Array(1, 2, 3f),
      Array(4, 5, 6f),
      Array(7, 8, 9f),
      Array(3, 2, 1f)
    )
    val sub = matrix(1 to 2, 0 until 2)
    require(sub === Matrix(
      Array(4, 5f),
      Array(7, 8f)
    ))
    val replace = Matrix(
      Array(-4, -5f),
      Array(-7, -8f)
    )
    matrix(1 to 2, 0 until 2) = replace
    require(matrix === Matrix(
      Array(1, 2, 3f),
      Array(-4, -5, 6f),
      Array(-7, -8, 9f),
      Array(3, 2, 1f)
    ))
    val tiny = Matrix(
      Array(99f)
    )
    matrix(1 to 2, 0 to 1) = tiny
    require(matrix === Matrix(
      Array(1, 2, 3f),
      Array(99, 99, 6f),
      Array(99, 99, 9f),
      Array(3, 2, 1f)
    ))
  }


  test("JamaConversions") {
    val matrix = Matrix(
      Array(1, 2, 3f),
      Array(4, 5, 6f),
      Array(7, 8, 9f),
      Array(3, 2, 1f)
    )
    val jamaMatrix = Matrix.toJamaMatrix(matrix)
    for (r <- 0 until matrix.rows; c <- 0 until matrix.columns)
      require(matrix(r, c) == jamaMatrix.get(r, c))
    val recovered = Matrix(jamaMatrix)
    require(recovered === matrix)
  }

  test("Flip") {
    val matrix = Matrix(
      Array[Float](1, 2, 3, 4),
      Array[Float](5, 6, 7, 8),
      Array[Float](3, 4, 5, 6)
    )
    val flipped = Matrix(
      Array[Float](6, 5, 4, 3),
      Array[Float](8, 7, 6, 5),
      Array[Float](4, 3, 2, 1)
    )
    require(matrix.flip == flipped)
  }

  test("Separate") {
    val average = new Matrix(31, 31) + (1f / (31 * 31))
    average.separate match {
      case Some((vert, horiz)) =>
        require(average ~== (vert outerProduct horiz))
      case None =>
        require(false)
    }
    val non = new Matrix(5, 5) {
      for (row <- 0 until rows; col <- 0 until columns)
        this(row, col) = row + col
    }
    non.separate match {
      case Some((v, h)) =>
        require(false)
      case None =>
    }
    val sobel = Matrix(
      Array(-1f, 0f, 1f),
      Array(-2f, 0f, 2f),
      Array(-1f, 0f, 1f)
    )
    sobel.separate match {
      case Some((vert, horiz)) =>
        require(sobel ~== (vert outerProduct horiz))
      case None =>
        require(false)
    }
  }
}