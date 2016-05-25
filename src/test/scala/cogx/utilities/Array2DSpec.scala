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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class Array2DSpec extends FunSuite with MustMatchers {

  test("ApplyUpdate") {
    def value(row: Int, col: Int) = row * 100 + col
    val Rows = 3
    val Columns = 4
    val a = new Array2D[Int](Rows, Columns)
    require(a.rows == Rows && a.columns == Columns)
    for (row <- 0 until Rows; col <- 0 until Columns)
      a(row, col) = value(row, col)
    for (row <- 0 until Rows; col <- 0 until Columns)
      require(a(row, col) == value(row, col))
  }

  test("Equality") {
    val a = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    val b = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    val c = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8)
    )
    require(a === a)
    require(a === b)
    require(!(a === c))
  }

  test("Subarray") {
    val a = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    val sub1 = a.subarray(1, 1, 2, 3)
    val expect1 = Array2D(
      Array(6, 7, 8),
      Array(10, 11, 12)
    )
    require(sub1 === expect1)
    val sub2 = a.subarray(0, 1, 3, 2)
    val expect2 = Array2D(
      Array(2, 3),
      Array(6, 7),
      Array(10, 11)
    )
    require(sub2 === expect2)
  }

  test("MapReduce") {
    val a = Array2D[Int](
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    val b: Array2D[Double] = a.map[Double](2.0 * _.toDouble)
//    val b: Array2D[Double] = a.map(2.0 * _.toDouble)
    val bExpected = Array2D[Double](
      Array(2.0, 4.0, 6.0, 8.0),
      Array(10.0, 12.0, 14.0, 16.0),
      Array(18.0, 20.0, 22.0, 24.0)
    )
    require(b === bExpected)
    require(a.reduce[Int](_ min _) == 1)
    require(a.reduce[Int](_ max _) == 12)
    require(b.reduce[Double](_ min _) == 2)
    require(b.reduce[Double](_ max _) == 24)
  }

  test("ToArray") {
    val a = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    val flat = a.flatten
    val expect = Array(1,2,3,4,5,6,7,8,9,10,11,12)
    require(flat.length == expect.length)
    for (i <- 0 until flat.length)
      require(flat(i) == expect(i))
  }

  test("ArgMax") {
    class Pair(val x: Int, val y: Int) {
      def value =
        if (x == y) 10 * x
        else x + y
    }
    val a = Array2D(
      Array(new Pair(0, 0), new Pair(0, 1)),
      Array(new Pair(1, 0), new Pair(1, 1)),
      Array(new Pair(2, 0), new Pair(2, 1))
    )
    val winner = a.argmax(_.value)
    require(winner == a(1, 1))
  }

  test("ColumnRow") {
    def equal[T](a: Array[T], b: Array[T]): Boolean = {
      if (a.length != b.length)
        return false
      for (i <- 0 until a.length)
        if (a(i) != b(i))
          return false
      true
    }
    val a = Array2D(
      Array(1, 2, 3, 4),
      Array(5, 6, 7, 8),
      Array(9, 10, 11, 12)
    )
    require(equal(a.row(0), Array(1, 2, 3, 4)))
    require(equal(a.row(1), Array(5, 6, 7, 8)))
    require(equal(a.row(2), Array(9, 10, 11, 12)))

    require(equal(a.column(0), Array(1, 5, 9)))
    require(equal(a.column(1), Array(2, 6, 10)))
    require(equal(a.column(2), Array(3, 7, 11)))
    require(equal(a.column(3), Array(4, 8, 12)))
  }

  test("Flatten") {
    val nw = Array2D(
      Array(1, 2, 3),
      Array(7, 8, 9)
    )
    val ne = Array2D(
      Array(4, 5, 6),
      Array(10, 11, 12)
    )
    val sw = Array2D(
      Array(13, 14, 15),
      Array(19, 20, 21)
    )
    val se = Array2D(
      Array(16, 17, 18),
      Array(22, 23, 24)
    )
    /*
    val array4D = new Array2D[Array2D[Int]](2, 2)
    array4D(0, 0) = nw
    array4D(0, 1) = ne
    array4D(1, 0) = sw
    array4D(1, 1) = se
    val flat = Array2D(array4D)

    val expected = Array2D(
      Array(1,2,3,4,5,6),
      Array(7,8,9,10,11,12),
      Array(13,14,15,16,17,18),
      Array(19,20,21,22,23,24)
    )
    require(flat === expected)
    */
  }
}