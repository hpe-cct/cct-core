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
class VectorSpec extends FunSuite with MustMatchers {

  test("Constructors") {
    val v1 = new Vector(17)
    require(v1.length == 17)
    val v2 = new Vector(1f, 2f, 34.5f, 9f)
    require(v2.length == 4)
    val v3 = v2.copy
    for (i <- 0 until v2.length)
      require(v2(i) == v3(i))
  }

  test("ElementAccess") {
    val v = new Vector(1f, 2f, 34.5f, 9f)
    require(v(0) == 1 && v(1) == 2 && v(2) == 34.5f && v(3) == 9)
    v(1) = 123
    require(v(0) == 1 && v(1) == 123 && v(2) == 34.5f && v(3) == 9)
  }

  test("VectorScalarOperations") {
    val v = new Vector(2f, 3f, 7f)
    require(v + 1 === new Vector(3f, 4f, 8f))
    require(v - 1 === new Vector(1f, 2f, 6f))
    require(v * 2 === new Vector(4f, 6f, 14f))
    require(v / 2 === new Vector(1f, 1.5f, 3.5f))
    require(-v === new Vector(-2f, -3f, -7f))
    v += 1
    require(v === new Vector(3f, 4f, 8f))
    v -= 1
    require(v === new Vector(2f, 3f, 7f))
    v *= 2
    require(v === new Vector(4f, 6f, 14f))
    v /= 2
    require(v === new Vector(2f, 3f, 7f))
    val v2 = new Vector(3)
    v2 := v
    require(v2 === new Vector(2f, 3f, 7f))
  }

  test("VectorVectorOperations") {
    val v1 = new Vector(2f, 3f, 5f)
    val v2 = new Vector(7f, 11f, 13f)
    require(v1 !=== v2)
    require(v1 + v2 === new Vector(9f, 14f, 18f))
    require(v1 - v2 === new Vector(-5f, -8f, -8f))
    require(v1 :* v2 === new Vector(14f, 33f, 65f))
    require(v1 :/ v2 === new Vector(2.0f/7f, 3.0f/11f, 5.0f/13f))
    val v3 = v1 concat v2
    require(v3 === new Vector(2f, 3f, 5f, 7f, 11f, 13f))
    val dotProduct = v1 dot v2
    require(dotProduct == 2*7 + 3*11 + 5*13)
    val outer = v1.shape(v1.length, 1) * v2.transpose
    val outerExpected = Matrix(
      Array(14f, 22f, 26f),
      Array(21f, 33f, 39f),
      Array(35f, 55f, 65f)
    )
    //(outer - outerExpected).print
    require(outer === outerExpected)
    v1 += v2
    require(v1 === new Vector(9f, 14f, 18f))
    v1 -= v2
    require(v1 === new Vector(2f, 3f, 5f))
    v1 :*= v2
    require(v1 === new Vector(2*7f, 3*11f, 5*13f))
    v1 :/= v2
    require(v1 === new Vector(2f, 3f, 5f))
  }

  test("MatrixConversions") {
    val v = new Vector(2f, 3f, 5f)
    val m = v.transpose
    require(m.rows == 1 && m.columns == v.length)
    require(m(0, 0) == 2 && m(0, 1) == 3 && m(0, 2) == 5)
    val v2 = new Vector(2f, 3, 5, 7, 11, 13)
    val m2 = v2.shape(2, 3)
    require(m2.rows == 2 && m2.columns == 3)
    require(m2(0, 0) == 2 && m2(0, 1) == 3 && m2(0, 2) == 5)
    require(m2(1, 0) == 7 && m2(1, 1) == 11 && m2(1, 2) == 13)
    val m3 = v2.rectangularize
    require((m3.rows == 3 && m3.columns == 2) ||
            (m3.rows == 2 && m3.columns == 3))

    var dataArray = v2.asArray
    require(dataArray(2) == v2(2))
    dataArray(2) = 4.0f
    require(dataArray(2) == v2(2))
  }

  test("MapReduce") {
    val v1 = new Vector(2f, 3, 5)
    val v2 = v1.map(_ * 3)
    require(v2 === new Vector(6f, 9, 15))
    v1.mapSelf(_ * 2)
    require(v1 === new Vector(4f, 6, 10))
    val sum = v1.reduce(_ + _)
    require(sum == 4 + 6 + 10)
    val product = v1.reduce(_ * _)
    require(product == 4 * 6 * 10)
  }

  test("Subvector") {
    val Length = 25
    val v1 = new Vector(Length) {
      for (i <- 0 until length)
        this(i) = i
    }
    val vLow = v1.subvector(0 until Length / 2)
    require(vLow.length == Length / 2)
    for (i <- 0 until Length/2)
      require(vLow(i) == i)

    val vHigh = v1.subvector(Length / 2 until Length)
    require(vHigh.length == Length - (Length / 2) )
    for (i <- 0 until vHigh.length)
      require(vHigh(i) == i + Length / 2)
  }

  test("Miscellaneous") {
    val v1 = new Vector(2f, -3, 5, 0)
    require(v1.abs === new Vector(2f, 3, 5, 0))
    require(v1.sgn === new Vector(1f, -1, 1, 0))
    require(v1.rectify === new Vector(2f, 0, 5, 0))
    require(v1.normL1 == 2 + (-3).abs + 5)
    require(v1.normL2 == math.sqrt(2*2 + 3*3 + 5*5f).toFloat)
    require(v1.argmax == 2)
  }

  test("Sort") {
    val v1 = new Vector(2f, -3, 5, 0)
    val sorted = v1.sort
    require(sorted === new Vector(-3f, 0, 2, 5))
  }

  test("Flip") {
    val v1 = new Vector(2f, -3, 5, 0)
    val flipped = v1.flip
    val expect = new Vector(0f, 5, -3, 2)
    require(flipped === expect)
  }
}