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

/** Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ArrayMathSpec extends FunSuite with MustMatchers with ArrayMath {

  test("ArrayArray") {
    val a = Array(2.0f, 3.0f, 5.0f)
    val b = Array(7.0f, 11.0f, 13.0f)
    val aPlusB = Array(9.0f, 14.0f, 18.0f)
    val aMinusB = Array(-5.0f, -8.0f, -8.0f)
    val aTimesB = Array(14.0f, 33.0f, 65.0f)
    val aDivB = Array(2.0f / 7f, 3.0f / 11f, 5.0f / 13f)
    val big = Array(7.0f, 11.0f, 13.0f, 17.0f, 19.0f)
    require(!areEqual(a, b))
    require(!areEqual(b, big))
    require(areEqual(a, a))

    require(areEqual(aPlusB, add(a, b)))
    require(areEqual(aMinusB, subtract(a, b)))
    require(areEqual(aTimesB, multiply(a, b)))
    require(areEqual(aDivB, divide(a, b)))

    require(areEqual(a, add(a, null)))
    require(areEqual(a, subtract(a, null)))
    require(areEqual(a, multiply(a, null)))
    require(areEqual(a, divide(a, null)))

    require(areEqual(a, add(null, a)))
    require(areEqual(a, subtract(null, a)))
    require(areEqual(a, multiply(null, a)))
    require(areEqual(a, divide(null, a)))

    require(areEqual(null, add(null, null)))
    require(areEqual(null, subtract(null, null)))
    require(areEqual(null, multiply(null, null)))
    require(areEqual(null, divide(null, null)))
  }

  test("ArrayScalar") {
    val a = Array(2.0f, 3.0f, 5.0f)
    val b = 3.0f
    val aPlusB = Array(5.0f, 6.0f, 8.0f)
    val aMinusB = Array(-1.0f, 0.0f, 2.0f)
    val aTimesB = Array(6.0f, 9.0f, 15.0f)
    val aDivB = Array(2.0f / 3f, 3.0f / 3f, 5.0f / 3f)

    require(areEqual(aPlusB, add(a, b)))
    require(areEqual(aMinusB, subtract(a, b)))
    require(areEqual(aTimesB, multiply(a, b)))
    require(areEqual(aDivB, divide(a, b)))

    require(areEqual(a, add(a, 0)))
    require(areEqual(a, subtract(a, 0)))
    require(areEqual(a, multiply(a, 1.0f)))
    require(areEqual(a, divide(a, 1.0f)))

    require(areEqual(null, add(null, b)))
    require(areEqual(null, subtract(null, b)))
    require(areEqual(null, multiply(null, b)))
    require(areEqual(null, divide(null, b)))
  }

  /** Return true if "a" and "b" are identical. Also returns true if both are
    * null.
    */
  private def areEqual(a: Array[Float], b: Array[Float]): Boolean = {
    if (a == null && b == null)
      true
    else if (b == null)
      false
    else if (a == null)
      false
    else if (a.length != b.length)
      false
    else {
      for (i <- 0 until a.length)
        if (a(i) != b(i))
          return false
      true
    }
  }
}