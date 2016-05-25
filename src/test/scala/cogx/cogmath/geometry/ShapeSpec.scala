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

package cogx.cogmath.geometry

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ShapeSpec extends FunSuite with MustMatchers {

  test("DimensionsAndPoints") {
    val shape3 = Shape(2, 3, 7)
    val shape2 = Shape(2, 3)
    val shape1 = Shape(5)
    val shape0 = Shape()

    require(shape3.dimensions == 3)
    require(shape2.dimensions == 2)
    require(shape1.dimensions == 1)
    require(shape0.dimensions == 0)

    require(shape3.points == 2 * 3 * 7)
    require(shape2.points == 2 * 3)
    require(shape1.points == 5)
    require(shape0.points == 1)

    require(shape3.lastDimension == 7)
    require(shape2.lastDimension == 3)
    require(shape1.lastDimension == 5)
    require(shape0.lastDimension == 0)
  }

  test("Apply") {
    val shape = Shape(3, 5, 11)
    require(shape(0) == 3)
    require(shape(1) == 5)
    require(shape(2) == 11)
  }

  test("Equals") {
    val shape1 = Shape(3, 5, 11)
    val shape2 = Shape(3, 5, 11)
    val shape3 = Shape(1, 2, 3)
    require(shape1 == shape2)
    require(shape2 == shape1)
    require(!(shape1 == shape3))
    require(shape1 != shape3)
  }

  test("Indices") {
    val shape1 = Shape(2, 3, 2)
    val expectedIndices = Array(
      Array(0, 0, 0),
      Array(0, 0, 1),
      Array(0, 1, 0),
      Array(0, 1, 1),
      Array(0, 2, 0),
      Array(0, 2, 1),
      Array(1, 0, 0),
      Array(1, 0, 1),
      Array(1, 1, 0),
      Array(1, 1, 1),
      Array(1, 2, 0),
      Array(1, 2, 1)
    )
    var i = 0
    for (indices <- shape1.indices) {
      for (x <- 0 until 3)
        require(indices(x) == expectedIndices(i)(x))
      i += 1
    }
  }

  test("Concatenate") {
    val shape1 = Shape(2, 3, 4)
    val shape2 = Shape(5, 6)
    val shape3 = shape1 concatenate shape2
    require(shape3 == Shape(2, 3, 4, 5, 6))
  }

  test("Drop") {
    val shape = Shape(2, 3, 4)
    require(shape.drop(1) == Shape(3, 4))
    require(shape.drop(2) == Shape(4))
    require(shape.drop(3) == Shape())
  }

  test("DropLast") {
    val shape = Shape(2, 3, 4)
    require(shape.dropLast(1) == Shape(2, 3))
    require(shape.dropLast(2) == Shape(2))
    require(shape.dropLast(3) == Shape())
  }

  test("<=") {
    val shape1 = Shape(2, 3, 4)
    require(shape1 <= shape1)
    val shape2 = Shape(5, 6)
    require(!(shape1 <= shape2))
    require(!(shape2 <= shape1))
    val shape3 = Shape(2, 2, 4)
    val shape4 = Shape(1, 3, 4)
    val shape5 = Shape(2, 3, 2)
    require(shape3 <= shape1)
    require(shape4 <= shape1)
    require(shape5 <= shape1)
    require(!(shape1 <= shape3))
    require(!(shape1 <= shape4))
    require(!(shape1 <= shape5))
  }

}