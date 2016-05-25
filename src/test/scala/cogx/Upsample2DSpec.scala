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

package cogx

import org.scalatest.FunSuite
import cogx.reference.{RefTestInterface, RefScalarField}
import cogx.helper.ScalarFieldBuilderInterface
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author Matthew Pickett
 */
@RunWith(classOf[JUnitRunner])
class Upsample2DSpec extends FunSuite
                     with ScalarFieldBuilderInterface {
  val x = RefScalarField(Matrix(
    Array(1,2,3,4f),
    Array(5,6,7,8f),
    Array(9,10,11,12f)
  ))

  val y0 = RefScalarField(Matrix(
    Array(1, 0, 0, 2, 0, 0, 3, 0, 0, 4, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(5, 0, 0, 6, 0, 0, 7, 0, 0, 8, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(9, 0, 0, 10, 0, 0, 11, 0, 0, 12, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f)
  ))

  val y1 = RefScalarField(Matrix(
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 1, 0, 0, 2, 0, 0, 3, 0, 0, 4, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 5, 0, 0, 6, 0, 0, 7, 0, 0, 8, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 9, 0, 0, 10, 0, 0, 11, 0, 0, 12, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f)
  ))

  val y2 = RefScalarField(Matrix(
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 1, 0, 0, 2, 0, 0, 3, 0, 0, 4f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 5, 0, 0, 6, 0, 0, 7, 0, 0, 8f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0f),
    Array(0, 0, 9, 0, 0, 10, 0, 0, 11, 0, 0, 12f)
  ))

  test("upsample a scalar field") {
    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val xs = TestScalarField(x)

      val upsample30 = upsample(xs, 3, 0)
      val upsample31 = upsample(xs, 3, 1)
      val upsample32 = upsample(xs, 3, 2)
      val upsample10 = upsample(xs, 1, 0)

      probe(upsample30, upsample31, upsample32, upsample10)
    }

    import graph._
    withRelease {
      step
      require(readScalar(upsample30) == y0)
      require(readScalar(upsample31) == y1)
      require(readScalar(upsample32) == y2)
      require(readScalar(upsample10) == x)
    }
  }
}
