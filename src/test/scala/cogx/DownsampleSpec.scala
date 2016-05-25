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

import cogx.api.CogFunctionAPI
import org.scalatest.FunSuite
import cogx.reference.{RefTestInterface, RefScalarField}
import cogx.helper.ScalarFieldBuilderInterface
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author Matthew Pickett
 */
@RunWith(classOf[JUnitRunner])
class DownsampleSpec
  extends FunSuite
  with ScalarFieldBuilderInterface
  with CogFunctionAPI
{
  test("downsample a 1D scalar field"){
    val x = RefScalarField(12, (c)=>c)
    val y0 = RefScalarField(4, (c)=>3*c)
    val y1 = RefScalarField(4, (c)=>3*c+1f)

    val graph = new ComputeGraph(optimize = false) with
      RefTestInterface { val xf = TestScalarField(x)

      val downsample30 = downsample(xf, 3, 0)
      val downsample31 = downsample(xf, 3, 1)

      probe(downsample30, downsample31)
    }

    import graph._
    withRelease {
      step
      require(readScalar(downsample30) == y0)
      require(readScalar(downsample31) == y1)
    }
  }

  test("downsample a 2D scalar field"){
    val x = RefScalarField(Matrix(
      Array(1,2,3,4f),
      Array(5,6,7,8f),
      Array(9,10,11,12f),
      Array(13,14,15,16f)
    ))

    val y_2_0 = RefScalarField(Matrix(
      Array(1,3f),
      Array(9,11f)
    ))

    val y_2_1 = RefScalarField(Matrix(
      Array(6,8f),
      Array(14,16f)
    ))

    val y_3_0 = RefScalarField(Matrix(
      Array(1,4f),
      Array(13,16f)
    ))

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val xf = TestScalarField(x)

      val downsample20 = downsample(xf, 2, 0)
      val downsample21 = downsample(xf, 2, 1)
      val downsample30 = downsample(xf, 3, 0)

      probe(downsample20, downsample21, downsample30)
    }

    import graph._
    withRelease {
      step
      require(readScalar(downsample20) == y_2_0)
      require(readScalar(downsample21) == y_2_1)
      require(readScalar(downsample30) == y_3_0)
    }
  }

  test("make sure edge overlap error is triggered on bad phase parameter"){
    val x = RefScalarField(Matrix(
      Array(1,2,3,4f),
      Array(5,6,7,8f),
      Array(9,10,11,12f),
      Array(13,14,15,16f)
    ))

    val thrown = intercept[IllegalArgumentException] {
      val graph = new ComputeGraph(optimize = false) with RefTestInterface {
        val xf = TestScalarField(x)

        val output = downsample(xf, 3, 1)
      }

      import graph._
      withRelease {
        step
      }
    }

    require(thrown.getMessage == "requirement failed: phase overlaps the boundary of the field")
  }
}
