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

import cogx.api.{CogFunctionAPI, ImplicitConversions}
import cogx.helper.{ComplexFieldBuilderInterface, MatrixFieldBuilderInterface, ScalarFieldBuilderInterface, VectorFieldBuilderInterface}
import cogx.reference._
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.junit.JUnitRunner

import scala.language.reflectiveCalls

/** Test code for ScalarFields.
 */

@RunWith(classOf[JUnitRunner])
class ConvolutionSpec
  extends FunSuite
  with MustMatchers
  with ImplicitConversions
  with ScalarFieldBuilderInterface
  with ComplexFieldBuilderInterface
  with MatrixFieldBuilderInterface
  with VectorFieldBuilderInterface
  with CogFunctionAPI {
  val Optimize = true

  test("convolution / associative property") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val filter1 = Matrix.random(5, 5)
      val filter2 = Matrix.random(3, 3)
      val combined = filter1 convolve filter2
      val image = ScalarField.random(128, 128)

      // Associate law of convolution says the two following computations
      // are equivalent:
      val out1 = convolve(convolve(image, ScalarField(filter1), BorderValid),
        ScalarField(filter2), BorderValid)
      val out2 = convolve(image, ScalarField(combined), BorderValid)

      probe(out1, out2)
    }
    import graph._
    withRelease {
      step
      require(readScalar(out1) ~== readScalar(out2))
    }
  }

}