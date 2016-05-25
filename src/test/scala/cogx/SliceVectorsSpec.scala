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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import cogx.reference.{RefTestInterface, RefScalarField, RefVectorField}
import org.scalatest.FunSuite
import cogx.helper.{ScalarFieldBuilderInterface, VectorFieldBuilderInterface}

/**
 * @author Dick Carter
 */
@RunWith(classOf[JUnitRunner])
class SliceVectorsSpec extends FunSuite
                       with VectorFieldBuilderInterface
                       with ScalarFieldBuilderInterface
{
  val Optimize = true
  /** Test that the SliceVectorsHyperKernel, which runs in SmallTensorAddressing
    * mode, can reference a large-tensor input.  This is also a good test for
    * the HyperKernel merger, as the sum-tree should be merged into a single
    * kernel along with the slice kernels.
    */
  test("large tensor inputs") {
    val LargeTensorSize = 17
    val InitVector = Vector(LargeTensorSize, (c) => (c + 1).toFloat)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inputAsVectorField = TestVectorField(RefVectorField(InitVector))

      val inputs = Array.tabulate(LargeTensorSize) {
        i => vectorElement(inputAsVectorField, i)
      }

      val output = inputs.reduceLeft(_ + _)

      probe(output)
    }

    import graph._
    withRelease {
      step
      require(readScalar(output).read() == LargeTensorSize * (LargeTensorSize + 1) / 2)
    }
  }

  /** Test that the SliceVectorsHyperKernel can merge with other kernels
    * running in SmallTensorAddressing mode.
    */
  test("merging with small tensor kernels") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val vecA = TestVectorField(Shape(4))
      val vecAmodified = vecA + 1f
      val whichInput = vectorElement(vecAmodified, 0)
      val inputs = Array.tabulate(2) {
        (i) => TestScalarField(RefScalarField(i.toFloat))
      }
      val output = select(inputs, whichInput)

      probe(inputs: _*)
      probe(output)
    }

    import graph._
    withRelease {
      step
      require(readScalar(output) == readScalar(inputs(1)))
    }
  }

  /** Test that the SliceVectorsHyperKernel can merge with other kernels
    * running in SmallTensorAddressing mode.
    */
  test("not merging with non-small tensor kernels") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val vecA = TestVectorField(Shape(7))
      val vecAmodified = vecA + 1f
      val whichInput = vectorElement(vecAmodified, 0)
      val inputs = Array.tabulate(2){
        (i) => TestScalarField(RefScalarField(i.toFloat * 2))
      }
      val output = select(inputs, whichInput)

      probe(inputs: _*)
      probe(output)
    }

    import graph._
    withRelease {
      step
      require(readScalar(output) == readScalar(inputs(1)))
    }
  }
}
