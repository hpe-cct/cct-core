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
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.reference._

import cogx.helper.ScalarFieldBuilderInterface
import cogx.helper.ComplexFieldBuilderInterface
import cogx.helper.MatrixFieldBuilderInterface
import cogx.helper.VectorFieldBuilderInterface

import cogx.api.ImplicitConversions
import cogx.platform.cpumemory.ScalarFieldMemory

/** Test code for feedback, the <== operator.
 */
@RunWith(classOf[JUnitRunner])
class FeedbackSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with ScalarFieldBuilderInterface
        with ComplexFieldBuilderInterface
        with MatrixFieldBuilderInterface
        with VectorFieldBuilderInterface
{
  val Optimize = false

  test("counter") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val counter = ScalarField(1.234f)
      val nextCounter = counter + 1
      counter <== nextCounter
    }
    import graph._
    withRelease {
      reset
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      read(nextCounter).asInstanceOf[ScalarFieldMemory].print()
      println()
      val start = readScalar(counter)
      step
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      read(nextCounter).asInstanceOf[ScalarFieldMemory].print()
      println()
      require(readScalar(counter) == start + 1)
      reset
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      require(readScalar(counter) == start)
    }
  }

  test("feedback to self") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val counter = ScalarField(4, 4, (r, c) => r + c)//.random(1)
      counter <== counter
    }
    import graph._
      withRelease {
      reset
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      val start = readScalar(counter)
      step
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      require(readScalar(counter) == start)
      reset
      read(counter).asInstanceOf[ScalarFieldMemory].print()
      println()
      require(readScalar(counter) == start)
    }
  }

  test("ping pong") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val aField = ScalarField(5, 5, (r, c) => r + c)
      val bField = ScalarField(5, 5, (r, c) => r - c)
      aField <== bField
      bField <== aField
    }
    import graph._
    withRelease {
      graph.print()
      reset
      val aStart = readScalar(aField)
      val bStart = readScalar(bField)
      step
      require(readScalar(aField) == bStart)
      require(readScalar(bField) == aStart)
      step
      require(readScalar(aField) == aStart)
      require(readScalar(bField) == bStart)
      step
      require(readScalar(aField) == bStart)
      require(readScalar(bField) == aStart)
      reset
      require(readScalar(aField) == aStart)
      require(readScalar(bField) == bStart)
      step
      require(readScalar(aField) == bStart)
      require(readScalar(bField) == aStart)
    }
  }

  test("ping pong / add") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      //val a = ScalarField(2, 2, (r, c) => r + c)
      //val b = ScalarField(2, 2, (r, c) => r - c)
      val aField = ScalarField(3f)
      val bField = ScalarField(5f)
      aField <== bField + 1
      bField <== aField + 1
    }
    import graph._
    withRelease {
      graph.print()
      reset
      read(aField).asInstanceOf[ScalarFieldMemory].print()
      read(bField).asInstanceOf[ScalarFieldMemory].print()
      println()
      val aStart = readScalar(aField)
      val bStart = readScalar(bField)
      step
      read(aField).asInstanceOf[ScalarFieldMemory].print()
      read(bField).asInstanceOf[ScalarFieldMemory].print()
      println()
      require(readScalar(aField) == bStart + 1)
      require(readScalar(bField) == aStart + 1)
      step
      read(aField).asInstanceOf[ScalarFieldMemory].print()
      read(bField).asInstanceOf[ScalarFieldMemory].print()
      println()
      require(readScalar(aField) == aStart + 2)
      require(readScalar(bField) == bStart + 2)
      graph.step
      require(readScalar(aField) == bStart + 3)
      require(readScalar(bField) == aStart + 3)
      reset
      require(readScalar(aField) == aStart)
      require(readScalar(bField) == bStart)
      step
      require(readScalar(aField) == bStart + 1)
      require(readScalar(bField) == aStart + 1)
    }
  }
}