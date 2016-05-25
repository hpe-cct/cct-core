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

/** Test code for Actuators.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ActuatorWithScalarFieldReaderSpec
        extends FunSuite
        with MustMatchers
{

  // These tests were leveraged from those targeting arrays.
  // Easiest to create a function expecting an iterator by
  // passing an array to a curried argument list, thereby
  // creating an "array writer."
  def writer1D(a: Array[Float])(rdr: ScalarFieldReader): Unit = {
    rdr.get(a)
  }

  def writer2D(a: Array[Array[Float]])(rdr: ScalarFieldReader): Unit = {
    rdr.get(a)
  }

  def writer3D(a: Array[Array[Array[Float]]])(rdr: ScalarFieldReader): Unit = {
    rdr.get(a)
  }

  test("0 D") {
    val output = new Array[Float](1)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(1.234f)
      Actuator(field, writer1D(output) _)
    }
    import graph._
    withRelease {
      reset
      require(output(0) == 0f)
      step
      require(output(0) == 1.234f)
    }
  }

  test("0 D with non-zero init") {
    val output = new Array[Float](1)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(1.234f)
      Actuator(field, writer1D(output) _, 4.567f)
    }
    import graph._
    withRelease {
      reset
      require(output(0) == 4.567f)
      step
      require(output(0) == 1.234f)
    }
  }

  test("1 D") {
    val output = new Array[Float](7)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(7, (col) => col + 1.234f)
      Actuator(field, writer1D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (col <- 0 until output.length)
        require(output(col) == 0f)
      step
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
    }
  }

  test("1 D with non-zero init") {
    val output = new Array[Float](7)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(7, (col) => col + 1.234f)
      Actuator(field, writer1D(output) _, (col: Int) => col + 9.876f)
    }
    import graph._
    withRelease {
      reset
      for (col <- 0 until output.length)
        require(output(col) ~== col + 9.876f)
      step
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
    }
  }

  test("2 D") {
    val output = Array.ofDim[Float](2, 3)
    val graph = new ComputeGraph(optimize = false) {
      val field = ScalarField(2, 3, (row, col) => 5 * row + col)
      Actuator(field, writer2D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 0f)
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 5 * row + col)
    }
  }

  test("2 D with non-zero init") {
    val Rows = 2
    val Columns = 3
    val output: Array[Array[Float]] = Array.ofDim[Float](Rows, Columns)
    val graph = new ComputeGraph(optimize = false) {
      val field = ScalarField(Rows, Columns, (row, col) => 5 * row + col)
      Actuator(field, writer2D(output) _, (row, col) => 3f * row - col, () => {})
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 3 * row - col)
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 5 * row + col)
    }
  }

  test("3 D") {
    val Layers = 2
    val Rows = 3
    val Columns = 5
    val output = Array.ofDim[Float](Layers, Rows, Columns)
    val graph = new ComputeGraph(optimize = false) {
      val field = ScalarField(Layers, Rows, Columns,
        (layer, row, col) => 11 * layer + 5 * row + col)
      Actuator(field, writer3D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 0f)
      }
      step
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 11 * layer + 5 * row + col)
      }
    }
  }

  test("3 D with non-zero init") {
    val Layers = 2
    val Rows = 3
    val Columns = 5
    val output = Array.ofDim[Float](Layers, Rows, Columns)
    val graph = new ComputeGraph(optimize = false) {
      val field = ScalarField(Layers, Rows, Columns,
        (layer, row, col) => 11 * layer + 5 * row + col)
      Actuator(field, writer3D(output) _, (layer, row, col) => 7f * layer - 3 * row + col)
    }
    import graph._
    withRelease {
      reset
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 7 * layer - 3 * row + col)
      }
      step
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 11 * layer + 5 * row + col)
      }
    }
  }
}
