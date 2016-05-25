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

import scala.collection.mutable.ArrayBuffer

/** Test code for Actuators.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class UnpipelinedActuatorWithScalarFieldReaderSpec
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

  def chunkedWriter1D(a: Array[Float], chunkSize: Int)(rdr: ScalarFieldReader): Unit = {
    val chunk = new Array[Float](chunkSize)
    for (startIndex <- 0 until a.size by chunkSize) {
      if (startIndex + chunkSize <= a.size) {
        rdr.get(startIndex, chunk)
        Array.copy(chunk, 0, a, startIndex, chunkSize)
      }
      else {
        val lastBit = new Array[Float](a.size - startIndex)
        rdr.get(startIndex, lastBit)
        Array.copy(lastBit, 0, a, startIndex, lastBit.size)
      }
    }
  }

  def simplerChunkedWriter1D(a: Array[Float], chunkSize: Int)(rdr: ScalarFieldReader): Unit = {
    val chunk = new Array[Float](chunkSize)
    for (index <- 0 until a.size by chunkSize) {
      val length = Math.min(chunkSize, a.size - index)
      rdr.get(index, a, index, length)
    }
  }

  def writer2D(a: Array[Array[Float]])(rdr: ScalarFieldReader): Unit = {
    rdr.get(a)
  }

  def writer3D(a: Array[Array[Array[Float]]])(rdr: ScalarFieldReader): Unit = {
    rdr.get(a)
  }

  test("0D UnpipelinedActuator") {
    val output = new Array[Float](1)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(1.234f)
      UnpipelinedActuator(field, writer1D(output) _)
    }
    import graph._
    withRelease {
      reset
      require(output(0) == 1.234f)
      step
      require(output(0) == 1.234f)
    }
  }

  test("1D UnpipelinedActuator") {
    val output = new Array[Float](7)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(7, (col) => col + 1.234f)
      UnpipelinedActuator(field, writer1D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
      step
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
    }
  }

  test("1D UnpipelinedActuator with chunked data fill") {
    val output = new Array[Float](128)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(128, (col) => col + 1.234f)
      UnpipelinedActuator(field, chunkedWriter1D(output, 10) _)
    }
    import graph._
    withRelease {
      reset
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
      step
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
    }
  }

  test("1D UnpipelinedActuator with simpler chunked data fill") {
    val output = new Array[Float](128)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(128, (col) => col + 1.234f)
      UnpipelinedActuator(field, simplerChunkedWriter1D(output, 10) _)
    }
    import graph._
    withRelease {
      reset
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
      step
      for (col <- 0 until output.length)
        require(output(col) == col + 1.234f)
    }
  }

  test("2D UnpipelinedActuator") {
    val output = Array.ofDim[Float](2, 3)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(2, 3, (row, col) => 5 * row + col)
      UnpipelinedActuator(field, writer2D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 5 * row + col)
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == 5 * row + col)
    }
  }

  test("3D UnpipelinedActuator") {
    val Layers = 2
    val Rows = 3
    val Columns = 5
    val output = Array.ofDim[Float](Layers, Rows, Columns)
    val graph = new ComputeGraph(optimize = true) {
      val field = ScalarField(Layers, Rows, Columns,
        (layer, row, col) => 11 * layer + 5 * row + col)
      UnpipelinedActuator(field, writer3D(output) _)
    }
    import graph._
    withRelease {
      reset
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 11 * layer + 5 * row + col)
      }
      step
      for (layer <- 0 until output.length;
           row <- 0 until output(0).length;
           col <- 0 until output(0)(0).length) {
        require(output(layer)(row)(col) == 11 * layer + 5 * row + col)
      }
    }
  }

  /** A test generator for sensors driving actuators directly */
  def testSensorDrivingActuator(rows: Int, cols: Int, pipelinedSensor: Boolean): Unit = {
    val rng = new Random
    var time = 0
    // Rather than worry about how many steps the simulation will be run, and how many unique states
    // the sensor will go through (given the optional update), we create some Arrays that expand as needed.

    // generate an array of data to be injected into the compute graph through a sensor.  The array is indexed
    // by the simulation time, which may not correspond to the state index (given optional update)
    object getData {
      val dataSeries = ArrayBuffer[Array[Float]]()
      def apply(t: Int) = {
        val state = t
        while(state >= dataSeries.length)
          dataSeries += Array.tabulate(rows*cols){(i) => rng.nextFloat()}
        dataSeries(state)
      }
    }
    def resetHook() {
      //      println("reset called")
      time = 0
    }
    def nextInput(): Iterator[Float] = {
      //          println("nextInput called")
      val retVal = getData(time).toIterator
      time += 1
      retVal
    }
    def optionNextInput(): Option[Iterator[Float]] = {
      //          println("nextInput called")
      val retVal = getData(time).toIterator
      time += 1
      Some(retVal)
    }
    val output = Array.ofDim[Float](rows, cols)
    val graph = new ComputeGraph(optimize = true) {
      val field =
        if (pipelinedSensor)
          new Sensor(rows, cols, optionNextInput _, resetHook _)
      else
          new UnpipelinedSensor(rows, cols, nextInput _, resetHook _)
      UnpipelinedActuator(field, output)
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == getData(0)(row*cols + col))
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == getData(1)(row*cols + col))
    }

  }

  test("2D UnpipelinedActuator driven by an UnpipelinedSensor") {
    testSensorDrivingActuator(2, 3, false)
  }

  test("2D UnpipelinedActuator driven by an Sensor") {
    testSensorDrivingActuator(5, 7, true)
  }

}
