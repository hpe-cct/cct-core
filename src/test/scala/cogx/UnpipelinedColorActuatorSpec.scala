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
class UnpipelinedColorActuatorSpec
        extends FunSuite
        with MustMatchers
{

  test("2D UnpipelinedColorActuator") {
    val output = Array.ofDim[Pixel](2, 3)

    def rcToPixel(r: Int, c: Int) = new Pixel(r, c, r + c)
    val graph = new ComputeGraph(optimize = true) {
      val field = ColorField(2, 3, (row: Int, col: Int) => rcToPixel(row, col))
      UnpipelinedColorActuator(field, output)
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        println(output(row)(col))
      println
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        println(rcToPixel(row, col))
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == rcToPixel(row, col))
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == rcToPixel(row, col))
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
      val dataSeries = ArrayBuffer[Array[Pixel]]()
      def apply(t: Int) = {
        val state = t
        while(state >= dataSeries.length)
          dataSeries += Array.tabulate(rows*cols){i =>
            new Pixel(
              rng.nextInt(256).toByte,
              rng.nextInt(256).toByte,
              rng.nextInt(256).toByte
            )
          }
        dataSeries(state)
      }
    }
    def resetHook() {
      //      println("reset called")
      time = 0
    }
    def pixelIteratorToByteIterator(it: Iterator[Pixel]) = {
      val iterator = new Iterator[Byte] {
        var p: Pixel = null
        var i = 0

        def next() = {
          val retVal = i % 3 match {
            case 0 => p = it.next(); p.red
            case 1 => p.green
            case 2 => p.blue
          }
          i += 1
          retVal
        }

        def hasNext() = i/3 < rows*cols
      }
      iterator
    }
    def nextInput(): Iterator[Byte] = {
      //          println("nextInput called")
      val retVal = pixelIteratorToByteIterator(getData(time).toIterator)
      time += 1
      retVal
    }
    def optionNextInput(): Option[Iterator[Byte]] = {
      //          println("nextInput called")
      val retVal =  pixelIteratorToByteIterator(getData(time).toIterator)
      time += 1
      Some(retVal)
    }
    val output = Array.ofDim[Pixel](rows, cols)
    val graph = new ComputeGraph(optimize = true) {
      val field =
        if (pipelinedSensor)
          new ColorSensor(rows, cols, optionNextInput _, resetHook _)
      else
          new UnpipelinedColorSensor(rows, cols, nextInput _, resetHook _)
      UnpipelinedColorActuator(field, output)
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

  test("2D UnpipelinedColorActuator driven by an UnpipelinedColorSensor") {
    testSensorDrivingActuator(2, 3, false)
  }

  test("2D UnpipelinedColorActuator driven by a ColorSensor") {
    testSensorDrivingActuator(5, 7, true)
  }

}
