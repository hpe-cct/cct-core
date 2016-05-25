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
import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

/** Tests for both pipelined and unpipelined color sensors.
 *
 * @author Dick Carter
 */
@RunWith(classOf[JUnitRunner])
class ColorSensorSpec extends FunSuite  {
  test("2D ColorSensor"){
    new ColorSensorProblemGen(2, pipelined = true, supplyInputAlways = true)
  }
  test("2D ColorSensor with optional update"){
    new ColorSensorProblemGen(2, pipelined = true, supplyInputAlways = false)
  }
  test("2D UnpipelinedColorSensor"){
    new ColorSensorProblemGen(2, pipelined = false, supplyInputAlways = true)
  }
}

//test that data can be injected into a ComputeGraph with a VectorSensor and
//  subsequently read out
class ColorSensorProblemGen(fieldDims:Int, pipelined: Boolean, supplyInputAlways: Boolean = true){
  //generate a random field shape given the specified dimensions
  val maxDimSize = 20
  val rng = new Random
  val fieldShape = Shape(Array.tabulate(fieldDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })

  val fieldPoints = fieldShape.points

  // Rather than worry about how many steps the simulation will be run, and how many unique states
  // the sensor will go through (given the optional update), we create some Arrays that expand as needed.
  
  // generate an array of data to be injected into the compute graph through a sensor.  The array is indexed
  // by the simulation time, which may not correspond to the state index (given optional update)
  object getData {
    val dataSeries = ArrayBuffer[Array[Pixel]]()
    def apply(t: Int) = {
      val state = update.state(t)
      while(state >= dataSeries.length)
        dataSeries += Array.tabulate(fieldPoints){i =>
          new Pixel(
            rng.nextInt(256).toByte,
            rng.nextInt(256).toByte,
            rng.nextInt(256).toByte
          )
        }
      dataSeries(state)
    }
  }

  // generate an array of Booleans that says whether the sensor supplies data for this invocation of nextInput()
  // This array is indexed with the time.  A translation from simulation time to state index is also provided.
  object update {
    val bools = ArrayBuffer[Boolean](true) // make bools(0) == true, i.e. always supply data initially

    def apply(t: Int) = {
      while(t >= bools.length)
        bools += supplyInputAlways || (rng.nextFloat() > 0.5f)
      bools(t)
    }
    // translate a time 't' to a state index of the data array
    def state(t: Int) = {
      var stateIndex = -1
      for (i <- 0 to t)
        if (apply(i))
          stateIndex += 1
      stateIndex
    }
  }

  var resetSeen = false

  var time = 0

  def resetHook() {
//    println("reset called")
    resetSeen = true
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

      def hasNext() = i/3 < fieldShape.points
    }
    iterator
  }

  def optionalNextInput(): Option[Iterator[Byte]] = {
//    println("option nextInput called")
    val retVal =
      if (update(time)) {
        val state = update.state(time)
//        println(s"iterator returning Some(data($state))")
        Some(pixelIteratorToByteIterator(getData(state).toIterator))
      }
      else {
//        println("Iterator returning None")
        None
      }
    time += 1
    retVal
  }

  def nextInput(): Iterator[Byte] = {
//    println("nextInput called")
    val retVal = pixelIteratorToByteIterator(getData(time).toIterator)
    time += 1
    retVal
  }

  //inject data into a compute graph and initialize it
  val cg = new ComputeGraph{
    val sensor =
      if (pipelined)
        new ColorSensor(fieldShape, optionalNextInput _, resetHook _)
      else
        new UnpipelinedColorSensor(fieldShape, nextInput _, resetHook _)
    // Throw in a GPU kernel load for good measure
    val sensorPlus1 = sensor + 1f
    probe(sensor, sensorPlus1)
  }
  import cg._
  withRelease {
    require(!resetSeen, "Reset seen before it was expected")
    require(time == 0, "Expected 0 nextInput() calls, found " + time)
    reset
    require(resetSeen, "Reset not seen when it was expected")
    val expectedNextInputCalls = if (pipelined) 2 else 1
    require(time == expectedNextInputCalls,
      s"Expected $expectedNextInputCalls nextInput() calls, found " + time)

    //read the contents of the sensor
    def readFromGraph = {
      val reader = read(sensor) match {
        case cfr: ColorFieldReader => cfr
        case _ => throw new RuntimeException
      }
      val p = new Pixel()
      val dataFromGraph = fieldShape.indices.map(idx => {
        fieldShape.dimensions match {
          case 2 =>
            val p = new Pixel()
            reader.read(idx(0), idx(1), p)
            p
          case x => throw new RuntimeException(s"Read of ${x}D color field not supported.")
        }
      }).toIterable
      dataFromGraph
    }

    def check(t: Int ): Unit = {
//      println("wrote " + getData(update.state(t))(0))
//      println("size read " + readFromGraph.size)
//      println("read ")
//      readFromGraph.foreach(println(_))
      val eq = getData(update.state(t)).zip(readFromGraph).map(x => x._1 == x._2).reduce(_&&_)
//      println(s"Performing check $t")
      require(eq, "Data mismatch seen for Sensor(" + fieldShape + ")")
    }

    val NumSteps = 5
    check(0)
    for (t <- 0 until NumSteps) {
      step
      check(t+1)
    }
  }
}
