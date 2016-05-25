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

/** Tests for both pipelined and unpipelined vector sensors.
 *
 * @author Matthew Pickett and Dick Carter
 */
@RunWith(classOf[JUnitRunner])
class VectorSensorSpec extends FunSuite  {
  test("0D VectorSensor"){
    new VectorSensorProblemGen(0, pipelined = true, supplyInputAlways = true)
  }
  test("1D VectorSensor"){
    new VectorSensorProblemGen(1, pipelined = true, supplyInputAlways = true)
  }
  test("2D VectorSensor"){
    new VectorSensorProblemGen(2, pipelined = true, supplyInputAlways = true)
  }
  test("3D VectorSensor"){
    new VectorSensorProblemGen(3, pipelined = true, supplyInputAlways = true)
  }
  test("0D VectorSensor with optional update"){
    new VectorSensorProblemGen(0, pipelined = true, supplyInputAlways = false)
  }
  test("1D VectorSensor with optional update"){
    new VectorSensorProblemGen(1, pipelined = true, supplyInputAlways = false)
  }
  test("2D VectorSensor with optional update"){
    new VectorSensorProblemGen(2, pipelined = true, supplyInputAlways = false)
  }
  test("3D VectorSensor with optional update"){
    new VectorSensorProblemGen(3, pipelined = true, supplyInputAlways = false)
  }
  test("0D UnpipelinedVectorSensor"){
    new VectorSensorProblemGen(0, pipelined = false, supplyInputAlways = true)
  }
  test("1D UnpipelinedVectorSensor"){
    new VectorSensorProblemGen(1, pipelined = false, supplyInputAlways = true)
  }
  test("2D UnpipelinedVectorSensor"){
    new VectorSensorProblemGen(2, pipelined = false, supplyInputAlways = true)
  }
  test("3D UnpipelinedVectorSensor"){
    new VectorSensorProblemGen(3, pipelined = false, supplyInputAlways = true)
  }
}

//test that data can be injected into a ComputeGraph with a VectorSensor and
//  subsequently read out
class VectorSensorProblemGen(fieldDims:Int, pipelined: Boolean, supplyInputAlways: Boolean = true){

  //generate a random field shape given the specified dimensions
  val maxDimSize = 20
  val rng = new Random
  val fieldShape = Shape(Array.tabulate(fieldDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })
  val vectorSize = rng.nextInt(maxDimSize) + 1
  val tensorShape = Shape(vectorSize)

  val fieldPoints = fieldShape.points

  // Rather than worry about how many steps the simulation will be run, and how many unique states
  // the sensor will go through (given the optional update), we create some Arrays that expand as needed.
  
  // generate an array of data to be injected into the compute graph through a sensor.  The array is indexed
  // by the simulation time, which may not correspond to the state index (given optional update)
  object getData {
    val dataSeries = ArrayBuffer[Array[Vector]]()
    def apply(t: Int) = {
      val state = update.state(t)
      while(state >= dataSeries.length) {
        val data = Array.tabulate(fieldPoints){(i) => Vector.random(vectorSize)}
        dataSeries += data
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

  def optionalNextInput(): Option[Iterator[Vector]] = {
//    println("option nextInput called")
    val retVal =
      if (update(time)) {
        val state = update.state(time)
//        println(s"iterator returning Some(data($state))")
        Some(getData(state).toIterator)
      }
      else {
//        println("Iterator returning None")
        None
      }
    time += 1
    retVal
  }

  def nextInput(): Iterator[Vector] = {
//    println("nextInput called")
    val retVal = getData(time).toIterator
    time += 1
    retVal
  }

  //inject data into a compute graph and initialize it
  val cg = new ComputeGraph{
    val sensor =
      if (pipelined)
        new VectorSensor(fieldShape, tensorShape, optionalNextInput _, resetHook _)
      else
        new UnpipelinedVectorSensor(fieldShape, tensorShape, nextInput _, resetHook _)
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
        case vfr: VectorFieldReader => vfr
        case _ => throw new RuntimeException
      }
      val vec = new Vector(vectorSize)
      val dataFromGraph = fieldShape.indices.map(idx => {
        val v = new Vector(tensorShape.points)
        fieldShape.dimensions match {
          case 0 => reader.read(vec); vec.copy
          case 1 => reader.read(idx(0), vec); vec.copy
          case 2 => reader.read(idx(0), idx(1), vec); vec.copy
          case 3 => reader.read(idx(0), idx(1), idx(2), vec); vec.copy
        }
      }).toIterable
      dataFromGraph
    }

    def check(t: Int ): Unit = {
      val eq = getData(update.state(t)).zip(readFromGraph).map(x => x._1 == x._2).reduce(_&&_)
//      println(s"Performing check $t")
      require(eq, "Data mismatch seen for VectorSensor(" + fieldShape + "," + tensorShape + ")")
    }

    val NumSteps = 5
    check(0)
    for (t <- 0 until NumSteps) {
      step
      check(t+1)
    }
  }
}
