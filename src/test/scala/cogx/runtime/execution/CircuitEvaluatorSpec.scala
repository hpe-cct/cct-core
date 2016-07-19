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

package cogx.runtime.execution

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

import cogx.api.ImplicitConversions
import cogx.compiler.parser.syntaxtree.ScalarField
import cogx.runtime.ComputeGraph
import cogx.platform.cpumemory.readerwriter.ScalarFieldReader

/** Code to test ComputeGraph.step and ComputeGraph.step(N)
  *
  * @author Matthew Pickett and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class CircuitEvaluatorSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
{
  /** Test ComputeGraph.step and ComputeGraph.step(N) */
  test("ComputeGraph step and step(N)") {

    val cg = new ComputeGraph{
      val counter = ScalarField(0f)
      counter <== counter + 1f
    }

    try {
      cg.step
      val counter1 = cg.read(cg.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter1 == 1.0f, "This should be 1.0: " + counter1)

      cg.step(10)
      val counter11 = cg.read(cg.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter11 == 11.0f, "This should be 11.0: " + counter11)

      cg.step(100)
      val counter111 = cg.read(cg.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter111 == 111.0f, "This should be 111.0: " + counter111)

      // Test batching of step count by underlying platform
      cg.step(389)
      val counter500 = cg.read(cg.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter500 == 500.0f, "This should be 500.0: " + counter500)
    }
    finally
      cg.release
  }

  /** Test ComputeGraph.run */
  test("ComputeGraph run") {

    val cg = new ComputeGraph{
      val counter = ScalarField(0f)
      counter <== counter + 1f
    }

    var time = 0L
    def setTime(newTime: Long) { time = newTime }

    // 200msec delays allow simulation to run and also give time for the
    // asynchronous cg.time(callback) to set the local time variable
    try {
      cg.time(setTime)
      Thread.sleep(200)
      require(time == 0, "Time should be 0, saw " + time)

      cg.run

      Thread.sleep(200)

      cg.stop

      cg.time(setTime)
      Thread.sleep(200)
      require(time > 10, "Time should be greater than 10, saw " + time)

    }
    finally
      cg.release
  }

  /** Test ComputeGraph.step followed by ComputeGraph.run */
  test("ComputeGraph step, then run") {

    val cg = new ComputeGraph{
      val counter = ScalarField(0f)
      counter <== counter + 1f
    }

    var time = 0L
    def setTime(newTime: Long) { time = newTime }

    // 200msec delays allow simulation to run and also give time for the
    // asynchronous cg.time(callback) to set the local time variable
    try {
      cg.step(10)
      cg.time(setTime)
      Thread.sleep(200)
      require(time == 10, "Time should be 10, saw " + time)

      cg.run

      Thread.sleep(200)

      cg.stop

      cg.time(setTime)
      Thread.sleep(200)
      require(time > 10, "Time should be greater than 10, saw " + time)

    }
    finally
      cg.release
  }

  /** Test ComputeGraph.stop and ComputeGraph.run of multiple ComputeGraphs. */
  test("Multiple ComputeGraph stop and run") {
    // Helper function to create a series of unmergeable kernels
    def flipN(f: ScalarField, numFlips: Int): ScalarField = {
      if (numFlips == 0)
        f
      else
        flipN(f.flip, numFlips - 1)
    }

    val cg1 = new ComputeGraph{
      val counter = ScalarField(1000,1000)
      counter <== flipN(counter, 11)
    }
    val cg2 = new ComputeGraph{
      val counter = ScalarField(1000,1000)
      counter <== flipN(counter, 11)
    }

    var time = 0L
    def setTime(newTime: Long) { time = newTime }

    // 200msec delays allow simulation to run and also give time for the
    // asynchronous cg.time(callback) to set the local time variable
    try {
      cg1.step(10)
      cg1.time(setTime)
      Thread.sleep(200)
      require(time == 10, "Time should be 10, saw " + time)
      cg2.step(10)
      cg2.time(setTime)
      Thread.sleep(200)
      require(time == 10, "Time should be 10, saw " + time)

      cg1.run
      cg2.run

      Thread.sleep(1000)

      cg1.stop
      cg2.stop

      cg1.time(setTime)
      Thread.sleep(200)
      require(time > 10, "Time should be greater than 10, saw " + time)
      cg2.time(setTime)
      Thread.sleep(200)
      require(time > 10, "Time should be greater than 10, saw " + time)

    }
    finally {
      cg1.release
      cg2.release
    }
  }
  /** Test ability of a ComputeGraph to function after another ComputeGraph has been released.
    */
  test("Multiple ComputeGraph, overlapped step and release.") {
    val cgA = new ComputeGraph{
      val counter = ScalarField()
      counter <== counter + 1
    }
    val cgB = new ComputeGraph{
      val counter = ScalarField()
      counter <== counter + 1
    }

    try {
      try {
        cgA.step(10)
        val counter10A = cgA.read(cgA.counter).asInstanceOf[ScalarFieldReader].read()
        require(counter10A == 10.0f, "This should be 10.0: " + counter10A)

        cgB.step(10)
        val counter10B = cgB.read(cgB.counter).asInstanceOf[ScalarFieldReader].read()
        require(counter10B == 10.0f, "This should be 10.0: " + counter10B)
      }
      finally
        cgA.release

      cgB.step(10)
      val counter20B = cgB.read(cgB.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter20B == 20.0f, "This should be 20.0: " + counter20B)
    }
    finally {
      cgB.release
    }
  }

  /** Test ability of a ComputeGraph to function while another ComputeGraph is running.
    */
  test("Multiple ComputeGraph, overlapped step and run.") {
    var time = 0L
    def setTime(newTime: Long) { time = newTime }

    // 200msec delays allow simulation to run and also give time for the
    // asynchronous cg.time(callback) to set the local time variable
    val cgA = new ComputeGraph{
      val counter = ScalarField()
      counter <== counter + 1
    }
    val cgB = new ComputeGraph{
      val counter = ScalarField()
      counter <== counter + 1
    }

    try {
      // Start cgA running
      cgA.run
      Thread.sleep(200)

      // Step and test cgB while cgA is running
      cgB.step(10)
      val counter10B = cgB.read(cgB.counter).asInstanceOf[ScalarFieldReader].read()
      require(counter10B == 10.0f, "This should be 10.0: " + counter10B)

      // Verify that cgA was indeed running (by checking the cycle count)
      cgA.stop

      cgA.time(setTime)
      Thread.sleep(200)
      require(time > 0, "Time should be greater than 0, saw " + time)
    }
    finally {
      cgA.release
      cgB.release
    }
  }
}
