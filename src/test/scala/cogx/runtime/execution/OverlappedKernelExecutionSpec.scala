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


import cogx.`package`._
import cogx.reference.RefTestInterface
import cogx.runtime.ComputeGraph

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

/** Code to test the effectiveness of the Cog.outOfOrderExecution flag.
  *
  * Test model has two compute-intensive operations that each consume a single workgroup
  * that could run in parallel.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class OverlappedKernelExecutionSpec
  extends FunSuite
  with MustMatchers
{
  test("overlapped kernel execution") {
    // A compute-intensive kernel with little I/O
    def slow(f: Field): Field = GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f)
      var result = _floatVar()
      result = x
      val i = _intVar()
      _for(i := 0, i < 10000, i += 1) {
        result *= result + 0.00001f
        result := _sqrt(result)
      }
      _writeTensor(_out0, result)
    }

    val testSteps = 500
    val warmUpSteps1 = 5000
    val warmUpSteps2 = 1000

    def makeAndTimeGraph(overlappedQueues: Boolean, warmUpSteps: Int, testSteps: Int) = {
      val cg = new ComputeGraph with RefTestInterface {
        Cog.outOfOrderExecution = overlappedQueues
        val inputA = ScalarField.random(16, 16)
        val outputA = slow(inputA)

        val inputB = ScalarField.random(16, 16)
        val outputB = slow(inputB)

        probe(outputA, inputA, outputB, inputB)
      }
      cg.reset
      cg.step(warmUpSteps)
      val start = System.nanoTime()
      cg.step(testSteps)
      val durationMsec = (System.nanoTime() - start) / 1000000.0f
      cg.release
      durationMsec
    }
    // Each test that "counts" comes on the heals of a previous test for consistency.
    // Long warm-up to try and get GPU clocks stable- very device-specific frankly.
    val throwAwayTime = makeAndTimeGraph(true, warmUpSteps1, testSteps)
    val overLappedTime = makeAndTimeGraph(true, warmUpSteps2, testSteps)
    val nonOverlappedTime = makeAndTimeGraph(false, warmUpSteps2, testSteps)
    println("Overlapped computegraph execution time " + overLappedTime + " msec.")
    println("NonOverlappedTime computegraph execution time = " + nonOverlappedTime + " msec.")
    val speedUp = nonOverlappedTime / overLappedTime

    println("Overlapped execution speedup = " + speedUp)

    if (speedUp < 1.2)
      println("This GPU driver does not appear to support overlapped kernel execution (speedup should be roughly 2X).")
    else if (speedUp > 1.75)
      println("This GPU driver appears to support overlapped kernel execution (speedup roughly 2X).")
    else
      println("This GPU driver appears to support some form of overlapped kernel execution (although speedup < expected 2X).")

    // No real test here, just informational output about the drivers support for out-of-order execution
    require(true)
  }
}
