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

package cogx.runtime.singlegpu

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import cogx.api.ImplicitConversions
import cogx.compiler.parser.syntaxtree.{ScalarField, Field}
import cogx.runtime.ComputeGraph
import cogx.platform.cpumemory.ScalarFieldMemory

/** Test code for ComputeGraph.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ComputeGraphSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
{
  /** Lowpass, single-pole, IIR filter, with state. */
  def filter(in: Field, rate: Float): Field = {
    val capacitor = Field(in.fieldType)                    // 0D, 1D, 2D or 3D !!! scalar, vector, matrix, ....
    capacitor <== rate * in + (1 - rate) * capacitor
    capacitor
  }

  /** Average `in1` and `in1`. */
  def average(in1: Field, in2: Field): Field =
    (in1 + in2) / 2

  test("counter") {
    val graph = new ComputeGraph {
      val counter = ScalarField()
      counter <== counter + 1
    }
    graph.reset
    graph.step
    graph.step
  }

  /** Two ComputeGraphs instantiated at the same time */
  test("multiple compute graphs") {
    val graph1 = new ComputeGraph {
      val incrementer = ScalarField()
      incrementer <== incrementer + 1
    }
    graph1.reset

    val graph2 = new ComputeGraph {
      val decrementer = ScalarField()
      decrementer <== decrementer - 1
    }
    graph2.reset
    graph1.step
    graph2.step

    require(graph1.read(graph1.incrementer).asInstanceOf[ScalarFieldMemory].read() == 1f)
    require(graph2.read(graph2.decrementer).asInstanceOf[ScalarFieldMemory].read() == -1f)
  }

  test("filter") {
    /*
    val graph = new ComputeGraph {
      // Declare inputs, constants
      val in1 = ScalarField(4, 4, (_, _) => 5f)
      val in2 = ScalarField(4, 4)

      // Define computation
      val o = filter(in1, 0.9f)
      val avg = average(in1, in2)

      // Declare outputs
      val out1 = Output(o)
    }
    println("calling ComputeGraph.reset")
    graph.reset
    println("calling ComputeGraph.step")
    graph.step
    println("calling ComputeGraph.step")
    graph.step
    */
  }
}