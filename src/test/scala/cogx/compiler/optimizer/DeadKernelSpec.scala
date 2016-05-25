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

package cogx.compiler.optimizer

import cogx.runtime.ComputeGraph

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.api.CogFunctionAPI
import cogx.compiler.parser.syntaxtree.{VectorField, ScalarField}
import cogx.cogmath.algebra.real.Vector

/** Test code for the dead kernel elimination optimizer.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class DeadKernelSpec
  extends FunSuite
  with MustMatchers
  with CogFunctionAPI
{
  /** Check that the 'cg' ComputeGraph has 'n' HyperKernels */
  def requireHyperKernelCount(cg: ComputeGraph, n: Int) {
    require(cg.hyperkernelCount == n,
      "Expecting " + n + " HyperKernels, found " + cg.hyperkernelCount)
  }

  test("Dead kernel elimination") {
    val cg = new ComputeGraph(optimize = true) {
      // Constant field kernel that drives nothing
      val vec = VectorField(10, 10, (r,c) => Vector(8, i => i))

      // The part of the compute graph that should not go away
      val counter = ScalarField(10, i => i)
      counter <== counter + 1
      probe(counter)

      // Create an unprobed DAG of kernels that should be removed
      val multiplied2 = counter * 2
      val multiplied3 = counter * 3
      val multiplied4 = counter * 4
      val foo1 = multiplied2 + multiplied3
      val foo2 = multiplied2 + multiplied4
      val foo3 = multiplied3 + multiplied4
    }
    import cg._
    withRelease {
      step
      // Only device kernel that remains is the +1 kernel
      requireHyperKernelCount(cg, 1)
    }
  }

  test("Remove all kernels") {
    val cg = new ComputeGraph(optimize = true) {
      // Create an unprobed DAG of kernels that should be removed
      val input = ScalarField(10, i => i)
      val multiplied2 = input * 2
      val multiplied3 = input * 3
      val multiplied4 = input * 4
      val foo1 = multiplied2 + multiplied3
      val foo2 = multiplied2 + multiplied4
      val foo3 = multiplied3 + multiplied4
    }
    import cg._
    withRelease {
      step
      // Nothing remains
      requireHyperKernelCount(cg, 0)
    }
  }

  test("Respects probes") {
    val cg = new ComputeGraph(optimize = true) {
      // The part of the compute graph that should not go away
      val input = ScalarField(10, i => i)
      val multiplied2 = input * 2
      probe(multiplied2)
      // The following should be removed
      val flipped = flip(multiplied2)
    }
    import cg._
    withRelease {
      step
      // Only device kernel that remains is the *2 kernel
      requireHyperKernelCount(cg, 1)
    }
  }


}