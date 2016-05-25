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

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

import cogx.helper.ScalarFieldBuilderInterface
import cogx.helper.ComplexFieldBuilderInterface
import cogx.helper.MatrixFieldBuilderInterface
import cogx.helper.VectorFieldBuilderInterface

import cogx.api.ImplicitConversions

/** Test code for auto naming.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class AutonamingSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with ScalarFieldBuilderInterface
        with ComplexFieldBuilderInterface
        with MatrixFieldBuilderInterface
        with VectorFieldBuilderInterface
{
  val Optimize = false

  test("autoname") {
    // Outer class
    class Diff(in1: Field, in2: Field) {
      val diff = in1 - in2
    }
    val graph = new ComputeGraph(optimize = false) {
      // Inner class
      class Sum(in1: Field, in2: Field) {
        val sum = in1 + in2
      }

      // field
      val a = ScalarField(2, 2)

      // 1D array of fields
      val b1 = Array.tabulate[Field](3) {
        i => ScalarField(2, 2)
      }
      // 2D array of fields
      val b2 = Array.tabulate[Field](2, 2) {
        (i, j) => ScalarField(2, 2)
      }
      // 3D array of fields
      val b3 = Array.tabulate[Field](2, 2, 2) {
        (i, j, k) => ScalarField(2, 2)
      }

      // module
      val d = new Sum(a, a)

      // 1D array of modules
      val e1 = Array.tabulate(4) {
        _ => new Diff(a, a)
      }

      // 2D array of modules
      val e2 = Array.tabulate(2, 2) {
        (_, _) => new Diff(a, a)
      }

      // 3D array of modules
      val e3 = Array.tabulate(2, 2, 2) {
        (_, _, _) => new Sum(a, a)
      }
    }
    import graph._
    withRelease {
      reset

      require(graph.a.name == "a")

      require(b1(0).name == "b1(0)")
      require(b1(1).name == "b1(1)")
      require(b1(2).name == "b1(2)")

      require(b2(0)(0).name == "b2(0)(0)")
      require(b2(0)(1).name == "b2(0)(1)")
      require(b2(1)(0).name == "b2(1)(0)")
      require(b2(1)(1).name == "b2(1)(1)")

      require(b3(0)(0)(0).name == "b3(0)(0)(0)")
      require(b3(0)(0)(1).name == "b3(0)(0)(1)")
      require(b3(0)(1)(0).name == "b3(0)(1)(0)")
      require(b3(0)(1)(1).name == "b3(0)(1)(1)")
      require(b3(1)(0)(0).name == "b3(1)(0)(0)")
      require(b3(1)(0)(1).name == "b3(1)(0)(1)")
      require(b3(1)(1)(0).name == "b3(1)(1)(0)")
      require(b3(1)(1)(1).name == "b3(1)(1)(1)")

      require(d.sum.name == "d.sum")

      require(e1(0).diff.name == "e1(0).diff")
      require(e1(1).diff.name == "e1(1).diff")
      require(e1(2).diff.name == "e1(2).diff")
      require(e1(3).diff.name == "e1(3).diff")

      println("--------------" + e2(0)(0).diff.name)
      require(e2(0)(0).diff.name == "e2(0)(0).diff")
      require(e2(0)(1).diff.name == "e2(0)(1).diff")
      require(e2(1)(0).diff.name == "e2(1)(0).diff")
      require(e2(1)(1).diff.name == "e2(1)(1).diff")

      require(e3(0)(0)(0).sum.name == "e3(0)(0)(0).sum")
      require(e3(0)(0)(1).sum.name == "e3(0)(0)(1).sum")
      require(e3(0)(1)(0).sum.name == "e3(0)(1)(0).sum")
      require(e3(0)(1)(1).sum.name == "e3(0)(1)(1).sum")
      require(e3(1)(0)(0).sum.name == "e3(1)(0)(0).sum")
      require(e3(1)(0)(1).sum.name == "e3(1)(0)(1).sum")
      require(e3(1)(1)(0).sum.name == "e3(1)(1)(0).sum")
      require(e3(1)(1)(1).sum.name == "e3(1)(1)(1).sum")
     }
  }
}