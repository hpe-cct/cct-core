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

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.compiler.parser.syntaxtree.{ScalarField, SyntaxTree}
import cogx.compiler.codegenerator.opencl.generator.OpenCLCodeGenerator
import cogx.compiler.codegenerator.KernelCircuit

/** Test code for the RedundantInputs optimizer.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class RedundantInputsSpec extends FunSuite with MustMatchers {

  test("all") {
    val tree = new SyntaxTree {
      val a = ScalarField(1f)
      val b = a + a
    }
    val kernelCircuit: KernelCircuit =
      (new OpenCLCodeGenerator).generateCircuit(tree)
    require(kernelCircuit.size == 2)
    val (aKernel, _) = tree.a.getSource
    val (bKernel, _) = tree.b.getSource
    require(kernelCircuit contains aKernel)
    require(kernelCircuit contains bKernel)

    // Optimize and check result
    RedundantInputs.optimize(kernelCircuit)
    require(kernelCircuit.size == 2)
    val roots = kernelCircuit.roots
    require(roots.length == 1)
    //    require(roots(0).name == "b")
    require(roots(0).inputs.length == 1)
  }
}