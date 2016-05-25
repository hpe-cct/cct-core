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
import cogx.api.CogFunctionAPI
import cogx.compiler.parser.syntaxtree.{ScalarField, SyntaxTree}
import cogx.compiler.codegenerator.opencl.generator.OpenCLCodeGenerator
import cogx.compiler.codegenerator.KernelCircuit

/** Test code for the CommonSubexpression optimizer.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class CommonSubexpressionSpec
  extends FunSuite
  with MustMatchers
  with CogFunctionAPI
{

  test("common subexpression elimination") {
    // Create a syntax tree with known redundancy.
    val syntaxTree = new SyntaxTree {
      val in1 = ScalarField(4, 4, (row, col) => row + col)
      val in2 = ScalarField(4, 4, (row, col) => row - col)
      val sum1 = in1 + in2
      val sum2 = in1 + in2  // This is a duplicate
      val finalSum = sum1 + sum2
    }
    val kernelCircuit: KernelCircuit =
      (new OpenCLCodeGenerator).generateCircuit(syntaxTree)
    require(kernelCircuit.size == 5)
    CommonSubexpression.optimize(kernelCircuit)
    require(kernelCircuit.size == 4)
    val (in1Kernel, _) = syntaxTree.in1.getSource
    val (in2Kernel, _) = syntaxTree.in2.getSource
    val (sum1Kernel, _) = syntaxTree.sum1.getSource
    val (sum2Kernel, _) = syntaxTree.sum2.getSource
    val (finalKernel, _) = syntaxTree.finalSum.getSource
    require(kernelCircuit contains in1Kernel)
    require(kernelCircuit contains in2Kernel)
    require(kernelCircuit contains finalKernel)
    require(kernelCircuit.contains(sum1Kernel) || kernelCircuit.contains(sum2Kernel))
  }

  test("Array avoidance in Opcodes") {
    // Create a syntax tree with known redundancy.

    // The operations tested have Opcodes that originally had Arrays of parameters.
    // As a result, the hashCodes of equivalent operations never compared and CSE failed.
    // This problem has been fixed by switching the parameters to Seq's, but this test
    // will ensure that the problem is not inadvertently reintroduced.
    val syntaxTree = new SyntaxTree {
      val in1 = ScalarField(4, 4, (row, col) => row + col)
      val in2 = ScalarField(4, 4, (row, col) => row - col)
      val shiftA = shift(in1, 1, 1)
      val shiftB = shift(in1, 1, 1)    // This is a duplicate

      val subSpaceA = in2(0 until 1, 0 until 1)
      val subSpaceB = in2(0 until 1, 0 until 1)     // This is a duplicate
    }
    val kernelCircuit: KernelCircuit =
      (new OpenCLCodeGenerator).generateCircuit(syntaxTree)
    require(kernelCircuit.size == 6,
      s"Expecting pre-optimized circuit to have 6 kernels, found instead ${kernelCircuit.size}")
    CommonSubexpression.optimize(kernelCircuit)
    require(kernelCircuit.size == 4,
      s"Expecting optimized circuit to have 4 kernels, found instead ${kernelCircuit.size}")
    val (in1Kernel, _) = syntaxTree.in1.getSource
    val (shiftAKernel, _) = syntaxTree.shiftA.getSource
    val (shiftBKernel, _) = syntaxTree.shiftB.getSource
    val (in2Kernel, _) = syntaxTree.in2.getSource
    val (subSpaceAKernel, _) = syntaxTree.subSpaceA.getSource
    val (subSpaceBKernel, _) = syntaxTree.subSpaceB.getSource
    require(kernelCircuit contains in1Kernel)
    require(kernelCircuit contains in2Kernel)
    require(kernelCircuit.contains(shiftAKernel) || kernelCircuit.contains(shiftBKernel))
    require(kernelCircuit.contains(subSpaceAKernel) || kernelCircuit.contains(subSpaceBKernel))
  }
}