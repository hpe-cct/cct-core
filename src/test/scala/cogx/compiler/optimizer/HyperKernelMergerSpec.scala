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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.compiler.parser.syntaxtree.{ScalarField, SyntaxTree}
import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.codegenerator.opencl.generator.OpenCLCodeGenerator

/** Test code.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class HyperKernelMergerSpec extends FunSuite with MustMatchers {

  import scala.language.reflectiveCalls

  /** "Glue" trait that makes it easy to import old tests into the framework. */
  trait Tester {
    def check(): Unit
  }

  test("binary tree with identical inputs") {
    /** Test the HyperKernel merger on a Task with 4 identical inputs
      * arranged as a binary tree, i.e. b = (a+a)+(a+a).  This threw an
      * exception in HyperKernel.doMerge(), so this isolated test was created
      * prior to the bug fix.
      */
    val tree = new SyntaxTree {
      val a = ScalarField(1f)
      val b = a + a
    }
    val kernelCircuit: KernelCircuit =
      (new OpenCLCodeGenerator).generateCircuit(tree)
    require(kernelCircuit.size == 2)
    HyperKernelMerger.optimize(kernelCircuit)
    require(kernelCircuit.size == 2, kernelCircuit.size + "")

  }

  test("All") {
    /*
    val app = new CogFragment {
      val tests = Array[Tester] (
        TestBinaryTreeOfIdenticalInputs,
        TestLargeArgCountKernel,
        TestInputReplacement
      )
      */

      /** Test the HyperKernel merger on a Task with 4 identical inputs
        * arranged as a binary tree, i.e. B <== (A+A)+(A+A).  This threw an
        * exception in HyperKernel.doMerge(), so this isolated test was created
        * prior to the bug fix.
        */
      /*
      object TestBinaryTreeOfIdenticalInputs extends Tester {
        val A = new DynamicScalarField(ScalarField(1f))
        val fourAs = DynamicScalarField((A+A)+(A+A), "FourAs")

        def check {
          require(fourAs.read.read() == 4f)
        }
      }
      */

      /** Test that the HyperKernel merger doesn't make one huge kernel
        * that exceeds the device's limit on parameter memory. A first
        * try at this test created many independent inputs, but this
        * stressed the threads/user limit of the system.  To get this
        * to run on all systems, a single DynamicVectorField is created
        * and individual inputs are extracted with tensors(i).
        *
        * Unfortunately, the new SliceVectorsHyperKernel is mergeable into
        * the sum-tree, resulting in a hugely merged kernel with the
        * DynamicVectorField as input.  The fix was to pass each of the
        * tensors through the forward-unmergeable scalarreduce kernel.
        */
      /*
      object TestLargeArgCountKernel extends Tester {

        // The current limit for the systems we have is 1088 args.
        // Max param memory = 4352 bytes, but address bits = 32 (4 bytes).

        // If the merger ignores the TooManyArgs limit, the sum-tree will be
        // merged into a single kernel that will exceed an OpenCL limit for
        // the number of input operands.

        val TooManyArgs = 2048
        val InitVector = Vector(TooManyArgs, (c) => 1.0f)
        val inputAsVectorField = new DynamicVectorField(VectorField(1, (c) => InitVector))

        val inputs = Array.tabulate(TooManyArgs) { i =>
          inputAsVectorField.tensors(i).fieldReduceSum
        }

        // The commented-out simple approach to a many-input reduction could easily overflow the
        // compiler thread's call stack (without a -Xss arg).  Since we don't want to stress that here,
        // we instead create a flatter binary-tree via flatReduceSum().

        //    val output = DynamicScalarField(inputs.reduceLeft[ScalarFieldExpr](_ + _))

        // Create a minimal-depth sum expression of inputs() from i to j inclusive.
        def flatReduceSum(i: Int, j: Int): ScalarFieldExpr = {
          if (i == j)
            inputs(i)
          else {
            val midpoint = (i + j)/2
            flatReduceSum(i, midpoint) + flatReduceSum(midpoint+1, j)
          }
        }

        val output = DynamicScalarField(flatReduceSum(0, TooManyArgs - 1))

        def check {
          require(output.read.read() == TooManyArgs)
        }
      }
      */

      /** Test the HyperKernel merger on a DAG with a kernel that does a nonlocal
        * read of an input (the shiftCyclic kernel), where the kernel's input gets
        * replaced by a merged kernel ( in * 10f + 1f).
        */
      /*
      object TestInputReplacement extends Tester {
        val inField = ScalarField(Matrix(
          Array[Float](1, 2, 3),
          Array[Float](4, 5, 6),
          Array[Float](7, 8, 9)
        ))
        val outField = (inField * 10f + 1f).shiftCyclic(1,1)
        val in = new DynamicScalarField(inField)
        val out = DynamicScalarField((in * 10f + 1f).shiftCyclic(1,1))

        def check {
          require(out.read == outField)
        }
      }
      */
    //}

    //app.step(1)
    //app.tests.foreach(_.check)
    //app.quit()
  }
}
