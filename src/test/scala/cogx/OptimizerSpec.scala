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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.helper.ScalarFieldBuilderInterface
import cogx.reference.{RefTestInterface, RefScalarField}
import cogx.compiler.optimizer.HyperKernelMultiOutputMerger

/** Test code for all Cog compiler optimizations.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class OptimizerSpec
        extends FunSuite
        with ScalarFieldBuilderInterface
        with MustMatchers
{

  /** Check that the 'cg' ComputeGraph has 'n' HyperKernels */
  def requireHyperKernelCount(cg: ComputeGraph, n: Int) {
    require(cg.hyperkernelCount == n,
    "Expecting " + n + " HyperKernels, found " + cg.hyperkernelCount)
  }

  /** Test the HyperKernel merger on a circuit with redundant inputs */
  test("redundant inputs") {
    val result = new Array[Float](1)
    val graph = new ComputeGraph(optimize = true) {
      val a = ScalarField(3f)
      val a1 = a + a + a
      val a2 = a1 + 2
      val a3 = a2 + 1
      Actuator(a3, result)
    }
    import graph._
    withRelease {
      step
      require(result(0) == 12f)
      requireHyperKernelCount(graph, 1)
    }
  }


  /** Test the HyperKernel merger on a circuit with 4 identical inputs
    * arranged as a binary tree, i.e. B <== (A+A)+(A+A).
    */
  test("binary tree of identical inputs") {
    val result = Array(0f)
    val graph = new ComputeGraph(optimize = true) {
      val A = ScalarField(1f)
      val fourAs = (A + A) + (A + A)
      Actuator(fourAs, result)
    }
    import graph._
    withRelease {
      step
      require(result(0) == 4f)
      requireHyperKernelCount(graph, 1)
    }
  }

  test("common subexpression") {
    val result = Array(0f)
    val graph = new ComputeGraph(optimize = true) {
      val a = ScalarField(3f)
      val b = ScalarField(97f)
      val sum1 = a + b
      val sum2 = a + b
      val sumOfSums = sum1 + sum2
      Actuator(sumOfSums, result)
    }
    import graph._
    withRelease {
      step
      require(result(0) == 200f)
      requireHyperKernelCount(graph, 1)
    }
  }

  /** Test the HyperKernel merger on a DAG with a kernel that does a nonlocal
    * read of an input (the shiftCyclic kernel), where the kernel's input gets
    * replaced by a merged kernel ( in * 10f + 1f).
    */
  test("input replacement") {
    val in = Matrix(
      Array[Float](1, 2, 3),
      Array[Float](4, 5, 6),
      Array[Float](7, 8, 9)
    )
    val inShiftCyclic_1_1 = Matrix(
      Array[Float](9, 7, 8),
      Array[Float](3, 1, 2),
      Array[Float](6, 4, 5)
    )
    val out = RefScalarField(inShiftCyclic_1_1 * 10f + 1f)

    val Rows = in.rows
    val Columns = in.columns
    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val inField = ScalarField(Rows, Columns, (r,c) => in(r,c))
      val outField = shiftCyclic(inField * 10f + 1f, 1,1)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readScalar(outField) == out)
      requireHyperKernelCount(graph, 2)
    }
  }

  /** Test the common subexpression elimination.  The following graph produced
    * a 'kernel not removed' error in the early history of Cog 4.0
    */
  test("CSE kernel removal") {
    val graph = new ComputeGraph(optimize = true) with RefTestInterface {
      val b = ScalarField(Shape())
      val c = ScalarField(Shape())
      val x = 3f + 1f / (c + 1f)
      val y = b * (2f / (c + 1f))

      probe(x, y)
    }

    import graph._
    withRelease {
      step
      require(readScalar(x).read() == 4f)
      require(readScalar(y).read() == 0f)
      requireHyperKernelCount(graph, 1)
    }
  }

  /** Test the latch sharing.  A probed field should not share it's buffer
    * with any more fields.  Note that optimize = false currently sets
    * the probe flag for all fields.
    */
  test("Latch Sharing respects probes") {
    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val one = ScalarField(1f)
      val two = one + 1f
      val three = two + 1f
      val four = three + 1f
      val five = four + 1f
      probeAll
    }

    import graph._
    withRelease {
      step
      // Does `one` mistakenly share a buffer with `three`?
      require(readScalar(one).read() == 1f)
      // Does `two` mistakenly share a buffer with `four`?
      require(readScalar(two).read() == 2f)
      // Does `three` mistakenly share a buffer with `five`?
      require(readScalar(three).read() == 3f)
      require(readScalar(four).read() == 4f)
      require(readScalar(five).read() == 5f)
    }
  }

  /** Test the latch sharing.  A latch that is the output of a CPU kernel
    * or is read by CPU kernels may represent corner-cases for the latch
    * sharing algorithm.  For example, right now, the cpu portion of all
    * latches are marked invalid before a kernel circuit is stepped.  If
    * a cpu kernel reads or writes such a latch, the cpu portion of the data
    * will be marked valid for the remainder of the computation, even if a
    * downstream device kernel writes the latch (i.e. device kernels don't
    * separately invalidate the cpu portion of the latch).  This behavior
    * will cause false data reads to downstream cpu kernels and probes.
    */
  test("Latch Sharing respects cpu kernels") {
    /** Simple CPU kernel that multiplies its input by 10 */
    object Times10 extends Operator {
      def compute(in: ScalarFieldReader, out:ScalarFieldWriter)  {
//        println("" + System.nanoTime() + " compute " + this + " THREAD = " + Thread.currentThread())
        require(in.fieldShape.dimensions == 2)
        val rows = in.fieldShape(0)
        val cols = in.fieldShape(1)
        out.setShape(in.fieldShape)
        for( row <- 0 until rows; col <- 0 until cols){
          val inVal = in.read(row, col)
          out.write(row, col, inVal * 10f)
        }
      }
    }
    val Rows = 10
    val Columns = 10

    val TrimmedRows = 5
    val TrimmedColumns = 5

    val firstExpectedResult =
      RefScalarField(Rows, Columns, (r,c) => 3000f)
    val secondExpectedResult =
      RefScalarField(Rows, Columns, (r,c) =>
        if (r < TrimmedRows && c < TrimmedColumns) 200.0f else 0.0f)

    val graph = new ComputeGraph with RefTestInterface {

      // Test 1: system should probably avoid reusing a buffer read by a cpu kernel

      val x = ScalarField(Rows, Columns, (r,c) => 1f)
      val xPlus2 = x + 2f

      //This is not correct
      val xPlus2Times10 =  Times10(xPlus2)
      val xPlus2Times1000 =  xPlus2Times10*100f

      probe(xPlus2Times1000, "")

      // Test 2: system should probably avoid reusing a buffer written by a cpu kernel

      val xTimes10 = Times10(x)
      val trimmed = trim(xTimes10, Shape(TrimmedRows,TrimmedColumns))
      val expanded = expand(trimmed, BorderZero, x.fieldShape)
      val expandedX2 = expanded * 2f
      val xTimes200 = Times10(expandedX2)

      probe(xTimes200, "")

    }

    import graph._
    withRelease {
      step
      // xPlus2's cpu-data will be marked valid by read of Times10 cpu kernel.
      // If xPlus2's latch is shared with xPlus2Times1000 field, the value of
      // xPlus2Times1000 (3000.0f) may look like the xPlus2 value (3.0f).
      require(readScalar(xPlus2Times1000) == firstExpectedResult)

      // The first Times10 kernel that creates xTimes10 will have a latch
      // with a cpu-portion marked valid.  If that same latch is used by
      // the expandedX2 kernel, then the value may be misread by the following
      // Times10 cpu kernel

      require(readScalar(xTimes200) == secondExpectedResult)
    }
  }

  /** Test the HyperKernel merger on a circuit with a probe on the first
    * input of a kernel (second input should still be merged) */
  test("merge into second input with first input probed") {
    val graph = new ComputeGraph(optimize = true) {
      val x = ScalarField(Shape())
      val y = ScalarField(Shape())
      val z = ScalarField(Shape())
      val x2 = x*2f
      val output = x2 + (y + z)
      probe(output, "")
      probe(x2, "")
    }
    import graph._
    withRelease {
      step
      // x2 is one HyperKernel, rest of circuit the other

      // Note: improved merger can merge in the presence of probes,
      // creating multi-output kernels
      requireHyperKernelCount(graph, 1)
    }
  }

  /** Test the HyperKernel merger on a circuit that should merge into a single
    * kernel.  Requires a multi-pass technique over both the standard and
    * multi-output mergers.*/
  test("merge into multi-output kernel") {
    val Size = Shape(10,10)
    val graph = new ComputeGraph(optimize = true) {
      val A = ScalarField(Size)
      val B = ScalarField(Size)
      val C = ScalarField(Size)
      val D = A * B + C
      val E = A * B - C
      val F = B + A * C
      probe(F)
    }
    import graph._
    withRelease {
      step
      if (HyperKernelMultiOutputMerger.Enabled)
        requireHyperKernelCount(graph, 1)
    }
  }

  /** Test the HyperKernel merger on a circuit that should merge into a single
    * kernel.  Requires a multi-pass technique over both the standard and
    * multi-output mergers.*/
  test("merge into multi-output kernel with different tensorShape outputs") {
    val Size = Shape(10,10)
    val graph = new ComputeGraph(optimize = true) {
      val A = VectorField(Size, Shape(4))
      val B = A * -1f
      val C = reduceSum(A)
      probe(C)
    }
    import graph._
    withRelease {
      step
      if (HyperKernelMultiOutputMerger.Enabled)
        requireHyperKernelCount(graph, 1)
    }
  }

  /** Stress test all optimizations.
    *
    * The idea here is to build two versions, optimized and unoptimized, of
    * some big, hairy computation that will necessarily invoke all of the
    * optimizers. Then compare the results of the two computations--they
    * should be identical
    */
  test("all") {
    val Rows = 15
    val Columns = 17
    val aStart = Matrix.random(Rows, Columns)
    val bStart = Matrix.random(Rows, Columns)

    def computation(out: Array[Array[Float]]) {
      val a = ScalarField(Rows, Columns, (row, col) => aStart(row, col))
      val b = ScalarField(Rows, Columns, (row, col) => bStart(row, col))
      val sum1 = a + a + b
      val sum2 = a + b + a
      val sum3 = a + a + b
      val sumOfSums = (sum1 + sum2 + sum3) / 2f
      Actuator(sumOfSums, out)
    }

    // Unoptimized version:
    val result1 = Array.ofDim[Float](Rows, Columns)
    val graph1 = new ComputeGraph(optimize = false) {
      computation(result1)
    }

    graph1.withRelease {
      graph1.step
    }

    // Optimized version:
    val result2 = Array.ofDim[Float](Rows, Columns)
    val graph2 = new ComputeGraph(optimize = true) {
      computation(result2)
    }

    graph2.withRelease {
      graph2.step
      // Check that the results are identical.
      for (row <- 0 until Rows; col <- 0 until Columns)
        require(result1(row)(col) == result2(row)(col))
    }
  }


  /** Test that the HyperKernel merger doesn't make one huge kernel
    * that exceeds the device's limit on parameter memory. A first
    * try at this test created many independent inputs, but this
    * stressed the threads/user limit of the system.  To get this
    * to run on all systems, a single DynamicVectorField is created
    * and individual inputs are extracted with tensors(i).
    */
  /*
  test("too large arg count") {
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
}