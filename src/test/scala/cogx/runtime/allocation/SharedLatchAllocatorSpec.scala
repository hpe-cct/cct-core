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
import cogx.reference.{RefScalarField, RefTestInterface}
import org.scalatest.FunSuite
import cogx.helper.{ScalarFieldBuilderInterface, VectorFieldBuilderInterface}
import cogx.platform.opencl.OpenCLPlatform

/** Tests for the allocators, which map the storage needs of the VirtualFieldRegisters to a set of shared field buffers.
  *
  * @author Dick Carter
  */
@RunWith(classOf[JUnitRunner])
class SharedLatchAllocatorSpec extends FunSuite
                       with VectorFieldBuilderInterface
                       with ScalarFieldBuilderInterface
{
  /** This test is designed to expose a bug found in an earlier version of the InOrderSharedLatchAllocator
    * in which e.g. the buffer used for the output of the 'setCorners' CPU kernel in the test below was shared by
    * the input to the slow convolve operation.  Since a CPU kernel pushes its output down to the GPU, regardless
    * of whether it's necessary, the 'setCorners' kernel could clobber the input to the convolve kernel
    * before it had finished.  The flaw in the earlier allocator was that it assumed that when any kernel was
    * launched, all earlier kernels had finished executing.  This is only true for GPU kernels being launched, and
    * then only when considering other GPU kernels. The CPU kernels are never enqueued in the GPU execution queue and
    * can execute asynchronously w.r.t. GPU kernels (except where events are used to preserve kernel I/O dependencies).
    */
  test("combination of CPU and GPU kernels") {
    val Size = 8000
    // A FilterSize of 63 was seen to trip the slow kernel timeout on Windows (> 2 seconds by default).
    val FilterSize = 31

    // A simple User function that writes the corner output points of a 2D Field.  It's normally not a
    // good idea to leave some of the output values unwritten, but this way the kernel executes quickly.
    class setCorners(f: Float) extends Operator {

      def compute(field: ScalarFieldReader, out:ScalarFieldWriter) {
        val rows = field.fieldShape(0)
        val columns = field.fieldShape(1)
        out.setShape(rows, columns)
        out.write(0,0,f)
        out.write(0,columns-1,f)
        out.write(rows-1,0,f)
        out.write(rows-1,columns-1,f)
      }
    }

    // Using an object here improves the appearance of the operator's use below.
    object setCorners {
      def apply(f: Float) = new setCorners(f)
    }

    def testWithOutOfOrderExecution(outOfOrder: Boolean): Unit = {
      val savedOutOfOrderExecutionFlag = Cog.outOfOrderExecution
      try {
        Cog.outOfOrderExecution = outOfOrder
        val graph = new ComputeGraph(fftUse = UseFFTNever) with RefTestInterface {

          val in = ScalarField(Size, Size)
          // A big filter that does little: only a single 1f value in the center means that it passes input to output
          val filter = ScalarField(FilterSize, FilterSize, (r,c) => if (r == FilterSize/2 && c == FilterSize/2) 1f else 0f)
          val inPlus1 = in + 1f

          val slowGPUOut = inPlus1.convolve(filter, BorderZero)
          val outCPU = setCorners(10f)(in)

          probe(in, outCPU, slowGPUOut)
        }

        import graph._
        withRelease {
          step
          val expected = 1.0f
          def good(value: Float) = math.abs(expected - value) < 0.00001

          def check(row: Int, col: Int): Unit = {
            val valueRead = readScalar(slowGPUOut).read(row, col)
            val good = math.abs(expected - valueRead) < 0.00001
            require(good, s"Mismatch at (r,c) = ($row,$col), value = $valueRead, expected $expected.")
          }
          // Only the output corners are disturbed in a deterministic way
          check(0,0)
          check(0, Size-1)
          check(Size-1, 0)
          check(Size-1, Size-1)
        }
      }
      finally
        Cog.outOfOrderExecution = savedOutOfOrderExecutionFlag
   }

    testWithOutOfOrderExecution(false)  // Should use the InOrderSharedLatchAllocator
    testWithOutOfOrderExecution(true)   // Should use the OutOfOrderSharedLatchAllocator

  }

  /** Test that the default device for the platform can allocate a single buffer as big as it promises it can.
    */
  test("maximum gpu buffer allocation") {

    val numDevices = OpenCLPlatform().devices.length

    def testMaxAlloc(deviceIndex: Int): Unit = {
      val cg = new ComputeGraph(device = Some(deviceIndex)) with RefTestInterface {
        val maxAllocBytes = OpenCLPlatform().devices(deviceIndex).maxMemAllocSize
        val maxFieldSizeBytes = Int.MaxValue.toLong
        val neededSlices = math.ceil(maxAllocBytes.toDouble / maxFieldSizeBytes).toInt
        // Having many slices helps keep the DirectBuffer and Heap sizes low.
        // We can't read the big field directly because the 2GB DirectBuffer size limit
        // may be insufficient to represent the field.
        val minSlices = 16
        val numSlices = math.max(minSlices, neededSlices)
        val BytesPerFloat = 4
        val fieldElements = (maxAllocBytes / (numSlices * BytesPerFloat)).toInt
        val bigAllocElements = fieldElements.toLong * numSlices
        val bigAllocMBytes = math.round(bigAllocElements.toDouble * BytesPerFloat / 1024 / 1024).toInt

        println(s"Allocating maximally large gpu buffer of size $bigAllocMBytes MB.")

        val initField = RefScalarField.random(fieldElements)
        val slice = TestScalarField(initField)

        val inputs = Array.tabulate(numSlices){ i => slice + i }
        val bigField = vectorField(inputs)
        val outputs = Array.tabulate(numSlices){ (i) => bigField.vectorElement(i) }
        probe(outputs: _*)
      }
      import cg._
      withRelease {
        step
        System.out.print("Comparing expected large buffer state...")
        for (i <- 0 until inputs.length)
          require( initField + i ~== readScalar(outputs(i)) )
        println("done.")
      }
    }

    for (i <- 0 until numDevices)
      testMaxAlloc(i)
  }

  /** Test that the shared latch allocator does not try to share OpenCL image and buffer memories.
    */
  test("combination of same-sized OpenCL image and buffer memories") {
    val Size = 100
    // If the allocator is blind to the differences between OpenCL Image memories and OpenCL buffer memories,
    // it might try to use the same SharedLatch for 'color' as 'flipped' below.  In that case, an exception
    // will be thrown.
    val cg = new ComputeGraph {
      val in = ScalarField(Size,Size)
      val color = colorField(in, in, in)
      val flipped = color.flip.red.flip

      probe(flipped)
    }
    cg.withRelease {
      cg.step
    }
  }

}
