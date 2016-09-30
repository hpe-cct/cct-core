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

package cogx.platform

import cogx.cogmath.geometry.Shape
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.opencl._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import cogx.platform.cpumemory.ScalarFieldMemory

/** Test code for the classes and objects in this package. Also supplies an
  * example of how the classes work together to implement a computation.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class PlatformSpec extends FunSuite with MustMatchers {
  // Dummy opcode.
  private case object DummyOp extends Opcode("dummy")
  // Size of fields in test
  private val Size = 10
  // Type of fields in test
  private val fieldType = new FieldType(Shape(Size), Shape(), Float32)
  // Data pattern for source
  private def sourceData(index: Int): Float =
    index.toFloat

  /** Sources a fixed scalar field. */
  class DataSource extends OpenCLCpuSingleOutputKernel(DummyOp, Array(), fieldType, false) {
    override def reset {
      val cpuMemory = outputRegister(0).slave.cpuMemory.asInstanceOf[ScalarFieldMemory]
      for (i <- 0 until Size)
        cpuMemory.write(i, sourceData(i))
      outputRegister(0).slave.write
    }

    def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {}

    /** Create a clone of this kernel that uses a new set of kernels as inputs.
      * Useful for breaking a large circuit apart into smaller subcircuits. */
    def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel =
      new DataSource
  }

  /** Sinks a data field from another kernel. */
  class DataSink(input: AbstractKernel)
  extends OpenCLCpuSingleOutputKernel(DummyOp, Array(input.outputs(0)), fieldType, false)
  {
    // Output result, only for testing.
    val result = new Array[Float](Size)

    def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
      // trash the buffer before reading
      val cpuMemory = in(0).slave.cpuMemory.asInstanceOf[ScalarFieldMemory]
      for (i <- 0 until Size)
        cpuMemory.write(i, -1f)

      // Now read it.
      in(0).slave.read
      for (i <- 0 until Size)
        result(i) = cpuMemory.read(i)
    }

    /** Create a clone of this kernel that uses a new set of kernels as inputs.
      * Useful for breaking a large circuit apart into smaller subcircuits. */
    def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
      require(inputs.length == 1)
      new DataSink(inputs(0).source)
    }

  }

  /** Doubles an array on the GPU. */
  class DeviceDoubler(in: AbstractKernel, field: FieldType)
          extends OpenCLDeviceKernel(DummyOp, Array(in.outputs(0)), Array(field))
  {
    val workGroup = new WorkGroupParameters(1,
      globalLayers = 1,
      globalRows = 1,
      globalColumns = field.fieldShape.points,
      localLayers = 1,
      localRows = 1,
      localColumns = 1
    )
    val kernelCode: String =
      "__kernel void " +
              "dbl(global const float *a, " +
              "       global float *answer) { " +
              "  unsigned int xid = get_global_id(0); " +
              "  answer[xid] = a[xid] * 2.0f;" +
              "}"

    /** Create a clone of this kernel that uses a new set of kernels as inputs.
      * Useful for breaking a large circuit apart into smaller subcircuits. */
    def copyWithNewInputs(inputs: Array[VirtualFieldRegister]) = {
      require(inputs.length == 1)
      new DeviceDoubler(inputs(0).source, field)
    }
  }

  /** Does a very simple test to verify that CPU and GPU computations can
    * synchronize and compute correctly.
    *
    * Computation:
    * {{{
    *    DataSource --> DeviceDoubler --> DataSink
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * DeviceDoubler is a CPU kernel that reads the output of DataSource and
    * doubles every value in that field and writes it to an output buffer.
    * DataSink reads the output of the DeviceDoubler.
    *
    * Note that this simple test requires communication back and forth between
    * the CPU and GPU.
    */
  test("OpenCL platform") {
  /*
    val device = OpenCLPlatform.devices(0)
    val buffer1 = device.createFieldBuffer[ScalarFieldMemory](fieldType)
    val buffer2 = device.createFieldBuffer[ScalarFieldMemory](fieldType)
    val source = new DataSource
    val doubler = new DeviceDoubler(source, fieldType)
    val sink = new DataSink(doubler)
    device.addKernel(source)
    device.addKernel(doubler)
    device.addKernel(sink)
    device.instantiateKernel(source)
    device.instantiateKernel(doubler)
    device.instantiateKernel(sink)
    source.bindOutputBuffer(0, buffer1)
    doubler.bindInputBuffer(0, buffer1)
    doubler.bindOutputBuffer(0, buffer2)
    sink.bindInputBuffer(0, buffer2)

    // cycle 1
    source.startComputation//(Array())
    doubler.startComputation//(Array(source.done))
    sink.startComputation//(Array(doubler.done))

    buffer1.invalidateCpuMemory
    buffer2.invalidateCpuMemory

    // cycle 2
    source.startComputation//(Array())
    doubler.startComputation//(Array(source.done))
    sink.startComputation//(Array(doubler.done))

    // Verify that data went through the chain correctly (was doubled)
    val result = sink.result
    import PoorFloat._
    for (i <- 0 until result.length) {
      printf("PlatformSpec, index = %d\n", i)
      printf("  expected %f, actual %f\n", sourceData(i) * 2, result(i))
      require(result(i) ~== sourceData(i) * 2)
    }

    OpenCLPlatform.release
    */
  }
}