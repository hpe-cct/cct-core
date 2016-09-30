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
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.hyperkernels.{BinaryConstHyperKernel, BinaryHyperKernel}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import cogx.platform.opencl._
import akka.actor.{ActorSystem, Props, TypedActor, TypedProps}
import cogx.runtime.execution.{CircuitEvaluator, ClusterSupervisor, CogActorSystem}
import cogx.runtime.EvaluatorInterface
import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.cpumemory.ScalarFieldMemory
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.opencl.cpukernels.{ConstantFieldKernel, RecurrentFieldKernel}
import cogx.compiler.parser.op.{AddOp, ConstantScalar0DOp, ConstantScalar1DOp, MultiplyConstOp}
import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel, SmallTensorAddressing}
import cogx.compiler.optimizer.{HyperKernelMultiOutputMerger, KernelCircuitOptimizer}
import cogx.runtime.allocation.AllocationMode
import cogx.runtime.allocation.AllocationMode.SingleGPU

/** Test code for OpenCLEvaluator.  Also includes tests of KernelCircuits
  * that have mulit-output CPU and GPU kernels.
  *
  * @author Greg Snider and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class OpenCLEvaluatorSpec extends FunSuite with MustMatchers {
  // Dummy opcode.
  private case object DummyOp extends Opcode("dummy")
  // Size of fields in test
  private val Size = 10
  // Type of fields in test
  private val fieldType = new FieldType(Shape(Size), Shape(), Float32)
  // Data pattern for source
  private def sourceData(index: Int): Float = index.toFloat

  /** Probe all outputs of multiple AbstractKernels */
  def probeKernels(kernels: AbstractKernel*) { kernels.foreach(_.markProbed)}

  /** Sources a fixed scalar field a: a ramp function scaled by 'm' */
  class DataSource(m: Float)
    extends ConstantFieldKernel(fieldType, ConstantScalar1DOp(sourceData(_) * m)) {
    def this() = this(1.0f)
  }

  /** Opcode for the SwapScalar- holds scale factors */
  case class ScaleOp(in0Factor: Float, in1Factor: Float) extends Opcode("dummy")

  /** Hyperkernel that swaps its inputs and scales them by different factors. */
  class SwapScaler(input0: VirtualFieldRegister, input1: VirtualFieldRegister, operation: ScaleOp)
    extends HyperKernel(operation, Array(input0, input1), Array(input1.fieldType, input0.fieldType), SmallTensorAddressing)
  {
    require(input0.fieldType == input1.fieldType)
    addCode(f"    @out0 = read(@in1) * ${operation.in1Factor}%.1ff;\n" +
      f"        @out1 = read(@in0) * ${operation.in0Factor}%.1ff;\n")
  }

  /** Track down where the field memory is after the optimizer has run */
  def kernelToScalarFieldMemory(circuit: KernelCircuit, k: AbstractKernel, outputIndex: Int) = {
    val reg = circuit.findStolenOutput(k.outputs(outputIndex)).asInstanceOf[VirtualFieldRegister]
    val mem = reg.register.asInstanceOf[OpenCLFieldRegister].slave.read
    mem.asInstanceOf[ScalarFieldMemory]
  }

  /** Check that the `kc` KernelCircuit has `expectedCount` HyperKernels */
  def requireHyperKernelCount(kc: KernelCircuit, expectedCount: Int) {
    val actualCount = kc.filteredSize(_.isInstanceOf[HyperKernel])
    require(actualCount == expectedCount,
      "Expecting " + expectedCount + " HyperKernels, found " + actualCount)
  }

  /** Create and return the evaluator (normally handled by ComputeGraph) */
  class Evaluator(circuit: KernelCircuit, platform: OpenCLPlatform) {
    // Some code borrowed from ComputeGraph

    /** Actor system, used only for actor-based evaluators. */
    val actorSystem: ActorSystem = CogActorSystem()

    /** Evaluator that can execute the compiled circuit. */
    val evaluator: EvaluatorInterface =  {
      val ta = TypedActor(actorSystem).typedActorOf(TypedProps(
        classOf[EvaluatorInterface],
        new CircuitEvaluator(circuit, platform, AllocationMode.default, 0)).
              withDispatcher("CogActors.one-actor-per-thread-dispatcher"),
        name="CircuitEvaluator").
              asInstanceOf[EvaluatorInterface]

      val evaluatorActor = TypedActor(actorSystem).getActorRefFor(ta)
      ta.tellIdentity(evaluatorActor)
      ta
    }

    def step = evaluator.step
    def reset = evaluator.reset
    def release = {
      actorSystem.shutdown()
      platform.release()
    }
  }

  /** Does a very simple test to verify that CPU computations can
    * synchronize and compute correctly.
    *
    * Computation:
    * {{{
    *    DataSource --> DeviceDoubler --> DeviceDoubler --> DeviceDoubler
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * DeviceDoubler is a CPU kernel that reads the output of DataSource and
    * doubles every value in that field and writes it to an output buffer.
    *
    */
  test("single-output cpu kernel circuit") {

    /** Doubles a data field from another kernel. */
    class Doubler(input: AbstractKernel)
            extends OpenCLCpuSingleOutputKernel(DummyOp, Array(input.outputs(0)), fieldType)
    {
      def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
        val inputField = in(0).slave.read.asInstanceOf[ScalarFieldMemory]
        val outputField = out.master.cpuMemory.asInstanceOf[ScalarFieldMemory]

        for (i <- 0 until Size)
          outputField.write(i, 2f * inputField.read(i))

        // Copy the CPU memory to the GPU.
        out.master.write
      }

      /** Create a clone of this kernel that uses a new set of kernels as inputs.
        * Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
        require(inputs.length == 1)
        new Doubler(inputs(0).source)
      }

    }

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    val circuit = new KernelCircuit {
      val source = new DataSource
      val times2 = new Doubler(source)
      val times4 = new Doubler(times2)
      val times8 = new Doubler(times4)
      probeKernels(source, times2, times4, times8)
    }

    val evaluator = new Evaluator(circuit, platform)

    try {
      evaluator.reset

      // Now check the read mechanism.
      val fieldMemory = circuit.source.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory2 = circuit.times2.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory4 = circuit.times4.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory8 = circuit.times8.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      for (i <- 0 until Size) {
        require(fieldMemory.read(i) == sourceData(i))
        require(fieldMemory2.read(i) == sourceData(i) * 2)
        require(fieldMemory4.read(i) == sourceData(i) * 2 * 2)
        require(fieldMemory8.read(i) == sourceData(i) * 2 * 2 * 2)
      }
    }
    finally
      evaluator.release
  }

  /** Another test to verify that CPU computations can synchronize and
    *  compute correctly, this time with a multi-output cpu kernel
    *
    * Computation:
    * {{{
    *    DataSource --> |             | --> |             |
    *                   | SwapScaler  |     | SwapScaler  |
    *    DataSource --> |             | --> |             |
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapScaler is a CPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("multi-output cpu kernel circuit") {

    /** Scale and swap data from two other fields. */
    class SwapScaler(input0: VirtualFieldRegister, input1: VirtualFieldRegister)
            extends OpenCLCpuKernel(DummyOp, Array(input0, input1), Array(input1.fieldType, input0.fieldType))
    {
      def compute(in: Array[OpenCLFieldRegister], out: Array[OpenCLFieldRegister]) {
        val inputField0 = in(0).slave.read.asInstanceOf[ScalarFieldMemory]
        val inputField1 = in(1).slave.read.asInstanceOf[ScalarFieldMemory]
        val outputField0 = out(0).master.cpuMemory.asInstanceOf[ScalarFieldMemory]
        val outputField1 = out(1).master.cpuMemory.asInstanceOf[ScalarFieldMemory]

        for (i <- 0 until Size) {
          outputField0.write(i, 2f * inputField1.read(i))
          outputField1.write(i, 3f * inputField0.read(i))
        }

        // Copy the CPU memory to the GPU.
        out(0).master.write
        out(1).master.write
      }

      /** Create a clone of this kernel that uses a new set of kernels as inputs.
        * Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
        new SwapScaler(inputs(0), inputs(1))
      }

    }

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    // number suffixes on the fields indicate the applied data scale factor
    val circuit = new KernelCircuit {
      val source1 = new DataSource(1)
      val source5 = new DataSource(5)
      val times_10_3 = new SwapScaler(source1.outputs(0), source5.outputs(0))
      val times_6_30 = new SwapScaler(times_10_3.outputs(0), times_10_3.outputs(1))
      probeKernels(times_10_3, times_6_30)
    }

    val evaluator = new Evaluator(circuit, platform)

    try {
      evaluator.reset

      // Now check the read mechanism.
      val fieldMemory10 = circuit.times_10_3.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory3 = circuit.times_10_3.outputRegister(1).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory6 = circuit.times_6_30.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory30 = circuit.times_6_30.outputRegister(1).slave.read.asInstanceOf[ScalarFieldMemory]
      for (i <- 0 until Size) {
        require(fieldMemory10.read(i) == sourceData(i) * 10)
        require(fieldMemory3.read(i) == sourceData(i) * 3)
        require(fieldMemory6.read(i) == sourceData(i) * 6)
        require(fieldMemory30.read(i) == sourceData(i) * 30)
      }
    }
    finally
      evaluator.release
  }

  /** Does a very simple test to verify that CPU and GPU computations can
    * synchronize and compute correctly.
    *
    * Computation:
    * {{{
    *    DataSource --> DeviceDoubler --> DeviceDoubler --> DeviceDoubler --> DeviceDoubler
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * DeviceDoubler is a CPU kernel that reads the output of DataSource and
    * doubles every value in that field and writes it to an output buffer.
    *
    * Note that this simple test requires communication back and forth between
    * the CPU and GPU.
    */
  test("feedforward") {

    // Doubles an array on the GPU.
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
        """__kernel void dbl(global const float *a, global float *answer) {
          |    unsigned int xid = get_global_id(0);
          |    answer[xid] = a[xid] * 2.0f;
          | }
        """.stripMargin

      /** Create a clone of this kernel that uses a new set of kernels as inputs.
        * Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
        require(inputs.length == 1)
        new DeviceDoubler(inputs(0).source, inputs(0).fieldType)
      }
    }

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    val circuit = new KernelCircuit {
      val source = new DataSource
      val doubler = new DeviceDoubler(source, fieldType)
      val doubler2 = new DeviceDoubler(doubler, fieldType)
      val doubler3 = new DeviceDoubler(doubler2, fieldType)
      val doubler4 = new DeviceDoubler(doubler3, fieldType)
      probeKernels(source, doubler3, doubler4)
    }

    val evaluator = new Evaluator(circuit, platform)

    try {
      evaluator.reset
      // Now check the read mechanism.
      val sourceMemory = circuit.source.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      //    val fieldMemory = circuit.doubler.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      //    val fieldMemory2 = circuit.doubler2.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory3 = circuit.doubler3.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory4 = circuit.doubler4.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]

      for (i <- 0 until Size) {
        require(sourceMemory.read(i) == sourceData(i))

        // Buffer sharing invalidates the following checks, since
        // fieldMemory and fieldMemory3 use the same buffer, as does
        // fieldMemory2 and fieldMemory4:

        // require(fieldMemory.read(i) == sourceData(i) * 2)
        // require(fieldMemory2.read(i) == sourceData(i) * 2 * 2)

        require(fieldMemory3.read(i) == sourceData(i) * 2 * 2 * 2)
        require(fieldMemory4.read(i) == sourceData(i) * 2 * 2 * 2 * 2)
      }
    }
    finally
      evaluator.release
  }

  /** A simple test of feedback: a 0D counter.
    */
  test("feedback counter") {
//    OpenCLPlatform.release         ??

    // Initial value for counter
    val InitialCount = -4f

    // Adds one to each value in an array on the GPU
    class DeviceIncrementer(in: AbstractKernel, field: FieldType)
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
        """__kernel void incr(global const float *a, global float *answer) {
          |    unsigned int xid = get_global_id(0);
          |    answer[xid] = a[xid] + 1.0f;
          | }
        """.stripMargin

      /** Create a clone of this kernel that uses a new set of kernels as inputs.
        * Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
        require(inputs.length == 1)
        new DeviceIncrementer(inputs(0).source, inputs(0).fieldType)
      }
    }

    val fieldType = new FieldType(Shape(), Shape(), Float32)

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()
    // Counter register

    val circuit = new KernelCircuit {
      val counter = new RecurrentFieldKernel(fieldType, ConstantScalar0DOp(() => InitialCount))
      val incrementer = new DeviceIncrementer(counter, fieldType)
      counter.recurrence = incrementer.outputs(0)
    }

    val evaluator = new Evaluator(circuit, platform)

    try {
      evaluator.reset

      // Check that the counter actually counts.
      for (cycle <- 0 until 20) {
        // Read counter output
        val counterMemory =
          circuit.counter.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
        val counterValue = counterMemory.read
        require(counterValue == InitialCount + cycle)
        evaluator.step
      }

      // Check reset
      evaluator.reset
      // Read counter output
      val counterMemory =
        circuit.counter.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val counterValue = counterMemory.read
      require(counterValue == InitialCount)
    }
    finally
      evaluator.release
  }

  /** Another test to verify that CPU computations can synchronize and
    *  compute correctly, this time with a multi-output gpu kernel
    *
    * Computation:
    * {{{
    *    DataSource --> |             | --> |             |
    *                   | SwapScaler  |     | SwapScaler  |
    *    DataSource --> |             | --> |             |
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapScaler is a GPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("multi-output gpu kernel circuit") {

    /** Scale and swap data from two other fields. Note: this is not a HyperKernel. */
    class SwapScaler(input0: VirtualFieldRegister, input1: VirtualFieldRegister)
            extends OpenCLDeviceKernel(DummyOp, Array(input0, input1), Array(input1.fieldType, input0.fieldType))
    {
      require(input0.fieldType == input1.fieldType)
      val workGroup = new WorkGroupParameters(1,
        globalLayers = 1,
        globalRows = 1,
        globalColumns = input0.fieldType.fieldShape.points,
        localLayers = 1,
        localRows = 1,
        localColumns = 1
      )
      val kernelCode: String =
        """__kernel void incr(global const float *in0,
          |                   global const float *in1,
          |                   global float *answer0,
          |                   global float *answer1) {
          |    unsigned int xid = get_global_id(0);
          |    answer0[xid] = in1[xid] * 2.0f;
          |    answer1[xid] = in0[xid] * 3.0f;
          | }
        """.stripMargin

      /** Create a clone of this kernel that uses a new set of kernels as inputs.
        * Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
        new SwapScaler(inputs(0), inputs(1))
      }
    }

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    // number suffixes on the fields indicate the applied data scale factor
    val circuit = new KernelCircuit {
      val source1 = new DataSource(1)
      val source5 = new DataSource(5)
      val times_10_3 = new SwapScaler(source1.outputs(0), source5.outputs(0))
      val times_6_30 = new SwapScaler(times_10_3.outputs(0), times_10_3.outputs(1))
      probeKernels(times_10_3, times_6_30)
    }

    val evaluator = new Evaluator(circuit, platform)

    try {
      evaluator.reset

      // Now check the read mechanism.
      val fieldMemory10 = circuit.times_10_3.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory3 = circuit.times_10_3.outputRegister(1).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory6 = circuit.times_6_30.outputRegister(0).slave.read.asInstanceOf[ScalarFieldMemory]
      val fieldMemory30 = circuit.times_6_30.outputRegister(1).slave.read.asInstanceOf[ScalarFieldMemory]
      for (i <- 0 until Size) {
        require(fieldMemory10.read(i) == sourceData(i) * 10)
        require(fieldMemory3.read(i) == sourceData(i) * 3)
        require(fieldMemory6.read(i) == sourceData(i) * 6)
        require(fieldMemory30.read(i) == sourceData(i) * 30)
      }
    }
    finally
      evaluator.release
  }
  /** BigTensorMode kernels output their data from within the CodeFragment, not from an OutputFragment.
    * This test verifies that when two BigtensorMode kernels get merged, the proper output field name
    * is used within the BigTensorMode kernels (is no longer necessarily _out_field_0).
    */
  test("multi-output gpu kernel created by merging two BigTensorMode kernels") {

    // Dummy opcode.
    case object DummyOp0 extends Opcode("dummy1")
    case object DummyOp1 extends Opcode("dummy2")

    class BigTensorKernel(in: VirtualFieldRegister,
                                          operation: Opcode)
            extends HyperKernel(operation, Array(in), in.fieldType, BigTensorAddressing)
    {
      addCode("// inputting from field fieldName(@in0)\n" +
              "    // outputting to field @outFieldName0\n" +
              "    column = _column;\n" +
              "    @outElementNonlocal0 = read(@in0);\n")
    }

    /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
    val platform = OpenCLPlatform()

    // number suffixes on the fields indicate the applied data scale factor
    val circuit = new KernelCircuit {
      val source = new DataSource(1)
      val out0 = new BigTensorKernel(source.outputs(0), DummyOp0)
      val out1 = new BigTensorKernel(source.outputs(0), DummyOp1)
      probeKernels(out0, out1)
    }

    KernelCircuitOptimizer.optimize(circuit, platform.kernelCodeGenParams)
    val evaluator = new Evaluator(circuit, platform)

    // Track down where the field memory is after the optimizer has run
    def kernelToScalarFieldMemory(k: AbstractKernel) = {
      val reg = circuit.findStolenOutput(k.outputs(0)).asInstanceOf[VirtualFieldRegister]
      val mem = reg.register.asInstanceOf[OpenCLFieldRegister].slave.read
      mem.asInstanceOf[ScalarFieldMemory]
    }

    try {
      evaluator.reset
      val fieldMemory0 = kernelToScalarFieldMemory(circuit.out0)
      val fieldMemory1 = kernelToScalarFieldMemory(circuit.out1)
      for (i <- 0 until Size) {
        require(fieldMemory0.read(i) == sourceData(i))
        require(fieldMemory1.read(i) == sourceData(i))
      }
    }
    finally
      evaluator.release
  }
  /** This is a recasting of a previous test, this time with the SwapScaler being a multi-output
    * HyperKernel, subject to merging.
    *
    * Computation:
    * {{{
    *    DataSource --> |             | --> |             |
    *                   | SwapScaler  |     | SwapScaler  |
    *    DataSource --> |             | --> |             |
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapDoubler is a GPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("multi-output primitive gpu kernel") {

    def runTest(optimize: Boolean, expectedKernelCount: Int) {
      /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
      val platform = OpenCLPlatform()

      // number suffixes on the fields indicate the applied data scale factor
      val circuit = new KernelCircuit {
        val op = ScaleOp(3,2)
        val source1 = new DataSource(1)
        val source5 = new DataSource(5)
        val times_10_3 = new SwapScaler(source1.outputs(0), source5.outputs(0), op)
        val times_6_30 = new SwapScaler(times_10_3.outputs(0), times_10_3.outputs(1), op)
        if (!optimize)
          times_10_3.markProbed
        times_6_30.markProbed
      }

      if (optimize)
        KernelCircuitOptimizer.optimize(circuit, platform.kernelCodeGenParams)
      val evaluator = new Evaluator(circuit, platform)

      try {
        evaluator.reset
        // Now check the read mechanism.

        if (!optimize) {
          val fieldMemory10 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 0)
          val fieldMemory3 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 1)
          for (i <- 0 until Size) {
            require(fieldMemory10.read(i) == sourceData(i) * 10)
            require(fieldMemory3.read(i) == sourceData(i) * 3)
          }
        }
        val fieldMemory6 = kernelToScalarFieldMemory(circuit, circuit.times_6_30, 0)
        val fieldMemory30 = kernelToScalarFieldMemory(circuit, circuit.times_6_30, 1)
        for (i <- 0 until Size) {
          require(fieldMemory6.read(i) == sourceData(i) * 6)
          require(fieldMemory30.read(i) == sourceData(i) * 30)
        }
        requireHyperKernelCount(circuit, expectedKernelCount)
      }
      finally
        evaluator.release
    }

    runTest(optimize=false, 2)
    runTest(optimize=true, 1)
  }
  /** This builds on a previous test and employs the `SwapScalar` in a circuit subject to common
    * subexpression elimination:
    *
    * Computation:
    * {{{
    *    DataSourceA --> |              | -------> |              |
    *                    | SwapScalerC  |          | SwapScalerD  |
    *    DataSourceB --> |              | -------> |              |
    *
    *
    *    DataSourceA --> |              | -------> |              |
    *                    | SwapScalerC  |          | SwapScalerE  |
    *    DataSourceB --> |              | -------> |              |
    * }}}
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapDoubler is a GPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("multi-output primitive gpu kernel with CSE") {

    def runTest(optimize: Boolean, expectedKernelCount: Int) {
      /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
      val platform = OpenCLPlatform()
      // number suffixes on the fields indicate the applied data scale factor
      val circuit = new KernelCircuit {
        val source1 = new DataSource(1)
        val source5 = new DataSource(5)
        val op = ScaleOp(3,2)
        val op2 = ScaleOp(7,11)
        val times_10_3 = new SwapScaler(source1.outputs(0), source5.outputs(0), op)
        val times_10_3_also = new SwapScaler(source1.outputs(0), source5.outputs(0), op)
        val times_6_30 = new SwapScaler(times_10_3.outputs(0), times_10_3.outputs(1), op)
        val times_33_70 = new SwapScaler(times_10_3_also.outputs(0), times_10_3_also.outputs(1), op2)
        if (!optimize)
          times_10_3.markProbed
        probeKernels(times_6_30, times_33_70)
      }

      if (optimize)
        KernelCircuitOptimizer.optimize(circuit, platform.kernelCodeGenParams)
      val evaluator = new Evaluator(circuit, platform)

      try {
        evaluator.reset
        // Now check the read mechanism.

        if (!optimize) {
          val fieldMemory10 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 0)
          val fieldMemory3 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 1)
          for (i <- 0 until Size) {
            require(fieldMemory10.read(i) == sourceData(i) * 10)
            require(fieldMemory3.read(i) == sourceData(i) * 3)
          }
        }
        val fieldMemory6 = kernelToScalarFieldMemory(circuit, circuit.times_6_30, 0)
        val fieldMemory30 = kernelToScalarFieldMemory(circuit, circuit.times_6_30, 1)
        val fieldMemory33 = kernelToScalarFieldMemory(circuit, circuit.times_33_70, 0)
        val fieldMemory70 = kernelToScalarFieldMemory(circuit, circuit.times_33_70, 1)
        for (i <- 0 until Size) {
          require(fieldMemory6.read(i) == sourceData(i) * 6)
          require(fieldMemory30.read(i) == sourceData(i) * 30)
          require(fieldMemory33.read(i) == sourceData(i) * 33)
          require(fieldMemory70.read(i) == sourceData(i) * 70)
        }
        requireHyperKernelCount(circuit, expectedKernelCount)
      }
      finally
        evaluator.release
    }

    val optimizedKernels = if (HyperKernelMultiOutputMerger.Enabled) 1 else 3
    runTest(optimize=false, 4)
    runTest(optimize=true, optimizedKernels)
  }

  /** This test includes 4 independent small circuits, often with multi-output primitive kernels with
    * unconnected outputs.  It was designed to exercise the check in the HyperKernel merging logic
    * that requires:
    *
    * mergeOK = ... &&
    *   (sourceKernel.drivesOnly(sinkKernel) || sourceKernel.drivesAllOf(sinkKernel))
    *
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapDoubler is a GPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("multi-output primitive gpu kernels with unconnected outputs") {

    def runTest(optimize: Boolean, expectedKernelCount: Int) {
      /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
      val platform = OpenCLPlatform()
      // number suffixes on the fields indicate the applied data scale factor
      val circuit = new KernelCircuit {
        val source1 = new DataSource(1)
        val source2 = new DataSource(2)
        val source3 = new DataSource(3)
        val source4 = new DataSource(4)
        val source5 = new DataSource(5)
        val op = ScaleOp(3,2)
        val op2 = ScaleOp(7,11)
        val op3 = ScaleOp(2,6)
        val times_10_3 = new SwapScaler(source1.outputs(0), source5.outputs(0), op)
        val times_9 = BinaryConstHyperKernel(Array(times_10_3.outputs(1)), MultiplyConstOp(3f), times_10_3.outputs(1).fieldType)

        val times_4_3 = new SwapScaler(source1.outputs(0), source2.outputs(0), op)
        val times_20 = BinaryConstHyperKernel(Array(times_4_3.outputs(0)), MultiplyConstOp(5f), times_4_3.outputs(1).fieldType)
        val times_12 = BinaryConstHyperKernel(Array(times_4_3.outputs(1)), MultiplyConstOp(4f), times_4_3.outputs(1).fieldType)

        val times_55_14 = new SwapScaler(source2.outputs(0), source5.outputs(0), op2)
        val times_24_110 = new SwapScaler(times_55_14.outputs(0), source4.outputs(0), op3)

        val times_30_6 = new SwapScaler(source3.outputs(0), source5.outputs(0), op3)
        val times_36 = BinaryHyperKernel(times_30_6.outputs.toArray, AddOp, source1.fieldType)
        val times_40 = BinaryHyperKernel(Array(times_36.outputs(0),source4.outputs(0)), AddOp, source1.fieldType)

        if (!optimize)
          times_10_3.outputs.foreach(_.markProbed())
        probeKernels(times_9, times_24_110, times_40, times_20, times_12)
      }

      if (optimize)
        KernelCircuitOptimizer.optimize(circuit, platform.kernelCodeGenParams)
      val evaluator = new Evaluator(circuit, platform)

      try {
        evaluator.reset
        // Now check the read mechanism.

        if (!optimize) {
          val fieldMemory10 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 0)
          val fieldMemory3 = kernelToScalarFieldMemory(circuit, circuit.times_10_3, 1)
          for (i <- 0 until Size) {
            require(fieldMemory10.read(i) == sourceData(i) * 10)
            require(fieldMemory3.read(i) == sourceData(i) * 3)
          }
        }
        val fieldMemory9 = kernelToScalarFieldMemory(circuit, circuit.times_9, 0)
        val fieldMemory24 = kernelToScalarFieldMemory(circuit, circuit.times_24_110, 0)
        val fieldMemory110 = kernelToScalarFieldMemory(circuit, circuit.times_24_110, 1)
        val fieldMemory40 = kernelToScalarFieldMemory(circuit, circuit.times_40, 0)
        val fieldMemory20 = kernelToScalarFieldMemory(circuit, circuit.times_20, 0)
        val fieldMemory12 = kernelToScalarFieldMemory(circuit, circuit.times_12, 0)
        for (i <- 0 until Size) {
          require(fieldMemory9.read(i) == sourceData(i) * 9)
          require(fieldMemory24.read(i) == sourceData(i) * 24)
          require(fieldMemory110.read(i) == sourceData(i) * 110)
          require(fieldMemory40.read(i) == sourceData(i) * 40)
          require(fieldMemory20.read(i) == sourceData(i) * 20)
          require(fieldMemory12.read(i) == sourceData(i) * 12)
        }
        requireHyperKernelCount(circuit, expectedKernelCount)
      }
      finally
        evaluator.release
    }

    runTest(optimize=false, 10)
    runTest(optimize=true, 4)
  }
  /** This test includes 2 independent small circuits.  It tests whether a probed field register can be merged without
    * disturbing the ability to read it.  Such probed fields create additional outputs on the merged kernel.
    *
    * The DataSource is a CPU kernel that writes a 1D field to a buffer. The
    * SwapDoubler is a GPU kernel that reads the output of two DataSources and
    * writes those values to its two outputs after scaling the values differently
    * and swapping the outputs (i.e. first input dictates second output).
    *
    */
  test("single- and multi-output gpu kernels with probes") {

    def runTest(optimize: Boolean, expectedKernelCount: Int) {
      /** Selects the OpenCLPlatform to use and creates a unique context for this KernelCircuit. */
      val platform = OpenCLPlatform()
      // number suffixes on the fields indicate the applied data scale factor
      val circuit = new KernelCircuit {
        val source1 = new DataSource(1)
        val source2 = new DataSource(2)
        val source3 = new DataSource(3)
        val op = ScaleOp(3,4)
        // First circuit is a chain of two single-ouptut kernels
        val times_4 = BinaryConstHyperKernel(Array(source1.outputs(0)), MultiplyConstOp(4f), source1.outputs(0).fieldType)
        val times_20 = BinaryConstHyperKernel(Array(times_4.outputs(0)), MultiplyConstOp(5f), times_4.outputs(0).fieldType)
        times_4.markProbed(0)
        times_20.markProbed(0)

        // Second circuit is a chain of two kernels, the first kernel being a multi-output kernel
        val times_12_6 = new SwapScaler(source2.outputs(0), source3.outputs(0), op)
        val times_18 = BinaryConstHyperKernel(Array(times_12_6.outputs(1)), MultiplyConstOp(3f), times_12_6.outputs(1).fieldType)
        times_12_6.markProbed(1)
        times_18.markProbed(0)
      }

      if (optimize)
        KernelCircuitOptimizer.optimize(circuit, platform.kernelCodeGenParams)
      val evaluator = new Evaluator(circuit, platform)

      try {
        evaluator.reset
        val fieldMemory4 = kernelToScalarFieldMemory(circuit, circuit.times_4, 0)
        val fieldMemory20 = kernelToScalarFieldMemory(circuit, circuit.times_20, 0)
        val fieldMemory6 = kernelToScalarFieldMemory(circuit, circuit.times_12_6, 1)
        val fieldMemory18 = kernelToScalarFieldMemory(circuit, circuit.times_18, 0)
        for (i <- 0 until Size) {
          require(fieldMemory4.read(i) == sourceData(i) * 4)
          require(fieldMemory20.read(i) == sourceData(i) * 20)
          require(fieldMemory6.read(i) == sourceData(i) * 6)
          require(fieldMemory18.read(i) == sourceData(i) * 18)
        }
        requireHyperKernelCount(circuit, expectedKernelCount)
      }
      finally
        evaluator.release
    }

    runTest(optimize=false, 4)
    runTest(optimize=true, 2)
  }
}
