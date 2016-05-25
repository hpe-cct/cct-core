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

package cogx.compiler.codegenerator.opencl.cpukernels

import cogx.platform.types.KernelTypes.{KernelType, CPUOuterProductKernelType}
import cogx.platform.types._
import cogx.compiler.parser.op.OuterProductOp
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.platform.cpumemory.ScalarFieldMemory

/**
  * Computes the outer product of the two field inputs.  Code ported from the
  * Cog 3.5 ScalarField class.
  *
  * @author Dick Carter
  */
private[cogx]
class CPUOuterProductKernel private (in: Array[VirtualFieldRegister], op: Opcode, resultType: FieldType)
        extends OpenCLCpuSingleOutputKernel(op, in, resultType)
{
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType: KernelType = CPUOuterProductKernelType

  /** Outer product of "in1" and "in2".
    *
    * If either field is 0 dimensional, it's
    * treated as a scalar multiplication of the other field.
    */
  def outerProduct(in1: ScalarFieldMemory, in2: ScalarFieldMemory, out: ScalarFieldMemory) {

    def inRead(mem: ScalarFieldMemory)(indices: Array[Int]) = mem.dimensions match {
      case 0 => mem.read()
      case 1 => mem.read(indices(0))
      case 2 => mem.read(indices(0), indices(1))
      case 3 => mem.read(indices(0), indices(1), indices(2))
      case _ => throw new RuntimeException("Input dimension too big.")
    }

    val in1Reader = inRead(in1)(_)
    val in2Reader = inRead(in2)(_)

    def outWrite(mem: ScalarFieldMemory)(indices: Array[Int], value: Float) = mem.dimensions match {
      case 0 => mem.write(value)
      case 1 => mem.write(indices(0), value)
      case 2 => mem.write(indices(0), indices(1), value)
      case 3 => mem.write(indices(0), indices(1), indices(2), value)
      case _ => throw new RuntimeException("Output dimension too big.")
    }

    val outWriter = outWrite(out)(_, _)

    for (i <- in1.fieldShape.indices) {
      val iValue = in1Reader(i)
      for (j <- in2.fieldShape.indices) {
        val jValue = in2Reader(j)
        val resultIndex = Array.concat(i, j)
        outWriter(resultIndex, iValue * jValue)
      }
    }
  }

  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
    val inputField1 = in(0).slave.read.asInstanceOf[ScalarFieldMemory]
    val inputField2 = in(1).slave.read.asInstanceOf[ScalarFieldMemory]
    val outputField = out.master.cpuMemory.asInstanceOf[ScalarFieldMemory]

    outerProduct(inputField1, inputField2, outputField)
    // Copy the CPU memory to the GPU.
    out.master.write
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel =
    new CPUOuterProductKernel(inputs, op, resultType)
}

private[cogx]
object CPUOuterProductKernel {

  def apply(in: Array[VirtualFieldRegister], operation: Opcode, resultType: FieldType) = {
    require(operation == OuterProductOp)
    new CPUOuterProductKernel(in, operation, resultType)
  }
}
