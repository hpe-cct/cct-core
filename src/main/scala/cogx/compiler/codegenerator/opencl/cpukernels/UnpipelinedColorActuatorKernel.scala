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

import cogx.platform.checkpoint.{ObjectSaver, Saveable}
import cogx.platform.types.KernelTypes.ActuatorKernelType
import cogx.platform.types.{Pixel, VirtualFieldRegister, FieldType, AbstractKernel}
import cogx.platform.opencl.{OpenCLCpuKernel, OpenCLFieldRegister}
import cogx.compiler.parser.op._
import cogx.platform.cpumemory.ColorFieldMemory

/**
  *  The kernel to handle unpipelined actuators.
  *
  * @author Dick Carter
  */
private[cogx]
class UnpipelinedColorActuatorKernel(in: VirtualFieldRegister, op: UnpipelinedColorActuatorOp,
                                     actuatorClassname: String, restoreParameters: () => String)
        extends OpenCLCpuKernel(op, Array(in), Array[FieldType]())
        with Saveable
{
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType = ActuatorKernelType

  /** The version of the kernel. */
  def majorVersion = 1

  /** The version of the kernel. */
  def minorVersion = 0

  /** Invoke the user-supplied resetHook upon reset */
  override def reset() {
    op.resetHook()
  }

  def compute(in: Array[OpenCLFieldRegister], out: Array[OpenCLFieldRegister]) {
    // Read the GPU data to the CPU buffer
    val cpuMemory = in(0).slave.read.asInstanceOf[ColorFieldMemory]

    // Pass the data to the user-defined `nextOutput` function.
    op.newOutput(cpuMemory.iterator)
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    // Note that we intentionally do NOT copy the old value of 'source' here.
    // These copy methods exist to support cloning kernels from a large circuit
    // into several smaller subcircuits. There's probably a clone of the
    // original actuator driver somewhere, and that's probably what this copy
    // needs to be hooked up to.
    new UnpipelinedColorActuatorKernel(in, op, actuatorClassname, restoreParameters)
  }
  /** Save this instance using the facilities of the ObjectSaver */
  override def save(saver: ObjectSaver): Unit = {
    super.save(saver)
    saver.writeInt("majorVersion", majorVersion)
    saver.writeInt("minorVersion", minorVersion)
    saver.writeString("actuatorClassname", actuatorClassname)
    saver.writeString("restoreParameters", restoreParameters())
  }
}
