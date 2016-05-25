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

import cogx.cogmath.algebra.real.Vector
import cogx.platform.checkpoint.{ObjectSaver, Saveable}
import cogx.platform.types.KernelTypes.ActuatorKernelType
import cogx.platform.types.{VirtualFieldRegister, FieldType, AbstractKernel}
import cogx.platform.opencl.{OpenCLCpuKernel, OpenCLFieldRegister}
import cogx.compiler.parser.op._
import cogx.platform.cpumemory.{VectorFieldMemory, ScalarFieldMemory}

/**
  *  The kernel to handle unpipelined vector actuators.
  *
  * @author Dick Carter
  */
private[cogx]
class UnpipelinedVectorActuatorKernel(in: VirtualFieldRegister, op: UnpipelinedVectorActuatorOp,
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
    //Read GPU data to CPU buffer
    val cpuMemory = in(0).slave.read match {
      case r: VectorFieldMemory => r
      case x => throw new RuntimeException("Wrong field type for VectorActuator: " + x)
    }
    val fieldShape = cpuMemory.fieldShape
    val tensorShape = cpuMemory.tensorShape
    val vec = Vector(tensorShape(0), (i)=>0f)
    val data = fieldShape.indices.flatMap(idx => {
      fieldShape.dimensions match {
        case 0 => cpuMemory.read(vec); vec.toArray
        case 1 => cpuMemory.read(idx(0), vec); vec.toArray
        case 2 => cpuMemory.read(idx(0), idx(1), vec); vec.toArray
        case 3 => cpuMemory.read(idx(0), idx(1), idx(2), vec); vec.toArray
      }
    }).toIterator

    // Pass the data to the user-defined `nextOutput` function.
    op.newOutput(data)
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    // Note that we intentionally do NOT copy the old value of 'source' here.
    // These copy methods exist to support cloning kernels from a large circuit
    // into several smaller subcircuits. There's probably a clone of the
    // original actuator driver somewhere, and that's probably what this copy
    // needs to be hooked up to.
    new UnpipelinedVectorActuatorKernel(in, op, actuatorClassname, restoreParameters)
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
