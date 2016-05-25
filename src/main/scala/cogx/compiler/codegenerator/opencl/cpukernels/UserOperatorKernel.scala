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

import cogx.platform.types.{VirtualFieldRegister, AbstractKernel}
import cogx.compiler.parser.op.UserOpcode
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.platform.cpumemory.AbstractFieldMemory

/** Kernel which executes a user-defined operator.
  *
  * @author Greg Snider
  */
private[cogx]
class UserOperatorKernel(in: Array[VirtualFieldRegister],
                          opcode: UserOpcode)
        extends OpenCLCpuSingleOutputKernel(opcode, in, opcode.operator.fieldType)
{

  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
    // Read the GPU data to the CPU input buffers
    val inputCpuMemory: Array[AbstractFieldMemory] =
      in.map(_.slave.read.asInstanceOf[AbstractFieldMemory])

    // Get the output CPU buffer
    val outputCpuMemory = out.master.cpuMemory.asInstanceOf[AbstractFieldMemory]

    // Invoke the user operator
    opcode.operator.execute(inputCpuMemory :_*)(outputCpuMemory)

    // Write the result back to the GPU.
    out.master.write
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel =
    new UserOperatorKernel(inputs, opcode)

}