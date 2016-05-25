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

package cogx.platform.opencl

import cogx.compiler.parser.op.RestoredOpcode
import cogx.platform.checkpoint.ObjectRestorer
import cogx.platform.types.{AbstractKernel, VirtualFieldRegister, FieldType}
import cogx.runtime.ComputeGraphRestorerState

/**
  * @author Dick Carter
  */
private[cogx]
class OpenCLRestoredDeviceKernel(opcode: RestoredOpcode,
                                val kernelCode: String,
                                val simpleKernelName: String,
                                val workGroup: WorkGroupParameters,
                                in: Array[VirtualFieldRegister],
                                resultTypes: Array[FieldType]) extends OpenCLDeviceKernel(opcode, in, resultTypes) {

  /** Useful name for debugging. */
  override def toString: String = {
    simpleKernelName + "_" + id + "() => " + fieldTypesString +
      " '" + name + "'" + ", inputs = " + inputFieldTypesString
  }

  /** Create a clone of this kernel that uses a new set of kernels as inputs.
    * Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    new OpenCLRestoredDeviceKernel(opcode, kernelCode, simpleKernelName, workGroup, inputs, resultTypes)
  }
}

object OpenCLRestoredDeviceKernel {
  /** Create a FieldType instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created FieldType based on the read information.
    */
  def restore(restorer: ObjectRestorer, kernelId: Int, inputRegisters: Array[VirtualFieldRegister],
              resultTypes: Array[FieldType]): OpenCLRestoredDeviceKernel = {
    val kernelCodes = restorer.asInstanceOf[ComputeGraphRestorerState].kernelCodes
    val workGroupParameters = restorer.readRestorable("workGroupParameters",
      WorkGroupParameters).asInstanceOf[WorkGroupParameters]
    val kernelCodeIndex = restorer.readInt("kernelCodeIndex")

    val kernelCode = kernelCodes(kernelCodeIndex).codeAsRun
    val simpleKernelName = kernelCodes(kernelCodeIndex).nameWithIdRemoved
    new OpenCLRestoredDeviceKernel(RestoredOpcode(kernelId.toString), kernelCode, simpleKernelName,
      workGroupParameters, inputRegisters, resultTypes)
  }
}