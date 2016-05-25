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

import cogx.platform.checkpoint.{ObjectRestorer, Saveable, ObjectSaver}
import cogx.platform.types.KernelTypes.ConstantKernelType
import cogx.platform.types._
import cogx.compiler.parser.op._
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.runtime.{ComputeGraphSaverState, ComputeGraph}


/** A kernel representing a constant field.
  *
  * @param fieldType The type of the constant field.
  *
  * @author Greg Snider
  */
private[cogx]
class ConstantFieldKernel(fieldType: FieldType, opcode: ConstantOp)
        extends OpenCLCpuSingleOutputKernel(opcode, Array(), fieldType, needActor = false)
        with ConstantHelper
        with Saveable
{
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType = ConstantKernelType

  /** Compute; since the field is just a constant there's nothing to do here,
    * all the heavy lifting is done in reset.
    */
  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
  }

  /** Constants don't compute, so clocking does nothing. */
  override def phase0Clock() { }

  /** Initialize the constant field in the output buffer for this kernel. */
  override def reset() { reset(outputRegister(0), fieldType, opcode) }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]) =
    new ConstantFieldKernel(fieldType, opcode)

  /** Save this instance using the facilities of the ObjectSaver */
  override def save(saver: ObjectSaver): Unit = {
    val saverState = saver.asInstanceOf[ComputeGraphSaverState]
    import saverState._
    super.save(saver)
//    val cg = saver.globalState("computeGraph").asInstanceOf[ComputeGraph]
    saveData(saver, computeGraph.readVirtualRegister(outputs(0)), fieldType, opcode)
//    saveData(saver, cg.readVirtualRegister(outputs(0)), fieldType, opcode)
  }
}

/** A factory method to restore a constant field kernel from its stored data.
  *
  * @author Dick Carter
  */
private[cogx]
object ConstantFieldKernel extends ConstantHelper {
  /** Create a ConstantFieldKernel instance through use of the provided ObjectRestorer
    *
    * @param restorer The restorer through which to read the new object state.
    * @return The created ConstantFieldKernel based on the read information.
    */
  def restore(restorer: ObjectRestorer, resultType: FieldType): ConstantFieldKernel = {
    val opcode = restoreData(restorer, resultType).asInstanceOf[ConstantOp]
    new ConstantFieldKernel(resultType, opcode)
  }
}