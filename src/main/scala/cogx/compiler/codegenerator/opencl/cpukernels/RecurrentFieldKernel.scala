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

import cogx.cogmath.collection.IdentityHashMap
import cogx.platform.checkpoint.{ObjectRestorer, RestoreFactory, ObjectSaver, Saveable}
import cogx.platform.types.KernelTypes.RecurrenceKernelType
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.platform.opencl._
import cogx.platform.cpumemory._
import cogx.compiler.parser.op._
import cogx.runtime.{ComputeGraphSaverState, ComputeGraph, ComputeGraphRestorerState}

import scala.collection.mutable


/** A kernel representing an input that is the output of some other kernel
  * delayed one cycle, called a recurrence.
  *
  * This kind of kernel doesn't do any processing, it's just a relay from
  * feedback output to input. So all compute methods do nothing except
  * generate an output event for kernels that use it. All processing is done
  * by the recurrence's driver.
  *
  * Recurrences must be initialized, though, so the reset method here is
  * actively used.
  *
  * @param fieldType The type of the recurrence.
  *
  * @author Greg Snider
  */
private[cogx]
class RecurrentFieldKernel(fieldType: FieldType, opcode: NulleryOpcode)
        extends OpenCLCpuSingleOutputKernel(opcode, Array(), fieldType, needActor = false)
        with ConstantHelper
        with Saveable
{
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType = RecurrenceKernelType

  /** Optional input value which changes the field each cycle. */
  private var recurrenceInput: VirtualFieldRegister = null

  /** Assign a recurrence input to this kernel. */
  def recurrence_=(input: VirtualFieldRegister) {
    recurrenceInput = input
  }

  /** Get the bound recurrence input to this kernel, if any. */
  def recurrence: VirtualFieldRegister =
    recurrenceInput

  // We force two RecurrentFieldKernels to always appear different.  Otherwise, two
  // such kernels with equal initialization by the same function object may be consolidated
  // to ill effect (they might have different recurrences).  Put another way, we don't want to
  // consolidate pipeline registers since it won't be common and there are too many corner cases.
  override def canEqual(other: Any): Boolean = false

  /** Initialize the constant field in the output buffer for this kernel. */
  override def reset() {
    opcode match {
      case NullOp =>  // Probably a SensorKernel in pipelined mode, which will source reset data.
      case op: ConstantOp => reset(outputRegister(0), fieldType, op)
      case x => throw new RuntimeException(s"Internal error: expected ConstantOp or NullOp, found: $x")
    }
  }

  /** Clock output registers. The RecurrentFieldKernel shares its fieldregister
    * with the kernel that writes it dynamically.  Only one of these two players needs
    * to respond to the phase0Clock.  As we contemplate clocking individual pipeline
    * stages separately, it makes the most sense to have the RecurrentFieldKernel perform
    * the clocking action- the other kernel is typically viewed as part of the previous
    * pipestage.  The only trick is with pipelined sensors that update their values optionally.
    * This scenario is recognized by the RecurrenceKernel's NullOp (it has no initialization
    * responsibility because the init data comes from the Sensor).  Also, if the  Sensor
    * has not written data, the cpuMemoryValid flag will appear false and no actual clocking
    * should be performed by the RecurrentFieldKernel.
    *
    */
  override def phase0Clock() {
    opcode match {
      case NullOp =>
      // Probably a SensorKernel in pipelined mode, which will source reset data.
        for (outputRegister <- outputRegisters) {
          if (outputRegister.master.cpuMemoryValid) {
            outputRegister.clock()
            outputRegister.master.invalidateCpuMemory
          }
        }
      case op: ConstantOp =>
        for (outputRegister <- outputRegisters) {
          outputRegister.clock()
        }
      case x => throw new RuntimeException(s"Internal error: expected ConstantOp or NullOp, found: $x")
    }
  }

  /** Dummy compute method, does nothing. */
  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister) {
  }

  /** A string description used for debugging. */
  override def toString: String = {
    val feedback =
      if (recurrence == null)
        ""
      else if (recurrence eq this.outputs(0))
        " {FEEDBACK from self}"
      else
        " {FEEDBACK from kernel " + recurrence.source.id + "}"
    getClass.getSimpleName + id + " => " + fieldType.toString + feedback +
            " '" + name + "'"
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): RecurrentFieldKernel = {
    // Note that we intentially do NOT copy the old value of 'recurrence' here.
    // These copy methods exist to support cloning kernels from a large circuit
    // into several smaller subcircuits. There's probably a clone of the
    // original recurrence driver somewhere, and that's probably what this copy
    // needs to be hooked up to.
    new RecurrentFieldKernel(fieldType, opcode)
  }


  /** Save this instance using the facilities of the ObjectSaver */
  override def save(saver: ObjectSaver): Unit = {
    val saverState = saver.asInstanceOf[ComputeGraphSaverState]
    import saverState._
    super.save(saver)
    val recurrenceIndex = vfrToIndex.get(recurrence) match {
      case Some(i) => i
      case None => throw new RuntimeException("Internal error: expecting to find recurrence field in database.")
    }
    saver.writeInt("recurrenceField", recurrenceIndex)
    saveData(saver, computeGraph.readVirtualRegister(recurrence), fieldType, opcode)
  }
}

/** A factory method to restore a recurrent field kernel from its stored data.
  *
  * @author Dick Carter
  */
private[cogx]
object RecurrentFieldKernel extends ConstantHelper {
  /** Create a RecurrentFieldKernel instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created RecurrentFieldKernel based on the read information.
    */
  def restore(restorer: ObjectRestorer, resultType: FieldType): RecurrentFieldKernel = {
    val kernelToRecurrenceIndex = restorer.asInstanceOf[ComputeGraphRestorerState].recurrences
    val recurrenceFieldIndex = restorer.readInt("recurrenceField")
    val opcode = restoreData(restorer, resultType)
    val restoredKernel = new RecurrentFieldKernel(resultType, opcode)
    kernelToRecurrenceIndex(restoredKernel) = recurrenceFieldIndex
    restoredKernel
  }

}
