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

package cogx.platform.types

import java.util.NoSuchElementException

/** Facility for saving/restoring a Cog kernel code type.
  *
  * The 'stock' Scala sealed abstract class lacks a factory maker of class instances given a stored id.
  * The apply() method below achieves this, but requires that all additions or deletions to the case objects
  * be reflected in the 'codeTypes' Seq.  Important then to keep this in-sync!!!
  *
  * A plain Scala enumeration lacks the compile-time exhaustive match checking of sealed abstract classes.
  * I was also concerned that over time there might be deletions in the list of supported code types.  It
  * would be important to keep the id's the same for the values that had already been stored (i.e. better
  * to allow holes to appear in the sequence of valid id's than to shift the id assignments).
  *
  * Created by Dick Carter.
  */

object KernelTypes {
  /** The kernel code language. */
  sealed abstract class KernelType(val name: String, val supportedByNativeRuntime: Boolean) {
    override def toString = name
  }

  /** Identifies a device (i.e. gpu) kernel. */
  case object DeviceKernelType extends KernelType("DeviceKernel", true)

  /** Identifies a sensor kernel. */
  case object SensorKernelType extends KernelType("SensorKernel", true)

  /** Identifies an actuator kernel. */
  case object ActuatorKernelType extends KernelType("ActuatorKernel", true)

  /** Identifies a recurrence (i.e. stateful-field-managing) kernel. */
  case object RecurrenceKernelType extends KernelType("RecurrenceKernel", true)

  /** Identifies a kernel that produces a constant field. */
  case object ConstantKernelType extends KernelType("ConstantKernel", true)

  /** Identifies a kernel that does nothing, introduced to ensure the kernel DAG has kernels as roots (not fields). */
  case object NullKernelType extends KernelType("NullKernel", true)

  /** Identifies a kernel that does nothing, introduced to ensure the kernel DAG has kernels as roots (not fields). */
  case object UnrestorableKernelType extends KernelType("UnrestorableKernel", true)

  /** The CPU-based scalar reduce median kernel- not supported by the native runtime */
  case object CPUScalarReduceMedianKernelType extends KernelType("CPUScalarReduceMedianKernel", false)

  /** The CPU-based outer product kernel- not supported by the native runtime */
  case object CPUOuterProductKernelType extends KernelType("CPUOuterProductKernel", false)

  /** The universe of KernelTypes: Important to keep this consistent with the above!!! */
  private val codeTypes =
    Seq(DeviceKernelType, SensorKernelType, ActuatorKernelType, RecurrenceKernelType, ConstantKernelType,
      NullKernelType, UnrestorableKernelType, CPUScalarReduceMedianKernelType, CPUOuterProductKernelType)

  private val typesString = codeTypes.mkString(" or ")

  private val idToKernelType = codeTypes.map(x => (x.name, x)).toMap

  /** Factory method for creating the CogKernelCodeType from its stored id */
  def apply(name: String) = try {
    idToKernelType(name)
  } catch {
    case e: NoSuchElementException =>
      throw new RuntimeException(s"Illegal KernelType $name, expecting $typesString")
  }
}
