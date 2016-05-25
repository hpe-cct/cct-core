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

import cogx.platform.cpumemory.AbstractFieldMemory

/** This package defines abstract "field buffers" which are used across all
  * platforms (CUDA and OpenCL)
  *
  * A field buffer consists of both a CPU part (AbstractFieldMemory) and a
  * platform dependent GPU part. Only the CPU part is visible here, since the
  * CPU part is platform dependent. This could be optimized to reduce
  * CPU memory utilization, for example by allocation only GPU memory if the CPU
  * memory is not used, but we allocate both for simplicity.
  *
  * A CPU kernel reads a field buffer by calling `read`; the returned field
  * memory is a copy of the field data on the GPU. A CPU kernel writes a
  * field buffer by grabbing the field memory, cpuMemory, writing it directly,
  * then calling `write`; this synchronously copies the field data to the GPU.
  *
  * @author Greg Snider
  */
private[cogx]
trait AbstractFieldBuffer[T <: AbstractFieldMemory] {

  /** The type of field held by this buffer. */
  val fieldType: FieldType

  /** The CPU memory buffer holding the field data. */
  def cpuMemory: T

  /** Read the field, copying from the GPU if necessary. */
  def read: T

  /** Write the field, copying to the GPU if necessary. */
  def write(): Unit

  /** Name of a kernel, for debugging. */
  private var _name = ""

  /** Get the name of the kernel. */
  def name = _name

  /** Assign a name to the kernel. */
  def name_=(name: String) {_name = name}

  /** Call to mark cpuMemory stale due to a kernel computation. */
  def invalidateCpuMemory: Unit
}