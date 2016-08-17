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

package cogx.runtime.allocation

import cogx.platform.opencl.OpenCLDevice
import cogx.platform.types.{FieldMemoryLayoutImpl, VirtualFieldRegister}

import scala.collection.mutable.ArrayBuffer
import AllocateFieldRegisters._
import cogx.platform.cpumemory.BufferType

/** Holds the single shared FieldRegister for a set of kernels
  *
  * @param device The device on which the OpenCL buffer will be allocated
  * @param firstVirtualRegister The kernel that prompted creation of this latch
  * @param seal After adding this kernel, prevent further kernel additions.
  * @param bufferType The type of cpu memory (pinned versus pageable) to be allocated for the buffers.
  *
  * @author Dick Carter
  */
class SharedLatch(device: OpenCLDevice, firstVirtualRegister: VirtualFieldRegister, seal: Boolean, bufferType: BufferType) {
  /** The virtual registers sharing this latch */
  val virtualRegisters = ArrayBuffer[VirtualFieldRegister]()
  /** Are more virtual registers prohibited from sharing this latch?  */
  def isSealed = _isSealed
  /** The last virtual register that was assigned to use this latch */
  def lastSharingVirtualRegister = virtualRegisters.last
  /** The kernels that must complete before reallocation to another virtual register */
  def lastConsumers = lastSharingVirtualRegister.sinks
  /** Add a kernel to the list of kernels that share this latch */
  def addVirtualRegister(virtualRegister: VirtualFieldRegister, seal: Boolean) {
    virtualRegisters += virtualRegister
    val newUseBytes = new FieldMemoryLayoutImpl(virtualRegister.fieldType).longBufferSizeBytes
    _isSealed ||= seal
    _maxGlobalMemoryUseBytes = math.max(_maxGlobalMemoryUseBytes,
      new FieldMemoryLayoutImpl(virtualRegister.fieldType).longBufferSizeBytes)
    _minGlobalMemoryUseBytes =
      if (virtualRegisters.size == 1)
        newUseBytes
      else
        math.min(_minGlobalMemoryUseBytes, newUseBytes)
  }

  // Are more virtual registers prohibited from sharing this latch?
  private var _isSealed = false

  // Of the VirtualFieldRegister uses of this shared latch, what is the biggest size?
  private var _maxGlobalMemoryUseBytes: Long = 0L
  // Of the VirtualFieldRegister uses of this shared latch, what is the smallest size?
  private var _minGlobalMemoryUseBytes: Long = 0L

  /** Of the VirtualFieldRegister uses of this shared latch, what is the biggest size? */
  def maxGlobalMemoryUseBytes = _maxGlobalMemoryUseBytes
  /** Of the VirtualFieldRegister uses of this shared latch, what is the smallest size? */
  def minGlobalMemoryUseBytes = _minGlobalMemoryUseBytes

  addVirtualRegister(firstVirtualRegister, seal)

  /** The single FieldRegister shared by this set of kernels */
  lazy val register = allocateFieldLatch(device, lastSharingVirtualRegister.fieldType, bufferType, _maxGlobalMemoryUseBytes)

  override def toString = "latch with contents: " + virtualRegisters.mkString(", ")
}
