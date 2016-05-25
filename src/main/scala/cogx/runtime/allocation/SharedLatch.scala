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
import cogx.platform.types.VirtualFieldRegister

import scala.collection.mutable.ArrayBuffer

import AllocateFieldRegisters._

/**
 * Created by Dick Carter on 10/15/14.
 */
/** Holds the single shared FieldRegister for a set of kernels
  *
  * @param device The device on which the OpenCL buffer will be allocated
  * @param firstVirtualRegister The kernel that prompted creation of this latch
  * @param seal After adding this kernel, prevent further kernel additions.
  *
  * @author Dick Carter
  */
class SharedLatch(device: OpenCLDevice, firstVirtualRegister: VirtualFieldRegister, seal: Boolean) {
  /** The virtual registers sharing this latch */
  val virtualRegisters = ArrayBuffer[VirtualFieldRegister](firstVirtualRegister)
  /** Are more virtual registers prohibited from sharing this latch?  */
  var isSealed = seal
  /** The last virtual register that was assigned to use this latch */
  def lastSharingVirtualRegister = virtualRegisters.last
  /** The kernels that must complete before reallocation to another virtual register */
  def lastConsumers = lastSharingVirtualRegister.sinks
  /** Add a kernel to the list of kernels that share this latch */
  def addVirtualRegister(virtualRegister: VirtualFieldRegister, seal: Boolean) {
    virtualRegisters += virtualRegister
    isSealed ||= seal
  }

  /** The single FieldRegister shared by this set of kernels */
  lazy val register = allocateFieldLatch(device, firstVirtualRegister)

  override def toString = "latch with contents: " + virtualRegisters.mkString(", ")
}
