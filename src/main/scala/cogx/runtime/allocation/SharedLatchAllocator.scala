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

import cogx.cogmath.collection.IdentityHashMap
import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.OpenCLDevice
import cogx.platform.types.VirtualFieldRegister

/**
* Created by carteric on 10/15/14.
*/
trait SharedLatchAllocator {
  /**
   *
   * @param kernelCircuit The hypercircuit of kernels that output the virtual registers to allocate latches for.
   * @param device The device being scheduled for (so the latches can allocate their OpenCL buffers eventually
   * @param requiresLatch A function telling this routine which latches can share latches
   * @return  A map from each virtual register of the circuit to its shared latch
   */
  def calcSharedLatches(kernelCircuit: KernelCircuit,
                        device: OpenCLDevice,
                        requiresLatch: (VirtualFieldRegister) => Boolean): IdentityHashMap[VirtualFieldRegister, SharedLatch]
}
