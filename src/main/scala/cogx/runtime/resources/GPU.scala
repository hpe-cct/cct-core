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

package cogx.runtime.resources

import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.{OpenCLAbstractKernel, OpenCLDevice}

/** A GPU is the abstraction of an OpenCL compute device with lots of cores,
  * typically called a "GPU profile."
  *
  * @param deviceIndex Index of OpenCL device (GPU) on a node's OpenCL
  *        platform (the binding to the actual device is done at runtime).
  *
  * @author Greg Snider
  */
private[runtime]
class GPU(val deviceIndex: Integer) {
  /** Circuit bound to this device, if any. */
  var circuit: KernelCircuit = null
  /** Kernels ordered so that they can be executed bottom-up. */
  var orderedKernels: Seq[OpenCLAbstractKernel] = null
}
