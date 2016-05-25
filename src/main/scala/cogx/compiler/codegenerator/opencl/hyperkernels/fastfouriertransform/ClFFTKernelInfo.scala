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

package cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform

import com.jogamp.opencl.CLKernel

import scala.collection.mutable.StringBuilder

/** Information about each kernel synthesized for the FFT.
  *
  * @author Greg Snider and Dick Carter
  */
class ClFFTKernelInfo(val kernel_name: String,
                      val dir: ClFFTKernelDir,
                      val num_workgroups: Int,
                      val num_xforms_per_workgroup: Int,
                      val num_workitems_per_workgroup: Int,
                      val in_place_possible: Boolean) {

  /** The mutable kernel source code as a StringBuilder. */
  val kernel_string = new StringBuilder(ClFFTDefines.string)
  /** Getter method for the OpenCL kernel. */
  def kernel = _kernel
  /** Setter method of the OpenCL kernel- callable once. */
  def kernel_=(newKernel: CLKernel): Unit = {
    require(_kernel == null, "Setting FFT kernel twice not allowed!")
    _kernel = newKernel
  }
  /** Set the minimum required amount of local memory for the kernel. */
  def setMinLMemSize(minSize: Int): Unit = {
    if (minSize > _lmem_size)
      _lmem_size = minSize
  }
  /** The local memory size requirement of the kernel. */
  def lmem_size = _lmem_size

  private var _lmem_size = 0
  private var _kernel: CLKernel = null
}
