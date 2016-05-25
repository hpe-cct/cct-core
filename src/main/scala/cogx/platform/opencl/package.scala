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

package cogx.platform

/** This packages wraps the OpenCL primitive classes with new classes that
  * simplify their usage. This was done for several reasons:
  *
  * 1. The OpenCL API is large and complicated; wrapping them simplifies this
  * by eliminating features that aren't needed for Cog.
  *
  * 2. Nvidia's implementation of OpenCL performs poorly when OpenCL contexts
  * are shared by more than one device. Better performance results when one
  * context and one command queue are assigned to a device, and scheduling
  * (and synchronization) is done externally. Thus we can simplify the
  * interface by burying contexts and
  *
  * 3. It provides a centralized scheme for resource allocation and release,
  * necessary for the Cog environment where the node on which a kernel
  * executes can be dynamically changed at runtime.
  *
  * A computation is executed using a combination of OpenCL device kernels
  * and "CPU kernels" (analogous to "native kernels" in the OpenCL spec, which
  * must be written in C or C++). Synchronization of all kernels is done
  * using OpenCL 1.1 CLEvents, so CPU kernels are platform dependent.
  *
  * @author Greg Snider
  */
package object opencl
