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

package cogx.compiler.codegenerator

import cogx.platform.types.AbstractKernel
import cogx.cogmath.hypercircuit.Hypercircuit

/** Manages a Circuit (directed, acylic graph) of OpenCL kernels so that data is
  * exchanged properly between host and device, and the kernels get executed
  * in an order that respects their dependencies. Execution is out-of-order,
  * allowing maximum parallelism in the GPU.
  *
  * Here's a model of a simple Circuit. Input comes from the host via
  * the CPU Kernels at the bottom. Computation is done on the OpenCL device
  * with the DeviceKernels. The result is returned to the host with the
  * CPUKernel at the top. Note that Device Kernels and CPU Kernels can be
  * intermixed in the computation.
  *
  * The kernels are represented internally as 'hypernodes', which support
  * multiple outputs.  The kernel-to-kernel communication paths shown as
  * arcs below are represented internally as 'hyperedges'.
  * {{{
  *                    ^
  *                    |
  *                +--------+
  *                |  CPU   |
  *                | Kernel |
  *                +--------+
  *                    ^  ^
  *                    |  |
  *                    |  +-----------+
  *                    |              |
  *                +---+----+     +---+----+
  *                | Device |     | Device |
  *                | Kernel |     | Kernel |
  *                +--------+     +--------+
  *                  ^    ^           ^
  *                  |    |           |
  *               +--+    +--+  +-----+
  *               |          |  |
  *          +----+---+   +--+--+--+
  *          | Device |   |  CPU   |
  *          | Kernel |   | Kernel |
  *          +--------+   +--------+
  *            ^   ^             ^
  *            |   |             |
  *            |   +---------+   |
  *            |             |   |
  *       +----+---+       +-+---+--+
  *       |  CPU   |       |  CPU   |
  *       | Kernel |       | Kernel |
  *       +--------+       +--------+
  *
  * }}}
  *
  * @author Greg Snider
  */
private[cogx]
class KernelCircuit extends Hypercircuit[AbstractKernel]