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

package cogx.runtime.allocation.circuit

import java.util.concurrent.TimeUnit

import cogx.platform.types._
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import akka.util.Timeout
import cogx.compiler.parser.op._
import cogx.runtime.FieldID


/** An InputProxyKernel acts much like a Sensor or a constant field, but is
  * actually linked, under the covers, to an OutputProxyKernel on another
  * node. This and the OutputProxyKernel are used to break apart a circuit into
  * subcircuits; a signal crossing a node boundary must go out one node through
  * an OutputProxyKernel, and into the destination node(s) through an
  * InputProxyKernel.
  *
  * @param proxyFor  The original virtual field register (from the unpartitioned circuit) whose
  *                  data must be proxied (i.e. is consumed by kernels in
  *                  multiple subcircuits after partitioning)
  * @author Greg Snider
  */
private[cogx]
class InputProxyKernel(proxyFor: VirtualFieldRegister)
    extends OpenCLCpuSingleOutputKernel(InputProxyOp, Array.empty[VirtualFieldRegister], proxyFor.fieldType, needActor=true) {

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  val proxyForKernel = proxyFor.source
  val outputIdx = proxyFor.sourceOutputIndex match {
    case Some(i) => i
    case None => throw new RuntimeException("Compiler error: missing kernel output.")
  }

  /** Input and output proxy kernels save the id of the proxied node as it
    * existed in the original, unpartitioned circuit. This avoids having to
    * explicitly hook up input and output proxies to each other - the actor
    * supervisor hierarchy can find the appropriate field based on this ID. */
  val proxiedKernelId = FieldID(proxyForKernel.id, outputIdx)

  /** ID of the OutputProxyKernel repsonsible for feeding this InputProxyKernel
    * with data. */
  var sourceProxyId: Int = -1

  outputs(0).name = {
    val s = proxyFor.name match {
      case "" => proxyForKernel.id
      case x  => x
    }
    "InputProxy("+s+")"
  }

  /** Code which the user kernel must execute. */
  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister): Unit = {
    // No-op; all the work of proxying data is delegated to an actor.
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    * as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel =
    new InputProxyKernel(proxyFor)
}