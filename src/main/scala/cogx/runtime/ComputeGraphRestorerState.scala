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

package cogx.runtime

import cogx.cogmath.collection.IdentityHashMap
import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.parser.syntaxtree.SyntaxTree
import cogx.platform.opencl.KernelSourceCode
import cogx.platform.types.{AbstractKernel, VirtualFieldRegister, VirtualFieldRegisterInfo}

import scala.collection.mutable.ArrayBuffer

/** The KernelCircuit is a "web of objects" that must be restored in a particular order that requires some
  * global state.  These collections are mixed into the ObjectRestorer to permit this.
  *
 * @author Dick Carter
 */
trait ComputeGraphRestorerState {
  /** The unique kernel codes (perhaps fewer than the number of kernels). */
  val kernelCodes = new ArrayBuffer[KernelSourceCode]()
  /** The fieldType, probed state and name of the virtual field registers of the KernelCircuit. */
  val fieldInfos = new ArrayBuffer[VirtualFieldRegisterInfo]()
  /** The VirtualFieldRegisters of the KernelCircuit, in the order created by a post-order kernel DAG traversal. */
  val vfrs = new ArrayBuffer[VirtualFieldRegister]()
  /** An empty HashMap from AbstractKernels (RecurrentFieldKernels actually) and the vfr index of the Recurrence */
  val recurrences = new IdentityHashMap[AbstractKernel, Int]
  /** The kernelCircuit that is created by the restore process. */
  val restoredCircuit = new KernelCircuit()
  /** A partial syntax tree with only actuators and sensors created during the restore process (Scala runtime only) */
  lazy val restoredSyntaxTree = new SyntaxTree()
}
