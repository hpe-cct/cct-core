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
import cogx.platform.opencl.KernelSourceCode
import cogx.platform.types.VirtualFieldRegister

import scala.collection.mutable

/** The KernelCircuit is a "web of objects" that must be saved in a particular order that requires some
  * global state.  These collections are mixed into the ObjectSaver to permit this.
  *
  * @author Dick Carter
  */
trait ComputeGraphSaverState {
  /** A mapping between the KernelSourceCode (whose 'equals' is based on its MD5 hash) and an integer id */
  val kernelCodeToIndex = mutable.HashMap[KernelSourceCode, Int]()
  /** A map of VirtualFieldRegister to an integer id. */
  val vfrToIndex = new IdentityHashMap[VirtualFieldRegister, Int]
  /** The compute graph being saved (gives access to FieldReaders through the only allowed path). */
  var computeGraph: ComputeGraph = null
}
