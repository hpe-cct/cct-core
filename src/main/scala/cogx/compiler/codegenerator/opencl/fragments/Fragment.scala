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

package cogx.compiler.codegenerator.opencl.fragments

import cogx.cogmath.hypergraph.acyclic.DAGNode


/** A fragment of OpenCL kernel code.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class Fragment extends DAGNode[Fragment] {
  /** The system-defined (as opposed to user-defined) name for this fragment. */
  def name: String

  /** OpenCL code associated with this fragment. */
  def code: String

  /** Read a value (e.g. float, float4) from a fragment. */
  def read(addressing: AddressingMode): String

  /** Read a complete tensor (e.g. float, float4) from a fragment.  New, addressMode-less API.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensor(isLocal: Boolean): String

  /** Read a component element from a tensor (e.g. float) from a fragment.  New, addressMode-less API.
    * @param isLocal Does the read use the default row, column, layer values for the thread?
    */
  def _readTensorElement(isLocal: Boolean): String

  /** Type of the value produced by this fragment. */
  def clType: CLType
}

