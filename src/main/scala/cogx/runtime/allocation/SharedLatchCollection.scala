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

import cogx.platform.types.FieldType

import scala.collection.mutable.ArrayBuffer

/** Interface for a number of collections that manage shared latches and suggest a number of shared
  * latches for a given VirtualFieldRegister based on its FieldType.
  *
  * @author Dick Carter
  */
trait SharedLatchCollection {

  /** For a given FieldType, what are the latches that might be suitable for a given new use of the given fieldType. */
  def get(fieldType: FieldType): IndexedSeq[SharedLatch]

  /** Add a newly created SharedLatch to the collection. */
  def addLatch(fieldType: FieldType, newSharedLatch: SharedLatch): Unit

  /** A way to traverse the collection of SharedLatches, typically ordered by some measure
    * (e.g. FieldType, field size in bytes, etc.).
    */
  def values: IndexedSeq[SharedLatch]
}
