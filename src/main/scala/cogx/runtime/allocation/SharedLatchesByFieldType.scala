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

/** A database of SharedLatches grouped into ArrayBuffers of same-fieldType shared latches.
  *
  * @author Dick Carter
  */
class SharedLatchesByFieldType() extends SharedLatchCollection {
  /** The shared latches of the kernel circuit, grouped according to FieldType */
  private val latches = new collection.mutable.HashMap[FieldType, ArrayBuffer[SharedLatch]]()

  /** For a given FieldType, what are the latches that might be suitable for a given new use of the given fieldType. */
  def get(fieldType: FieldType): IndexedSeq[SharedLatch] = {
    val ret = latches.get(fieldType) match {
      case Some(latchArray) => latchArray
      case None => ArrayBuffer[SharedLatch]()
    }
    ret.toIndexedSeq
  }

  /** Add a newly created SharedLatch to the collection. */
  def addLatch(fieldType: FieldType, newSharedLatch: SharedLatch): Unit = {
    latches.get(fieldType) match {
      case Some(latchArray) => latchArray += newSharedLatch
      case None => latches.put(fieldType, ArrayBuffer(newSharedLatch))
    }
  }

  /** A way to traverse the collection of SharedLatches, but grouped according to similarity by some measure
    * (e.g. FieldType, field size in bytes, etc.).
    */
  def values: IndexedSeq[SharedLatch] = latches.values.toIndexedSeq.flatten
}
