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

import cogx.platform.types.{FieldMemoryLayoutImpl, FieldType}

import scala.collection.mutable.ArrayBuffer

/** A database of SharedLatches grouped into ArrayBuffers of same-sized shared latches.
  *
  * @author Dick Carter
  */
class SharedLatchesByFieldSize() extends SharedLatchCollection {
  /** The shared latches of the kernel circuit, grouped according to Field size in bytes */
  private val bufferLatches = new collection.mutable.HashMap[Long, ArrayBuffer[SharedLatch]]()

  /** Latches for ColorMemories, backed by OpenCL Image Memories, which should not be mixed with Buffer Memories. */
  private val imageLatches = new SharedLatchesByFieldType

  private def fieldSizeBytes(fieldType: FieldType) = new FieldMemoryLayoutImpl(fieldType).longBufferSizeBytes

  /** For a given FieldType, what are the latches that might be suitable for a given new use of the given fieldType. */
  def get(fieldType: FieldType): IndexedSeq[SharedLatch] = {
    if (fieldType.isImage)
      imageLatches.get(fieldType)
    else {
      val ret = bufferLatches.get(fieldSizeBytes(fieldType)) match {
        case Some(latchArray) => latchArray
        case None => ArrayBuffer[SharedLatch]()
      }
      ret.toIndexedSeq
    }
  }

  /** Add a newly created SharedLatch to the collection. */
  def addLatch(fieldType: FieldType, newSharedLatch: SharedLatch): Unit = {
    if (fieldType.isImage)
      imageLatches.addLatch(fieldType, newSharedLatch)
    else {
      bufferLatches.get(fieldSizeBytes(fieldType)) match {
        case Some(latchArray) => latchArray += newSharedLatch
        case None => bufferLatches.put(fieldSizeBytes(fieldType), ArrayBuffer(newSharedLatch))
      }
    }
  }

  /** A way to traverse the collection of SharedLatches, typically ordered by some measure
    * (e.g. FieldType, field size in bytes, etc.).
    */
  def values: IndexedSeq[SharedLatch] =
    bufferLatches.values.toIndexedSeq.flatten ++ imageLatches.values
}
