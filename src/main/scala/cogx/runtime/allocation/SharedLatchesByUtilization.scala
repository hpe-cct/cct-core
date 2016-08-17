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

/** A database of SharedLatches managed based on the latch "utilization".  For any given VirtualFieldRegister use,
  * the utilization is the ratio of the VFR_size / latch_size.  When candidate latches are identified for a given
  * FieldType, the utilization of the latch is guaranteed not to fall below a `minUtilization.`
  *
  * @param minUtilization Threshold below which no use of a SharedLatch can fall, relative to the maximum use.
  *
  * @author Dick Carter
  */
class SharedLatchesByUtilization(minUtilization: Float = 0.25f) extends SharedLatchCollection {
  /** The shared latches of the kernel circuit, grouped according to Field size in bytes */
  private val latches = ArrayBuffer[SharedLatch]()

  /** Latches for ColorMemories, backed by OpenCL Image Memories, which should not be mixed with Buffer Memories. */
  private val imageLatches = new SharedLatchesByFieldType

  private def fieldSizeBytes(fieldType: FieldType) = new FieldMemoryLayoutImpl(fieldType).longBufferSizeBytes

  /** For a given FieldType, what are the latches that might be suitable for a given new use of the given fieldType. */
  def get(fieldType: FieldType): IndexedSeq[SharedLatch] = {
    if (fieldType.isImage)
      imageLatches.get(fieldType)
    else {
      val newUseSize = fieldSizeBytes(fieldType)
      // We don't want any use of the buffer to occupy less than half the buffer
      val feasible = latches.filter((latch) => {
        newUseSize * minUtilization <= latch.minGlobalMemoryUseBytes && newUseSize >= latch.maxGlobalMemoryUseBytes * minUtilization
      })
      val canHoldNewUse = feasible.filter(newUseSize <= _.maxGlobalMemoryUseBytes)
      val wouldGetBigger = feasible.filter(newUseSize > _.maxGlobalMemoryUseBytes)
      // For the latches that can hold this use, sort from smallest (best) to largest (worst)
      val orderedCanHoldNewUse = canHoldNewUse.sortWith(_.maxGlobalMemoryUseBytes < _.maxGlobalMemoryUseBytes)
      // For the latches that would grow based on this use, sort from biggest (best) to smallest (worst)
      val orderedWouldGetBigger = wouldGetBigger.sortWith(_.maxGlobalMemoryUseBytes > _.maxGlobalMemoryUseBytes)

      val orderedList = (orderedCanHoldNewUse ++ orderedWouldGetBigger).toIndexedSeq
      orderedList
    }
  }

  /** Add a newly created SharedLatch to the collection. */
  def addLatch(fieldType: FieldType, newSharedLatch: SharedLatch): Unit = {
    if (fieldType.isImage)
      imageLatches.addLatch(fieldType, newSharedLatch)
    else
      latches += newSharedLatch
  }

  /** A way to traverse the collection of SharedLatches, typically ordered by some measure
    * (e.g. FieldType, field size in bytes, etc.).
    */
  def values: IndexedSeq[SharedLatch] =
    latches.toIndexedSeq ++ imageLatches.values
}
