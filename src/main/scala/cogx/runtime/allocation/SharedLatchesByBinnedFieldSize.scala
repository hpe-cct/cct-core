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

/** A database of SharedLatches grouped into buckets, where the nth bucket is for shared latches whose
  * size K satisfies 2^(N-1) < K < 2^N.  A given shared latch can have VirtualFieldRegister uses of different
  * sizes and the size of the shared latch for the purposes of bucket assignment is the largest such use.
  *
  * @author Dick Carter
  */
class SharedLatchesByBinnedFieldSize() extends SharedLatchCollection {
  /** The shared latches of the kernel circuit, grouped according to Field size in bytes */
  private object latches {
    val buckets = new ArrayBuffer[ArrayBuffer[SharedLatch]]
    def apply(i: Int) = {
      while (buckets.size <= i)
        buckets += new ArrayBuffer[SharedLatch]
      buckets(i)
    }
  }

  /** Latches for ColorMemories, backed by OpenCL Image Memories, which should not be mixed with Buffer Memories. */
  private val imageLatches = new SharedLatchesByFieldType

  private def fieldSizeBytes(fieldType: FieldType) = new FieldMemoryLayoutImpl(fieldType).longBufferSizeBytes

  private def floorLog2(x: Long): Int = if (x <= 1) 0 else 1 + floorLog2(x / 2)

  private def ceilLog2(x: Long): Int = {
    val floor = floorLog2(x)
    if (x == (1 << floor))
      floor
    else
      floor + 1
  }

  private def bucketIndex(size: Long) = ceilLog2(size)


  /** For a given FieldType, what are the latches that might be suitable for a given new use of the given fieldType. */
  def get(fieldType: FieldType): IndexedSeq[SharedLatch] = {
    if (fieldType.isImage)
      imageLatches.get(fieldType)
    else {
      val bucket = bucketIndex(fieldSizeBytes(fieldType))
      latches(bucket).toIndexedSeq
    }
  }

  /** Add a newly created SharedLatch to the collection. */
  def addLatch(fieldType: FieldType, newSharedLatch: SharedLatch): Unit = {
    if (fieldType.isImage)
      imageLatches.addLatch(fieldType, newSharedLatch)
    else {
      val bucket = bucketIndex(fieldSizeBytes(fieldType))
      latches(bucket) += newSharedLatch
    }
  }

  /** A way to traverse the collection of SharedLatches, typically ordered by some measure
    * (e.g. FieldType, field size in bytes, etc.).
    */
  def values: IndexedSeq[SharedLatch] =
    latches.buckets.toIndexedSeq.flatten ++ imageLatches.values
}
