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

package cogx.platform.cpumemory

import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.platform.opencl.OpenCLParallelCommandQueue
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** A class for producing a field memory from a field type.
  *
  * @author Greg Snider
  */

class FieldMemory private {

  private val indirectPool =
    new mutable.HashMap[FieldType, ArrayBuffer[AbstractFieldMemory]]
  private val directPool =
    new mutable.HashMap[FieldType, ArrayBuffer[AbstractFieldMemory]]
  private val pinnedPool =
    new mutable.HashMap[FieldType, ArrayBuffer[AbstractFieldMemory]]

  /** Create a field memory backed by an indirect buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  private[cogx]
  def indirect(fieldType: FieldType) = allocate(fieldType, IndirectBuffer)

  /** Create a field memory backed by an direct buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  def direct(fieldType: FieldType) = allocate(fieldType, DirectBuffer)

  /** Create a field memory backed by a pinned, direct buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  private[cogx]
  def pinned(fieldType: FieldType, commandQueue: OpenCLParallelCommandQueue) =
  {
    allocate(fieldType, PinnedDirectBuffer, commandQueue)
  }

  /** Allocate an AbstractFieldMemory from the pool.
    *
    * @param fieldType The type of the desired memory.
    * @param bufferType Type of buffer to use for holding the data in the
    *        field memory.
    * @return Allocated memory.
    */
  private def allocate(fieldType: FieldType,
                       bufferType: BufferType, commandQueue: OpenCLParallelCommandQueue = null): AbstractFieldMemory =
  {
    var memory: AbstractFieldMemory = null
    val pool = bufferType match {
      case IndirectBuffer => indirectPool
      case DirectBuffer => directPool
      case PinnedDirectBuffer => pinnedPool
    }
    synchronized {
      if (!pool.contains(fieldType))
        pool(fieldType) = new ArrayBuffer[AbstractFieldMemory]
      val bucket = pool(fieldType)
      if (bucket.size == 0) {
        val memory =     fieldType.elementType match {
          case Uint8Pixel =>
            new ColorFieldMemory(fieldType, bufferType, commandQueue)
          case Complex32 =>
            fieldType.tensorShape.dimensions match {
              case 0 =>
                new ComplexFieldMemory(fieldType, bufferType, commandQueue)
              case 1 =>
                new ComplexVectorFieldMemory(fieldType, bufferType, commandQueue)
              case x =>
                throw new RuntimeException("unsupported tensor dimensions: " + x)
            }
          case Float32 =>
            fieldType.tensorShape.dimensions match {
              case 0 =>
                new ScalarFieldMemory(fieldType, bufferType, commandQueue)
              case 1 =>
                new VectorFieldMemory(fieldType, bufferType, commandQueue)
              case 2 =>
                new MatrixFieldMemory(fieldType, bufferType, commandQueue)
              case x =>
                throw new RuntimeException("unsupported tensor dimensions: " + x)
            }
          case x =>
            throw new RuntimeException("unsupported field element type: " + x)
        }
        bucket += memory
      }
      memory = bucket.remove(0)
    }
    memory
  }

  /** Release a field memory back to the pool.
    *
    * @param memory The field memory to be released.
    */
  def release(memory: AbstractFieldMemory) {
    val pool = memory.bufferType match {
      case IndirectBuffer => indirectPool
      case DirectBuffer => directPool
      case PinnedDirectBuffer => pinnedPool
    }
    synchronized {
      require(pool.contains(memory.fieldType),
        "released field memory was not created in the pool")
      val bucket: ArrayBuffer[AbstractFieldMemory] = pool(memory.fieldType)
      bucket += memory
    }
  }

  /** Create a copy of `memory` */
  def copy(memory: AbstractFieldMemory) = {
    val newMemory = allocate(memory.fieldType, DirectBuffer)
    memory.copyTo(newMemory)
    newMemory
  }

  /** Destroy all field memories in the cache.
    *
    * This should only be done when you're sure there are no useful
    * references remaining to any AbstractFieldMemory. Typically this is
    * done at the end of execution of a compute graph.
    */
  private[cogx]
  def destroyAll() {
    val pools = Array(indirectPool, directPool, pinnedPool)
    for (pool <- pools) {
      for (arrayBuffer <- pool.values)
        for (memory <- arrayBuffer)
          memory.destroyDirectBuffer()
      pool.clear()
    }
  }
}

/** A factory for producing field memory allocators.
  *
  * Currently each ComputeGraph gets its own allocator.  Some uses within the Cogdebugger didn't
  * have access to the ComputeGraph's allocator and so use a global one.
  *
  * @author Greg Snider
  */
object FieldMemory {
  lazy val globalAllocator = FieldMemory()

  def apply(): FieldMemory = new FieldMemory()

  /** Create a field memory backed by an indirect buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  private[cogx]
  def indirect(fieldType: FieldType) = globalAllocator.indirect(fieldType)

  /** Create a field memory backed by an direct buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  def direct(fieldType: FieldType) = globalAllocator.direct(fieldType)

  /** Create a field memory backed by a pinned, direct buffer.
    *
    * @param fieldType The type of the desired field memory.
    * @return The allocated field memory, which may be upcast to the appropriate
    *         class based on fieldType.
    */
  private[cogx]
  def pinned(fieldType: FieldType, commandQueue: OpenCLParallelCommandQueue) = globalAllocator.pinned(fieldType, commandQueue)

  /** Release a field memory back to the pool.
    *
    * @param memory The field memory to be released.
    */
  def release(memory: AbstractFieldMemory) = globalAllocator.release(memory)

  /** Create a copy of `memory` */
  def copy(memory: AbstractFieldMemory) = globalAllocator.copy(memory)

  /** Destroy all field memories in the cache.
    *
    * This should only be done when you're sure there are no useful
    * references remaining to any AbstractFieldMemory. Typically this is
    * done at the end of execution of a compute graph.
    */
  private[cogx]
  def destroyAll() = globalAllocator.destroyAll()
}