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
import com.jogamp.common.nio.Buffers
import java.nio.{ByteOrder, ByteBuffer, Buffer}
import cogx.platform.opencl.OpenCLParallelCommandQueue
import com.jogamp.opencl.CLBuffer
import com.jogamp.opencl.CLMemory.{Map, Mem}
import cogx.platform.cpumemory.readerwriter.FieldReader

/** CPU container for a tensor field, including methods for reading and writing
  * it. This implemented using direct, NIO buffers for efficiency.
  *
  * This organizes the data within the buffer so that memory accesses by the GPU
  * are efficient. This also provides a uniform representation of fields that's
  * consistent on both CPU and GPU.
  *
  * See the FieldMemoryLayout class for description of the physical layout.
  *
  * @author Greg Snider
  */
abstract class AbstractFieldMemory(fieldType: FieldType, val bufferType: BufferType)
        extends FieldMemoryLayout(fieldType)
        with FieldReader
{
  /** The actual memory for the field. */
  protected var _byteBuffer: ByteBuffer = null
  protected var _directBuffer: Buffer = null

  /** The direct buffer for this memory. This is always a view of _byteBuffer,
    * but upcast to the appropriate subclass of Buffer using, for example,
    * ByteBuffer.asFloatBuffer. The view calls are somewhat dangerous because
    * they create a view with proper endianness for Java (which uses the native
    * endianness internally) but they can also change the endianness of
    * _byteBuffer. This is subtle, so read the Java documentation carefully if
    * you need to change this.
    */
  def directBuffer: Buffer

  /** Copy the data in `this` to another field memory.
    *
    * @param that Field memory to receive a copy of the data in this.
    */
  def copyTo(that: AbstractFieldMemory) {
    require(this.fieldType == that.fieldType)
    val sourceBuf = this._byteBuffer.duplicate() // makes this routine thread-safe
    that._byteBuffer.clear  // prepare for writing
    sourceBuf.rewind // prepare for reading
    that._byteBuffer.put(sourceBuf)
    that._byteBuffer.flip   // writing done, prepare for reading
  }

  /** Allocates a direct ByteBuffer with native ordering.
    *
    * @param bytes Number of bytes in the buffer
    * @return A direct byte buffer.
    */
  protected def allocateDirectByteBuffer(bytes: Int): ByteBuffer = {
    try {
      // Buffers documentation claims this always allocates native order.
      Buffers.newDirectByteBuffer(bytes)
    } catch {
      case e: com.jogamp.opencl.CLException =>
        println(e.toString)
        System.gc()
        try {
          Buffers.newDirectByteBuffer(bytes)
        } catch {
          case x: com.jogamp.opencl.CLException =>
            println(x.toString)
            System.gc()
            Buffers.newDirectByteBuffer(bytes)
        }
      case x: Throwable =>
        println(x.toString)
        throw x
    }
  }

  /** Allocates an indirect ByteBuffer with native ordering.
    *
    * @param bytes Number of bytes in the buffer
    * @return An indirect byte buffer.
    */
  protected def allocateIndirectByteBuffer(bytes: Int): ByteBuffer = {
    val buffer = ByteBuffer.allocate(bytes)
    buffer.order(ByteOrder.nativeOrder)
    buffer
  }

  /** Allocates a pinned, direct ByteBuffer with native (?) ordering.
    *
    * There is no way in OpenCL to create a pinned buffer, but the "NVIDIA
    * OpenCL Best Practices Guide" suggests that this has the highest
    * probability of success.
    *
    * @param bytes Number of bytes in the buffer
    * @param commandQueue Command queue needed for mapping allocated buffer to
    *        a direct buffer.
    * @return A pinned, direct byte buffer.
    */
  protected def allocatePinnedDirectByteBuffer(bytes: Int,
                 commandQueue: OpenCLParallelCommandQueue): ByteBuffer =
  {
    val context = commandQueue.clContext
    val buf: CLBuffer[_] = context.createBuffer(bytes, Mem.ALLOCATE_BUFFER)
    val byteBuffer = commandQueue.putMapBuffer(buf, Map.READ_WRITE)
    // Now that we have coaxed the system into creating a pinned ByteBuffer,
    // we can throw away the CLBuffer (freeing its implied GPU global memory allocation).

    // Update: the following approach causes JVM crashes on the AMDSDK 2.9.1.  No crashes
    // are seen if the buffer is not released, so it seems that the CLBuffer.release may
    // be affecting the associated ByteBuffer.  Further study is needed.
    buf.release
    byteBuffer
  }

  private var _destroyed = false
  def destroyed = _destroyed

  private[cpumemory] def destroyDirectBuffer() {
    if (_byteBuffer != null) {
      // Note: DirectBuffer memory exhaustion problems can often be solved through a JVM arg such as:
      //
      // -XX:MaxDirectMemorySize=20g

      // By removing our references to the ByteBuffers below, we can let the garbage collector
      // destroy those fairly lightweight heap objects, thereby triggering a release of
      // the underlying non-heap storage.  There might be a lurking timing issue though,
      // as an exhaustion of non-heap direct buffer memory may not trigger the needed
      // heap-memory garbage collection.  Below would be the best location to insert a
      // "direct buffer cleaner" if needed, such as:
//      DirectBufferCleaner(_byteBuffer)
      _byteBuffer = null
      _directBuffer = null
    }
    _destroyed = true
  }

  /** Print out the field for debugging. */
  def print(): Unit
}
