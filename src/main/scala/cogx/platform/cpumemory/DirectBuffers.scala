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

import java.nio.ByteBuffer
import com.jogamp.opencl.llb.CLMemObjBinding._
import com.jogamp.opencl.{CLMemory, CLCommandQueue}

/** A factory/cache for creating CPU memory that has a fixed virtual address
  * (and is potentially unpageable) used for holding field data on the CPU side
  * of an OpenCL buffer. All buffer allocation should be done by calling this
  * object.
  *
  * The issue of buffering field data on the CPU and GPU side and copying it
  * back-and-forth is not simple. The issues are:
  *
  * 1. What is the most efficient way of allocating the CPU memory?
  *
  * 2. What is the most efficient way of transferring memory between the CPU
  * and GPU?
  *
  * First, for obvious reasons, the GPU drivers require that the CPU buffer has
  * a fixed virtual address. This means JVM heap memory cannot be used since
  * the garbage collector can run at any time and change the address of any
  * memory object while compacting. The only way to get a fixed virtual address
  * is to a allocate a Java Buffer (or one of its subclasses: ByteBuffer,
  * FloatBuffer, ...) in direct mode. This means that the Buffer is not
  * allocated on the heap, but outside of it, making garbage collection tricky.
  * So, a direct buffer is required for holding field data on the CPU side.
  *
  * Second, the GPU drivers are much more efficient if the direct buffer is
  * "pinned" or non-pageable. It appears that if it isn't pinned, the GPU
  * driver will simply allocate a pinned buffer and copy the unpinned direct
  * buffer into it.
  *
  * Java does not support the "pinned" concept directly, nor does OpenCL. But
  * the "Nvidia OpenCL Best Practices Guide" rev 1.0 suggests that the
  * following buffer allocation functions are likely to allocate a pinned,
  * direct buffer:
  *
  *   clCreateBuffer(clContext, CL_MEM_READ_ONLY | CL_MEM_ALLOC_HOST_PTR,
  *   memSize, NULL, NULL)
  *
  *   clCreateBuffer(clContext, CL_MEM_WRITE_ONLY | CL_MEM_ALLOC_HOST_PTR,
  *   memSize, NULL, NULL)
  *
  * The CL_MEM_READ_ONLY flag means that the GPU kernel can only read it, so
  * it is write-only by the CPU. The CL_MEM_ALLOC_HOST_PTR flag forces the
  * allocation of the buffer, which will certainly be direct and probably be
  * pinned.
  *
  * After that, the command queue must be queried to map this to a ByteBuffer:
  *
  *   // For CPU --> GPU
  *
  *   commandQueue.putMapBuffer(clBuffer, CLMemory.Map.WRITE, true)
  *
  *   // For CPU <-- GPU
  *
  *   commandQueue.putMapBuffer(clBuffer, CLMemory.Map.READ, true)
  *
  *
  *
  * @author Greg Snider
  */
private[cogx]
object DirectBuffers {
  /** Maximum size of a pinned buffer (OpenCL limitation). */
  private val MaxPinnedBytes = 0x7fffffff
  /** Flags to allocate a pinned buffer written by CPU. */
  private val ToGPUPinnedBuffer =
    CL_MEM_ALLOC_HOST_PTR | CL_MEM_READ_ONLY
  /** Flags to allocate a pinned buffer read by CPU. */
  private val FromGPUPinnedBuffer =
    CL_MEM_ALLOC_HOST_PTR | CL_MEM_WRITE_ONLY


  /** Allocate a direct buffer for communication with the GPU.
    *
    * This will allocate a pinned, direct buffer if small enough (< 2GB, a
    * limitation of OpenCL), otherwise an unpinned, direct buffer.
    *
    * @param commandQueue CommandQueue that will use
    * @param bytes Number of bytes in the buffer
    * @param toGPU True if buffer is to be used for CPU --> GPU transfers,
    *        false if to be used for CPU <-- GPU transfers.
    * @return A direct byte buffer, possibly pinned.
    */

  def allocate(commandQueue: CLCommandQueue,
               bytes: Long, toGPU: Boolean): ByteBuffer =
  {
    if (bytes <= MaxPinnedBytes) {
      val context = commandQueue.getContext
      commandQueue.synchronized  {
        if (toGPU) {
          // Create a "half" buffer with CPU memory but no GPU memory.
          val clBuffer = context.createBuffer(bytes.toInt, ToGPUPinnedBuffer)
          // Get the byte buffer.
          commandQueue.putMapBuffer(clBuffer, CLMemory.Map.WRITE, true)
        } else {
          // Create a "half" buffer with CPU memory but no GPU memory.
          val clBuffer = context.createBuffer(bytes.toInt, FromGPUPinnedBuffer)
          // Get the byte buffer.
          commandQueue.putMapBuffer(clBuffer, CLMemory.Map.READ, true)
        }
      }
    } else {
      throw new RuntimeException("Direct buffers larger than 2GB not yet supported")
    }
  }
}