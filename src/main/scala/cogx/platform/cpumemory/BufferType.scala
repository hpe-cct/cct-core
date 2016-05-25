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

/** Types of Java Buffers used to hold AbstractFieldMemory data.
  *
  * @author Greg Snider
  */
private[cogx]
sealed abstract class BufferType

/** A buffer allocated on the heap; easy for the garbage collector to find
  * and dispose.
  */
private[cogx]
case object IndirectBuffer extends BufferType

/** A buffer allocated in direct memory, off the heap. The garbage collector
  * has great difficulty getting rid of these and they must usually be
  * programmatically released by the application.
  */
private[cogx]
case object DirectBuffer extends BufferType

/** A direct buffer that's also "pinned" to physical memory. This shares all
  * the downsides of DirectBuffers, and also uses a scarce resource. Use with
  * care.
  */
private[cogx]
case object PinnedDirectBuffer extends BufferType