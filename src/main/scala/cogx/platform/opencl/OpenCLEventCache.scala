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

package cogx.platform.opencl

import com.jogamp.common.nio.CachedBufferFactory

/** Factory for efficiently allocating events needed for OpenCL synchronization.
  *
  * Each event needs to be in a direct buffer that's page aligned, so that means
  * 4K bytes / event is probably minimal. Direct buffers are also expensive to
  * create, so caching them should help performance.
  *
  * To use this, pass the CLEventFactory as the first parameter in all
  *
  *    `new CLEventList(CLEventFactory, ...)`
  *
  * calls.
  *
  * @author Greg Snider
  */

private [cogx] object OpenCLEventCache {
  /** Factory for create CLEvents. */
  val CLEventFactory = CachedBufferFactory.createSynchronized
}