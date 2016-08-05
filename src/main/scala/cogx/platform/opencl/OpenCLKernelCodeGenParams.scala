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

/** A bundle of platform parameters that affect kernel code generation and optimization.
  *
  * @author Dick Carter
  *
  * @param maxMemAllocSize The maximum amount of memory permitted that can be allocated as one Buffer on the device.
  * @param maxConstantBufferSize The maximum amount of constant memory permitted on the device.
  * @param localMemSize The maximum amount of local memory (workgroup-shared memory) permitted on the device.
  * @param warpSize The number of threads that execute in lock-step without need for memory synchronization.
  */
case class OpenCLKernelCodeGenParams(maxMemAllocSize: Long, maxConstantBufferSize: Long, localMemSize: Long, warpSize: Int)
