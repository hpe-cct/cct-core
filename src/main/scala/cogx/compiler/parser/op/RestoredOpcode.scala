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

package cogx.compiler.parser.op

import cogx.platform.types.Opcode

/** Rather than save all the Opcodes to the ComputeGraphFile, and provide
  * a restore mechanism, we simply create the opcode for a restored kernel
  * with the name AddKernel_24 (say) as RestoredOpcode("AddKernel_24").  All
  * kernels will compare as not equal (since the names are unique), but this
  * is OK because the runtime won't be performing any kernel DAG optimization.
  *
  * @author Dick Carter
  */
case class RestoredOpcode(kernelName: String) extends Opcode(kernelName)
