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

import cogx.compiler.gpu_operator.GPUOperator
import cogx.platform.types.Opcode

import scala.collection.mutable

/** Opcode for a user-defined GPU operator.
  *
  * @param gpuOperator The GPUOperator for the opcode.
  * @param nameSuffix The user-supplied name to be attached to the opcode string for debugging.
  */
private[cogx]
class UserGPUOpcode private (val gpuOperator: GPUOperator, nameSuffix: String)
        extends Opcode(nameSuffix) {
  override def toString = "UserGPU_" + nameSuffix
}

/** Companion object which generates unique opcodes for user GPU operators. */
private[cogx]
object UserGPUOpcode {
  /** Unique id for last opcode assigned. */
  private var opcodes = 0

  /** Cache of previously generated opcodes. */
  private val opcodeCache = mutable.HashMap[GPUOperator, UserGPUOpcode]()

  /** Create a unique UserGPUOpcode for each unique user GPU operator.
    *
    * Multiple ComputeGraphs should be able to share the opcode cache, even if the
    * opcodes are being simultaneously created by multiple threads (note `synchronized` below).
    *
    * @param gpuOperator The GPUOperator to create the unique UserGPUOpcode for.
    * */
  def apply(gpuOperator: GPUOperator): UserGPUOpcode = synchronized {
    opcodeCache.getOrElseUpdate(gpuOperator, {
      // Lookup failed: make a new UserGPUOpcode incorporating user-supplied name if not ""
      opcodes += 1
      var opName = opcodes.toString
      if (gpuOperator.name != "")
        opName = gpuOperator.name + "_" + opName
      new UserGPUOpcode(gpuOperator, opName)
    })
  }
}
