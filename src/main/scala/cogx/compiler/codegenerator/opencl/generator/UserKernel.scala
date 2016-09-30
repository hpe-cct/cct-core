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

package cogx.compiler.codegenerator.opencl.generator

import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.compiler.parser.op.UserGPUOpcode
import cogx.platform.types.{FieldType, VirtualFieldRegister}

/** Transforms a GPUOperator to a HyperKernel.
  *
  * @author Greg Snider
  */
object UserKernel {
  /** Debugging flag. */
  private val Debug = false

  /** Create a multi-output hyperkernel from user-generated kernel code.
    *
    * @param opcode The opcode for the user's GPU operator.
    * @param inputs The fields driving the operator / kernel.
    * @param resultTypes The types of fields created by the operator / kernel.
    * @return HyperKernel implementing the opcode.
    */
  def apply(opcode: UserGPUOpcode,
            inputs: Array[VirtualFieldRegister],
            resultTypes: Array[FieldType]): HyperKernel =
  {
    // Figure out global threads.
    // If globalThreads has not been set up by the user, the default
    // workField type is the type of the first output
    val workType: FieldType = opcode.globalThreads match {
      case Some(fieldType) => fieldType
      case None => resultTypes(0)
    }

    val addressMode = opcode.addressing
    val code = opcode.code

    // Figure out local threads.
    opcode.localThreads match {
      case Some(workGroupShape) =>
        val workGroupRows = workGroupShape(0)
        val workGroupColumns = workGroupShape(1)
        new HyperKernel(opcode, inputs, resultTypes, addressMode) {
          override lazy val workFieldType = workType
          override lazy val workGroup =
            HyperKernel.computeWorkGroupParameters(workType, addressMode,
              workGroupRows, workGroupColumns)
          if (Debug)
            workGroup.print()
          addCode(code)
        }
      case None =>
        new HyperKernel(opcode, inputs, resultTypes, addressMode) {
          override lazy val workFieldType = workType
          addCode(code)
        }
    }
  }

  /** Create a single-output hyperkernel from user-generated kernel code.
    *
    * @param opcode The opcode for the user's GPU operator.
    * @param inputs The fields driving the operator / kernel.
    * @param resultType The type of field created by the operator / kernel.
    * @return HyperKernel implementing the opcode.
    */
  def apply(opcode: UserGPUOpcode,
            inputs: Array[VirtualFieldRegister],
            resultType: FieldType): HyperKernel = apply(opcode, inputs, Array(resultType))
}
