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

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.compiler.parser.op.UserGPUWithVariantsOpcode
import cogx.parameters.Cog
import cogx.platform.types.{FieldType, VirtualFieldRegister}
import cogx.runtime.execution.{ProfileSample, Profiler}

/** Transforms a GPUOperator with variants to a HyperKernel, using a Profiler to select the fastest.
  *
  * @author Dick Carter
  */
object UserWithVariantsKernel {
  // A knob to disable profiler-based variant selection and force a particular variant (for testing).
  private[cogx] var forceVariantSelection: Int = -1

  /** Create a multi-output hyperkernel from user-generated kernel code.
    *
    * @param _opcode The opcode for the user's GPU operator.
    * @param inputs The fields driving the operator / kernel.
    * @param resultTypes The types of fields created by the operator / kernel.
    * @param profiler The profiler to use to pick the best variant
    * @return HyperKernel implementing the opcode.
    */
  def apply(_opcode: UserGPUWithVariantsOpcode,
            inputs: Array[VirtualFieldRegister],
            resultTypes: Array[FieldType],
            profiler: Profiler): HyperKernel =
  {
    // Select a test-framework-specified variant, or the fastest as timed by the Profiler.
    val opcodeIndex =
      if (forceVariantSelection != -1)
        forceVariantSelection
      else {
        val experimentName = _opcode.name
        val variantNames = _opcode.variantOpcodes.map(_.nameSuffix)
        def variantGenerator(i: Int)(inputs: Array[VirtualFieldRegister]): Unit = {
          UserKernel(_opcode.variantOpcodes(i), inputs, resultTypes)
        }
        profiler.bestVariant(experimentName, variantNames, variantGenerator, inputs, resultTypes)
      }

    UserKernel(_opcode.variantOpcodes(opcodeIndex), inputs, resultTypes)
  }

  /** Create a single-output hyperkernel from user-generated kernel code.
    *
    * @param opcode The opcode for the user's GPU operator.
    * @param inputs The fields driving the operator / kernel.
    * @param resultType The type of field created by the operator / kernel.
    * @param profiler The profiler to use to pick the best variant
    * @return HyperKernel implementing the opcode.
    */
  def apply(opcode: UserGPUWithVariantsOpcode,
            inputs: Array[VirtualFieldRegister],
            resultType: FieldType, profiler: Profiler): HyperKernel = apply(opcode, inputs, Array(resultType), profiler)
}
