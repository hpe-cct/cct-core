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
      else if (_opcode.variantOpcodes.length == 1)
        0
      else
        bestVariant(_opcode, inputs, resultTypes, profiler)

    UserKernel(_opcode.variantOpcodes(opcodeIndex), inputs, resultTypes)
  }

  // Profile each variant with the Profiler and return the index of the fastest.
  private def bestVariant(_opcode: UserGPUWithVariantsOpcode,
                               inputs: Array[VirtualFieldRegister],
                               resultTypes: Array[FieldType],
                               profiler: Profiler): Int = {
    val inputFieldTypes = inputs.map(_.fieldType)
    val numVariants = _opcode.variantOpcodes.length

    val start = System.nanoTime()
    // We always need to print something, since profiling can stretch the 1st-time compile to minutes
    // and we don't want the user to think the compiler is wedged.
    print(s"Profiling ${_opcode.name}: ")

    // For each opcode, get a profiling 'sample' of its execution time
    val variantProfileSamples = _opcode.variantOpcodes.map( opcode =>
      profiler.profile(inputFieldTypes,
        (inputRegisters) => { UserKernel(opcode, inputRegisters, resultTypes) }
      )
    )
    // Pick the fastest variant
    val bestVariant = variantProfileSamples.zipWithIndex.reduceLeft( (a,b) => if (a._1.avgStepTimeUsec < b._1.avgStepTimeUsec) a else b)._2

    // Print details about the sample if it was performed earlier (i.e. the sample came from the cache).
    def printSample(sample: ProfileSample, selected: String, nameSuffix: String): Unit = {
      print(f"  ${sample.avgStepTimeUsec}%9.1f uSec $selected$nameSuffix")
      if (sample.fromCache) {
        val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        val dateTime: LocalDateTime =
          LocalDateTime.ofInstant(Instant.ofEpochMilli(sample.creationTimeMsec), ZoneId.systemDefault())
        val prettyDate = dateTime.format(dateTimeFormatter)
        val warmupSteps = sample.warmupSteps
        val runSteps = sample.runSteps
        println(s", profiled $prettyDate with (warm-up,run) steps = ($warmupSteps, $runSteps).")
      }
      else
        println
    }

    // Print out information about the profiling process.
    if (Cog.verboseProfiler) {
      println
      for (i <- 0 until numVariants) {
        val selected = if (i == bestVariant) "* " else "  "
        printSample(variantProfileSamples(i), selected, _opcode.variantOpcodes(i).nameSuffix)
      }
      val durationUsec = (System.nanoTime() - start) / 1000
      println(s"Variant selection took $durationUsec uSec.")
      println
    }
    else
      printSample(variantProfileSamples(bestVariant), "", _opcode.variantOpcodes(bestVariant).nameSuffix)

    bestVariant
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
