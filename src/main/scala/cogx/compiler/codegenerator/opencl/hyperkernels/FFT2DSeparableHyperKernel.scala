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

package cogx.compiler.codegenerator.opencl.hyperkernels


import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, HyperKernel}
import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{ClFFTDirection, Forward, FFT2DKernelCache}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.compiler.parser.op.{UnaryOpcode, FFT2DSubOp}
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Creates a kernel that performs the FFT (or inverse FFT) on just the rows
  * or columns of a 2D complex field. Separating the rows and columns allows
  * this to be used for the DCT.
  */
private[cogx]
object FFT2DSeparableHyperKernel {
  val DeviceMaxWorkItemsPerWorkGroup = 256

  /** Creates a kernel that performs the FFT (or inverse FFT) on just the rows
    * or columns of a 2D complex field.
    *
    * @param in The input virtual field register to be FFT'd.
    * @param operation The opcode for this operation, with dir and dim info.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, operation: UnaryOpcode, resultType: FieldType):
  HyperKernel =
  {
    require(isComplexField(in.fieldType))
    require(in.fieldType.dimensions == 2)
    require(in.fieldType.tensorOrder == 0)
    var direction: ClFFTDirection = Forward
    var transformRows = false
    var factor = 1.0f
    operation match {
      case FFT2DSubOp(level, dir, scaleFactor) =>
        transformRows = (level == 0)
        direction = dir
        factor = scaleFactor
      case x =>
        throw new RuntimeException("internal error " + getClass.getSimpleName)
    }

    val fftRows = in.fieldType.fieldShape(0)
    val fftColumns = in.fieldType.fieldShape(1)
    val workDimensions = FFT2DKernelCache.workDimensions(fftRows, fftColumns, DeviceMaxWorkItemsPerWorkGroup)
    val sourceCodes = FFT2DKernelCache.sourceCodes(fftRows, fftColumns, DeviceMaxWorkItemsPerWorkGroup)
    val kernelNames = FFT2DKernelCache.kernelNames(fftRows, fftColumns, DeviceMaxWorkItemsPerWorkGroup)
    val passes = kernelNames.length

    // The FFT planner will generate either 2 or 3 passes. The first pass is for
    // transforming the rows. If the number of columns is small enough, the
    // second pass will transform the columns, otherwise the second pass will
    // transpose the output of the first pass and the third pass will complete
    // the FFT.  Apply the scale factor specied by the op on the last pass.
    if (transformRows) {
      // Transforming rows, this will need one pass.
      val pass = 0
      new FFT2DHyperKernel(Array(in), FFT2DSubOp(pass, direction, factor), Array(resultType),
        BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
    } else {
      // Transforming columns, this requires one or two passes
      if (passes == 2) {
        val pass = 1
        new FFT2DHyperKernel(Array(in), FFT2DSubOp(pass, direction, factor), Array(resultType),
          BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
      } else if (passes == 3) {
        var pass = 1
        val transpose = new FFT2DHyperKernel(Array(in), FFT2DSubOp(pass, direction), Array(resultType),
          BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
        pass = 2
        new FFT2DHyperKernel(transpose.outputs.toArray, FFT2DSubOp(pass, direction, factor), Array(resultType),
          BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
      } else {
        require(requirement = false,
          "Internal error: too many FFT passes: " + passes)
        null
      }
    }
  }
}
