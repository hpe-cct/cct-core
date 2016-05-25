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

import cogx.compiler.codegenerator.opencl.fragments.{BigTensorAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{Forward, WorkDimensions, FFT1DKernelCache}
import cogx.platform.types.{Opcode, VirtualFieldRegister, FieldType}
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.common.FieldPolicies._

/** Performs a 1D FFT.
  *
  * This kernel works on both ComplexField inputs and split real and imaginary
  * ScalarFields (and also vector versions of each).  The opcode for the complex
  * field case is a UnaryOpcode, while it's a MulitOutputOpcode for the split
  * real/imaginary case.  The base class for these two opcodes, while they have
  * a lot of similar parameters, is Opcode.  You'll find some code below to
  * extract similar parameters out of these two opcode types.
  *
  * @author Dick Carter and Greg Snider
  * @param in The virtual field registers of the input field to be 1D FFT'd.
  * @param opcode The binary opcode for this operation.
  * @param resultTypes The FieldTypes of the result of this kernel.
  * @param addressMode The addressing mode of this kernel
  * @param workDimensions The kernel launch parameters of this kernel.
  * @param sourceCode The FFT-planner-generated source code.
  * @return The synthesized hyperkernel.
  */
private[cogx]
class FFT1DHyperKernel private[hyperkernels] (in: Array[VirtualFieldRegister],
                                              opcode: Opcode,
                                              resultTypes: Array[FieldType],
                                              addressMode: AddressingMode,
                                              workDimensions: WorkDimensions,
                                              sourceCode: String)
        extends HyperKernel(opcode, in, resultTypes, addressMode) {

  val fftPlanes = in(0).fieldType.tensorShape.points

  // For an FFT operating on a VectorField, expand the 1D thread organization
  // to 2D.  The plane designator becomes "_row", not the usual "_tensorElement".
  // The _tensorElement variable is not available in BigTensorAddressing mode,
  // which this kernel operates in because each thread writes multiple output
  // elements.
  val workFieldShape =
    if (fftPlanes == 1)
      Shape(workDimensions.gWorkItems.toInt)
    else
      Shape(fftPlanes, workDimensions.gWorkItems.toInt)

  val planeDesignator = if (fftPlanes == 1) "0" else "_row"

  // The FFT runs with a 1D thread organization.  We pick up the local and
  // global workItem counts from the workDimensions object created by the
  // FFT planner and set the kernel launch parameters to match.  The kernel
  // runs in BigTensorAddressing mode since each thread writes multiple output
  // elements.  Note: a 32 x 32 FFT was seen to run with 128 threads organized
  // as 2 blocks of 64 threads each.

  override lazy val workFieldType = new FieldType(workFieldShape, resultTypes(0).tensorShape, resultTypes(0).elementType)
  override lazy val workGroup = HyperKernel.computeWorkGroupParameters(workFieldType, addressing, 1, workDimensions.lWorkItems.toInt)

  val (splitRealImaginary, dir, scaleFactor) = opcode match {
    case x: FFT1DSubOp => (false, x.dir, x.scaleFactor)
    case y: FFT1DSubOpRI => (true, y.dir, y.scaleFactor)
    case z => throw new RuntimeException(s"unexpected opcode $z, expecting FFTOp or FFTOpRI")
  }

  val realInput =
    if (splitRealImaginary)
      in.length == 1
    else
      !isComplexField(in(0).fieldType)
  val realOutput =
    if (splitRealImaginary)
      resultTypes.length == 1
    else
      !isComplexField(resultTypes(0))

  val scaling = if (scaleFactor == 1.0f) "" else scaleFactor + "f * "

  /** supply values for former arguments to the FFT kernel */
  def postProcess(source: String) = source.
          replaceAll("%dirVal%", dir.value.toString).
          replaceAll("%dirName%", dir.name).
          replaceAll("%batchSize%", workDimensions.batchSize.toString).
          replaceAll("%realInput%", if (realInput) "1" else "0").
          replaceAll("%realOutput%", if (realOutput) "1" else "0").
          replaceAll("%splitRealImaginary%", if (splitRealImaginary) "1" else "0").
          replaceAll("%plane%", planeDesignator).
          replaceAll("%scalingMultiply%", scaling)

  addCode(postProcess(sourceCode))
//      debugCompile
}

/** Factory object for creating a chain of kernels to perform a 1D FFT
  */
private[cogx]
object FFT1DHyperKernel {
  val DeviceMaxWorkItemsPerWorkGroup = 256

  /** Create kernel DAG for 1D FFT.
    *
    * @param in The virtual field registers of the input field to be 1D FFT'd.
    * @param opcode The opcode for this operation, with dir and dim info.
    * @param resultTypes The FieldTypes of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], opcode: Opcode, resultTypes: Array[FieldType]):
    HyperKernel =
  {

    val inType0 = in(0).fieldType

    val (splitRealImaginary, dir, scaleFactor, dimensions) = opcode match {
      case x: FFTOp => (false, x.dir, x.scaleFactor, x.dimensions)
      case y: FFTOpRI => (true, y.dir, y.scaleFactor, y.dimensions)
      case z => throw new RuntimeException(s"unexpected opcode $z, expecting FFTOp or FFTOpRI")
    }

    if (in.length > 1)
      require(inType0 == in(1).fieldType)

    if (resultTypes.length > 1)
      require(resultTypes(0) == resultTypes(1))

    require(inType0.dimensions == 1)
    require(dimensions == 1)

    // We can't always predict the resultType, since the inverse FFT may be
    // either real or complex, depending on the original input.
    if (splitRealImaginary)
      require(isRealField(inType0) && inType0 == resultTypes(0))
    else if (dir == Forward)
      require(resultTypes(0) == toComplex(inType0))
    else
      require(toComplex(resultTypes(0)) == toComplex(inType0))

    val fftColumns = inType0.fieldShape(0)
    val workDimensions = FFT1DKernelCache.workDimensions(fftColumns,
      DeviceMaxWorkItemsPerWorkGroup)
    val sourceCodes = FFT1DKernelCache.sourceCodes(fftColumns,
      DeviceMaxWorkItemsPerWorkGroup)
    val kernelNames = FFT1DKernelCache.kernelNames(fftColumns,
      DeviceMaxWorkItemsPerWorkGroup)

    val passes = kernelNames.length
    require(passes > 0, "Internal error: expecting FFT passes > 0")

    def kernelChainUpToPass(pass: Int): HyperKernel = {
      val input = if (pass == 0) in else kernelChainUpToPass(pass - 1).outputs.toArray
      val resultFieldTypes =
        if (pass == passes - 1)
          resultTypes
        else if (splitRealImaginary)
          Array(inType0, inType0)
        else
          Array(toComplex(inType0))

      // Apply requested scaling to last pass only
      val scale = if (pass == passes - 1) scaleFactor else 1.0f

      if (splitRealImaginary)
        new FFT1DHyperKernel(input, FFT1DSubOpRI(pass, dir, scale), resultFieldTypes,
          BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
      else
        new FFT1DHyperKernel(input, FFT1DSubOp(pass, dir, scale), resultFieldTypes,
          BigTensorAddressing, workDimensions(pass), sourceCodes(pass))
    }

    kernelChainUpToPass(passes - 1)
  }
}
