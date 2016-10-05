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

import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.{AbstractKernel, ConvolutionFFTUsePolicy, ConvolutionSmallTensorUsePolicy, VirtualFieldRegister}
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.compiler.parser.syntaxtree.Field
import cogx.runtime.execution.Profiler

/** Generates OpenCL code for operations that produce complex scalar fields.
  *
  * @author Greg Snider
  */
private[cogx]
object ComplexFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param field The field which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param fftUse policy for FFT use in fast convolution.
    * @param smallTensorUse policy for when to use SmallTensorAddressing in convolution.
    * @param profiler A facility for getting kernel execution times
    * @return OpenCL kernel implementing the operation.
    */
  def apply(field: Field, inputs: Array[VirtualFieldRegister],
            codeGenParams: OpenCLKernelCodeGenParams,
            fftUse: ConvolutionFFTUsePolicy,
            smallTensorUse: ConvolutionSmallTensorUsePolicy,
            profiler: Profiler): AbstractKernel =
  {
    val opcode = field.opcode
    val fieldType = field.fieldType
    require(fieldType.tensorOrder == 0)
    opcode match {
      // CPU kernels:
      case op: ConstantOp =>
        if (field.hasRecurrence)
          new RecurrentFieldKernel(fieldType, op)
        else
          new ConstantFieldKernel(fieldType, op)
      case op: UserOpcode =>
        new UserOperatorKernel(inputs, op)

      // User GPU kernel:
      case op: UserGPUOpcode =>
        UserKernel(op, inputs, fieldType)
      case op: UserGPUWithVariantsOpcode =>
        UserWithVariantsKernel(op, inputs, fieldType, profiler)

      // single-function GPU kernels (listed here to match first before
      // the more generalized kernels below):

      case op: ConvolveOp =>
        DynamicConvolutionGenerator(inputs, op, fieldType, fftUse, smallTensorUse, codeGenParams, profiler)
      case FlipOp =>
        FlipHyperKernel(inputs, opcode, fieldType)   // Not tested XXX
      case op: FFT1DOp =>
        if (isComplexField(inputs(0).fieldType))
          FFT1DHyperKernel(inputs, op, Array(fieldType))
        else
          FFT1DHyperKernel(RealToComplexHyperKernel(inputs(0), RealToComplexOp, fieldType).outputs.toArray, op, Array(fieldType))
      case op: InverseFFT1DOp =>
        FFT1DHyperKernel(inputs, op, Array(fieldType))
      case op: FFT2DOp =>
        if (isComplexField(inputs(0).fieldType))
          FFT2DHyperKernel(inputs, op, Array(fieldType))
        else
          FFT2DHyperKernel(RealToComplexHyperKernel(inputs(0), RealToComplexOp, fieldType).outputs.toArray, op, Array(fieldType))
      case op: FFT2DSubOp =>
        if (isComplexField(inputs(0).fieldType))
          FFT2DSeparableHyperKernel(inputs(0), op, fieldType)
        else
          FFT2DSeparableHyperKernel(RealToComplexHyperKernel(inputs(0), RealToComplexOp, fieldType).outputs(0),
            op, fieldType)
      case op: InverseFFT2DOp =>
        FFT2DHyperKernel(inputs, op, Array(fieldType))
      case op: FFT3DOp =>
        FFT3DHyperKernel(inputs, op, Array(fieldType))
      case op: InverseFFT3DOp =>
        FFT3DHyperKernel(inputs, op, Array(fieldType))
      case RealToComplexOp =>
        RealToComplexHyperKernel(inputs(0), opcode, fieldType)
      case RealImaginaryToComplexOp =>
        RealImaginaryToComplexHyperKernel(inputs, opcode, fieldType)
      case PolarToComplexOp =>
        MagnitudePhaseToComplexHyperKernel(inputs, opcode, fieldType)
      case op: ReshapeOp =>
        ReshapeHyperKernel(inputs(0), op)
      case op: ExpandBorderOp =>
        ExpandBorderHyperKernel(inputs(0), op, fieldType)
      // Some real-field ops/kernels that also works on complex fields:
      case op: TrimOp =>
        TrimHyperKernel(inputs, op, fieldType)
      case op: SliceOp =>
        SliceHyperKernel(inputs(0), op, fieldType)
      case SlicePointOp =>
        SlicePointHyperKernel(Array(inputs(0), inputs(1)), opcode, fieldType)
      case StackOp =>
        StackHyperKernel(inputs, opcode, fieldType)
      // multi-function GPU kernels:  Note that since BinaryConstOpcode is a
      // subclass of UnaryOpcode, it must appear first in the match.
      case op: ComplexBinaryComplexConstOpcode =>
        ComplexBinaryConstHyperKernel(Array(inputs(0)), op, fieldType)
      case op: ComplexBinaryRealConstOp =>
        ComplexBinaryRealConstHyperKernel(Array(inputs(0)), op, fieldType)
      case op: ComplexUnaryOp =>
        ComplexUnaryHyperKernel(Array(inputs(0)), op, fieldType)
      case op: ComplexBinaryOp =>
        ComplexBinaryHyperKernel(Array(inputs(0), inputs(1)), op, fieldType)
      case op: TensorSliceOp =>
        require(inputs(0).fieldType.tensorShape.dimensions == 1)
        SliceVectorsHyperKernel(inputs(0), op, fieldType)
      case op: TensorReduceSumOp =>
        TensorReduceHyperKernel(inputs(0), op, fieldType)
      case _ =>
        throw new RuntimeException("Compiler not done yet: " + opcode)
    }
  }
}