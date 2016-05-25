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

import cogx.compiler.parser.op._
import cogx.platform.opencl.OpenCLPlatformParams
import cogx.platform.types._
import cogx.compiler.parser.op.{BinaryConstOpcode, BinaryOpcode, UnaryOpcode, ConstantOp}
import cogx.compiler.codegenerator.opencl.cpukernels.{UserOperatorKernel, ConstantFieldKernel, RecurrentFieldKernel}
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.parser.syntaxtree.Field
import cogx.compiler.codegenerator.opencl.hyperkernels.domaintransform.{TensorDomainFilterRowsHyperKernel, TensorDomainTransformRowsHyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.{SmallTensorAddressing, HyperKernel}

/** Generates OpenCL code for operations that produce matrix fields.
  *
  * @author Greg Snider
  */
private[cogx]
object MatrixFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param field The field which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param platformParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param fftUse policy for FFT use in fast convolution.
    * @param smallTensorUse policy for when to use SmallTensorAddressing in convolution.
    * @return OpenCL kernel implementing the operation.
    */
  def apply(field: Field, inputs: Array[VirtualFieldRegister],
            platformParams: OpenCLPlatformParams,
            fftUse: ConvolutionFFTUsePolicy,
            smallTensorUse: ConvolutionSmallTensorUsePolicy): AbstractKernel =
  {
    val opcode = field.opcode
    val fieldType = field.fieldType
    require(fieldType.tensorOrder == 2,
      "Bad tensor order (" + fieldType.tensorOrder + "), opcode: " + opcode)
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

      // single-function GPU kernels (listed here to match first before
      // the more generalized kernels below):
      case op: DomainTransformRowsOp =>
        TensorDomainTransformRowsHyperKernel(inputs, op, fieldType)
      case DomainFilterRowsOp =>
        TensorDomainFilterRowsHyperKernel(inputs, DomainFilterRowsOp, fieldType)
      case DownsampleOp(factor, phase) =>
        Downsample2DHyperKernel(inputs, DownsampleOp(factor, phase), fieldType)
      case UpsampleOp(factor, phase) =>
        Upsample2DHyperKernel(inputs, UpsampleOp(factor, phase), fieldType)
      case SupersampleOp =>
        SupersampleHyperKernel(inputs, opcode, fieldType)
      case FlipOp =>
        FlipHyperKernel(inputs, opcode, fieldType)   // Not tested XXX
      case op: TrimOp =>
        TrimHyperKernel(inputs, op, fieldType)
      case op: ReplicateOp =>
        ReplicateHyperKernel(inputs, op, fieldType)
      case op: SubfieldsOp =>
        Subfields2DHyperKernel(inputs(0), op, fieldType)
      case op: ExpandBorderOp =>
        ExpandBorderHyperKernel(inputs(0), op, fieldType)
      case op: SliceOp =>
        SliceHyperKernel(inputs(0), op, fieldType)
      case SlicePointOp =>
        SlicePointHyperKernel(Array(inputs(0), inputs(1)), opcode, fieldType)
      case StackOp =>
        StackHyperKernel(inputs, opcode, fieldType)
      case op: TensorStackOp =>
        StackTensorsHyperKernel(inputs, op, fieldType)
      case FieldReduceMaxOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, platformParams.warpSize)
      case FieldReduceMinOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, platformParams.warpSize)
      case FieldReduceSumOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, platformParams.warpSize)
      case MatrixTransposeOp =>
        TensorTransposeHyperKernel(inputs, opcode, fieldType)
      case VectorTransposeOp =>
        TensorTransposeHyperKernel(inputs, opcode, fieldType)
      case op: MatrixTransformMatrixOp =>
        MatrixMatrixTransformHyperKernel(inputs, op, fieldType)
      case MatrixInvertOp =>
        MatrixInvertHyperKernel(inputs(0), opcode, fieldType)
      case Transpose2DOp =>
        TransposeHyperKernel(inputs, Transpose2DOp, fieldType)
      case op: ConvolveOp =>
        DynamicConvolutionGenerator(inputs, op, fieldType, fftUse, smallTensorUse, platformParams)
      case PushOp =>
        require(fieldType.dimensions > 0)
        PushHyperKernel(inputs, opcode, fieldType)
      case op: WarpOp =>
        fieldType.dimensions match {
          case 2 => Warp2DHyperKernel(inputs, op, fieldType)
          case x => throw new RuntimeException("Warp not supported for " +
                  x + "-dimensional fields.")
        }
      case op: SubfieldOp =>
        fieldType.dimensions match {
          case 1 => SubfieldHyperKernel(inputs, op, fieldType)
          case 2 => SubfieldHyperKernel(inputs, op, fieldType)
          case x => throw new RuntimeException("Subfield not supported for " +
                  x + "-dimensional fields.")
        }
      case op: ReshapeOp =>
        ReshapeHyperKernel(inputs(0), op)
      case op: BilateralFilter2DOp =>
        BilateralFilterHyperKernel(inputs(0), op.spatialFilter, op.rangeSigma, op, fieldType)
      case FilterNaNOp =>
        NaNHyperKernel(inputs, FilterNaNOp, fieldType)

      // multi-function GPU kernels.  Note that since BinaryConstOpcode is a
      // subclass of UnaryOpcode, it must appear first in the match.

      case op: BinaryConstOpcode =>
        BinaryConstHyperKernel(inputs, op, fieldType)
      case op: BinaryOpcode =>
        BinaryHyperKernel(inputs, op, fieldType)
      case op: UnaryOpcode =>
        UnaryHyperKernel(inputs, op, fieldType)
      case _ =>
        throw new RuntimeException("Opcode not supported for matrix fields: " +
                opcode)
    }
  }
}
