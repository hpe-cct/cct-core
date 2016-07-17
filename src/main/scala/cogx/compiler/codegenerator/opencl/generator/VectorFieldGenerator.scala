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

import cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform.DCT2DHyperKernel
import cogx.compiler.parser.op._
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.{VirtualFieldRegister, ConvolutionSmallTensorUsePolicy, ConvolutionFFTUsePolicy, AbstractKernel}
import cogx.compiler.parser.op.{BinaryConstOpcode, BinaryOpcode, UnaryOpcode, ConstantOp}
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.syntaxtree.{RestoreHooks, Field}
import cogx.compiler.parser.op.DomainTransformRowsOp
import cogx.compiler.parser.op.ReshapeOp
import cogx.compiler.parser.op.SliceOp
import cogx.compiler.parser.op.DownsampleOp
import cogx.compiler.parser.op.ShiftOp
import cogx.compiler.parser.op.WarpOp
import cogx.compiler.parser.op.TrimOp
import cogx.compiler.parser.op.SubfieldsOp
import cogx.compiler.parser.op.ExpandBorderOp
import cogx.compiler.parser.op.UpsampleOp
import cogx.compiler.parser.op.TensorSliceOp
import cogx.compiler.parser.op.ConvolveOp
import cogx.compiler.parser.op.SubspaceOp
import cogx.compiler.parser.op.LocalMinPosition2DOp
import cogx.compiler.parser.op.BilateralFilter2DOp
import cogx.compiler.parser.op.SubfieldOp
import cogx.compiler.parser.op.ScalarMatrixConvolve2DOp
import cogx.compiler.parser.op.ReplicateOp
import cogx.compiler.parser.op.LocalMaxPosition2DOp
import cogx.compiler.parser.op.RandomOp
import cogx.compiler.parser.op.UserOpcode
import cogx.compiler.parser.op.TensorStackOp
import cogx.compiler.codegenerator.opencl.hyperkernels.domaintransform.{TensorDomainFilterRowsHyperKernel, TensorDomainTransformRowsHyperKernel}

/** Generates OpenCL code for operations that produce vector fields.
  *
  * @author Greg Snider
  */
private[cogx]
object VectorFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param field The field which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param codeGenParams A bundle of platform parameters that affect kernel code generation and optimization.
    * @param fftUse policy for FFT use in fast convolution.
    * @param smallTensorUse policy for when to use SmallTensorAddressing in convolution.
    * @return OpenCL kernel implementing the operation.
    */
  def apply(field: Field, inputs: Array[VirtualFieldRegister],
            codeGenParams: OpenCLKernelCodeGenParams,
            fftUse: ConvolutionFFTUsePolicy,
            smallTensorUse: ConvolutionSmallTensorUsePolicy): AbstractKernel =
  {
    val opcode = field.opcode
    val fieldType = field.fieldType
    require(fieldType.tensorOrder == 1)
    opcode match {
      // CPU kernels:
      case op: VectorSensorOp =>
        val f = field.asInstanceOf[RestoreHooks]
        new VectorSensorKernel(fieldType, op, f.classnameAsSaved, f.restoreParameters _)
      case op: ConstantOp =>
        // Recurrences hooked to sensors running in pipelined mode will have
        // a dummy ConstantOp which does not get used.  Instead the RecurrentFieldKernel
        // is given a NullOp so it knows not to write data upon reset.
        field.recurrence match {
          case Some(recurrenceField) => recurrenceField.opcode match {
            case op: PipelinedVectorSensorOp => new RecurrentFieldKernel(fieldType, NullOp)
            case _ => new RecurrentFieldKernel(fieldType, op)
          }
          case None => new ConstantFieldKernel(fieldType, op)
        }
      case op: UserOpcode =>
        new UserOperatorKernel(inputs, op)

      // User GPU kernel:
      case op: UserGPUOpcode =>
        UserKernel(op, inputs, fieldType)

      // DCT
      case DCT2DOp =>
        DCT2DHyperKernel(inputs(0), DCT2DOp, fieldType)
      case DCTInverse2DOp =>
        DCT2DHyperKernel(inputs(0), DCTInverse2DOp, fieldType)
      case DCTTransposed2DOp =>
        DCT2DHyperKernel(inputs(0), DCTTransposed2DOp, fieldType)
      case DCTInverseTransposed2DOp =>
        DCT2DHyperKernel(inputs(0), DCTInverseTransposed2DOp, fieldType)

      // single-function GPU kernels (listed here to match first before
      // the more generalized kernels below):
      case op: DomainTransformRowsOp =>
        TensorDomainTransformRowsHyperKernel(inputs, op, fieldType)
      case DomainFilterRowsOp =>
        TensorDomainFilterRowsHyperKernel(inputs, DomainFilterRowsOp, fieldType)
      case ColorFieldToVectorFieldOp =>
        ColorFieldToVectorFieldHyperKernel(inputs(0),
          ColorFieldToVectorFieldOp, fieldType)
      case FlipOp =>
        FlipHyperKernel(inputs, opcode, fieldType)  // Not tested XXX
      case BackwardGradientOp =>
        BackwardGradient2DHyperKernel(inputs(0), opcode, fieldType)
      case ForwardGradientOp =>
        ForwardGradient2DHyperKernel(inputs(0), opcode, fieldType)
      case CentralGradientOp =>
        CentralGradient2DHyperKernel(inputs(0), opcode, fieldType)
      case op: LocalMaxPosition2DOp =>
        LocalExtremaPosition2DHyperKernel(inputs(0), op, fieldType)
      case op: LocalMinPosition2DOp =>
        LocalExtremaPosition2DHyperKernel(inputs(0), op, fieldType)
      case DownsampleOp(factor, phase) =>
        Downsample2DHyperKernel(inputs, DownsampleOp(factor, phase), fieldType)
      case UpsampleOp(factor, phase) =>
        Upsample2DHyperKernel(inputs, UpsampleOp(factor, phase), fieldType)
      case op: TrimOp =>
        TrimHyperKernel(inputs, op, fieldType)
      case SupersampleOp =>
        SupersampleHyperKernel(inputs, SupersampleOp, fieldType)
      case op: ReplicateOp =>
        ReplicateHyperKernel(inputs, op, fieldType)
      case op: ExpandBorderOp =>
        ExpandBorderHyperKernel(inputs(0), op, fieldType)
      case op: SliceOp =>
        SliceHyperKernel(inputs(0), op, fieldType)
      case SlicePointOp =>
        SlicePointHyperKernel(Array(inputs(0), inputs(1)), opcode, fieldType)
      case StackOp =>
        StackHyperKernel(inputs, opcode, fieldType)
      case op: TensorSliceOp =>
        SliceMatricesHyperKernel(inputs(0), op, fieldType)
      case op: TensorReductionOp =>
        TensorReduceHyperKernel(inputs(0), op, fieldType)
      case op: TensorStackOp =>
        StackTensorsHyperKernel(inputs, op, fieldType)
      case op: RandomOp =>
        RandomHyperKernel(inputs(0), op)
      case FieldReduceMaxOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case FieldReduceMinOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case FieldReduceSumOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case WinnerTakeAllOp =>
        WinnerTakeAllHyperKernel(inputs(0), WinnerTakeAllOp, fieldType, codeGenParams.warpSize)
      case MaxPositionOp =>
        WinnerTakeAllHyperKernel(inputs(0), MaxPositionOp, fieldType, codeGenParams.warpSize)
//      case ImageToFieldOp =>
//        new Color2DToFieldKernel(inputs(0), fieldType)
      case MatrixTransformVectorOp =>
        MatrixVectorTransformHyperKernel(inputs, MatrixTransformVectorOp, fieldType, codeGenParams.maxConstantBufferSize)
      case Transpose2DOp =>
        TransposeHyperKernel(inputs, Transpose2DOp, fieldType)
      case SolveOp =>
        if (inputs(0).fieldType.tensorShape == Shape(2, 2))
          Solve2x2HyperKernel(inputs, SolveOp, fieldType)
        else
          throw new RuntimeException("solve not implemented yet for > 2x2.")
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
      case op: SubfieldsOp =>
        Subfields2DHyperKernel(inputs(0), op, fieldType)
      case op: SubspaceOp =>
        SubspaceHyperKernel(inputs, op, fieldType)
      case op: ReshapeOp =>
        ReshapeHyperKernel(inputs(0), op)
      case op: ScalarMatrixConvolve2DOp =>
        ScalarMatrixConvolve2DHyperKernel(inputs(0), inputs(1), op, fieldType)
      case op: ConvolveTiledOp =>
        ConvolveTiledHyperKernel(inputs, op, fieldType)
      case op: ConvolveToSmallFieldTiledOp =>
        ConvolveToSmallFieldPipelinedTiledHyperKernel(inputs, op, fieldType, codeGenParams)
      case op: ConvolveOp =>
        DynamicConvolutionGenerator(inputs, op, fieldType, fftUse, smallTensorUse, codeGenParams)
      case op: ComplexToRealOp =>
        ComplexToRealHyperKernel(inputs, op, fieldType)
      case NonMaximumSuppressionOp =>
        NonMaxSuppression2DHyperKernel(inputs(0), NonMaximumSuppressionOp, fieldType)
      case FieldArraySelectOp =>
        MultiplexerHyperKernel(inputs, FieldArraySelectOp, fieldType)
      case op: BilateralFilter2DOp =>
        BilateralFilterHyperKernel(inputs(0), op.spatialFilter, op.rangeSigma, op, fieldType)
      case op: ShiftOp =>
        ShiftHyperKernel(Array(inputs(0)), op, fieldType)
      case VectorElementsOp =>
        new VectorElementsHyperKernel(inputs, fieldType)

      // multi-function GPU kernels.  Note that since BinaryConstOpcode is a
      // subclass of UnaryOpcode, it must appear first in the match.

      case op: BinaryConstOpcode =>
        BinaryConstHyperKernel(inputs, op, fieldType)
      case op: BinaryOpcode =>
        BinaryHyperKernel(inputs, op, fieldType)
      case op: UnaryOpcode =>
        UnaryHyperKernel(inputs, op, fieldType)
      case _ =>
        throw new RuntimeException("Opcode not supported for vector fields: " +
                opcode)
    }
  }
}