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
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.platform.types.{AbstractKernel, ConvolutionFFTUsePolicy, ConvolutionSmallTensorUsePolicy, VirtualFieldRegister}
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.syntaxtree.{Field, RestoreHooks}
import cogx.compiler.parser.op.ReshapeOp
import cogx.compiler.parser.op.SliceOp
import cogx.compiler.parser.op.DownsampleOp
import cogx.compiler.parser.op.ShiftOp
import cogx.compiler.parser.op.WarpOp
import cogx.compiler.parser.op.TrimOp
import cogx.compiler.parser.op.ExpandBorderOp
import cogx.compiler.parser.op.UpsampleOp
import cogx.compiler.parser.op.TensorSliceOp
import cogx.compiler.parser.op.ConvolveOp
import cogx.compiler.parser.op.SubspaceOp
import cogx.compiler.parser.op.BilateralFilter2DOp
import cogx.compiler.parser.op.SubfieldOp
import cogx.compiler.parser.op.LocalMax2DOp
import cogx.compiler.parser.op.UserOpcode
import cogx.compiler.parser.op.LocalMin2DOp
import cogx.compiler.codegenerator.opencl.hyperkernels.domaintransform.{ColorDomainFilterRowsHyperKernel, ColorDomainTransformRowsHyperKernel, TensorDomainFilterRowsHyperKernel, TensorDomainTransformRowsHyperKernel}
import cogx.runtime.execution.Profiler

/** Generates OpenCL code for operations that produce scalar fields.
  *
  * @author Greg Snider
  */
private[cogx]
object ScalarFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param field The field which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param codeGenParams A bundle of platform parameters that affect kernel code generation and optimization.
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
    opcode match {
      // CPU kernels
      case op: SensorOp =>
        val f = field.asInstanceOf[RestoreHooks]
        new SensorKernel(fieldType, op, f.classnameAsSaved, f.restoreParameters _)
      case op: ConstantOp =>
        // Recurrences hooked to sensors running in pipelined mode will have
        // a dummy ConstantOp which does not get used.  Instead the RecurrentFieldKernel
        // is given a NullOp so it knows not to write data upon reset.
        field.recurrence match {
          case Some(recurrenceField) => recurrenceField.opcode match {
            case op: PipelinedSensorOp => new RecurrentFieldKernel(fieldType, NullOp)
            case _ => new RecurrentFieldKernel(fieldType, op)
          }
          case None => new ConstantFieldKernel(fieldType, op)
        }
      case op: UserOpcode =>
        new UserOperatorKernel(inputs, op)

      // User GPU kernel:
      case op: UserGPUOpcode =>
        UserKernel(op, inputs, fieldType)
      case op: UserGPUWithVariantsOpcode =>
        UserWithVariantsKernel(op, inputs, fieldType, profiler)

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
      case ConditionNumberOp =>
        ConditionNumberHyperKernel(inputs(0), ConditionNumberOp, fieldType)
      case DeterminantOp =>
        DeterminantHyperKernel(inputs(0), DeterminantOp, fieldType)
      case op: DomainTransformRowsOp =>
        if (isColorField(inputs(0).fieldType))
          ColorDomainTransformRowsHyperKernel(inputs, op, fieldType)
        else
          TensorDomainTransformRowsHyperKernel(inputs, op, fieldType)
      case DomainFilterRowsOp =>
        if (isColorField(inputs(0).fieldType))
          ColorDomainFilterRowsHyperKernel(inputs, DomainFilterRowsOp, fieldType)
        else
          TensorDomainFilterRowsHyperKernel(inputs, DomainFilterRowsOp, fieldType)
      case FieldReduceMaxOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case FieldReduceMinOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case FieldReduceSumOp =>
        ScalarReduceHyperKernel(inputs(0), opcode, fieldType, codeGenParams.warpSize)
      case FieldReduceMedianOp =>
        CPUScalarReduceMedianKernel(inputs(0), opcode, fieldType)
      case FlipOp =>
        FlipHyperKernel(inputs, opcode, fieldType)
      case NormalizeL1Op =>
        val sum = ScalarReduceHyperKernel(inputs(0), FieldReduceSumOp, to0D(fieldType), codeGenParams.warpSize)
        BinaryHyperKernel(Array(inputs(0), sum.outputs(0)), DivideOp, fieldType)
      case NormalizeL2Op =>
        val squared = BinaryHyperKernel(Array(inputs(0), inputs(0)), MultiplyOp, fieldType)
        val sumSquared = ScalarReduceHyperKernel(squared.outputs(0), FieldReduceSumOp, to0D(fieldType), codeGenParams.warpSize)
        BinaryHyperKernel(Array(inputs(0), sumSquared.outputs(0)), DivideOp, fieldType)
      case WinnerTakeAllOp =>
        WinnerTakeAllHyperKernel(inputs(0), WinnerTakeAllOp, fieldType, codeGenParams.warpSize)
      case op: TrimOp =>
        TrimHyperKernel(inputs, op, fieldType)
      case TensorDotOp =>
        // Switch to BinaryHyperKernel with MultiplyOp if tensor is order 0
        if (inputs(0).fieldType.tensorOrder == 0)
          BinaryHyperKernel(inputs, MultiplyOp, fieldType)
        else
          TensorDotHyperKernel(inputs, opcode, fieldType)
      case op: LocalMax2DOp =>
        LocalExtrema2DHyperKernel(inputs(0), op, fieldType)
      case op: LocalMin2DOp =>
        LocalExtrema2DHyperKernel(inputs(0), op, fieldType)
      case op: ConvolveColumns2DOp =>
        ConvolveColumns2DHyperKernel(inputs(0), inputs(1), op, fieldType)
      case op: ConvolveRows2DOp =>
        ConvolveRows2DHyperKernel(inputs(0), inputs(1), op, fieldType)
      case op: ConvolveTiledOp =>
        ConvolveTiledHyperKernel(inputs, op, fieldType)
      case op: ConvolveToSmallFieldTiledOp =>
        ConvolveToSmallFieldPipelinedTiledHyperKernel(inputs, op, fieldType, codeGenParams)
      case op: ConvolveOp =>
        DynamicConvolutionGenerator(inputs, op, fieldType, fftUse, smallTensorUse, codeGenParams)
      case op: ComplexToRealOp =>
        ComplexToRealHyperKernel(inputs, op, fieldType)
      case DownsampleOp(factor, phase) =>
        Downsample2DHyperKernel(inputs, DownsampleOp(factor, phase), fieldType)
      case SupersampleOp =>
        SupersampleHyperKernel(inputs, opcode, fieldType)
      case OuterProductOp =>
        CPUOuterProductKernel(inputs, opcode, fieldType)
      case CrossDotOp =>
        CrossDotHyperKernel(Array(inputs(0), inputs(1)), CrossDotOp, fieldType)
      case ReverseCrossDotOp =>
        ReverseCrossDotHyperKernel(Array(inputs(0), inputs(1)), opcode, fieldType)
      case op: SubspaceOp =>
        SubspaceHyperKernel(inputs, op, fieldType)
      case op: ReshapeOp =>
        ReshapeHyperKernel(inputs(0), op)
      case JoinOp =>
        JoinHyperKernel(inputs, opcode, fieldType)
      case op: ColorPlaneOp =>
        ColorPlaneHyperKernel(inputs, op, fieldType)
      case op: ExpandBorderOp =>
        ExpandBorderHyperKernel(inputs(0), op, fieldType)
      case op: SliceOp =>
        SliceHyperKernel(inputs(0), op, fieldType)
      case SlicePointOp =>
        SlicePointHyperKernel(Array(inputs(0), inputs(1)), opcode, fieldType)
      case StackOp =>
        StackHyperKernel(inputs, opcode, fieldType)
      case op: TensorSliceOp =>
        require(inputs(0).fieldType.tensorShape.dimensions == 1)
        SliceVectorsHyperKernel(inputs(0), op, fieldType)
      case op: TensorReductionOp =>
        TensorReduceHyperKernel(inputs(0), op, fieldType)
//      case ImageToFieldOp =>
//        new Color2DToFieldKernel(inputs(0), fieldType)
//      case UnaryTestOp =>
//        new UnaryTestOpKernel(inputs(0))
//      case BinaryTestOp =>
//        new BinaryTestOpKernel(Array(inputs(0), inputs(1)))
      case Transpose2DOp =>
        TransposeHyperKernel(inputs, Transpose2DOp, fieldType)
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
      case MedianFilterOp =>
        Median2D_3x3HyperKernel(inputs(0), opcode, fieldType)
      case UpsampleOp(factor, phase) =>
        Upsample2DHyperKernel(inputs, UpsampleOp(factor, phase), fieldType)
      case BackwardDivergenceOp =>
        BackwardDivergence2DHyperKernel(inputs(0), BackwardDivergenceOp, fieldType)
      case NonMaximumSuppressionOp =>
        NonMaxSuppression2DHyperKernel(inputs(0), NonMaximumSuppressionOp, fieldType)
      case OrientedNonMaximumSuppressionOp =>
        OrientedNonMaxSuppression2DHyperKernel(inputs(0), inputs(1),
          OrientedNonMaximumSuppressionOp, fieldType)

      case FieldArraySelectOp =>
        MultiplexerHyperKernel(inputs, FieldArraySelectOp, fieldType)
      case op: BilateralFilter2DOp =>
        BilateralFilterHyperKernel(inputs(0), op.spatialFilter, op.rangeSigma, op, fieldType)
      case op: ShiftOp =>
        ShiftHyperKernel(Array(inputs(0)), op, fieldType)

      // multi-function GPU kernels.  Note that since BinaryConstOpcode is a
      // subclass of UnaryOpcode, it must appear first in the match.

      case op: BinaryConstOpcode =>
        BinaryConstHyperKernel(inputs, op, fieldType)
      case op: BinaryOpcode =>
        BinaryHyperKernel(inputs, op, fieldType)
      case op: UnaryOpcode =>
        UnaryHyperKernel(inputs, op, fieldType)
      case x =>
        throw new RuntimeException("Compiler not done yet: " + x)
    }
  }

}