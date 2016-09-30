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

import cogx.platform.types.{AbstractKernel, VirtualFieldRegister}
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.compiler.parser.op.SliceOp
import cogx.compiler.parser.op.SubspaceOp
import cogx.compiler.parser.op.TrimOp
import cogx.compiler.parser.op.UpsampleOp
import cogx.compiler.parser.op.DownsampleOp
import cogx.compiler.parser.op.SubfieldOp
import cogx.compiler.parser.syntaxtree.{Field, RestoreHooks}
import cogx.compiler.codegenerator.opencl.hyperkernels.domaintransform.{ColorDomainFilterColumnsHyperKernel, ColorDomainFilterRowsHyperKernel}
import cogx.platform.opencl.OpenCLKernelCodeGenParams
import cogx.runtime.execution.Profiler

/** Generates OpenCL code for operations that produce color fields.
  *
  * @author Greg Snider
  */
private[cogx]
object ColorFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param field The field which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
    * @param profiler A facility for getting kernel execution times
    * @return OpenCL kernel implementing the operation.
    */
  def apply(field: Field,
            inputs: Array[VirtualFieldRegister],
            codeGenParams: OpenCLKernelCodeGenParams,
            profiler: Profiler): AbstractKernel =
  {
    val opcode = field.opcode
    val fieldType = field.fieldType
    opcode match {
      // CPU kernels:
      case op: ColorSensorOp =>
        val f = field.asInstanceOf[RestoreHooks]
        new ColorSensorKernel(fieldType, op, f.classnameAsSaved, f.restoreParameters _)
      case op: ConstantOp =>
        // Recurrences hooked to sensors running in pipelined mode will have
        // a dummy ConstantOp which does not get used.  Instead the RecurrentFieldKernel
        // is given a NullOp so it knows not to write data upon reset.
        field.recurrence match {
          case Some(recurrenceField) => recurrenceField.opcode match {
            case op: PipelinedColorSensorOp => new RecurrentFieldKernel(fieldType, NullOp)
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

      // GPU kernels:
      case DomainFilterColumnsOp =>
        ColorDomainFilterColumnsHyperKernel(inputs, DomainFilterColumnsOp, fieldType)
      case DomainFilterRowsOp =>
        ColorDomainFilterRowsHyperKernel(inputs, DomainFilterRowsOp, fieldType)
      case VectorFieldToColorFieldOp =>
        VectorFieldToColorFieldHyperKernel(inputs(0), VectorFieldToColorFieldOp,
          fieldType)
      case FlipOp =>
        FlipHyperKernel(inputs, opcode, fieldType)   // Not tested XXX
      case MergeColorPlanesOp =>
        CreateColorFieldHyperKernel(inputs, opcode, fieldType)
      case DownsampleOp(factor, phase) =>
        Downsample2DHyperKernel(inputs, DownsampleOp(factor, phase), fieldType)
//      case DiffuseDirichletOp =>
//        DiffusionDirichlet(inputs(0), inputs(1))
      case CannyOp =>
        Color2DCannyHyperKernel(inputs, opcode, fieldType)
      case UpsampleOp(factor, phase) =>
        Upsample2DHyperKernel(inputs, UpsampleOp(factor, phase), fieldType)
      case SupersampleOp =>
        SupersampleHyperKernel(inputs, opcode, fieldType)
      case op: SubspaceOp =>
        SubspaceHyperKernel(inputs, op, fieldType)
      case op: TrimOp =>
        TrimHyperKernel(inputs, op, fieldType)
//      case QuadImageToColorFieldOp =>
//        new QuadImage2DFormatConversionKernel(inputs(0), fieldType)
      case Transpose2DOp =>
        TransposeHyperKernel(inputs, Transpose2DOp, fieldType)
      case op: SliceOp =>
        SliceHyperKernel(inputs(0), op, fieldType)
//      case PushOp =>
//        new Color3DPushKernel(inputs, PushOp)
      case op: SubfieldOp =>
        fieldType.dimensions match {
          case 1 => SubfieldHyperKernel(inputs, op, fieldType)
          case 2 => SubfieldHyperKernel(inputs, op, fieldType)
          case x => throw new RuntimeException("Subfield not supported for " +
                  x + "-dimensional fields.")
        }

      case op: BinaryConstOpcode =>
        BinaryConstHyperKernel(inputs, op, fieldType)
      case op: BinaryOpcode =>
        ColorBinaryHyperKernel(Array(inputs(0), inputs(1)), op, fieldType)
      case op: UnaryOpcode =>
        UnaryHyperKernel(inputs, op, fieldType)
      case _ =>
        throw new RuntimeException("Color field op not supported: " + opcode)
    }
  }
}