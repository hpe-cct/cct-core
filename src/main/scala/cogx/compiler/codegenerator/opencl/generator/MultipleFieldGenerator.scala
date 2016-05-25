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
import cogx.compiler.codegenerator.opencl.hyperkernels._
import cogx.platform.types.{VirtualFieldRegister, ConvolutionSmallTensorUsePolicy, ConvolutionFFTUsePolicy, AbstractKernel}
import cogx.compiler.codegenerator.opencl.cpukernels._
import cogx.compiler.parser.syntaxtree.{RestoreHooks, Operation}
import cogx.compiler.parser.op.UserOpcode

/** Generates OpenCL code for operations that produce multiple fields.
  *
  * @author Greg Snider
  */
private[cogx]
object MultipleFieldGenerator {
  /** Generate an OpenCL kernel for a field.
    *
    * @param operation The Operation which will have a kernel generated for it.
    * @param inputs Inputs to the operation.
    * @param fftUse policy for FFT use in fast convolution.
    * @param smallTensorUse policy for when to use SmallTensorAddressing in convolution.
    * @return OpenCL kernel implementing the operation.
    */
  def apply(operation: Operation, inputs: Array[VirtualFieldRegister],
            fftUse: ConvolutionFFTUsePolicy,
            smallTensorUse: ConvolutionSmallTensorUsePolicy): AbstractKernel =
  {
    val opcode = operation.opcode
    val fieldTypes = operation.outputs.map(_.fieldType).toArray
    opcode match {
      case op: UnpipelinedActuatorOp =>
        val oper = operation.asInstanceOf[RestoreHooks]
        new UnpipelinedActuatorKernel(inputs(0), op, oper.classnameAsSaved, oper.restoreParameters _)
      case op: UnpipelinedVectorActuatorOp =>
        val oper = operation.asInstanceOf[RestoreHooks]
        new UnpipelinedVectorActuatorKernel(inputs(0), op, oper.classnameAsSaved, oper.restoreParameters _)
      case op: UnpipelinedColorActuatorOp =>
        val oper = operation.asInstanceOf[RestoreHooks]
        new UnpipelinedColorActuatorKernel(inputs(0), op, oper.classnameAsSaved, oper.restoreParameters _)
      case op: UserOpcode =>
        new UserOperatorKernel(inputs, op)
      // User GPU kernel:
      case op: UserGPUOpcode =>
        UserKernel(op, inputs, fieldTypes)
      case op: FFT1DOpRI =>
        FFT1DHyperKernel(inputs, op, fieldTypes)
      case op: InverseFFT1DOpRI =>
        FFT1DHyperKernel(inputs, op, fieldTypes)
      case op: FFT2DOpRI =>
        FFT2DHyperKernel(inputs, op, fieldTypes)
      case op: InverseFFT2DOpRI =>
        FFT2DHyperKernel(inputs, op, fieldTypes)
      case op: FFT3DOpRI =>
        FFT3DHyperKernel(inputs, op, fieldTypes)
      case op: InverseFFT3DOpRI =>
        FFT3DHyperKernel(inputs, op, fieldTypes)
      case x =>
        throw new RuntimeException("Compiler not done yet: " + x)
    }
  }

}