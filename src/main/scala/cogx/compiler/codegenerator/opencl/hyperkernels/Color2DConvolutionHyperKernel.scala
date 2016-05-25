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

import cogx.compiler.codegenerator.opencl.fragments._
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.ConvolveColor2DOp

/** StaticConvolution of a 2D image with a square convolution kernel. This uses
  * border-fill as the default method for handling edge effects.
  *
  * The first input is the image to be filtered. The second input is a constant
  * array holding the weights of the filter kernel, flattened into a 1D array.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The input virtual field registers driving this kernel.
  * @param operation The CannyOp opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  * @param samplingMode The boundary handling policy for the kernel's sampler.
  * @param filterSize The size N of the NxN filter kernel.
  */
@deprecated("Convolution directly on ColorFields no longer supported.", "4.1.1")
private [cogx] class Color2DConvolutionHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: ConvolveColor2DOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode,
                                 samplingMode: SamplingMode,
                                 filterSize: Int)
        extends HyperKernel(operation, in, resultType, addressMode, samplingMode)
{
  val code = new StringBuilder
  // The filter kernel (matrix) is written out in row-major order, but the
  // Image API seems to use (x, y) addressing rather than (row, column)
  // addressing. Therefore we need to be careful here and not accidentally
  // transpose the filter kernel.
  code.append("    float4 sum = (float4) (0.0f, 0.0f, 0.0f, 0.0f);\n")
  code.append("    const int offset = " + (filterSize / 2) + ";\n")
  code.append("    int filterIndex = 0;\n")
  code.append("    for (int rowDelta = -offset; rowDelta <= offset; rowDelta++) {\n")
  code.append("        for (int colDelta = -offset; colDelta <= offset; colDelta++) {\n")
  code.append("            column = filterIndex;\n")
  code.append("            float weight = readNonlocal(@in1);\n")
  code.append("            filterIndex++;\n")
  code.append("            row = _row + rowDelta;\n")
  code.append("            column = _column + colDelta;\n")
  code.append("            float4 pixel = readNonlocal(@in0);\n")
  code.append("            sum += pixel * weight;\n")
  code.append("        }\n")
  code.append("    }\n")
  code append "    @out0 =  sum;\n"

  addCode(code.toString)
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */

@deprecated("Convolution directly on ColorFields no longer supported.", "4.1.1")
private [cogx] object Color2DConvolutionHyperKernel {

  /**
   * Create an kernel that filters an image with a square convolution kernel.
   *
   * @param in The input virtual field registers driving this kernel.
   * @param operation The binary opcode for this operation.
   * @param resultType The FieldType of the result of this kernel.
   * @return Synthesized hyperkernel for the operation.
   */
  def apply(in: Array[VirtualFieldRegister], operation: ConvolveColor2DOp, resultType: FieldType): HyperKernel = {
    require(in.length == 2)
    val in0Type = in(0).fieldType
    require(isColorField(in0Type))
    require(in0Type.dimensions == 2)
    require(resultType == in0Type)

    require(in(1).fieldType.dimensions == 1)
    val filterSize = math.sqrt(operation.kernel.size).round.toInt
    require(filterSize * filterSize == operation.kernel.size, "Square kernel required.")
    require(filterSize % 2 == 1, "Odd size kernel required.")
    require(filterSize >= 3, "Kernel must be at least 3 x 3")

    // Since the output image is the same size as the input image, we have two
    // different ways we can handle the border pixels: border fill or zero
    // fill. The opcode tells us which to do. Border fill is more expensive.
    val samplingMode = operation match {
      case ConvolveColor2DOp(kernel, BorderClamp, _) => ClampToEdge
      case ConvolveColor2DOp(kernel, BorderZero, _) => ClampToZero
      case ConvolveColor2DOp(kernel, BorderCyclic, _) =>
        throw new RuntimeException("Cyclic 2D convolution not supported yet.")
      case ConvolveColor2DOp(kernel, _, _) => throw new RuntimeException("border policy not implemented here")
    }

    val addressing = SmallTensorAddressing
    new Color2DConvolutionHyperKernel(in, operation, resultType, addressing, samplingMode, filterSize)
  }
}
