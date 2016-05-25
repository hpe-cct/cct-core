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

import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, AddressingMode, HyperKernel}
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel._
import cogx.platform.types._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.parser.op.WarpOp

/** Warps a 2D scalar, vector or matrix field, guided by a vector field
  * called "the guide." The guide must either be zero-dimensional, in which
  * case the input field is translated uniformly, or must be a 2D vector
  * field with exactly the same shape as the input.
  *
  * The guiding vector with value (v1, v2) at a given point (row, col) extracts
  * the element at location (row - v1, col - v2) as its output. If that location
  * falls outside of the field, the BorderPolicy attached to the the opcode
  * determines how the missing value is computed. If either of the guide vector
  * components v1 and v2 is non-integral, bilinear interpolation is used to
  * to determine the approximate value.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param in The two input virtual field registers driving this kernel.
  * @param operation The binary opcode for this operation.
  * @param resultType The FieldType of the result of this kernel.
  * @param addressMode The addressing mode of this kernel.
  */
private[cogx]
class Warp2DHyperKernel private (in: Array[VirtualFieldRegister],
                                 operation: WarpOp,
                                 resultType: FieldType,
                                 addressMode: AddressingMode)
        extends HyperKernel(operation, in, resultType, addressMode)
{
  val guide = in(1)
  val guideType = guide.fieldType
  val guideIsSingleton = guideType.fieldShape.dimensions == 0
  val borderPolicy = operation.borderPolicy

  val readType = addressMode.clType(resultType).name
  val readZero = addressMode.clType(resultType).zero

  val code = new StringBuffer
  code append "\n"

  val inRows = in(0).fieldType.rows
  val inColumns = in(0).fieldType.columns

  // Read guide vector (guideRow, guideCol)

  // The task of reading in the 0D guide vector is surprisingly difficult:
  // the addressing mode, which really should govern thread responsibilities
  // and output addressing modes, also governs input reads.  In SmallTensorAddressing,
  // the guide-read job is relatively simple, since the guide vector is also a "small
  // tensor."  However, in TensorElementAddressing, the output _tensorElement
  // comes into play to mess up the readPoint operation.  The kludgy approach
  // taken here is to define a _tensorElement and tensorElement within a
  // confined scope to force the index value.  Since the guide input field is
  // 0D, any defined _row, _column and _layer for the output will be ignored.

  addressMode match {
    case SmallTensorAddressing =>
      // Read guide vector (guideRow, guideCol)
      if (guideIsSingleton) {
        code append "        float2 guideVector = readPoint(@in1);"
        code append "        float guideRow = guideVector.x;\n"
        code append "        float guideCol = guideVector.y;\n"
      }
      else {
        code append "        float2 guideVector = read(@in1);\n"
        code append "        float guideRow = guideVector.x;\n"
        code append "        float guideCol = guideVector.y;\n"
      }
    case TensorElementAddressing =>
      if (guideIsSingleton) {
        code append "        float guideRow = 0.0f, guideCol = 0.0f;\n"
        code append "        { // begin restricted scope for _tensorElement \n"
        code append "            int _tensorElement = 0; int tensorElement = 0;\n"
        code append "            guideRow = readPoint(@in1);\n"
        code append "            _tensorElement = 1; tensorElement = 1;\n"
        code append "            guideCol = readPoint(@in1);\n"
        code append "        } // end restricted scope for _tensorElement \n"
      }
      else {
        code append setLayerRowColumn(guideType, "_layer", "_row", "_column")
        code append "        tensorElement = 0;\n"
        code append "        float guideRow = readNonlocal(@in1);\n"
        code append "        tensorElement = 1;\n"
        code append "        float guideCol = readNonlocal(@in1);\n"
      }

    case x => throw new RuntimeException("Unsupported addressing mode: " + x)
  }
  code append "\n"

  // Note: this is not the same tensorElement as was used to read the guide above
  if (addressing == TensorElementAddressing)
    code.append("        tensorElement = _tensorElement;\n")

  // Find source location (srcRowReal, srcColReal) by subtracting guide vector
  // from our (row, col) location. Then compute (srcRow, srcCol) by truncating
  // fractional part.

  // The bilinear interpolation is performed over the values at locations:
  //
  // (srcRow0, srcCol0)
  // (srcRow0, srcCol1)
  // (srcRow1, srcCol0)
  // (srcRow1, srcCol1)

  code append "        float srcRowReal = _row - guideRow;\n"
  code append "        float srcColReal = _column - guideCol;\n"
  code append "        int srcRow0 = floor(srcRowReal);\n"
  code append "        int srcCol0 = floor(srcColReal);\n"
  code append "        int srcRow1 = srcRow0 + 1;\n"
  code append "        int srcCol1 = srcCol0 + 1;\n"
  code append "        float srcRowRem = srcRowReal - srcRow0;\n"
  code append "        float srcColRem = srcColReal - srcCol0;\n"
  code append "\n"

  // If we're outside the bounds of the input, implement the configured border
  // policy.
  borderPolicy match {
    case BorderCyclic =>
      code append s"        while (srcRow0 < 0) srcRow0 += $inRows;\n"
      code append s"        while (srcRow0 >= $inRows) srcRow0 -= $inRows;\n"
      code append s"        while (srcCol0 < 0) srcCol0 += $inColumns;\n"
      code append s"        while (srcCol0 >= $inColumns) srcCol0 -= $inColumns;\n"
      code append s"        while (srcRow1 < 0) srcRow1 += $inRows;\n"
      code append s"        while (srcRow1 >= $inRows) srcRow1 -= $inRows;\n"
      code append s"        while (srcCol1 < 0) srcCol1 += $inColumns;\n"
      code append s"        while (srcCol1 >= $inColumns) srcCol1 -= $inColumns;\n"
    case BorderZero =>
      // Additional processing is needed if any of the 4 locations referenced is
      // outside the input field.
      code append s"        int outOfBounds = 0;\n"
      code append s"        outOfBounds += srcRow0 < 0;\n"
      code append s"        outOfBounds += srcRow1 >= $inRows;\n"
      code append s"        outOfBounds += srcCol0 < 0;\n"
      code append s"        outOfBounds += srcCol1 >= $inColumns;\n"
    case BorderClamp =>
      code append s"        srcRow0 = max(0, min(srcRow0, $inRows - 1));\n"
      code append s"        srcCol0 = max(0, min(srcCol0, $inColumns - 1));\n"
      code append s"        srcRow1 = max(0, min(srcRow1, $inRows - 1));\n"
      code append s"        srcCol1 = max(0, min(srcCol1, $inColumns - 1));\n"
    case x =>
      throw new RuntimeException("Unsupported translate border policy: " + x)
  }
  code append "\n"

  // Compute the bilinear interpolation coefficients.
  //
  // We view the elements and their coefficients as follows:
  //
  //               col   col+1
  //      row ->   f00    f01
  //  row + 1 ->   f10    f11
  //
  // Compute the bilinear interpolation coefficients.
  code append s"        $readType a00 = (1.0f - srcColRem) * (1.0f - srcRowRem);\n"
  code append s"        $readType a01 = (srcColRem) * (1.0f - srcRowRem);\n"
  code append s"        $readType a10 = (1.0f - srcColRem) * (srcRowRem);\n"
  code append s"        $readType a11 = (srcColRem) * (srcRowRem);\n"
  code append "\n"

  // Loop over all elements of the output tensor, performing bilinear
  // interpolation of the input.
  if (borderPolicy == BorderZero)
    code append "    if (outOfBounds == 0) {\n"
  code append setLayerRowColumn(fieldType, null, "srcRow0", "srcCol0")
  code append s"        $readType f00 = readNonlocal(@in0);\n"
  code append s"        row = srcRow1;\n"
  code append s"        $readType f10 = readNonlocal(@in0);\n"
  code append s"        column = srcCol1;\n"
  code append s"        $readType f11 = readNonlocal(@in0);\n"
  code append s"        row = srcRow0;\n"
  code append s"        $readType f01 = readNonlocal(@in0);\n"
  code append s"        $readType interp = a00*f00 + a01*f01 + a10*f10 + a11*f11;\n"
  code append s"        @out0 = interp;\n"
  if (borderPolicy == BorderZero) {
    code append s"    } else {\n"
    code append s"        $readType f00 = $readZero;\n"
    code append s"        $readType f01 = $readZero;\n"
    code append s"        $readType f10 = $readZero;\n"
    code append s"        $readType f11 = $readZero;\n"
    code append s"        if (srcRow0 >= 0 && srcRow0 < $inRows && srcCol0 >= 0 && srcCol0 < $inColumns) {\n"
    code append s"            row = srcRow0;\n"
    code append s"            column = srcCol0;\n"
    code append s"            f00 = readNonlocal(@in0);\n"
    code append s"        }\n"
    code append s"        if (srcRow0 >= 0 && srcRow0 < $inRows && srcCol1 >= 0 && srcCol1 < $inColumns) {\n"
    code append s"            row = srcRow0;\n"
    code append s"            column = srcCol1;\n"
    code append s"            f01 = readNonlocal(@in0);\n"
    code append s"        }\n"
    code append s"        if (srcRow1 >= 0 && srcRow1 < $inRows && srcCol0 >= 0 && srcCol0 < $inColumns) {\n"
    code append s"            row = srcRow1;\n"
    code append s"            column = srcCol0;\n"
    code append s"            f10 = readNonlocal(@in0);\n"
    code append s"        }\n"
    code append s"        if (srcRow1 >= 0 && srcRow1 < $inRows && srcCol1 >= 0 && srcCol1 < $inColumns) {\n"
    code append s"            row = srcRow1;\n"
    code append s"            column = srcCol1;\n"
    code append s"            f11 = readNonlocal(@in0);\n"
    code append s"        }\n"
    code append s"        $readType interp = a00*f00 + a01*f01 + a10*f10 + a11*f11;\n"
    code append s"        @out0 = interp;\n"
    code append s"    }\n"
  }

  addCode(code.toString)
//      debugCompile
}

/** Factory object for creating kernels of this type.
  */
private[cogx]
object Warp2DHyperKernel {

  /** Create a hyperkernel that translates, perhaps non-uniformly, a 2D tensor field.
    *
    * @param in The virtual field registers for the input field to be warped,
    * and for the translation-specifying guide field.
    * @param operation The WarpOp for this operation, with its BorderPolicy.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: Array[VirtualFieldRegister], operation: WarpOp, resultType: FieldType):
  HyperKernel =
  {
    require(in.length == 2)
    val inType = in(0).fieldType
    require(inType.dimensions == 2)
    val guide = in(1)
    val guideType = guide.fieldType
    require(guideType.tensorShape.dimensions == 1)
    val guideIsSingleton = guideType.fieldShape.dimensions == 0

    val expectedResultType = if (guideIsSingleton) inType else inType.resize(guideType.fieldShape)
    require(expectedResultType == resultType)

    val addressing =
      if (isSmallTensorField(resultType))
        SmallTensorAddressing
      else
        TensorElementAddressing

    new Warp2DHyperKernel(in, operation, resultType, addressing)
  }
}
