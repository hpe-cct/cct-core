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

package cogx.compiler.parser.syntaxtree

import cogx.compiler.parser.semantics.{ResultType, SemanticError}
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op._
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.hyperkernels.{ConvolveToSmallFieldPipelinedTiledHyperKernel, ConvolveTiledHyperKernel, ConvolveHyperKernel}
import cogx.compiler.parser.op.ConvolveOp
import cogx.compiler.parser.op.SubfieldOp
import cogx.compiler.parser.op.ScalarMatrixConvolve2DOp
import cogx.compiler.parser.op.WarpOp

/** Factory for creating binary operators. This checks types to make sure
  * the operands may be legally combined, and computes the result type using
  * the Cog rules.
  *
  * @author Greg Snider
  */
private[cogx]
object BinaryOperator extends SemanticError {

  private def requireFieldDim(inType: FieldType, dim: Int) {
    if (inType.dimensions != dim)
      fieldDimensionError(inType.dimensions, dim)
  }

  private def requireTensorDim(inType: FieldType, tensorDim: Int) {
    if (inType.tensorOrder != tensorDim)
      tensorDimensionError(inType.tensorOrder, tensorDim)
  }

  /** Create a built-in binary operator operating on two fields, performing
    * full semantic checking.
    *
    * @param opcode The operation performed on the two fields.
    * @param in1 First input field.
    * @param in2 Second input field.
    *
    * @author Greg Snider
    */
  def apply(opcode: BinaryOpcode, in1: Field, in2: Field): Field = {
    val in1Type = in1.fieldType
    val in2Type = in2.fieldType

    /** Semantic check: is the operation permitted for this size/type of input? */
    opcode match {
      case ConvolveOp(borderPolicy, filterOrientation, samplingPolicy, vectorMode, batchSize) =>
        val in1Dim = in1Type.dimensions
        val in2Dim = in2Type.dimensions
        if (in1Dim < 1 || in1Dim > 2)
          fieldDimensionError(in1Type.dimensions)
        if (in1Dim != in2Dim)
          error("1st field's dimension " + in1Dim +
                  " must equal 2nd field's dimension " +in2Dim)
        val imageDepth = in1Type.tensorShape.points
        if (imageDepth % batchSize != 0)
          error("Image depth " + imageDepth + " must be a multiple of " +
            "the batchSize " + batchSize)
        val singleImageDepth = imageDepth / batchSize
        val filterDepth = in2Type.tensorShape.points
        vectorMode match {
          case ProjectFrame =>
            if (filterDepth % singleImageDepth != 0)
              error("Filter depth " + filterDepth + " must be a multiple of " +
                "the single image depth " + singleImageDepth)
          case ProjectFrameBlockReduceSum =>
            if (filterDepth % singleImageDepth != 0)
              error("Filter depth " + filterDepth + " must be a multiple of " +
                "the single image depth " + singleImageDepth)
          case BackProjectFrame =>
            if (filterDepth % singleImageDepth != 0)
              error("Filter depth " + filterDepth + " must be a multiple of " +
                "the single image depth " + singleImageDepth)
          case _ =>
        }

      case ConvolveRows2DOp(borderPolicy, filterOrientation) =>
        val in1Dim = in1Type.dimensions
        val in2Dim = in2Type.dimensions
        if (in1Dim != 2)
          fieldDimensionError(in1Type.dimensions)
        if (in2Dim != 1)
          fieldDimensionError(in2Type.dimensions)

      case ConvolveColumns2DOp(borderPolicy, filterOrientation) =>
        val in1Dim = in1Type.dimensions
        val in2Dim = in2Type.dimensions
        if (in1Dim != 2)
          fieldDimensionError(in1Type.dimensions)
        if (in2Dim != 1)
          fieldDimensionError(in2Type.dimensions)

      case ScalarMatrixConvolve2DOp(borderPolicy, filterOrientation, samplingPolicy) =>
        // Semantic checks taken from ScalarMatrixHyperKernel factory
        borderPolicy match {
          case BorderClamp =>
          case BorderCyclic =>
          case BorderZero =>
          case BorderFull =>
            error("unsupported: " + borderPolicy)
          case BorderValid =>
            error("unsupported: " + borderPolicy)
        }
        if (samplingPolicy != NoSamplingConvolution)
          error("sampling not supported yet")
        if(filterOrientation != CrossCorrelationOrientation)
          error("convolution not supported yet")
        val scalarType = in1Type
        requireFieldDim(scalarType, 2)
        requireTensorDim(scalarType, 0)
        val matrixType = in2Type
        requireFieldDim(matrixType, 2)
        requireTensorDim(matrixType, 2)
        if (matrixType.tensorShape(0) != matrixType.tensorShape(1))
          error("matrix in 2nd field must be square: " + matrixType.tensorShape)
        if (matrixType.tensorShape(0) % 2 != 1)
          error("matrix in field must have odd size: " + matrixType.tensorShape)
      case SolveOp =>
        if (in1Type.tensorShape != Shape(2,2))
          error("Only 2x2 tensors supported, found: " + in1Type.tensorShape)
        if (in2Type.tensorShape != Shape(2))
          error("Only length-2 vector supported, found: " + in2Type.tensorShape)
        if (in1Type.fieldShape != in2Type.fieldShape)
          typeError(opcode, in1Type, in2Type)
      case op: MatrixTransformMatrixOp =>
        requireTensorDim(in1Type, 2)
        requireTensorDim(in2Type, 2)
        val dotProductSizePerIn1 =
          if (op.transposeIn1) in1Type.tensorRows else in1Type.tensorColumns
        val dotProductSizePerIn2 =
          if (op.transposeIn2) in2Type.tensorColumns else in2Type.tensorRows
        if (dotProductSizePerIn1 != dotProductSizePerIn2)
          error("Incompatible matrix shapes: " + in1Type.tensorShape +
                  " * " + in2Type.tensorShape + ", op(transposeIn1,transposeIn2) = " + op)
      case MatrixTransformVectorOp =>
        requireTensorDim(in1Type, 2)
        requireTensorDim(in2Type, 1)
        if (in1Type.tensorColumns != in2Type.tensorColumns)
          error("Incompatible matrix / vector shapes: " + in1Type.tensorShape +
            " * " + in2Type.tensorShape)
      case CrossDotOp =>
        requireTensorDim(in2Type, 0)
        if (in1Type.tensorShape != in2Type.fieldShape)
          error("1st field's tensor shape " + in1Type.tensorShape +
                " must equal 2nd field's field shape " + in2Type.fieldShape)
      case ReverseCrossDotOp =>
        requireTensorDim(in2Type, 0)
        if (in1Type.fieldShape != in2Type.fieldShape)
          error("The 2 operand field shapes must match.  Found: " +
                  in1Type.tensorShape + " and " + in2Type.fieldShape)
      case SubfieldOp(shape, borderPolicy) =>
        val inDim = in1Type.dimensions
        if (inDim < 1 || inDim > 2)
          fieldDimensionError(in1Type.dimensions)
        val guideType = in2Type
        requireFieldDim(guideType, 0)
        requireTensorDim(guideType, 1)
        val guideLength = guideType.tensorShape(0)
        if (guideLength != inDim)
          typeError(opcode, in1Type, guideType)
      case OrientedNonMaximumSuppressionOp =>
        requireTensorDim(in1Type, 0)
        requireTensorDim(in2Type, 0)
        if (!(in1Type == in2Type))
          typeError(opcode, in1Type, in2Type)
      case WarpOp(borderPolicy) =>
        requireFieldDim(in1Type, 2)
        val guideType = in2Type
        if (guideType.tensorShape.dimensions != 1)
          typeError(opcode, in1Type, guideType)
        val guideIsSingleton = guideType.fieldShape.dimensions == 0
      case PushOp =>
        if (in1Type.drop(1) != in2Type)
          typeError(opcode, in1Type, in2Type)
      case SlicePointOp =>
        if (in2Type != new FieldType(Shape(), Shape(), Float32))
          typeError(opcode, in1Type, in2Type)
      case JoinOp =>
        if (in1Type.drop(1) != in2Type.drop(1))
          typeError(opcode, in1Type, in2Type)
      case OuterProductOp =>
        if (!isTensor0RealField(in1Type) || !isTensor0RealField(in2Type) ||
            (in1Type.dimensions + in2Type.dimensions > 3))
          typeError(opcode, in1Type, in2Type)
      case PolarToComplexOp =>
        if (!isRealField(in1Type) || (in1Type != in2Type))
          typeError(opcode, in1Type, in2Type)
        if (in1Type.tensorOrder > 1)
          error("Cannot create complex field of tensor order " +
                  in1Type.tensorOrder + " (max is 1).")
      case RealImaginaryToComplexOp =>
        if (!isRealField(in1Type) || (in1Type != in2Type))
          typeError(opcode, in1Type, in2Type)
        if (in1Type.tensorOrder > 1)
          error("Cannot create complex field of tensor order " +
                  in1Type.tensorOrder + " (max is 1).")
      case TensorDotOp =>
        if (!isRealField(in1Type))
          typeError(opcode, in1Type, in2Type)
        if (in1Type != in2Type && to0D(in1Type) != in2Type)
          typeError(opcode, in1Type, in2Type)
      case VectorElementsOp =>
        //operand must be a vector field
        if(in1Type.tensorOrder != 1) typeError(opcode, in1Type, in2Type)
        //indices must be a vector field
        if(in2Type.tensorOrder != 1) typeError(opcode, in1Type, in2Type)
        //indices must either be the same FieldShape as input or 0D
        if(in2Type.fieldShape != in1Type.fieldShape &&
                in2Type.fieldShape.dimensions != 0) typeError(opcode, in1Type, in2Type)
      case default => // OK by default
    }

    /** The FieldType of the result of the operation */
    val outType: FieldType = opcode match {
      // If you add an operation that produces a different type than the
      // default type, add that here. E.g. "case MyOp => new NodeType(...)"
      case op: ConvolveTiledOp =>
        ConvolveTiledHyperKernel.outFieldType(in1Type, in2Type)
      case op: ConvolveToSmallFieldTiledOp =>
        ConvolveToSmallFieldPipelinedTiledHyperKernel.outFieldType(in1Type, in2Type)
      case ConvolveOp(borderPolicy, filterOrientation, samplingPolicy, vectorMode, batchSize) =>
        ConvolveHyperKernel.outputFieldType(in1Type, in2Type,
                                                   borderPolicy, samplingPolicy, vectorMode, batchSize)
      case ScalarMatrixConvolve2DOp(borderPolicy, filterOrientation, samplingPolicy) =>
        in1Type.resizeTensor(Shape(in2Type.fieldShape.points))
      case SolveOp =>
        in2Type
      case op: MatrixTransformMatrixOp =>
        val resultRows =
          if (op.transposeIn1) in1Type.tensorColumns else in1Type.tensorRows
        val resultColumns =
          if (op.transposeIn2) in2Type.tensorRows else in2Type.tensorColumns
        val resultTensorShape = Shape(resultRows, resultColumns)
        if (in1Type.dimensions == 0)
          in2Type.resizeTensor(resultTensorShape)
        else
          in1Type.resizeTensor(resultTensorShape)
      case MatrixTransformVectorOp =>
        val resultTensorShape = Shape(in1Type.tensorRows)
        if (in1Type.dimensions == 0)
          in2Type.resizeTensor(resultTensorShape)
        else
          in1Type.resizeTensor(resultTensorShape)
      case CrossDotOp =>
        in1Type.resizeTensor(Shape())
      case ReverseCrossDotOp =>
        new FieldType(in1Type.tensorShape, Shape(), in1Type.elementType)
      case SubfieldOp(shape, borderPolicy) =>
        in1Type.resize(shape)
      case WarpOp(borderPolicy) =>
        val guideIsSingleton = in2Type.fieldShape.dimensions == 0
        if (guideIsSingleton) in1Type else in1Type.resize(in2Type.fieldShape)
      case PushOp =>
        in1Type
      case SlicePointOp =>
        in1Type.drop(1)
      case JoinOp =>
        val fieldShape = in1Type.fieldShape join in2Type.fieldShape
        in1Type.resize(fieldShape)
      case OuterProductOp =>
        val resultShape = in1Type.fieldShape concatenate in2Type.fieldShape
        new FieldType(resultShape, Shape(), in1Type.elementType)
      case PolarToComplexOp =>
        toComplex(in1Type)
      case RealImaginaryToComplexOp =>
        toComplex(in1Type)
      case TensorDotOp =>
        in1Type.resizeTensor(Shape())
      case OrientedNonMaximumSuppressionOp =>
        in1Type
      case ConvolveRows2DOp(borderPolicy, filterOrientation) =>
        in1Type
      case ConvolveColumns2DOp(borderPolicy, filterOrientation) =>
        in1Type
      case VectorElementsOp =>
        new FieldType(in1Type.fieldShape, in2Type.tensorShape, in1Type.elementType)
      case default =>
        ResultType(in1Type, in2Type) match {
          case Some(result) =>
            result
          case None =>
            typeError(opcode, in1Type, in2Type)
            null
        }
    }

    /** Produce the result Field */
    Field(opcode, Array(in1, in2), outType)
  }
}
