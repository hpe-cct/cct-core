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

import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{ClFFTDirection, Inverse}
import cogx.compiler.parser.op._
import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.algebra.real.Logarithm

/** Factory for creating unary operators.
  *
  * This does type casting so that the resultType of the operator is narrowed
  * precisely, permitting the user to see static typing.
  *
  * NOTE: IF YOU ARE CREATING A NEW OPCODE, YOU MUST DO 2 THINGS:
  *
  * 1. Semantic checking. Add an entry in the apply(UnaryOpcode, Field) method
  * to verify that the input field to the operator is valid.
  *
  * 2. Return type generation. Add a case in the `outType` statement to generate
  * the proper result type for your opcode.
  *
  * @author Greg Snider
  */
private[cogx]
object UnaryOperator extends SemanticError with Logarithm {

  private def requireFieldDim(inType: FieldType, dim: Int) {
    if (inType.dimensions != dim)
      fieldDimensionError(inType.dimensions, dim)
  }

  private def requireTensorDim(inType: FieldType, tensorDim: Int) {
    if (inType.tensorOrder != tensorDim)
      tensorDimensionError(inType.tensorOrder, tensorDim)
  }

  private def require2DScalarField(inType: FieldType) {
    requireFieldDim(inType, 2)
    requireTensorDim(inType, 0)
  }

  /** SEMANTIC CHECKING: is the operation permitted for this size/type of input?
    *
    * @param operation The operation performed on a field.
    * @param in The input field.
    * @return The output field.
    * @author Greg Snider and Dick Carter
    */
  def apply(operation: UnaryOpcode, in: Field): Field = {
    val inType = in.fieldType

    operation match {
      case op: FFTOp =>
        checkFFT(inType, op.dimensions, op.dir)
       case op: FFT2DSubOp =>
        checkFFT(inType, 2, op.dir)
       case op: FFT3DSubOp =>
        checkFFT(inType, 3, op.dir)
      case DCT2DOp =>
        checkDCT(inType)
      case DCTInverse2DOp =>
        checkDCT(inType)
      case DCTTransposed2DOp =>
        checkDCT(inType)
      case DCTInverseTransposed2DOp =>
        checkDCT(inType)
      case Transpose2DOp =>
        if (inType.fieldShape.dimensions != 2)
          error("Only 2D fields may be transposed")
        if (!isSmallTensorField(inType))
          error("Only small tensor fields may be transposed")
      case ConditionNumberOp =>
        if (inType.tensorShape != Shape(2, 2))
          error("Condition numbers can only be computed for matrix fields " +
                  "with 2 x 2 matrices")
      case ColorFieldToVectorFieldOp =>
        if (!isColorField(inType))
          typeError(operation, inType)
      case DeterminantOp =>
        if (inType.fieldShape.dimensions != 2)
          error("Only 2D fields may be transposed")
        if (inType.tensorShape != Shape(2, 2))
          error("Determinants computed only on matrix fields with 2 x 2 matrices")
      case VectorFieldToColorFieldOp =>
        if (inType.tensorOrder != 1)
          typeError(operation, inType)
        if (inType.tensorShape.points != 3)
          typeError(operation, inType)
      case op: ColorPlaneOp =>
        requireFieldDim(inType, 2)
        if (!isColorField(inType))
          typeError(operation, inType)
      case MatrixInvertOp =>
        val matrixShape = inType.tensorShape
        requireTensorDim(inType, 2)
        if (matrixShape(0) != matrixShape(1))
          error("Only square matrices can be inverted")
      case op: LocalMaxPosition2DOp =>
        val filter = op.kernel
        if (filter.rows != filter.columns || filter.rows % 2 != 1)
          error("Square odd-length kernel required, found: " + filter.shape)
        require2DScalarField(inType)
      case op: LocalMinPosition2DOp =>
        val filter = op.kernel
        if (filter.rows != filter.columns || filter.rows % 2 != 1)
          error("Square odd-length kernel required, found: " + filter.shape)
        require2DScalarField(inType)
      case MatrixTransposeOp =>
        if (!isRealField(inType))
          typeError(operation, inType)
        requireTensorDim(inType, 2)
      case VectorTransposeOp =>
        if (!isRealField(inType))
          typeError(operation, inType)
        requireTensorDim(inType, 1)
      case WinnerTakeAllOp =>
        if (!isRealField(inType))
          typeError(operation, inType)
      case MaxPositionOp =>
        if (!isTensor0RealField(inType))
          typeError(operation, inType)
      case op: TensorReductionOp =>
        // Only TensorReduceSum is supported universally.  Min and Max are
        // not supported on complex fields.
        op match {
          case TensorReduceSumOp(factor) =>
          case _ =>  if (!isRealField(inType))
            typeError(operation, inType)
        }
        if (inType.tensorOrder > 1 && inType.tensorShape.points != op.factor)
          error("Block tensor reduction is only supported for vector fields.")
        if (inType.tensorShape.points % op.factor != 0)
          error("Vector length " + inType.tensorShape.points +
                " must be a multiple of the block tensor reduction factor " +
                  op.factor)
      case TensorSliceOp(index) =>
        if (inType.tensorOrder < 1 || inType.tensorOrder > 2)
          tensorDimensionError(inType.tensorOrder)
        if (index < 0 && index >= inType.tensorShape(0))
          error("Index " + index + " is outside of allowed range: [0," +
                  inType.tensorShape(0) + "]")
      case DomainTransformRowsOp(spaceSigma, rangeSigma) =>
        if (!isColorField(inType)) {
          if (inType.fieldShape.dimensions != 2)
            error("This operation only works on 2D fields")
        }
      case NonMaximumSuppressionOp =>
        requireFieldDim(inType, 2)
        if (inType.tensorShape.dimensions > 1)
          tensorDimensionError(inType.tensorOrder)
      case TrimOp(resultShape) =>
        for (i <- 0 until inType.dimensions)
          if (resultShape(i) > inType.fieldShape(i))
            typeError(operation, inType)
      case BackwardDivergenceOp =>
        if (inType.tensorShape != Shape(2))
          error("input must be a length-2 vector field")
        requireFieldDim(inType, 2)
      case op: LocalMax2DOp =>
        val filter = op.kernel
        if (filter.rows != filter.columns || filter.rows % 2 != 1)
          error("Square odd-length kernel required, found: " + filter.shape)
        require2DScalarField(inType)
      case op: LocalMin2DOp =>
        val filter = op.kernel
        if (filter.rows != filter.columns || filter.rows % 2 != 1)
          error("Square odd-length kernel required, found: " + filter.shape)
        require2DScalarField(inType)
      // Forward and Central Gradient kernels can't input VectorFields yet
      case MedianFilterOp =>
        require2DScalarField(inType)
      case CentralGradientOp =>
        require2DScalarField(inType)
      case ForwardGradientOp =>
        require2DScalarField(inType)
      case BackwardGradientOp =>
        require2DScalarField(inType)
      case ExpandBorderOp(resultShape: Shape, borderPolicy: BorderPolicy) =>
        if (inType.dimensions == 0)
          fieldDimensionError(inType.dimensions)
        if (inType.dimensions != resultShape.dimensions)
          fieldDimensionError(inType.dimensions, resultShape.dimensions)
        for (i <- 0 until resultShape.dimensions)
          if (resultShape(i) < inType.fieldShape(i))
            typeError(operation, inType)
      case ReplicateOp(fieldShape) =>
        if (inType.dimensions > 2)
          fieldDimensionError(inType.dimensions)
        if (!isRealField(inType))
          typeError(operation, inType)
      case op: ReshapeOp =>
        val inPoints = inType.fieldShape.points * inType.tensorShape.points
        val outPoints = op.fieldShape.points * op.tensorShape.points
        if (inPoints != outPoints)
          error(s"Reshape to different size field. Input field points = $inPoints (inType = $inType), Output field points = $outPoints (op = $operation)")
      case SubfieldsOp(diameter) =>
        val MaxDiameter = 15
        require(diameter <= MaxDiameter, "diameter too large for subfields")
        require2DScalarField(inType)
        for (i <- 0 until inType.dimensions)
          if (inType.fieldShape(i) < diameter)
            typeError(operation, inType)
      case SubspaceOp(indices) =>
        requireFieldDim(inType, indices.length)
        for (dim <- 0 until indices.length) {
          val range = indices(dim)
          if (range.step != 1)
            typeError(operation, inType)
          if (range.start < 0 || range.end > inType.fieldShape(dim))
            typeError(operation, inType)
        }
      case SliceOp(index) =>
        val maxIndex = inType.fieldShape(0) - 1
        if (index < 0 || index > maxIndex) typeError(operation, inType)
      case NormalizeL1Op =>
        if (!isTensor0RealField(inType)) typeError(operation, inType)
      case NormalizeL2Op =>
        if (!isTensor0RealField(inType)) typeError(operation, inType)
      case FieldReduceMinOp =>
        if (!isRealField(inType)) typeError(operation, inType)
      case FieldReduceMaxOp =>
        if (!isRealField(inType)) typeError(operation, inType)
      case FieldReduceSumOp =>
        if (!isRealField(inType)) typeError(operation, inType)
      case FieldReduceMedianOp =>
        if (!isTensor0RealField(inType)) typeError(operation, inType)
      case RealToComplexOp =>
        if (!isRealField(inType))
          typeError(operation, inType)
      case op: ComplexToRealOp =>
        if (!isComplexField(inType))
          typeError(operation, inType)
      case default => // OK by default
    }

    /** RETURN TYPE for operator.
      *
      * The FieldType of the result of the operation. By default, the output
      * type of every unary operator is equal to the type of the input. If
      * that's not true for your new operator, you must add a case here to
      * define the proper output type.
      */
    val outType = operation match {
      // If you add an operation that produces a different type than the
      // default type, add that here. E.g. "case MyOp => new NodeType(...)"
      case op: FFTOp =>
        toComplex(inType)
      case op: FFT2DSubOp =>
        toComplex(inType)
      case op: FFT3DSubOp =>
        toComplex(inType)
      case DCTTransposed2DOp =>
        new FieldType(Shape(inType.columns, inType.rows), inType.tensorShape,
          inType.elementType)
      case DCTInverseTransposed2DOp =>
        new FieldType(Shape(inType.columns, inType.rows), inType.tensorShape,
          inType.elementType)
      case DCTRowsOp =>
        inType
      case DCTInverseRowsOp =>
        new FieldType(inType.fieldShape, Shape(), Float32)
      case DeterminantOp =>
        new FieldType(inType.fieldShape, Shape(), Float32)
      case VectorFieldToColorFieldOp =>
        new FieldType(inType.fieldShape, Shape(3), Uint8Pixel)
      case ColorFieldToVectorFieldOp =>
        new FieldType(inType.fieldShape, Shape(3), Float32)
      case op: ColorPlaneOp =>
        new FieldType(inType.fieldShape, Shape(), Float32)
      case op: LocalMinPosition2DOp =>
        inType.resizeTensor(Shape(2))
      case op: LocalMaxPosition2DOp =>
        inType.resizeTensor(Shape(2))
      case ConditionNumberOp =>
        new FieldType(inType.fieldShape, Shape(), Float32)
      case MatrixTransposeOp =>
        val matrixShape = inType.tensorShape
        val resultMatrixShape = Shape(matrixShape(1), matrixShape(0))
        inType.resizeTensor(resultMatrixShape)
      case VectorTransposeOp =>
        val vectorLength = inType.tensorShape(0)
        val matrixShape = Shape(1, vectorLength)
        inType.resizeTensor(matrixShape)
      case MaxPositionOp =>
        new FieldType(Shape(), Shape(inType.dimensions), inType.elementType)
      case op: TensorReductionOp =>
        if (inType.tensorShape.points == op.factor)
          inType.resizeTensor(Shape())
        else
          inType.resizeTensor(Shape(inType.tensorShape.points / op.factor))
      case TensorSliceOp(index) =>
        inType.resizeTensor(inType.tensorShape.drop(1))
      case DomainTransformRowsOp(spaceSigma, rangeSigma) =>
        if (isColorField(inType))
          // color field --> scalar field
          new FieldType(inType.fieldShape, Shape(), Float32)
        else
          // tensor field --> tensor field
          inType
      case Transpose2DOp =>
        val outShape = Shape(inType.fieldShape(1), inType.fieldShape(0))
        new FieldType(outShape, inType.tensorShape, inType.elementType)
      case TrimOp(resultShape) =>
        inType.resize(resultShape)
      case BackwardDivergenceOp =>
        inType.resizeTensor(Shape())
      case CentralGradientOp =>
        inType.resizeTensor(Shape(2))
      case ForwardGradientOp =>
        inType.resizeTensor(Shape(2))
      case BackwardGradientOp =>
        inType.resizeTensor(Shape(2))
      case ExpandBorderOp(resultShape: Shape, borderPolicy: BorderPolicy) =>
        inType.resize(resultShape)
      case ReplicateOp(resultShape: Shape) =>
        new FieldType(resultShape, inType.fieldShape, inType.elementType)
      case op: ReshapeOp =>
        new FieldType(op.fieldShape, op.tensorShape, inType.elementType)
      case SubfieldsOp(diameter) =>
        val outFieldShape = Shape(diameter, diameter)
        val inRows = inType.fieldShape(0)
        val inCols = inType.fieldShape(1)
        val vectorLength = (inRows - diameter + 1) * (inCols - diameter + 1)
        val outTensorShape = Shape(vectorLength)
        new FieldType(outFieldShape, outTensorShape, inType.elementType)
      case SubspaceOp(indices) =>
        val resultShape = new Shape(indices.map(_.length).toArray)
        inType.resize(resultShape)
      case SliceOp(index) => inType.drop(1)
      case DownsampleOp(factor: Int, phase: Int) => inType.downsample(factor)
      case UpsampleOp(factor: Int, phase: Int) => inType.upsample(factor)
      case SupersampleOp => inType.upsample(2)
      case op: FieldReductionOp =>
        inType.resize(Shape())
      case RealToComplexOp => toComplex(inType)
      case op: ComplexToRealOp => toReal(inType)
      case default =>
        inType
    }

    /** Produce the result Field */
    Field(operation, Array(in), outType)
  }

  /** Create a built-in unary operator.
    *
    * @param operation The operation performed on a field.
    * @param in The input field.
    * @author Greg Snider
    */
  def apply(operation: BinaryConstOpcode, in: Field): Field = {
    val outType = operation match {
      // If you add an operation that produces a different type than the
      // default type, add that here. E.g. "case MyOp => new NodeType(...)"
      case default =>
        in.fieldType
    }
    /** Produce the result Field */
    Field(operation, Array(in), outType)
  }

  /** Rule checking for all variants of the DCT. */
  private def checkDCT(inType: FieldType) {
    val MinDCTSize = 256
    val MaxDCTSize = 2048

    if (inType.rows != inType.columns)
      error("DCT requires square inputs (INTERNAL BUG!)")
    if (inType.fieldShape.dimensions != 2)
      error("DCT requires 2D input")
    if (!isPowerOf2(inType.rows) || !isPowerOf2(inType.columns))
      error("DCT requires number of rows and columns to be power of 2")
    if (inType.rows < MinDCTSize || inType.rows > MaxDCTSize)
      error("DCT rows must be in the range [" + MinDCTSize +
              ", " + MaxDCTSize + "]")
    if (inType.columns < MinDCTSize || inType.columns > MaxDCTSize)
      error("DCT columns must be in the range [" + MinDCTSize +
              ", " + MaxDCTSize + "]")
  }
  /** Rule checking for all variants of the FFT. */
  private def checkFFT(inType: FieldType, dimensions: Int, dir: ClFFTDirection) {
    val MinFFTSize = 1
    val MaxFFTSize = 2048

    if (dimensions != inType.dimensions)
      error("FFT op has wrong dimensionality (INTERNAL BUG!)")

    // Forward FFT can take either real or complex input
    if (dir == Inverse && inType.elementType != Complex32)
      error("Expecting complex input to inverse fft, found: " + inType)

    for (d <- 0 until dimensions) {
      val size = inType.fieldShape(d)
      if (!isPowerOf2(size))
        error("FFT requires power-of-2 field sizes, found " +
                size + " in " + inType)
      if (size < MinFFTSize || size > MaxFFTSize)
        error("FFT field size must be in the range [" + MinFFTSize +
                ", " + MaxFFTSize + "], found " + size + " in " + inType)

    }
  }
}