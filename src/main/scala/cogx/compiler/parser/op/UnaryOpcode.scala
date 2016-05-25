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

package cogx.compiler.parser.op

import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{Inverse, ClFFTDirection, Forward}
import cogx.platform.cpumemory.readerwriter.ScalarFieldReader
import cogx.platform.types.{FilterOrientation, BorderPolicy, Opcode}
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Matrix
import cogx.cogmath.algebra.complex.Complex
import java.util.concurrent.atomic.AtomicLong

/** An operation taking one input, producing a Field.
  *
  * @param name A short description string for the operation (hint:
  *             avoid characters that not legal in an OpenCL kernel name, like '-').
  * @author Greg Snider
  */
private[cogx]
sealed abstract class UnaryOpcode(name: String = "") extends Opcode(name)

// Unary operations that subclass UnaryOpcode directly, in alphabetical order.
// Operations that subclass a subclass of UnaryOpcode are listed separately
// below.

private[cogx] case object AbsOp extends UnaryOpcode
private[cogx] case object AcosOp extends UnaryOpcode
private[cogx] case object AcoshOp extends UnaryOpcode
private[cogx] case object AsinOp extends UnaryOpcode
private[cogx] case object BackwardDivergenceOp extends UnaryOpcode
private[cogx] case object BackwardGradientOp extends UnaryOpcode
private[cogx] case object CannyOp extends UnaryOpcode
private[cogx] case object CentralGradientOp extends UnaryOpcode
private[cogx] case object ConditionNumberOp extends UnaryOpcode
private[cogx] case object CosOp extends UnaryOpcode
private[cogx] case object CoshOp extends UnaryOpcode
private[cogx] case object DeterminantOp extends UnaryOpcode
private[cogx] case class  DomainTransformRowsOp(spaceSigma: Float, rangeSigma: Float) extends UnaryOpcode
private[cogx] case class  DownsampleOp(factor: Int, phase: Int) extends UnaryOpcode(factor.toString)
private[cogx] case class  ExpandBorderOp(resultShape: Shape, borderPolicy: BorderPolicy)
        extends UnaryOpcode
private[cogx] case object ExpOp extends UnaryOpcode
/** Set NaN values in a field to zero. */
private[cogx] case object FilterNaNOp extends UnaryOpcode
private[cogx] case object FloorOp extends UnaryOpcode
private[cogx] case object FlipOp extends UnaryOpcode
private[cogx] case object ForwardGradientOp extends UnaryOpcode
private[cogx] case object LogOp extends UnaryOpcode
private[cogx] case object MatrixInvertOp extends UnaryOpcode
private[cogx] case object MatrixTransposeOp extends UnaryOpcode
private[cogx] case object MaxPositionOp extends UnaryOpcode
private[cogx] case object MedianFilterOp extends UnaryOpcode
private[cogx] case object NonMaximumSuppressionOp extends UnaryOpcode
private[cogx] case object NormalizeL1Op extends UnaryOpcode
private[cogx] case object NormalizeL2Op extends UnaryOpcode
private[cogx] case class RandomOp(bits:Int) extends UnaryOpcode
private[cogx] case object ReciprocalOp extends UnaryOpcode
private[cogx] case object RectifyOp extends UnaryOpcode
private[cogx] case class ReplicateOp(resultShape: Shape) extends UnaryOpcode
private[cogx] case class ReshapeOp(fieldShape: Shape, tensorShape:Shape, checkLegacyReshape: Boolean) extends UnaryOpcode
// Warning: if the 'shifts' data member is an Array, equivalent ShiftHyperKernels will never be 'equal' (CSE issue)
private[cogx] case class ShiftOp(shifts: Seq[Int], borderPolicy: BorderPolicy)
        extends UnaryOpcode(borderPolicy.toString) {
  override def toString: String = "ShiftOp(" + shifts.mkString(",") + "," + borderPolicy + ")"
}
private[cogx] case object SignumOp extends UnaryOpcode
private[cogx] case object SinOp extends UnaryOpcode
private[cogx] case object SinhOp extends UnaryOpcode
private[cogx] case class  SliceOp(index: Int) extends UnaryOpcode
private[cogx] case object SqOp extends UnaryOpcode
private[cogx] case object SqrtOp extends UnaryOpcode
private[cogx] case class SubfieldsOp(diameter: Int) extends UnaryOpcode("" + diameter)
// Warning: if the 'indices' data member is an Array, equivalent SubspaceHyperKernels will never be 'equal' (CSE issue)
private[cogx] case class  SubspaceOp(indices: Seq[Range]) extends UnaryOpcode {
  override def toString: String = "SubspaceOp(" +
    indices.map(range => s"Range[${range.start},${range.last}]").mkString(",") +
    ")"
}
private[cogx] case object SupersampleOp extends UnaryOpcode
private[cogx] case object TanOp extends UnaryOpcode
private[cogx] case object TanhOp extends UnaryOpcode
private[cogx] case class  TensorSliceOp(index: Int) extends UnaryOpcode
private[cogx] case object Transpose2DOp extends UnaryOpcode
private[cogx] case class  TrimOp(resultShape: Shape) extends UnaryOpcode
private[cogx] case object UnaryMinusOp extends UnaryOpcode
private[cogx] case class  UpsampleOp(factor: Int, phase: Int) extends UnaryOpcode(factor.toString)
private[cogx] case object VectorTransposeOp extends UnaryOpcode
private[cogx] case object WinnerTakeAllOp extends UnaryOpcode
private[cogx] case object WinnerTakeAllPointToFieldOp extends UnaryOpcode
private[cogx] case object WinnerTakeAllReduceOp extends UnaryOpcode

/** Copy operation invokes UnaryHyperKernel(CopyOp) and cannot be
  * consolidated by common subexpression elimination.  Cures bug with:
  *
  * B <== A
  * C <== A
  */
private[cogx] case class CopyOp(uniqueId: Long) extends UnaryOpcode

/** Object that produces a stream of unique CopyOps. */
private[cogx] object UniqueCopyOp {
  private val id = new AtomicLong
  def apply() = new CopyOp(id.getAndIncrement)
}

// Other opcodes that subclass a subclass of UnaryOpcode

// Complex-to-real ops
private[cogx] sealed abstract class ComplexToRealOp(function: String = "") extends UnaryOpcode(function)
private[cogx] case object PhaseOp extends ComplexToRealOp
private[cogx] case object OrientationOp extends ComplexToRealOp
private[cogx] case object MagnitudeOp extends ComplexToRealOp
private[cogx] case object RealPartOp extends ComplexToRealOp
private[cogx] case object ImaginaryPartOp extends ComplexToRealOp

// Field reductions.
private[cogx] sealed abstract class FieldReductionOp(name: String = "") extends UnaryOpcode(name)
private[cogx] case object FieldReduceMaxOp extends FieldReductionOp
private[cogx] case object FieldReduceMinOp extends FieldReductionOp
private[cogx] case object FieldReduceSumOp extends FieldReductionOp
private[cogx] case object FieldReduceMedianOp extends FieldReductionOp

// Tensor reductions. These reduce each tensor in a field to a smaller tensor.
private[cogx] sealed abstract class TensorReductionOp(val factor: Int) extends UnaryOpcode("" + factor)
private[cogx] case class TensorReduceMaxOp(_factor: Int) extends TensorReductionOp(_factor)
private[cogx] case class TensorReduceMinOp(_factor: Int) extends TensorReductionOp(_factor)
private[cogx] case class TensorReduceSumOp(_factor: Int) extends TensorReductionOp(_factor)

// Color operations ------------------------------------------------------------

private[cogx] sealed abstract class ColorPlaneOp(plane: String = "") extends UnaryOpcode(plane)
private[cogx] case object ColorPlaneRedOp extends ColorPlaneOp
private[cogx] case object ColorPlaneGreenOp extends ColorPlaneOp
private[cogx] case object ColorPlaneBlueOp extends ColorPlaneOp
private[cogx] case object ColorPlaneLuminanceOp extends ColorPlaneOp
private[cogx] case object ColorFieldToVectorFieldOp extends UnaryOpcode("colorFieldToVectorField")
private[cogx] case object VectorFieldToColorFieldOp extends UnaryOpcode("vectorFieldToColorField")

// Complex operations-----------------------------------------------------------

private[cogx] sealed abstract class ComplexUnaryOp(function: String = "") extends UnaryOpcode(function)
private[cogx] case object ComplexExpOp extends ComplexUnaryOp
private[cogx] case object ConjugateOp extends ComplexUnaryOp
private[cogx] case object ComplexReciprocalOp extends ComplexUnaryOp
private[cogx] case object ComplexUnaryMinusOp extends ComplexUnaryOp
private[cogx] case object RealToComplexOp extends ComplexUnaryOp

/** Complex copy operation invokes ComplexUnaryHyperKernel(ComplexCopyOp) and
  * cannot be consolidated by common subexpression elimination.  Cures bug with:
  *
  * B <== A
  * C <== A
  */
private[cogx] case class ComplexCopyOp(uniqueId: Long) extends ComplexUnaryOp

/** Object that produces a stream of unique ComplexCopyOps. */
private[cogx] object UniqueComplexCopyOp {
  private val id = new AtomicLong
  def apply() = new ComplexCopyOp(id.getAndIncrement)
}

// Fourier domain operations
private[cogx] sealed abstract class FFTOp(val dimensions: Int,
                            val dir: ClFFTDirection,
                            val scaleFactor: Float)
        extends UnaryOpcode("" + dimensions + "_" + dir)

/** Opcode for 1D FFT */
private[cogx] case class FFT1DOp(factor: Float = 1.0f) extends FFTOp(1, Forward, factor)

/** Opcode for the various passes of the 1D FFT */
private[cogx] case class FFT1DSubOp(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
  extends UnaryOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 1D FFT */
private[cogx] case class InverseFFT1DOp(factor: Float = 1.0f) extends FFTOp(1, Inverse, factor)

/** Opcodes for 2D FFT */
private[cogx] case class FFT2DOp(factor: Float = 1.0f) extends FFTOp(2, Forward, factor)

/** Opcode for the various passes of the 2D FFT */
private[cogx] case class FFT2DSubOp(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
        extends UnaryOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 2D FFT */
private[cogx] case class InverseFFT2DOp(factor: Float = 1.0f) extends FFTOp(2, Inverse, factor)

/** Opcode for 3D FFT */
private[cogx] case class FFT3DOp(factor: Float = 1.0f) extends FFTOp(3, Forward, factor)

/** Opcode for the various passes of the 3D FFT */
private[cogx] case class FFT3DSubOp(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
        extends UnaryOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 3D FFT */
private[cogx] case class InverseFFT3DOp(factor: Float = 1.0f) extends FFTOp(3, Inverse, factor)

// Matrix operations

// Filtering
@deprecated("Convolution directly on ColorFields no longer supported.", "4.1.1")
private[cogx] case class ConvolveColor2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends UnaryOpcode("" + borderPolicy + "_" + filterOrientation)

private[cogx] case class LocalMax2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends UnaryOpcode("" + borderPolicy + "_" + filterOrientation)

private[cogx] case class LocalMin2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends UnaryOpcode("" + borderPolicy + "_" + filterOrientation)

private[cogx] case class LocalMaxPosition2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                                filterOrientation: FilterOrientation)
        extends UnaryOpcode("" + borderPolicy + "_" + filterOrientation)

private[cogx] case class LocalMinPosition2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                                filterOrientation: FilterOrientation)
        extends UnaryOpcode("" + borderPolicy + "_" + filterOrientation)

private[cogx] case class BilateralFilter2DOp(spatialFilter: Matrix, rangeSigma: Float)
        extends UnaryOpcode

// Output from computation graph by way of an 'Actuator'

/** An unpipelined Actuator for any dimension scalar field */
private[cogx] case class UnpipelinedActuatorOp(val newOutput: (ScalarFieldReader) => Unit, resetHook: () => Unit) extends UnaryOpcode
private[cogx] case class UnpipelinedVectorActuatorOp(val newOutput: (Iterator[Float]) => Unit, resetHook: () => Unit) extends UnaryOpcode

private[cogx] case class UnpipelinedColorActuatorOp(newOutput: (Iterator[Byte]) => Unit, resetHook: () => Unit) extends UnaryOpcode

private[cogx] sealed abstract class BinaryConstOpcode(val const: Float, name: String = "")
        extends UnaryOpcode(name)

private[cogx] case class AddConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class SubtractConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class MultiplyConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class DivideConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class ModuloConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class GreaterThanConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class GreaterThanEqualsConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class LessThanConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class LessThanEqualsConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class EqualsConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class NotEqualsConstOp(value: Float) extends BinaryConstOpcode(value)

private[cogx] case class MaxConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class MinConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class PowConstOp(exponent: Float) extends BinaryConstOpcode(exponent)
private[cogx] case class PownConstOp(exponent: Int) extends BinaryConstOpcode(exponent)

private[cogx] sealed abstract class ComplexBinaryRealConstOp(const: Float, function: String = "")
        extends BinaryConstOpcode(const, function)

private[cogx] case class ComplexAddRealConstOp(value: Float) extends ComplexBinaryRealConstOp(value)
private[cogx] case class ComplexSubtractRealConstOp(value: Float) extends ComplexBinaryRealConstOp(value)
private[cogx] case class ComplexMultiplyRealConstOp(value: Float) extends ComplexBinaryRealConstOp(value)
private[cogx] case class ComplexDivideRealConstOp(value: Float) extends ComplexBinaryRealConstOp(value)

private[cogx] sealed abstract class ComplexBinaryComplexConstOpcode(val const: Complex, name: String = "")
        extends UnaryOpcode(name)

private[cogx] case class ComplexAddConstOp(value: Complex) extends ComplexBinaryComplexConstOpcode(value)
private[cogx] case class ComplexSubtractConstOp(value: Complex) extends ComplexBinaryComplexConstOpcode(value)
private[cogx] case class ComplexMultiplyConstOp(value: Complex) extends ComplexBinaryComplexConstOpcode(value)
private[cogx] case class ComplexDivideConstOp(value: Complex) extends ComplexBinaryComplexConstOpcode(value)


// DCT opcodes
private[cogx] sealed abstract class DCTOpcode(name: String) extends UnaryOpcode(name)
private[cogx] case object DCT2DOp extends DCTOpcode("DCT2D")
private[cogx] case object DCTTransposed2DOp extends DCTOpcode("DCTTransposed2D")
private[cogx] case object DCTInverse2DOp extends DCTOpcode("DCTInverse2D")
private[cogx] case object DCTInverseTransposed2DOp extends DCTOpcode("DCTInverseTransposed2D")

// Additional opcodes used internally for the DCT, not visible to users
private[cogx] case object DCTInterleaveOp extends UnaryOpcode("DCTInterleave")
private[cogx] case object DCTDeinterleaveOp extends UnaryOpcode("DCTDeinterleave")
private[cogx] case object DCTRowsOp extends UnaryOpcode("DCTRows")
private[cogx] case object DCTInverseRowsOp extends UnaryOpcode("DCTInverseRows")
private[cogx] case object DCTTransposeFieldOp extends UnaryOpcode("DCTTransposeField")

// Output proxy, a stand-in for a kernel that is implemented on another node
private[cogx] case object OutputProxyOp extends UnaryOpcode("OutputProxy")

/*

  // OLD OPCODES FROM COG 3.X to aid conversion to 4.0

case object NullOp extends Opcode

/**Functions with a single, real argument. */
sealed abstract class UnaryOp(val function: String) extends Opcode(function)

case object AbsOp extends UnaryOp("fabs")

case object AcosOp extends UnaryOp("acos")

case object AcoshOp extends UnaryOp("acosh")

case object AsinOp extends UnaryOp("asin")

case object AsinhOp extends UnaryOp("asinh")

case object AtanhOp extends UnaryOp("atanh")

case object CosOp extends UnaryOp("native_cos")

case object CoshOp extends UnaryOp("cosh")

case object ExpOp extends UnaryOp("native_exp")

case object Exp2Op extends UnaryOp("native_exp2")

case object Exp10Op extends UnaryOp("native_exp10")

case object LogOp extends UnaryOp("native_log")

case object Log2Op extends UnaryOp("native_log2")

case object Log10Op extends UnaryOp("native_log10")

case object ReciprocalOp extends UnaryOp("reciprocal")

case object RectifyOp extends UnaryOp("rectify")

case object SignumOp extends UnaryOp("signum")

case object SinOp extends UnaryOp("native_sin")

case object SinhOp extends UnaryOp("sinh")

case object SqrtOp extends UnaryOp("native_sqrt")

case object SqOp extends UnaryOp("sq")

case object TanOp extends UnaryOp("native_tan")

case object TanhOp extends UnaryOp("tanh")

case object WinnerTakeAllReduceOp extends UnaryOp("winner_take_all_reduce")

case object WinnerTakeAllPointToFieldOp extends UnaryOp("winner_take_all_point2field")

case object FloorOp extends UnaryOp("floor")

/** Field reductions. These are not recommended since they can induce a
  * single point of failure.
  */
sealed abstract class FieldReductionOp(val function: String)
        extends Opcode(function)

case object FieldReduceMaxOp extends FieldReductionOp("max")

case object FieldReduceMinOp extends FieldReductionOp("min")

case object FieldReduceSumOp extends FieldReductionOp("sum")

case object FieldReduceMedianOp extends FieldReductionOp("reduceMedian")

/** Tensor reductions. These reduce each tensor in a field to a scalar.
  */
sealed abstract class TensorReductionOp(val function: String)
        extends Opcode(function)

case object TensorReduceMaxOp extends TensorReductionOp("max")

case object TensorReduceMinOp extends TensorReductionOp("min")

case object TensorReduceSumOp extends TensorReductionOp("sum")

/** Functions with two arguments, the first being a dynamic field,
  * the second being a constant, "const".
  */
sealed abstract class BinaryConstOp(val function: String, val const: Float)
        extends Opcode(function)

case class AddConstOp(value: Float) extends BinaryConstOp("add", value)

case class SubtractConstOp(value: Float) extends BinaryConstOp("subtract", value)

case class MultiplyConstOp(value: Float) extends BinaryConstOp("multiply", value)

case class DivideConstOp(value: Float) extends BinaryConstOp("divide", value)

case class MaxConstOp(value: Float) extends BinaryConstOp("fmax", value)

case class MinConstOp(value: Float) extends BinaryConstOp("fmin", value)

case class ModuloConstOp(value: Float) extends BinaryConstOp("fmod", value)

case class ClipOp(value: Float) extends BinaryConstOp("fmin", value)

case class ThresholdOp(value: Float) extends BinaryConstOp("threshold", value)

case class PowConstOp(exponent: Float) extends BinaryConstOp("pow", exponent)

case class PowNOp(exponent: Int) extends BinaryConstOp("pown", exponent)

case class GreaterThanConstOp(value: Float) extends BinaryConstOp("greaterThan", value)

case class GreaterThanEqualConstOp(value: Float) extends BinaryConstOp("greaterThanEqual", value)

case class LessThanConstOp(value: Float) extends BinaryConstOp("lessThan", value)

case class LessThanEqualConstOp(value: Float) extends BinaryConstOp("lessThanEqual", value)

case class EqualsConstOp(value: Float) extends BinaryConstOp("equals", value)

case class NotEqualsConstOp(value: Float) extends BinaryConstOp("notEquals", value)

case class ImageGreaterThanConstOp(value: Float) extends BinaryConstOp("greaterThanConstFloat4", value)

case class ImageGreaterThanEqualConstOp(value: Float) extends BinaryConstOp("greaterThanEqualConstFloat4", value)

case class ImageLessThanConstOp(value: Float) extends BinaryConstOp("lessThanConstFloat4", value)

case class ImageLessThanEqualConstOp(value: Float) extends BinaryConstOp("lessThanEqualConstFloat4", value)

case class ImageEqualsConstOp(value: Float) extends BinaryConstOp("equalsConstFloat4", value)

case class ImageNotEqualsConstOp(value: Float) extends BinaryConstOp("notEqualsConstFloat4", value)

/**Functions with two dynamic field arguments. */
sealed abstract class BinaryOp(val function: String) extends Opcode(function)

case object AddOp extends BinaryOp("add")

case object SubtractOp extends BinaryOp("subtract")

case object MultiplyOp extends BinaryOp("multiply")

case object DivideOp extends BinaryOp("divide")

case object ModuloOp extends BinaryOp("fmod")

case object Atan2Op extends BinaryOp("atan2")

case object MaxOp extends BinaryOp("fmax")

case object MinOp extends BinaryOp("fmin")

case object GreaterThanOp extends BinaryOp("greaterThan")

case object GreaterThanEqualOp extends BinaryOp("greaterThanEqual")

case object LessThanOp extends BinaryOp("lessThan")

case object LessThanEqualOp extends BinaryOp("lessThanEqual")

case object EqualsOp extends BinaryOp("equals")

case object NotEqualsOp extends BinaryOp("notEquals")

case object ImageGreaterThanOp extends BinaryOp("greaterThanFloat4")

case object ImageGreaterThanEqualOp extends BinaryOp("greaterThanEqualFloat4")

case object ImageLessThanOp extends BinaryOp("lessThanFloat4")

case object ImageLessThanEqualOp extends BinaryOp("lessThanEqualFloat4")

case object ImageEqualsOp extends BinaryOp("equalsFloat4")

case object ImageNotEqualsOp extends BinaryOp("notEqualsFloat4")


case object DifferentialOp extends Opcode


case object ReplicateOp extends Opcode

case class SubfieldsOp(diameter: Int, border: BorderPolicy) extends Opcode

/**Nonlocal operations. */
sealed abstract class NonlocalOp extends Opcode

case object DiffusionOp extends NonlocalOp

case object CompleteBoundariesOp extends NonlocalOp

//case object NormalizeL1Op extends NonlocalOp
//
//case object NormalizeL2Op extends NonlocalOp

case object WinnerTakeAllOp extends NonlocalOp

case object MaxPositionOp extends NonlocalOp

case class WarpOp(borderPolicy: BorderPolicy) extends Opcode(borderPolicy.toString)

case class SubfieldOp(borderPolicy: BorderPolicy) extends Opcode(borderPolicy.toString)


case object ForwardGradientOp extends Opcode

//case object BackwardDivergenceOp extends Opcode

case object MedianFilterOp extends Opcode

//case object MedianOp extends NonlocalOp

//case object OuterProductOp extends Opcode

//case object Subsample extends NonlocalOp
//case object Dot extends NonlocalOp

/**Operations which shouldn't be there. */
sealed abstract class WeirdOp extends Opcode

case object MaxLocationOp extends WeirdOp


case object ScalarDotOp extends Opcode

case object VectorDotOp extends Opcode

//case object TensorDotOp extends Opcode

//case object CrossDotOp extends Opcode

case object ReverseCrossDotOp extends Opcode

//// Matrix operations
//case object MatrixTransformMatrixOp extends Opcode
//
//case object MatrixTransformVectorOp extends Opcode
//
//case object MatrixTransposeOp extends Opcode
//
//case object MatrixInvertOp extends Opcode
//
//case object VectorTransposeOp extends Opcode

//case class  FixedMatrixTransformMatrixOp(matrix: Matrix) extends Opcode
//case class  FixedMatrixTransformVectorOp(matrix: Matrix) extends Opcode

//// Field, tensor shape ops
//case class TrimOp(newShape: Shape) extends Opcode
//
//case class ExpandBorderOp(newShape: Shape, borderPolicy: BorderPolicy) extends Opcode
//
//case class SubspaceOp(indices: Array[Range]) extends Opcode
//
//case class ReshapeOp(newShape: Array[Int]) extends Opcode
//
//case class SliceOp(index: Int) extends Opcode
//
//case object SlicePointOp extends Opcode
//
//case object StackOp extends Opcode
//
//case class TensorSliceOp(index: Int) extends Opcode
//
//case object TensorStackOp extends Opcode
//
//case object JoinOp extends Opcode

// Filtering
case class Convolve1DOp(kernel: Vector, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class ConvolveColor2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class ConvolveRows2DOp(kernel: Vector, borderPolicy: BorderPolicy,
                            filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class ConvolveColumns2DOp(kernel: Vector, borderPolicy: BorderPolicy,
                               filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class Convolve3DOp(kernel: Tensor3, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class ConvolveComplex2DOp(kernel: ComplexMatrix, borderPolicy: BorderPolicy,
                               filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class ConvolveDynamicOp(borderPolicy:BorderPolicy, filterOrientation: FilterOrientation,
                             samplingPolicy: ConvolutionSamplingPolicy)
        extends Opcode(borderPolicy.toString)

case class LocalMax2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class LocalMin2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class LocalMaxPosition2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class LocalMinPosition2DOp(kernel: Matrix, borderPolicy: BorderPolicy,
                        filterOrientation: FilterOrientation)
        extends Opcode(borderPolicy.toString + "_" + filterOrientation.toString)

case class BilateralFilter2DOp(spatialFilter: Matrix, rangeSigma: Float)
        extends Opcode


// Fourier domain operations
sealed abstract class FFTOp(val dimensions: Int, val dir: CLFFTDirection) extends Opcode

case object FFT1DOp extends FFTOp(1, Forward)

case object InverseFFT1DOp extends FFTOp(1, Inverse)

/** Opcode for old Interbuffer-style 2D FFT */
case object FFT2DOp extends FFTOp(2, Forward)

/** Opcode for the various passes of the 2D FFT */
case class FFT2DSubOp(level: Int, dir: CLFFTDirection) extends Opcode

case object InverseFFT2DOp extends FFTOp(2, Inverse)

/** Opcode for old Interbuffer-style 3D FFT */
case object FFT3DOp extends FFTOp(3, Forward)

/** Opcode for the various passes of the 3D FFT */
case class FFT3DSubOp(level: Int, dir: CLFFTDirection) extends Opcode

case object InverseFFT3DOp extends FFTOp(3, Inverse)

//case class ColorConvolutionOp(filter: Matrix) extends Opcode

// Complex operations-----------------------------------------------------------

case object RealToComplexOp extends Opcode

case object RealImaginaryToComplexOp extends Opcode

case object PolarToComplexOp extends Opcode

sealed abstract class ComplexUnaryOp(val function: String) extends Opcode(function)

case object ComplexExpOp extends ComplexUnaryOp("complexExp")

case object ConjugateOp extends ComplexUnaryOp("conjugate")

case object ComplexReciprocalOp extends ComplexUnaryOp("complexReciprocal")


sealed abstract class ComplexBinaryOp(val function: String) extends Opcode(function)

case object ComplexAddOp extends ComplexBinaryOp("complexAdd")

case object ComplexSubtractOp extends ComplexBinaryOp("complexSubtract")

case object ComplexMultiplyOp extends ComplexBinaryOp("complexMultiply")

case object ComplexDivideOp extends ComplexBinaryOp("complexDivide")

sealed abstract class ComplexBinaryConstOp(val function: String, val const: Complex)
        extends Opcode(function)

case class ComplexAddConstOp(value: Complex) extends ComplexBinaryConstOp("complexAdd", value)

case class ComplexSubtractConstOp(value: Complex) extends ComplexBinaryConstOp("complexSubtract", value)

case class ComplexMultiplyConstOp(value: Complex) extends ComplexBinaryConstOp("complexMultiply", value)

case class ComplexDivideConstOp(value: Complex) extends ComplexBinaryConstOp("complexDivide", value)

sealed abstract class ComplexBinaryRealConstOp(val function: String, val const: Float)
        extends Opcode(function)

case class ComplexAddRealConstOp(value: Float) extends ComplexBinaryRealConstOp("complexAddReal", value)

case class ComplexSubtractRealConstOp(value: Float) extends ComplexBinaryRealConstOp("complexSubtractReal", value)

case class ComplexMultiplyRealConstOp(value: Float) extends ComplexBinaryRealConstOp("complexMultiplyReal", value)

case class ComplexDivideRealConstOp(value: Float) extends ComplexBinaryRealConstOp("complexDivideReal", value)

// Dyad opcodes ----------------------------------------------------------------

// Extract components from a dyad field
sealed abstract class DyadUnaryOp extends Opcode

case object DyadSticknessOp extends DyadUnaryOp

case object DyadBallnessOp extends DyadUnaryOp

case object DyadOrientationOp extends DyadUnaryOp

// Stack stickness, ballness and orientation scalar fields into a dyad field.
case object DyadStackPropertiesOp extends Opcode

// Stack dyad components (xx, xy, yy) into a dyad field.
case object DyadStackTensorsOp extends Opcode

// Color operations ------------------------------------------------------------

sealed abstract class ColorPlaneOp(val plane: String)
        extends Opcode(plane)

case object ColorPlaneRedOp extends ColorPlaneOp("red")

case object ColorPlaneGreenOp extends ColorPlaneOp("green")

case object ColorPlaneBlueOp extends ColorPlaneOp("blue")

case object ColorPlaneLuminanceOp extends ColorPlaneOp("luminance")

case object DiffuseDirichletOp extends Opcode

case object MergeColorPlanesOp extends Opcode

case object ColorDiffusionOp extends Opcode

case object ColorOpaqueOp extends Opcode

//case class UpsampleOp(factor: Int, phase: Int) extends Opcode
//
//case class DownsampleOp(factor: Int, phase: Int) extends Opcode
//
//case object SupersampleOp extends Opcode

//case class SubsampleOp(factor: Int) extends Opcode

// This will go away!!!
case object CannyOp extends Opcode

// Diffusion opcodes
case object BoundaryDownsampleOp extends Opcode

case object BoundaryUpsampleOp extends Opcode

case class JacobiRelaxationOp(smooth: Matrix, scale: Float) extends Opcode

case class DirichletBoundaryMarkerOp(threshold: Float) extends Opcode

// Constants -------------------------------------------------------------------
case class FixedComplexMatrixOp(matrix: ComplexMatrix) extends Opcode

case class FixedComplexMatrix3DOp(matrix: ComplexTensor3) extends Opcode

case class FixedMatrixOp(matrix: Matrix) extends Opcode

case class FixedVectorOp(vector: Vector) extends Opcode


///////////////////////////////////////////////////////////////////////////////
//
// Special opcodes
//
///////////////////////////////////////////////////////////////////////////////

// Push an N-D frame onto an (N + 1)-D field. Used for temporal processing.
case object PushOp extends Opcode

// Compound op represents a kernel that is composed of multiple opcodes.
case object CompoundOp extends Opcode

// Compound op for kernels created by the HyperKernel merging process
case class MergedOp(opcodes: List[Opcode]) extends Opcode
//case class MergedOp(src: Opcode, sink: Opcode) extends Opcode

// External op represents a kernel that brings in data from outside.
case object ExternalOp extends Opcode

case class InputReduced(op: Opcode) extends Opcode

/** Dynamically select one field from an array of fields. */
case object FieldArraySelectOp extends Opcode

/** Set NaN values in a field to zero. */
case object FilterNaNOp extends Opcode

///////////////////////////////////////////////////////////////////////////////
//
// Field --> Image
// Image --> Field
//
///////////////////////////////////////////////////////////////////////////////

// ScalarField --> ScalarImage, 4 ScalarFields --> QuadImage
case object FieldToImageOp extends Opcode

// ScalarImage --> ScalarField, QuadImage --> VectorField
case object ImageToFieldOp extends Opcode

// ColorField --> QuadImage
case object ColorFieldToQuadImageOp extends Opcode

// QuadImage --> ColorField
case object QuadImageToColorFieldOp extends Opcode


// Dummy, the following will go away
case object ScalarImageTestOp extends Opcode

///////////////////////////////////////////////////////////////////////////////
//
// User opcodes. These allow a user to create custom CPU kernels.
//
///////////////////////////////////////////////////////////////////////////////

case class UserScalarFieldOp(f: (Array[Field]) => ScalarField) extends Opcode

case class UserVectorFieldOp(f: (Array[Field]) => VectorField) extends Opcode

case class UserMatrixFieldOp(f: (Array[Field]) => MatrixField) extends Opcode

case class UserColorFieldOp(f: (Array[Field]) => ColorField) extends Opcode

case class UserComplexScalarFieldOp(f: (Array[Field]) => ComplexField) extends Opcode

///////////////////////////////////////////////////////////////////////////////
//
// Experimental opcodes.
//
///////////////////////////////////////////////////////////////////////////////

case object SolvePoissonOp extends Opcode

case object PoissonDownsampleOp extends Opcode

case object PoissonUpsampleOp extends Opcode

case object GeneralConvolve2DOp extends Opcode


///////////////////////////////////////////////////////////////////////////////
//
// Opcodes for testing purposes
//
///////////////////////////////////////////////////////////////////////////////

case object UnaryTestOp extends Opcode

case object BinaryTestOp extends Opcode

 */