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
private[cogx] case object SignumOp extends UnaryOpcode with NeedsVectorLength
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
private[cogx] case class GreaterThanConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength
private[cogx] case class GreaterThanEqualsConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength
private[cogx] case class LessThanConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength
private[cogx] case class LessThanEqualsConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength
private[cogx] case class EqualsConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength
private[cogx] case class NotEqualsConstOp(value: Float) extends BinaryConstOpcode(value) with NeedsVectorLength

private[cogx] case class MaxConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class MinConstOp(value: Float) extends BinaryConstOpcode(value)
private[cogx] case class PowConstOp(exponent: Float) extends BinaryConstOpcode(exponent)
private[cogx] case class PownConstOp(exponent: Int) extends BinaryConstOpcode(exponent) with NeedsVectorLength

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
