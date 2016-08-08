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

import cogx.platform.types.{ConvolutionSamplingPolicy, FilterOrientation, BorderPolicy, Opcode}
import cogx.cogmath.geometry.Shape

/** An operation taking two inputs, producing a Field.
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
sealed abstract class BinaryOpcode(name: String = "") extends Opcode(name)

/** Mix-in for opcodes whose implementation of OpcodeToFunction(op) requires a vector length as an added parameter. */
private[cogx] trait NeedsVectorLength

/** Add two fields, tensor by tensor. */
private[cogx] case object AddOp                     extends BinaryOpcode
private[cogx] case object Atan2Op                   extends BinaryOpcode

// Convolve opcode - different ways of mixing the planes of a vector-vector
// convolution are embodied by the VectorMode helper class.

private[cogx] sealed abstract class VectorMode
private[cogx] case object PlaneByPlane extends VectorMode
private[cogx] case object ProjectFrame extends VectorMode
private[cogx] case object ProjectFrameBlockReduceSum extends VectorMode
private[cogx] case object BackProjectFrame extends VectorMode
private[cogx] case object BackProjectFrameBlockReduceSum extends VectorMode
private[cogx] case object FilterAdjoint extends VectorMode
private[cogx] case object FilterAdjointBlockReduceSum extends VectorMode

// The following organization of convolve opcodes allows the mini-tiled convolve
// kernels to accept either a fully-specified opcode, as might occur in tuning tests,
// or a simpler opcode that would invoke the recommended default parameters.
private[cogx] sealed abstract class AbstractConvolveOp(val borderPolicy:BorderPolicy,
                                                       val filterOrientation: FilterOrientation,
                                                       val samplingPolicy: ConvolutionSamplingPolicy,
                                                       val vectorMode: VectorMode,
                                                       val batchSize: Int)
  extends BinaryOpcode(vectorMode.toString + borderPolicy.toString)

/** A convolve operator with no additional parameters as might be supplied by performance tests
  */
private[cogx] case class ConvolveOp(_borderPolicy:BorderPolicy,
                                    _filterOrientation: FilterOrientation,
                                    _samplingPolicy: ConvolutionSamplingPolicy,
                                    _vectorMode: VectorMode,
                                    _batchSize: Int)
        extends AbstractConvolveOp(_borderPolicy, _filterOrientation, _samplingPolicy, _vectorMode, _batchSize)

/** A convolve operator with additional parameters, designed to allow programmatic testing and
  * tuning of convolve kernels that have each thread responsible for a "mini-tile" of the output.
  */
private[cogx] case class ConvolveTiledOp(localRows: Int, localColumns: Int,
                                         rowsPerThread: Int, colsPerThread: Int,
                                         _borderPolicy:BorderPolicy,
                                         _filterOrientation: FilterOrientation,
                                         _samplingPolicy: ConvolutionSamplingPolicy,
                                         _vectorMode: VectorMode,
                                         _batchSize: Int)
  extends AbstractConvolveOp(_borderPolicy, _filterOrientation, _samplingPolicy, _vectorMode, _batchSize)

/** A convolve operator with additional parameters, designed to allow programmatic testing and
  * tuning of the "convolve to small field" convolve kernels.
  */
private[cogx] case class ConvolveToSmallFieldTiledOp(localRows: Int, localColumns: Int,
                                                     rowsPerThread: Int,
                                                     _borderPolicy:BorderPolicy,
                                                     _filterOrientation: FilterOrientation,
                                                     _samplingPolicy: ConvolutionSamplingPolicy,
                                                     _vectorMode: VectorMode,
                                                     _batchSize: Int)
  extends AbstractConvolveOp(_borderPolicy, _filterOrientation, _samplingPolicy, _vectorMode, _batchSize)

private[cogx] case class ConvolveRows2DOp(borderPolicy:BorderPolicy,
                            filterOrientation: FilterOrientation)
        extends BinaryOpcode(borderPolicy.toString)

private[cogx] case class ConvolveColumns2DOp(borderPolicy:BorderPolicy,
                               filterOrientation: FilterOrientation)
        extends BinaryOpcode(borderPolicy.toString)

private[cogx] case object CrossDotOp                extends BinaryOpcode
/** Divide two fields, tensor by tensor. */
private[cogx] case object DivideOp                  extends BinaryOpcode
private[cogx] case object EqualsOp                  extends BinaryOpcode with NeedsVectorLength
private[cogx] case object GreaterThanEqualsOp       extends BinaryOpcode with NeedsVectorLength
private[cogx] case object GreaterThanOp             extends BinaryOpcode with NeedsVectorLength
private[cogx] case object JoinOp                    extends BinaryOpcode
private[cogx] case object LessThanEqualsOp          extends BinaryOpcode with NeedsVectorLength
private[cogx] case object LessThanOp                extends BinaryOpcode with NeedsVectorLength
private[cogx] case class MatrixTransformMatrixOp(
                           transposeIn1: Boolean,
                           transposeIn2: Boolean,
                           rowsPerThread: Option[Int] = None,
                           colsPerThread: Option[Int] = None) extends BinaryOpcode
private[cogx] case object MatrixTransformVectorOp   extends BinaryOpcode
private[cogx] case object MaxOp                     extends BinaryOpcode
private[cogx] case object MinOp                     extends BinaryOpcode
private[cogx] case object ModuloOp                  extends BinaryOpcode
/** Multiply two fields, tensor by tensor. */
private[cogx] case object MultiplyOp                extends BinaryOpcode
private[cogx] case object NotEqualsOp               extends BinaryOpcode with NeedsVectorLength
private[cogx] case object OuterProductOp            extends BinaryOpcode
/** Push an N-D frame onto an (N + 1)-D field. Used for temporal processing. */
private[cogx] case object PushOp                    extends BinaryOpcode
/** Cross-decorrelation of a "weights" matrix/vector field and a scalar field. */
private[cogx] case object ReverseCrossDotOp         extends BinaryOpcode
private[cogx] case class ScalarMatrixConvolve2DOp(borderPolicy:BorderPolicy,
                                    filterOrientation: FilterOrientation,
                                    samplingPolicy: ConvolutionSamplingPolicy)
        extends BinaryOpcode(borderPolicy.toString)
private[cogx] case object SlicePointOp              extends BinaryOpcode
private[cogx] case object SolveOp                   extends BinaryOpcode
private[cogx] case class SubfieldOp(shape: Shape, borderPolicy: BorderPolicy)
        extends BinaryOpcode(borderPolicy.toString)
/** Subtract two fields, tensor by tensor. */
private[cogx] case object SubtractOp                extends BinaryOpcode
/** Compute the scalar dot product for corresponding tensors in two fields. */
private[cogx] case object TensorDotOp               extends BinaryOpcode
private[cogx] case object VectorElementsOp          extends BinaryOpcode
private[cogx] case class WarpOp(borderPolicy: BorderPolicy)
        extends BinaryOpcode(borderPolicy.toString)

/** Non maximum suppression guided by a local gradient. */
private[cogx] case object OrientedNonMaximumSuppressionOp extends BinaryOpcode

// Complex operations
private[cogx] sealed abstract class ComplexBinaryOp extends BinaryOpcode

private[cogx] case object ComplexAddOp              extends ComplexBinaryOp
private[cogx] case object ComplexSubtractOp         extends ComplexBinaryOp
private[cogx] case object ComplexMultiplyOp         extends ComplexBinaryOp
private[cogx] case object ComplexDivideOp           extends ComplexBinaryOp
private[cogx] case object PolarToComplexOp          extends ComplexBinaryOp
private[cogx] case object RealImaginaryToComplexOp  extends ComplexBinaryOp

// Temporary op in support of vector fft processing
private[cogx] case object FFTMultiplyOp             extends BinaryOpcode
