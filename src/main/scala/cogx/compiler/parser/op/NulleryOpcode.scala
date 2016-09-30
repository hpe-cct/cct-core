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

import cogx.platform.types.{Pixel, Opcode}
import cogx.cogmath.algebra.real.{Vector, Matrix}
import cogx.cogmath.algebra.complex.{ComplexVector, Complex}

/** An operation taking no inputs, producing a Field
  *
  * TODO: currently, equivalent constant ops do not compare as equal, which means that
  * some common subexpression elimination opportunities are missed.
  *
  * @author Greg Snider
  */
private[cogx]
sealed abstract class NulleryOpcode(name: String = "") extends Opcode(name)

////////////////////////////// Sensors /////////////////////////////////////////

// The `nextInput` function for unpipelined sensors used to be solely a () => Iterator[Float].  Iterators
// proved to be too slow for big datasets, so new signatures were introduced that permitted bulk puts to
// the underlying DirectBuffers.  For 0D and 1D sensors, this signature is () => Array[Float], for 2D sensors
// it's () => Array[Array[Float], etc.
//
// The generalization of `nextInput` is therefore to a () => _
//
// Pipelined sensors were similarly generalized, only with Option[ ] surrounding the nextInput return value.
//

/** Scalar field input to computation graph, no implicit pipelining. */
private[cogx] abstract class SensorOp(val nextInput: () => Option[_],
                                         val resetHook: () => Unit,
                                         val desiredRate: Double,
                                         val pipelined: Boolean)
        extends NulleryOpcode

// Convert the Iterator[Float] to Option[Iterator[Float]], since the same SensorKernel handles both opcodes.

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class UnpipelinedSensorOp(_nextInput: () => _,
                                           _resetHook: () => Unit, _desiredRate: Double)
        extends SensorOp(new Function0[Option[_]] { def apply() = Some(_nextInput()) },
          _resetHook, _desiredRate, pipelined = false)

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class PipelinedSensorOp(_nextInput: () => Option[_],
                                           _resetHook: () => Unit, _desiredRate: Double)
        extends SensorOp(_nextInput, _resetHook, _desiredRate, pipelined = true)

/** Vector field input to computation graph */

/** Vector field input to computation graph. */
private[cogx] abstract class VectorSensorOp(val nextInput: () => Option[Iterator[Vector]],
                                      val resetHook: () => Unit,
                                      val desiredRate: Double,
                                      val pipelined: Boolean)
  extends NulleryOpcode

// Convert the Iterator[Float] to Option[Iterator[Float]], since the same SensorKernel handles both opcodes.

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class UnpipelinedVectorSensorOp(_nextInput: () => Iterator[Vector],
                                             _resetHook: () => Unit, _desiredRate: Double)
  extends VectorSensorOp(new Function0[Option[Iterator[Vector]]] { def apply() = Some(_nextInput()) },
    _resetHook, _desiredRate, pipelined = false)

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class PipelinedVectorSensorOp(_nextInput: () => Option[Iterator[Vector]],
                                           _resetHook: () => Unit, _desiredRate: Double)
  extends VectorSensorOp(_nextInput, _resetHook, _desiredRate, pipelined = true)

/** Color image input to computation graph */
private[cogx] abstract class ColorSensorOp(val nextInput: () => Option[Iterator[Byte]],
                                      val resetHook: () => Unit,
                                      val desiredRate: Double,
                                      val pipelined: Boolean)
  extends NulleryOpcode

// Convert the Iterator[Float] to Option[Iterator[Float]], since the same SensorKernel handles both opcodes.

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class UnpipelinedColorSensorOp(_nextInput: () => Iterator[Byte],
                                             _resetHook: () => Unit, _desiredRate: Double)
  extends ColorSensorOp(new Function0[Option[Iterator[Byte]]] { def apply() = Some(_nextInput()) },
    _resetHook, _desiredRate, pipelined = false)

/** Scalar field input to computation graph, with an embedded pipeline stage for better CPU/GPU parallelism. */
private[cogx] case class PipelinedColorSensorOp(_nextInput: () => Option[Iterator[Byte]],
                                           _resetHook: () => Unit, _desiredRate: Double)
  extends ColorSensorOp(_nextInput, _resetHook, _desiredRate, pipelined = true)

////////////////////////////// Constants ///////////////////////////////////////

/** An opcode representing a constant value in the compute graph. */
private[cogx] abstract class ConstantOp extends NulleryOpcode

/** A constant field in a computation graph. All 'numbers' in all tensors are the same. */
private[cogx] case class ConstantScalarOp(value: Float)
  extends ConstantOp {
  override def toString: String = "ConstantScalarOp(" + value + ")"
}


/** A 0-D constant scalar field in a computation graph. */
private[cogx] case class ConstantScalar0DOp(value: () => Float)
        extends ConstantOp {
  override def toString: String = "ConstantScalar0DOp(" + value() + ")"
}

/** A 1-D constant scalar field in a computation graph */
private[cogx] case class ConstantScalar1DOp(value: (Int) => Float)
        extends ConstantOp

/** A 2-D constant scalar field in a computation graph */
private[cogx] case class ConstantScalar2DOp(value: (Int, Int) => Float)
        extends ConstantOp

/** A 3-D constant scalar field in a computation graph */
private[cogx] case class ConstantScalar3DOp(value: (Int, Int, Int) => Float)
        extends ConstantOp



/** A 0-D constant vector field in a computation graph. */
private[cogx] case class ConstantVector0DOp(value: () => Vector)
        extends ConstantOp

/** A 1-D constant vector field in a computation graph */
private[cogx] case class ConstantVector1DOp(value: (Int) => Vector)
        extends ConstantOp

/** A 2-D constant vector field in a computation graph */
private[cogx] case class ConstantVector2DOp(value: (Int, Int) => Vector)
        extends ConstantOp

/** A 3-D constant vector field in a computation graph */
private[cogx] case class ConstantVector3DOp(value: (Int, Int, Int) => Vector)
        extends ConstantOp



/** A 0-D constant matrix field in a computation graph. */
private[cogx] case class ConstantMatrix0DOp(value: () => Matrix)
        extends ConstantOp

/** A 1-D constant matrix field in a computation graph */
private[cogx] case class ConstantMatrix1DOp(value: (Int) => Matrix)
        extends ConstantOp

/** A 2-D constant matrix field in a computation graph */
private[cogx] case class ConstantMatrix2DOp(value: (Int, Int) => Matrix)
        extends ConstantOp

/** A 3-D constant matrix field in a computation graph */
private[cogx] case class ConstantMatrix3DOp(value: (Int, Int, Int) => Matrix)
        extends ConstantOp



/** A 0-D constant complex field in a computation graph. */
private[cogx] case class ConstantComplex0DOp(value: () => Complex)
        extends ConstantOp

/** A 1-D constant complex field in a computation graph */
private[cogx] case class ConstantComplex1DOp(value: (Int) => Complex)
        extends ConstantOp

/** A 2-D constant complex field in a computation graph */
private[cogx] case class ConstantComplex2DOp(value: (Int, Int) => Complex)
        extends ConstantOp

/** A 3-D constant complex field in a computation graph */
private[cogx] case class ConstantComplex3DOp(value: (Int, Int, Int) => Complex)
        extends ConstantOp



/** A 0-D constant complex vector field in a computation graph. */
private[cogx] case class ConstantComplexVector0DOp(value: () => ComplexVector)
        extends ConstantOp

/** A 1-D constant complex vector field in a computation graph */
private[cogx] case class ConstantComplexVector1DOp(value: (Int) => ComplexVector)
        extends ConstantOp

/** A 2-D constant complex vector field in a computation graph */
private[cogx] case class ConstantComplexVector2DOp(value: (Int, Int) => ComplexVector)
        extends ConstantOp

/** A 3-D constant complex vector field in a computation graph */
private[cogx] case class ConstantComplexVector3DOp(value: (Int, Int, Int) => ComplexVector)
        extends ConstantOp



/** A constant color image. */
private[cogx] case class ConstantColorOp(value: (Int, Int) => Pixel)
        extends ConstantOp

////////////////////////////// Null op /////////////////////////////////////////

/** Null opcode, does nothing */
private[cogx] case object NullOp extends NulleryOpcode

/// Input proxy op ///
private[cogx] case object InputProxyOp extends NulleryOpcode
