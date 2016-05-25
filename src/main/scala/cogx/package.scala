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

package cogx

/** The Cog library. All cog services can be accessed with a single call:
  * {{{
  * import cogx._
  * }}}
  *
  * Update to the message below: this doesn't fix things, at least totally.
  * If you're running into trouble and desperate for a temporary fix, add
  * a dummy type declaration to force recompilation of this file.
  *
  * A bug in the IntelliJ IDEA editor forced a somewhat unconventional approach
  * to creating this object.  Rather than:
  *
  * package object cogx { }
  *
  * we use an alternate syntax that accomplishes the same thing:
  *
  * package cogx
  * object `package` { }
  *
  * @author Greg Snider
  */
object `package` extends CogXInterface

trait CogXInterface
        extends cogx.api.CogFunctionAPI
        with cogx.api.ImplicitConversions
        with cogx.compiler.gpu_operator.UserGPULibrary
        with cogx.cogmath.algebra.complex.ComplexImplicits
{

  //---------------------------------------------------------------------------
  // Compiler / Runtime configuration parameters.
  //---------------------------------------------------------------------------
  val Cog = parameters.Cog

  //---------------------------------------------------------------------------
  // Geometry
  //---------------------------------------------------------------------------
  type Shape = cogx.cogmath.geometry.Shape
  val  Shape = cogx.cogmath.geometry.Shape

  //---------------------------------------------------------------------------
  // Element types
  //---------------------------------------------------------------------------
  type ElementType = cogx.platform.types.ElementTypes.ElementType

  val Int8 = cogx.platform.types.ElementTypes.Int8
  val Uint8 = cogx.platform.types.ElementTypes.Uint8
  val Int16 = cogx.platform.types.ElementTypes.Int16
  val Uint16 = cogx.platform.types.ElementTypes.Uint16
  val Int32 = cogx.platform.types.ElementTypes.Int32
  val Uint32 = cogx.platform.types.ElementTypes.Uint32
  val Int64 = cogx.platform.types.ElementTypes.Int64
  val Uint64 = cogx.platform.types.ElementTypes.Uint64

  val Uint8Pixel = cogx.platform.types.ElementTypes.Uint8Pixel

  val Float16 = cogx.platform.types.ElementTypes.Float16
  val Float32 = cogx.platform.types.ElementTypes.Float32
  val Float64 = cogx.platform.types.ElementTypes.Float64

  val Complex16 = cogx.platform.types.ElementTypes.Complex16
  val Complex32 = cogx.platform.types.ElementTypes.Complex32
  val Complex64 = cogx.platform.types.ElementTypes.Complex64

  //---------------------------------------------------------------------------
  // Real linear algebra
  //---------------------------------------------------------------------------
  type Vector = cogx.cogmath.algebra.real.Vector
  val  Vector = cogx.cogmath.algebra.real.Vector
  type Matrix = cogx.cogmath.algebra.real.Matrix
  val  Matrix = cogx.cogmath.algebra.real.Matrix
  type Tensor3 = cogx.cogmath.algebra.real.Tensor3
  val  Logarithm = cogx.cogmath.algebra.real.Logarithm
  type Logarithm = cogx.cogmath.algebra.real.Logarithm

  //---------------------------------------------------------------------------
  // Complex linear algebra
  //---------------------------------------------------------------------------
  type Complex = cogx.cogmath.algebra.complex.Complex
  val  Complex = cogx.cogmath.algebra.complex.Complex
  type ComplexVector = cogx.cogmath.algebra.complex.ComplexVector
  val  ComplexVector = cogx.cogmath.algebra.complex.ComplexVector
  type ComplexMatrix = cogx.cogmath.algebra.complex.ComplexMatrix
  val  ComplexMatrix = cogx.cogmath.algebra.complex.ComplexMatrix
  type ComplexTensor3 = cogx.cogmath.algebra.complex.ComplexTensor3

  //---------------------------------------------------------------------------
  // FFT math
  //---------------------------------------------------------------------------
  val  FFT2D = cogx.cogmath.fft.FFT2D

  //---------------------------------------------------------------------------
  // Fields
  //---------------------------------------------------------------------------
  type FieldType = cogx.platform.types.FieldType

  type Field = cogx.compiler.parser.syntaxtree.Field
  val  Field = cogx.compiler.parser.syntaxtree.Field

  type ScalarField = cogx.compiler.parser.syntaxtree.ScalarField
  val  ScalarField = cogx.compiler.parser.syntaxtree.ScalarField

  type VectorField = cogx.compiler.parser.syntaxtree.VectorField
  val  VectorField = cogx.compiler.parser.syntaxtree.VectorField

  type MatrixField = cogx.compiler.parser.syntaxtree.MatrixField
  val  MatrixField = cogx.compiler.parser.syntaxtree.MatrixField

  type Pixel =  cogx.platform.types.Pixel
  type ColorField = cogx.compiler.parser.syntaxtree.ColorField
  val  ColorField = cogx.compiler.parser.syntaxtree.ColorField

  type ComplexField = cogx.compiler.parser.syntaxtree.ComplexField
  val  ComplexField = cogx.compiler.parser.syntaxtree.ComplexField

  type ComplexVectorField = cogx.compiler.parser.syntaxtree.ComplexVectorField
  val  ComplexVectorField = cogx.compiler.parser.syntaxtree.ComplexVectorField

  //---------------------------------------------------------------------------
  // Field access on the CPU
  //---------------------------------------------------------------------------
  type FieldReader = cogx.platform.cpumemory.readerwriter.FieldReader
  type ScalarFieldReader = cogx.platform.cpumemory.readerwriter.ScalarFieldReader
  type ScalarFieldWriter = cogx.platform.cpumemory.readerwriter.ScalarFieldWriter
  type VectorFieldReader = cogx.platform.cpumemory.readerwriter.VectorFieldReader
  type VectorFieldWriter = cogx.platform.cpumemory.readerwriter.VectorFieldWriter
  type MatrixFieldReader = cogx.platform.cpumemory.readerwriter.MatrixFieldReader
  type MatrixFieldWriter = cogx.platform.cpumemory.readerwriter.MatrixFieldWriter
  type ComplexFieldReader = cogx.platform.cpumemory.readerwriter.ComplexFieldReader
  type ComplexFieldWriter = cogx.platform.cpumemory.readerwriter.ComplexFieldWriter
  type ComplexVectorFieldReader = cogx.platform.cpumemory.readerwriter.ComplexVectorFieldReader
  type ComplexVectorFieldWriter = cogx.platform.cpumemory.readerwriter.ComplexVectorFieldWriter
  type ColorFieldReader = cogx.platform.cpumemory.readerwriter.ColorFieldReader
  type ColorFieldWriter = cogx.platform.cpumemory.readerwriter.ColorFieldWriter


  type AbstractFieldMemory = cogx.platform.cpumemory.AbstractFieldMemory
  type VectorFieldMemory = cogx.platform.cpumemory.VectorFieldMemory
  type MatrixFieldMemory = cogx.platform.cpumemory.MatrixFieldMemory
  type ComplexFieldMemory = cogx.platform.cpumemory.ComplexFieldMemory
  type ImageMemory = cogx.platform.cpumemory.ColorFieldMemory
  val  FieldMemory = cogx.platform.cpumemory.FieldMemory
  type Operator = cogx.compiler.cpu_operator.Operator

  //---------------------------------------------------------------------------
  // Options for handling field borders
  //---------------------------------------------------------------------------
  type BorderPolicy = cogx.platform.types.BorderPolicy
  val  BorderClamp = cogx.platform.types.BorderClamp
  val  BorderZero = cogx.platform.types.BorderZero
  val  BorderCyclic = cogx.platform.types.BorderCyclic
  val  BorderValid = cogx.platform.types.BorderValid
  val  BorderFull = cogx.platform.types.BorderFull

  //---------------------------------------------------------------------------
  // Options for convolution / crossCorrelation sampling policy
  //---------------------------------------------------------------------------
  type ConvolutionSamplingPolicy = cogx.platform.types.ConvolutionSamplingPolicy
  val NoSamplingConvolution = cogx.platform.types.NoSamplingConvolution
  type UpsampleInputConvolution = cogx.platform.types.UpsampleInputConvolution
  val UpsampleInputConvolution = cogx.platform.types.UpsampleInputConvolution
  type DownsampleOutputConvolution = cogx.platform.types.DownsampleOutputConvolution
  val  DownsampleOutputConvolution = cogx.platform.types.DownsampleOutputConvolution

  //---------------------------------------------------------------------------
  // Options for specifying how FFT should be used for convolution
  //---------------------------------------------------------------------------
  type ConvolutionFFTUsePolicy = cogx.platform.types.ConvolutionFFTUsePolicy
  val  UseFFTNever = cogx.platform.types.UseFFTNever
  val  UseFFTAlways = cogx.platform.types.UseFFTAlways
  val  UseFFTWhenBest = cogx.platform.types.UseFFTWhenBest

  //---------------------------------------------------------------------------
  // Options for specifying how SmallTensorMode should be used for convolution
  //---------------------------------------------------------------------------
  type ConvolutionSmallTensorUsePolicy = cogx.platform.types.ConvolutionSmallTensorUsePolicy
  val  UseSmallTensorNever = cogx.platform.types.UseSmallTensorNever
  val  UseSmallTensorAlways = cogx.platform.types.UseSmallTensorAlways
  val  UseSmallTensorWhenBest = cogx.platform.types.UseSmallTensorWhenBest

  //---------------------------------------------------------------------------
  // Options for specifying which technology is used for saving/restoring ComputeGraphs
  //---------------------------------------------------------------------------
  type CheckpointerType = cogx.platform.types.CheckpointerType
  val  Hdf5CheckpointerType = cogx.platform.types.Hdf5CheckpointerType
  val  JavaCheckpointerType = cogx.platform.types.JavaCheckpointerType

  //---------------------------------------------------------------------------
  // User GPU functions
  //---------------------------------------------------------------------------
  val GPUOperator = cogx.compiler.gpu_operator.GPUOperator

  //---------------------------------------------------------------------------
  // Input / Output
  //---------------------------------------------------------------------------
  type Sensor = cogx.compiler.parser.syntaxtree.Sensor
  type UnpipelinedSensor = cogx.compiler.parser.syntaxtree.UnpipelinedSensor
  type ColorSensor = cogx.compiler.parser.syntaxtree.ColorSensor
  type UnpipelinedColorSensor = cogx.compiler.parser.syntaxtree.UnpipelinedColorSensor
  type VectorSensor = cogx.compiler.parser.syntaxtree.VectorSensor
  type UnpipelinedVectorSensor = cogx.compiler.parser.syntaxtree.UnpipelinedVectorSensor

  type Actuator = cogx.compiler.parser.syntaxtree.Actuator
  val  Actuator = cogx.compiler.parser.syntaxtree.Actuator
  type UnpipelinedActuator = cogx.compiler.parser.syntaxtree.UnpipelinedActuator
  val  UnpipelinedActuator = cogx.compiler.parser.syntaxtree.UnpipelinedActuator

  type ColorActuator = cogx.compiler.parser.syntaxtree.ColorActuator
  val ColorActuator = cogx.compiler.parser.syntaxtree.ColorActuator
  type UnpipelinedColorActuator = cogx.compiler.parser.syntaxtree.UnpipelinedColorActuator
  val UnpipelinedColorActuator = cogx.compiler.parser.syntaxtree.UnpipelinedColorActuator

  type VectorActuator = cogx.compiler.parser.syntaxtree.VectorActuator
  val  VectorActuator = cogx.compiler.parser.syntaxtree.VectorActuator
  type UnpipelinedVectorActuator = cogx.compiler.parser.syntaxtree.UnpipelinedVectorActuator

  type ComputeGraph = cogx.runtime.ComputeGraph
  val  ComputeGraph = cogx.runtime.ComputeGraph

  // Get a string describing the underlying hardware platform.
  def platformDescription: String = cogx.platform.opencl.OpenCLPlatform.descriptor

  //---------------------------------------------------------------------------
  // Utilities
  //---------------------------------------------------------------------------
  type Random = cogx.utilities.Random
  val Random = cogx.utilities.Random
}
