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

import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{ClFFTDirection, Inverse, Forward}
import cogx.platform.types.Opcode


/**
  * An operation that produces more than one output.  Generally, the names of the opcode base classes
  * reflect the number of inputs (UnaryOpcode, BinaryOpcode, etc.).  In this case the name refers to
  * the number of outputs.  The class with handle all multi-output operations for all input-counts.
  *
  * @author Dick Carter
  */
private[cogx]
sealed abstract class MultiOutputOpcode(name: String = "") extends Opcode(name)

// These fft opcodes are quite similar to the UnaryOpcodes for the fft kernel that operates on and creates
// single complex fields.  It was tempting to just add an opcode parameter 'splitRealImaginary: Boolean'
// to these existing opcodes.  However, the fact that the existing opcodes are processed by the UnaryOperator
// would be confusing.  These various Operator classes are how the compiler partitions the front-end
// semantic checking.  I decided to stick to this structure, even though it meant a bit of code duplication
// below.  The semantic checking for these fft opcodes is performed by the MultiOperator class.  -RJC

// Fourier domain operations
private[cogx] sealed abstract class FFTOpRI(val dimensions: Int,
                                          val dir: ClFFTDirection,
                                          val scaleFactor: Float)
  extends MultiOutputOpcode("" + dimensions + "_" + dir)

// 1D not supported by this approach, since it uses a CPU kernel currently

/** Opcode for 1D FFT */
private[cogx] case class FFT1DOpRI(factor: Float = 1.0f) extends FFTOpRI(1, Forward, factor)

/** Opcode for the various passes of the 2D FFT */
private[cogx] case class FFT1DSubOpRI(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
  extends MultiOutputOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 1D FFT */
private[cogx] case class InverseFFT1DOpRI(factor: Float = 1.0f) extends FFTOpRI(1, Inverse, factor)

/** Opcodes for 2D FFT */
private[cogx] case class FFT2DOpRI(factor: Float = 1.0f) extends FFTOpRI(2, Forward, factor)

/** Opcode for the various passes of the 2D FFT */
private[cogx] case class FFT2DSubOpRI(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
  extends MultiOutputOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 2D FFT */
private[cogx] case class InverseFFT2DOpRI(factor: Float = 1.0f) extends FFTOpRI(2, Inverse, factor)

/** Opcode for 3D FFT */
private[cogx] case class FFT3DOpRI(factor: Float = 1.0f) extends FFTOpRI(3, Forward, factor)

/** Opcode for the various passes of the 3D FFT */
private[cogx] case class FFT3DSubOpRI(level: Int, dir: ClFFTDirection, scaleFactor: Float = 1.0f)
  extends MultiOutputOpcode("Sub" + level + "_" + dir)

/** Opcode for inverse 3D FFT */
private[cogx] case class InverseFFT3DOpRI(factor: Float = 1.0f) extends FFTOpRI(3, Inverse, factor)

