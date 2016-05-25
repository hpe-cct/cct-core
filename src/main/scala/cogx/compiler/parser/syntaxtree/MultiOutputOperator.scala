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

import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{ClFFTDirection, Forward}
import cogx.compiler.parser.op._
import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types._
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
object MultiOutputOperator extends SemanticError with Logarithm {

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
    * @param opcode The operation performed on a field.
    * @param in The Array of input fields.
    * @return The Array of output fields.
    * @author Greg Snider and Dick Carter
    */
  def apply(opcode: MultiOutputOpcode, in: Array[Field]): Array[Field] = {
    val inType0 = in(0).fieldType

    opcode match {
      case op: FFTOpRI =>
        checkFFT(in, op.dimensions, op.dir)
       case op: FFT1DSubOpRI =>
        checkFFT(in, 1, op.dir)
       case op: FFT2DSubOpRI =>
        checkFFT(in, 2, op.dir)
       case op: FFT3DSubOpRI =>
        checkFFT(in, 3, op.dir)
      case default => // OK by default
    }

    /** return types for operator. */
    val resultTypes: Array[FieldType] = opcode match {
      // If you add an operation that produces a different type than the
      // default type, add that here. E.g. "case MyOp => new NodeType(...)"
      case op: FFTOpRI =>
        Array(inType0, inType0)
      case op: FFT1DSubOpRI =>
        Array(inType0, inType0)
      case op: FFT2DSubOpRI =>
        Array(inType0, inType0)
      case op: FFT3DSubOpRI =>
        Array(inType0, inType0)
      case _ =>
        // Default is that the number and type of the inputs dictate the output fieldTypes
        in.map(_.fieldType)
    }

    /** The operation for this multi-output operator (its HyperNode in the SyntaxTree) */
    val operation = Operation(opcode, in, resultTypes)

    /** Produce the result Fields */
    resultTypes.map(resultType => Field(operation, resultType))
  }

  /** Rule checking for all variants of the FFT. */
  private def checkFFT(in: Array[Field], dimensions: Int, dir: ClFFTDirection) {
    val inType0 = in(0).fieldType
    val MinFFTSize = 1
    val MaxFFTSize = 2048
    require(isRealField(inType0))

    in.length match {
      case 2 => require(inType0 == in(1).fieldType)
      case 1 => require(dir == Forward)
      case x => error(s"FFT op has wrong number of inputs: $x")
    }

    if (dimensions != inType0.dimensions)
      error("FFT op has wrong dimensionality (INTERNAL BUG!)")

    for (d <- 0 until dimensions) {
      val size = inType0.fieldShape(d)
      if (!isPowerOf2(size))
        error("FFT requires power-of-2 field sizes, found " +
                size + " in " + inType0)
      if (size < MinFFTSize || size > MaxFFTSize)
        error("FFT field size must be in the range [" + MinFFTSize +
                ", " + MaxFFTSize + "], found " + size + " in " + inType0)

    }
  }
}