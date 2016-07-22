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

package cogx.compiler.codegenerator.opencl

import cogx.compiler.parser.op._
import cogx.compiler.CompilerError
import cogx.platform.types.Opcode

/** Translates an opcode to an OpenCL function name.
  *
  * @author Greg Snider
  */
private[cogx]
object OpcodeToFunction extends CompilerError {

  /** Get the name of the OpenCL function corresponding to `opcode`. */
  def apply(opcode: Opcode): String = {
    opcode match {
      case op: ComplexBinaryOp =>
        op match {
          case ComplexAddOp => "complexAdd"
          case ComplexSubtractOp => "complexSubtract"
          case ComplexMultiplyOp => "complexMultiply"
          case ComplexDivideOp => "complexDivide"
          case PolarToComplexOp => "polarToComplex"
          case RealImaginaryToComplexOp => "realImaginaryToComplex"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: ComplexBinaryComplexConstOpcode =>
        op match {
          case ComplexAddConstOp(value) => "complexAdd"
          case ComplexSubtractConstOp(value) => "complexSubtract"
          case ComplexMultiplyConstOp(value) => "complexMultiply"
          case ComplexDivideConstOp(value) => "complexDivide"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: ComplexBinaryRealConstOp =>
        op match {
          case ComplexAddRealConstOp(value) => "complexAddReal"
          case ComplexSubtractRealConstOp(value) => "complexSubtractReal"
          case ComplexMultiplyRealConstOp(value) => "complexMultiplyReal"
          case ComplexDivideRealConstOp(value) => "complexDivideReal"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: ComplexUnaryOp =>
        op match {
          case ComplexExpOp => "complexExp"
          case ConjugateOp => "conjugate"
          case ComplexReciprocalOp => "complexReciprocal"
          case ComplexUnaryMinusOp => "negate"
          case RealToComplexOp => "realToComplex"
          case ComplexCopyOp(uniqueId) => "copy"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
        // The ComplexToRealOp ops are defined here so that the
        // ComplexToRealHyperKernel could be made better through use of
        // OpcodeToFunction() in the future.
      case op: ComplexToRealOp =>
        op match {
          case PhaseOp => "phase"
          case OrientationOp => "orientation"
          case MagnitudeOp => "magnitude"
          case RealPartOp => "realPart"
          case ImaginaryPartOp => "imaginaryPart"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: BinaryConstOpcode =>
        op match {
          case AddConstOp(value) => "add"
          case SubtractConstOp(value) => "subtract"
          case MultiplyConstOp(value) => "multiply"
          case DivideConstOp(value) => "divide"
          case ModuloConstOp(value) => "fmod"
          case GreaterThanConstOp(value) => "greaterThan"
          case GreaterThanEqualsConstOp(value) => "greaterThanEqual"
          case LessThanConstOp(value) => "lessThan"
          case LessThanEqualsConstOp(value) => "lessThanEqual"
          case EqualsConstOp(value) => "equals"
          case NotEqualsConstOp(value) => "notEquals"
          case MaxConstOp(value) => "fmax"
          case MinConstOp(value) => "fmin"
          case PowConstOp(exponent) => "pow"
          case PownConstOp(exponent) => "powInt"
          case _ => internalError("opcode " + op + " has no function"); ""
        }
      case op: BinaryOpcode =>
        op match {
          case AddOp => "add"
          case SubtractOp => "subtract"
          case MultiplyOp => "multiply"
          case DivideOp => "divide"
          case ModuloOp => "fmod"
          case GreaterThanOp => "greaterThan"
          case GreaterThanEqualsOp => "greaterThanEqual"
          case LessThanOp => "lessThan"
          case LessThanEqualsOp => "lessThanEqual"
          case EqualsOp => "equals"
          case NotEqualsOp => "notEquals"
          case MaxOp => "fmax"
          case MinOp => "fmin"
          case Atan2Op => "atan2"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: UnaryOpcode =>
        op match {
          case AbsOp => "fabs"
          case AcosOp => "acos"
          case AcoshOp => "acosh"
          case AsinOp => "asin"
          case CosOp => "native_cos"
          case CoshOp => "cosh"
          case ExpOp => "native_exp"
          case FloorOp => "floor"
          case LogOp => "native_log"
          case ReciprocalOp => "reciprocal"
          case RectifyOp => "rectify"
          case SignumOp => "signum"
          case SinOp => "native_sin"
          case SinhOp => "sinh"
          case SqOp => "sq"
          case SqrtOp => "sqrt"
          case TanOp => "native_tan"
          case TanhOp => "tanh"
          case UnaryMinusOp => "negate"
          case CopyOp(uniqueId) => "copy"
          // Field Reductions
          case FieldReduceMaxOp => "max"
          case FieldReduceMinOp => "min"
          case FieldReduceSumOp => "add"
          // Tensor Reductions
          case x: TensorReduceMaxOp => "fmax"
          case x: TensorReduceMinOp => "fmin"
          case x: TensorReduceSumOp => "add"
          case _ =>  internalError("opcode " + op + " has no function"); ""
        }
      case op: NulleryOpcode =>
        op match {
          case op: ConstantOp =>
            internalError("opcode " + op + " has no function"); ""
          case op: SensorOp =>
            internalError("opcode " + op + " has no function"); ""
          case op: VectorSensorOp =>
            internalError("opcode " + op + " has no function"); ""
          case op: ColorSensorOp =>
            internalError("opcode " + op + " has no function"); ""
          case NullOp =>
            internalError("opcode " + op + " has no function"); ""
          case InputProxyOp =>
            internalError("opcode " + op + " has no function"); ""
        }
    }
  }
}