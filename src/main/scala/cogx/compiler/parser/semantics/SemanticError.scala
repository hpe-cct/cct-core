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

package cogx.compiler.parser.semantics

import cogx.platform.types.{Opcode, FieldType}
import cogx.cogmath.geometry.Shape
import cogx.compiler.CompilerError

/** Reporting of semantic errors in user code. These print out a message
  * explaining the error, then throw an exception.
  *
  * @author Greg Snider
  */
private[cogx]
trait SemanticError extends CompilerError {

  /** Report a fatal semantic error (generic version with supplied message).
    *
    * @param message The string to follow "Semantic error: "
    */
  private[cogx]
  def error(message: String): Nothing = {
    throw new SemanticErrorException("\nSemantic error: " + message + "\n")
  }

  /** Check for a fatal semantic error (generic version with supplied message).
    *
    * @param required If false, report an error.
    * @param message The string to follow "Semantic error: "
    */
  private[cogx]
  def check(required: Boolean, message: String) {
    if (!required)
      error(message)
  }

  /** Report a fatal type conflict for a unary operation.
    *
    * @param operation The operation causing the type conflict.
    * @param in Type of the input.
    */
  private[cogx]
  def typeError(operation: Opcode, in: FieldType) =
    error("field type is incompatible with '" + operation + "':\n" +
              "   input: " + in.toString)

  /** Report a fatal type conflict for a binary operation.
    *
    * @param operation The operation causing the type conflict.
    * @param in1 Type of the first input.
    * @param in2 Type of the second input.
    */
  private[cogx]
  def typeError(operation: Opcode, in1: FieldType, in2: FieldType): Nothing =
    typeError(operation, Array(in1, in2))

  /** Report a fatal type conflict for a binary operation.
    *
    * @param operation The operation causing the type conflict.
    * @param in Types for all the inputs.
    */
  private[cogx]
  def typeError(operation: Opcode, in: Array[FieldType]): Nothing = {
    val fieldStr = Array.tabulate(in.length)(i =>
      "  input " + i + ": " + in(i).toString + "\n")
    val message =
      "field types are incompatible for '" + operation + "':\n" + fieldStr.mkString
    error(message)
  }

  /** Report a fatal type conflict for an actuator
    *
    * @param in Type of the input.
    */
  private[cogx]
  def actuatorTypeError(in: FieldType) =
    error("Improper field type connected to this Actuator.\n" +
              "   Attempted connection type: " + in.toString)

  /** Report a fatal actuator construction error.
    *
    * @param out The field shape of actuator.
    * @param outArray The array to be written by the actuator.
    */
  private[cogx]
  def actuatorArrayMatchError(out: Shape, outArray: Array[_]) {
    val arrayShape = {
      if (outArray.isInstanceOf[Array[Float]]) {
        // 0D or 1D field
        Shape(outArray.asInstanceOf[Array[Float]].length)
      } else if (outArray.isInstanceOf[Array[Array[Float]]]) {
        // 2D field
        val out = outArray.asInstanceOf[Array[Array[Float]]]

        Shape(out.length, out(0).length)
      } else if (outArray.isInstanceOf[Array[Array[Array[Float]]]]) {
        // 3D field
        val out = outArray.asInstanceOf[Array[Array[Array[Float]]]]
        Shape(out.length, out(0).length, out(1).length)
      } else
        internalError("unexpected field dimensionality.")
    }
    val message =
              "Actuator field shape: " + out +
              ", does not match shape of Array to be written: " + arrayShape
    error(message)
  }

  /** Report an attempt to bind a field to multiple syntax trees. */
  private[cogx]
  def fieldOwnershipError() =
    error("Attempted to bind a Field to more than one ComputeGraph.")

  /** Report an unsupported number of dimensions on a field. */
  private[cogx]
  def fieldDimensionError(fieldDimensions: Int) =
    error("Illegal number of dimensions for a field: " + fieldDimensions)

  /** Report a field dimension different than `expected`. */
  private[cogx]
  def fieldDimensionError(fieldDimensions: Int, expected: Int) =
    error("Illegal number of dimensions for a field: actual " + fieldDimensions +
      ", expected " + expected)

  /** Report an unsupported number of dimensions of the tensors of a field. */
  private[cogx]
  def tensorDimensionError(tensorDimensions: Int) =
    error("Illegal number of dimensions for the tensors of a field: " +
            tensorDimensions)

  /** Report a tensor dimension of a field different than `expected`. */
  private[cogx]
  def tensorDimensionError(tensorDimensions: Int, expected: Int) =
    error("Illegal number of dimensions for the tensors of a field: actual " +
            tensorDimensions + ", expected " + expected)

  /** Report a syntax tree that has no inputs. */
  private[cogx]
  def noInputsError() =
    error("A computation has been declared with no inputs.\n" +
              "At least one input is required")

  /** Flag an illegal feedback connection (<== operator). */
  private[cogx]
  def feedbackError(details: String) = error(details)

  /** Flag multiple feedback connections. */
  private[cogx]
  def missingFeedbackError() =
    error("missing feedback connections to a stateful field")

  /** Flag multiple feedback connections. */
  private[cogx]
  def multipleFeedbackError() =
    error("multiple feedback connections to a field")

  /** Flag multiple initializers. */
  private[cogx]
  def multipleInitializerError() = error("multiple initializers for a field")

  private[cogx]
  def missingSyntaxTree() =
    error("missing ComputeGraph context for building Cog functions")

  private[cogx]
  def nestedSyntaxTrees() = error("ComputeGraphs may not be nested")

  private[cogx]
  def fieldReadError() =
    error("attempt to read a field from within a ComputeGraph")
}