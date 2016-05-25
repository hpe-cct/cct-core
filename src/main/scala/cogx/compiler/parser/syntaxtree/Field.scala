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

import cogx.cogmath.hypercircuit.Hyperedge
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.CompilerError
import cogx.compiler.parser.op._
import cogx.parameters.Cog
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.platform.opencl.OpenCLAbstractKernel
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Matrix
import cogx.cogmath.algebra.complex.Complex
import cogx.compiler.codegenerator.common.FieldPolicies._

import cogx.api.{ImplicitConversions, CogFunctionAPI, CogOperatorAPI}

/** Base class for all Fields; defines the operators that can be applied to
  * Fields.
  *
  * '''Fields'''
  *
  * A field is a multidimensional array of tensors, where tensors are defined
  * to be multidimensional arrays of numbers. The dimensionality of the field
  * may be 0, 1, 2 or 3. The actual size of the field dimensions are called
  * the "field shape." To make programming easier, the field shape is described
  * using the terms `layers`, `rows` and `columns`. 3D fields uses all three
  * values. 2D fields use only `rows` and `columns` and have `layers` set to 1
  * for convenience. 1D fields use only `columns` and have `layers` and
  * `rows` set to 1 for convenience. 0D fields have only a single tensor and
  * have no need for `layers`, `rows` or `columns`, but for convenience these
  * values are set to 1.
  *
  * '''Tensors'''
  *
  * The dimensionality of a tensor is called its "order" which may be 0
  * (for scalars), 1 (vectors), or 2 (matrices). Tensors also have a shape which
  * uses similar naming as for field shapes. For example, a matrix has `rows`
  * and `columns`. All tensors within a given field have exactly the same
  * shape.
  *
  * '''Operators'''
  *
  * Operators take one or more fields (which can be considered as immutable
  * objects) and produce a result field. Each operator has a set of rules
  * defining the legal combinations of fields it accepts as inputs, and how
  * those inputs are combined to produce the output. Fortunately most operators
  * use only one of a small set of different rules; the most common rules are
  * now described:
  *
  * '''Algebraic binary operator rules'''
  *
  * Binary operators take two fields as inputs. Generally if one of them is
  * a complex field, the other will be implicitly converted to a complex
  * form (with zero imaginary components) before proceeding.
  *
  * The two inputs Fields are algebraically compatible if they satisfy one
  * of the following four conditions (which also define the nature of their
  * result):
  *
  * 1. They have exactly the same field shape and tensor shape. In this
  * case, corresponding elements of the two fields are combined to produce
  * the result: a field with the same field shape and tensor shape as the two
  * input fields.
  *
  * 2. They have exactly the same field shape, but one of them is a scalar
  * field and the other is a (non-scalar) tensor field. In this case the
  * scalar at each location in the scalar field is combined with the tensor
  * in the corresponding location in the tensor field. The result is a
  * tensor field with the same field shape and tensor shape as the input
  * tensor field.
  *
  * 3. One of them is a 0-dimensional scalar field. In this case the single
  * scalar of the 0D scalar field is combined with each element of every tensor
  * in tensor field. The result is a tensor field with the same field shape and
  * tensor shape as the input tensor field.
  *
  * 4. One of them is a 0-dimensional tensor field (non-scalar). In this case,
  * the tensor shape of the 0-dimensional field must exactly match the tensor
  * shape of the other field. The tensor from the 0-dimensional field is
  * combined element-wise with each of the tensors in the other field to
  * produce the result, which has the same field shape and tensor shape of
  * the larger input field.
  *
  * '''Algebraic unary operator rules'''
  *
  * Operators which take only one field as input (and an optional
  * numeric constant) produce a result with the same field shape and tensor
  * shape as the input field. If the input field is complex, the optional
  * numeric constant is converted to complex (with zero imaginary part) before
  * proceeding with the operation.
  *
  * '''Boolean result rules'''
  *
  * Operators which produce boolean results, such as the comparison operators,
  * use 1.0f to represent true and 0.0f to represent false.
  *
  * @param operation The operation that creates this field.
  * @param fieldType The field type of the result.
  * @author Greg Snider
  */
abstract class Field private[cogx] (val operation: Operation,
                                    val fieldType: FieldType)
        extends Hyperedge[Operation](operation)
        with RecurrenceTrait
        with SemanticError
        with CompilerError
        with ImplicitConversions
        with FieldName
        with FieldParameters
        with CogOperatorAPI
{
  /** The opcode of the operation that produces this field */
  val opcode = operation.opcode

  /** The input fields that drive the operation that produces this field */
  def inputs = operation.inputs

  /** Flag signifying that the field is of particular interest to the
    * user and must remain visible (cannot be optimized away). Setting this
    * true makes debugging easier, but can hurt performance since it precludes
    * optimizations that would make this field go away.
    */
  private var probed = false

  /** Return true if this field is marked as "probed". */
  private[cogx] def isProbed: Boolean =
    probed

  /** Mark this field as "probed", making it visible for debugging, though
    * possible hurting overall performance.
    */
  private[cogx] def markProbed() {
    probed = true
  }

  /** User mechanism for marking a field as "probed," optionally supplying a
    * name for the field.
    *
    * If a field is visible to reflection, it will be automatically named.
    * In that case supplying a `userName` is unnecessary and not recommended.
    *
    * @param userName User's name for the field. If not supplied, the field is
    *        probed but uses system-inferred naming for the field.
    * @return The field being probed.
    */
  def probe(userName: String = null): Field = {
    if (userName != null)
      setSimpleName(userName)
    markProbed()
    this
  }

  /** Add `this` field and `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field to be added to `this`.
    * @return Sum of the two fields.
    */
  def +(that: Field) =
    if (isComplexField(this.fieldType) || isComplexField(that.fieldType))
      ComplexInputBinaryOperator(ComplexAddOp, this, that)
    else
      BinaryOperator(AddOp, this, that)

  /** Subtract `that` field from `this` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field to be subtracted to `this`.
    * @return `this` - `that`
    */
  def -(that: Field) =
    if (isComplexField(this.fieldType) || isComplexField(that.fieldType))
      ComplexInputBinaryOperator(ComplexSubtractOp, this, that)
    else
      BinaryOperator(SubtractOp, this, that)

  /** Multiply `this` field by `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field to be multiplied by `this`.
    * @return `this` * `that`
    */
  def *(that: Field) =
    if (isComplexField(this.fieldType) || isComplexField(that.fieldType))
      ComplexInputBinaryOperator(ComplexMultiplyOp, this, that)
    else
      BinaryOperator(MultiplyOp, this, that)

  /** Divide `this` field by `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field that `this` will be divided by.
    * @return `this` / `that`
    */
  def /(that: Field) =
    if (isComplexField(this.fieldType) || isComplexField(that.fieldType))
      ComplexInputBinaryOperator(ComplexDivideOp, this, that)
    else
      BinaryOperator(DivideOp, this, that)

  /** Compute `this` modulo `that` to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` modulo `that`.
    * @return `this` % `that`
    */
  def %(that: Field) = BinaryOperator(ModuloOp, this, that)

  /** Compare two fields using the "greater than" operator; a numeric
    * value of 1.0f represents true (first operand > second operand) and a
    * numeric value of 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` > `that`.
    * @return `this` greaterThan `that`
    */
  def >(that: Field) = BinaryOperator(GreaterThanOp, this, that)

  /** Compare two fields using the "greater than or equal" operator; a numeric
    * value of 1.0f represents true (first operand >= second operand) and a
    * numeric value of 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` >= `that`.
    * @return `this` greaterThanOrEqual `that`
    */
  def >=(that: Field) = BinaryOperator(GreaterThanEqualsOp, this, that)

  /** Compare two fields using the "less than" operator; a numeric value of 1.0f
    * represents true (first operand < second operand) and a numeric value of
    * 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` < `that`.
    * @return `this` lessThan `that`
    */
  def <(that: Field) = BinaryOperator(LessThanOp, this, that)

  /** Compare two fields using the "less than equal" operator; a numeric value
    * of 1.0f represents true (first operand <= second operand) and a numeric
    * value of 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` <- `that`.
    * @return `this` lessThanOrEquel `that`
    */
  def <=(that: Field) = BinaryOperator(LessThanEqualsOp, this, that)

  /** Compare two fields using the "identically equal" operator; a numeric value
    * of 1.0f represents true (first operand == second operand) and a numeric
    * value of 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` === `that`.
    * @return `this` identicallyEqualTo `that`
    */
  def ===(that: Field) = BinaryOperator(EqualsOp, this, that)

  /** Compare two fields using the "not identically equal" operator; a numeric
    * value of 1.0f represents true (first operand != second operand) and a
    * numeric value of 0.0f represents false.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` !=== `that`.
    * @return `this` notIdenticallyEqualTo `that`
    */
  def !===(that: Field) = BinaryOperator(NotEqualsOp, this, that)

  /** Compute the outer product of Scalarfields `this` and `that`.  The
    * output field has a field shape that is the concatenation of the two
    * input field shapes, and must be at most 3-dimensional.  Each output
    * element is calculated as in:
    *
    * out(in1Indices, in2Indices) = in1(in1Indices) * in2(in2Indices)
    *
    * This operation is implemented by a CPU kernel, so will be slow for big inputs.
    *
    * @param that Operand for computing `this` `^` `that`.
    * @return A field equal to the outer product of `this` and `that`,
    */
  def ^(that: Field) = BinaryOperator(OuterProductOp, this, that)

  /** Add the constant `that` to every element in `this` to produce a result
    * with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be added to every number in `this`.
    * @return A field equal to the sum of `this` and `that`, where `that` has
    *         been added to every numeric component of `this`.
    */
  def +(that: Float) =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexAddRealConstOp(that), this)
    else
      UnaryOperator(AddConstOp(that), this)

  /** Subtract the constant `that` from every element in `this` to produce a
    * result with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be sutracted from every number in `this`.
    * @return A field equal to the difference of `this` and `that`, where `that`
    *         has been subtracted from every numeric component of `this`.
    */
  def -(that: Float) =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexSubtractRealConstOp(that), this)
    else
      UnaryOperator(SubtractConstOp(that), this)

  /** Multiply every element in `this` by `that` to produce a result
    * with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be multiplied with every number in `this`.
    * @return A field equal to the product of `this` and `that`, where
    *         every numeric component of `this` has been multiplied by `that`.
    */
  def *(that: Float) =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexMultiplyRealConstOp(that), this)
    else
      UnaryOperator(MultiplyConstOp(that), this)

  /** Divide every element in `this` by `that` to produce a result
    * with the same field shape and tensor shape as `this`.
    *
    * The user must assure that the divisor is not zero, since the result would
    * be NaN which does not throw an exception.
    *
    * @param that Divisor.
    * @return A field equal to `this` dividedBy `that`, where
    *         every numeric component of `this` has been divided by `that`.
    */
  def /(that: Float) =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexDivideRealConstOp(that), this)
    else
      UnaryOperator(DivideConstOp(that), this)

  /** Perform 1/x operation on every numeric element of a field.
    *
    * Somewhat dangerous to use if any element in the field could be zero, since
    * the resulting element would be NaN which does not throw an exception.
    *
    * @return Input field with each numeric element, x, mapped to 1/x.
    */
  def reciprocal =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexReciprocalOp, this)
    else
      UnaryOperator(ReciprocalOp, this)

  /** For every element, `x`, in `this`, compute `x modulo that`. The result
    * has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for modulo operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x modulo that`.
    */
  def %(that: Float) = UnaryOperator(ModuloConstOp(that), this)

  /** For every element, `x`, in `this`, compute `x greaterThan that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for > operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x greaterThan that`.
    */
  def >(that: Float) = UnaryOperator(GreaterThanConstOp(that), this)

  /** For every element, `x`, in `this`, compute `x greaterThanEqual that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for >= operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x greaterThanEqual that`.
    */
  def >=(that: Float) = UnaryOperator(GreaterThanEqualsConstOp(that), this)

  /** For every element, `x`, in `this`, compute `x lessThan that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for < operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x lessThan that`.
    */
  def <(that: Float) = UnaryOperator(LessThanConstOp(that), this)

  /** For every element, `x`, in `this`, compute `x lessThanEqual that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for <= operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x lessThanEqual that`.
    */
  def <=(that: Float) = UnaryOperator(LessThanEqualsConstOp(that), this)

  /** For every element, `x`, in `this`, compute `x identicallyEquals that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for === operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x identicallyEquals that`.
    */
  def ===(that: Float) = UnaryOperator(EqualsConstOp(that), this)


  /** For every element, `x`, in `this`, compute `x notIdenticallyEquals that`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for !=== operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x notIdenticallyEquals that`.
    */
  def !===(that: Float) = UnaryOperator(NotEqualsConstOp(that), this)

  /** Multiply each number in a field by -1.
    *
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @return The negative of the input field.
    */
  def unary_- =
    if (isComplexField(fieldType))
      UnaryOperator(ComplexUnaryMinusOp, this)
    else
      UnaryOperator(UnaryMinusOp, this)

  /** Add the constant `that` to every element in `this`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Constant to be added to number in `this`.
    * @return A field equal to the sum of `this` and `that`, where `that` has
    *         been added to every numeric component of `this`.
    */
  def +(that: Complex) =
    UnaryOperator(ComplexAddConstOp(that), CogFunctions.toGenericComplexField(this))

  /** Subtract the constant `that` from every element in `this`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Constant to be subtracted from each number in `this`.
    * @return A field equal to the difference of `this` and `that`, where `that`
    *         has been subtracted from every numeric component of `this`.
    */
  def -(that: Complex) =
    UnaryOperator(ComplexSubtractConstOp(that), CogFunctions.toGenericComplexField(this))

  /** Multiply every element in `this` by `that`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Multiplicand
    * @return A field equal to the product of `this` and `that`, where
    *         every numeric component of `this` has been multiplied by `that`.
    */
  def *(that: Complex) =
    UnaryOperator(ComplexMultiplyConstOp(that), CogFunctions.toGenericComplexField(this))

  /** Divide every element in `this` by `that`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Divisor.
    * @return A field equal to the quotient of `this` and `that`, where
    *         every numeric component of `this` has been divided by `that`.
    */
  def /(that: Complex) =
    UnaryOperator(ComplexDivideConstOp(that), CogFunctions.toGenericComplexField(this))

  /** Extract a contiguous subfield from a field, maintaining its
    * dimensionality.
    *
    * Each dimension of a field is indexed starting a zero. The user supplies
    * a range of indices, one for each dimension, that specifies the continguous
    * range of indices that should be extracted into the subfield.
    *
    * For example, consider this input field:
    * {{{
    *    0  1  2  3
    *    4  5  6  7
    *    8  9  0  1
    * }}}
    * This has 3 rows (first dimension) and 4 columns (second dimension) so
    * its shape is 3 x 4. Specify row range (1 to 2) and column range (1 to 3)
    * would extract the following subfield:
    * {{{
    *    5  6  7
    *    9  0  1
    * }}}
    *
    * If the use of this subfield does not shift the origin, then use a trim instead
    * which might be more efficient due to kernel merging.
    *
    * @param ranges For each dimension, a range of indices specifying where
    *        to extract the subfield.
    * @return Subfield of input as specified by the `range` parameters.
    */
  def apply(ranges: Range*) = {
    if (ranges.toArray.length == 0)
      this // Ignore extraneous parentheses at end of other field methods
    else if (ranges.forall(_.start == 0)) {
      val trimmedShape = Shape(ranges.map(_.length).toArray)
      UnaryOperator(TrimOp(trimmedShape), this)
    }
    else
      UnaryOperator(SubspaceOp(ranges), this)
  }

  /** Reduce the dimensionality of a field by 1 by "slicing" along one index
    * of the first dimension.
    *
    * For example, consider this input (3 rows x 4 columns) field:
    * {{{
    *    0  1  2  3
    *    4  5  6  7
    *    8  9  0  1
    * }}}
    * The first dimension is rows, so a "slice index" of 0 would return
    * the first row:
    * {{{
    *    0  1  2  3
    * }}}
    * A slice index of 1 would return the second row:
    * {{{
    *    4  5  6  7
    * }}}
    * And a slice index of 2 would return the third row:
    * {{{
    *    8  9  0  1
    * }}}
    *
    * @param index The index of the first dimension along which we will slice
    *       out a lower-dimensional field.
    * @return A sliced field, of dimension one less than the input field
    *       dimension.
    */
  def apply(index: Int) = UnaryOperator(SliceOp(index), this)
  // apply(dynamic_index) is listed below under binary operators

  /** Given a matrix field, compute the condition number for each matrix in
    * that field. This is currently limited to fields containing 2 x 2 matrices.
    *
    * @return Scalar field holding the condition numbers for the corresponding
    *         matrices in the input matrix field.
    */
  def conditionNumber = CogFunctions.conditionNumber(this)

  /** Compute the determinant of every matrix in a matrix field.
    *
    * @return Scalar field holding the determinants for the corresponding
    *         matrices in the input matrix field.
    */
  def determinant = CogFunctions.determinant(this)

  /** Raise each number in a field to the power `that`.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @return Input field with every number raised to the power `that`.
    */
  def pow(that: Int) = CogFunctions.pow(this, that)  // power

  /** Raise each number in a field to the power `that`.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @return Input field with every number raised to the power `that`.
    */
  def pow(that: Float) = CogFunctions.pow(this, that)  // power

  /** For every element, `x`, in `this`, compute `max(x, that)`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param value Operand for max operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `max(x, that)`.
    */
  def max(value: Float) = CogFunctions.max(this, value)

  /** For every element, `x`, in `this`, compute `min(x, that)`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param value Operand for min operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `min(x, that)`.
    */
  def min(value: Float) = CogFunctions.min(this, value)

  /** Take the absolute value of every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        absoluteValue(x).
    */
  def abs = CogFunctions.abs(this)

  /** Take the arccosine of every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        acos(x).
    */
  def acos = CogFunctions.acos(this)

  /** Take the arcsine of every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        asin(x).
    */
  def asin = CogFunctions.asin(this)

  /** Perform bilateral filtering using `spatialFilter` for spatial filtering
    * (typically this will be a truncated Gaussian) and a Gaussian with
    * width `rangeSigma` for range filtering.
    */
  def bilateralFilter(spatialFilter: Matrix, rangeSigma: Float) =
    CogFunctions.bilateralFilter(this, spatialFilter, rangeSigma)

  /** Take the cosine of every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        cos(x).
    */
  def cos = CogFunctions.cos(this)

  /** Take the hyperbolic cosine of every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        cosh(x).
    */
  def cosh = CogFunctions.cosh(this)

  /** Apply the exponential function to every numeric element in `this`. This
    * works for complex and real fields.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        exp(x).
    */
  def exp = CogFunctions.exp(this)

  /** Flip a field along every dimension.
    *
    *  Example. This 2D scalar field:
    *  {{{
    *     1  2  3
    *     4  5  6
    *     7  8  9
    *  }}}
    *  looks like this when flipped:
    *  {{{
    *     9  8  7
    *     6  5  4
    *     3  2  1
    *  }}}
    *
    * @return A flipped version of the input field.
    */
  def flip = CogFunctions.flip(this)

  /** Map each numeric element of the input field to the largest integer
    * which is less than or equal to that element.
    *
    * @return Field where every number element has been "floored" to an integer.
    */
  def floor = CogFunctions.floor(this)

  /** Apply the natural logarithm to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        log(x).
    */
  def log = CogFunctions.log(this)

  /** Apply the signum operator to every numeric element in `this`.
    *
    * Signum(x) is defined to be:
    *
    *  1 if x > 0
    *
    *  0 if x == 0
    *
    * -1 if x < 0
    *
    * @return Field equal to input field with each element, x, mapped to
    *        signum(x).
    */
  def signum = CogFunctions.signum(this)

  /** Apply the sine operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        sin(x).
    */
  def sin = CogFunctions.sin(this)

  /** Apply the hyperbolic sine operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        sinh(x).
    */
  def sinh = CogFunctions.sinh(this)

  /** Apply the square operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        (x * x).
    */
  def sq = CogFunctions.sq(this)

  /** Apply the square root operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        sqrt(x).
    */
  def sqrt = CogFunctions.sqrt(this)

  /** Apply the tangent operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        tan(x).
    */
  def tan = CogFunctions.tan(this)

  /** Apply the hyperbolic tangent operator to every numeric element in `this`.
    *
    * @return Field equal to input field with each element, x, mapped to
    *        tanh(x).
    */
  def tanh = CogFunctions.tanh(this)

  /** Compute the backward divergence of a 2D vector field.
    *
    * This is the adjoint operator of forwardGradient.
    *
    * @return A scalar field representing the backward divergence of an
    *        input vector field.
    */
  def backwardDivergence = CogFunctions.backwardDivergence(this)

  /** Compute the backward gradient of a 2D scalar or vector field.
    *
    * @return For a scalar field input, a vector field representing the
    *         forward gradient of the input; for a vector input field, an
    *         order 4 tensor field representing the forward gradient of the
    *         input.
    */
  def backwardGradient = CogFunctions.backwardGradient(this)

  /** Compute the central gradient of a scalar of vector field.
    *
    * The central gradient at the point (x, y) is computed using the values at
    * (x - 1, y), (x, y - 1), (x, y + 1), and (x + 1, y + 1)
    *
    * @return For a scalar field input, returns a vectorField representing the
    *        central gradient of the input. For a vector field input, returns
    *        an order-4 tensor field representing the central gradient of the
    *        input.
    */
  def centralGradient = CogFunctions.centralGradient(this)

  /** Expand an N-dimensional field by padding it with values per the border
    * policy, maintaining the origin. The supported border policies are
    * BorderZero, BorderClamp and BorderCyclic.
    *
    * With the BorderZero policy, the border values are all 0.
    *
    * Border Zero Example. A 3 x 4 input field:
    * {{{
    *    1  2  3  4
    *    5 -1 -1  6
    *    7  8  9  0
    * }}}
    * If we expand this to 7 x 8, we fill the new elements with zeroes:
    * {{{
    *    1  2  3  4  0  0  0  0
    *    5 -1 -1  6  0  0  0  0
    *    7  8  9  0  0  0  0  0
    *    0  0  0  0  0  0  0  0
    *    0  0  0  0  0  0  0  0
    *    0  0  0  0  0  0  0  0
    *    0  0  0  0  0  0  0  0
    * }}}
    *
    * With the BorderClamp policy, the border is extended outwards into the expanded
    * output field, wrapping around as though the output field were a torus.
    * This is useful when doing convolution with the FFT and one wishes to
    * minimize border effects by "border clamping".
    *
    * BorderClamp Example. A 3 x 4 input field:
    * {{{
    *    1  2  3  4
    *    5 -1 -1  6
    *    7  8  9  0
    * }}}
    * If we expand this to 7 x 8, we extend the borders, wrapping around as
    * though the output field were a torus:
    * {{{
    *    1  2  3  4  4  4  1  1
    *    5 -1 -1  6  6  6  5  5
    *    7  8  9  0  0  0  7  7
    *    7  8  9  0  0  0  0  0
    *    7  8  9  0  0  0  0  0
    *    1  2  3  4  0  0  0  0
    *    1  2  3  4  0  0  0  0
    * }}}
    *
    * With the BorderCyclic policy, the border values emulate a cyclic
    * "wrap-around" in the original field, as though the input field were a torus.
    * This is useful when doing cyclic convolution with the FFT and one wishes
    * expand the field to make it a power of 2 (necessary for the FFT) while
    * still preserving cyclic convolution
    *
    * BorderCyclic Example. A 3 x 4 input field:
    * {{{
    *    1  2  3  4
    *    5 -1 -1  6
    *    7  8  9  0
    * }}}
    * If we expand this to 7 x 8, we extend the borders, wrapping around as
    * though the input field were a torus:
    * {{{
    *    1  2  3  4  1  2  3  4
    *    5 -1 -1  6  5 -1 -1  6
    *    7  8  9  0  7  8  9  0
    *    1  2  3  4  1  2  3  4
    *    5 -1 -1  6  5 -1 -1  6
    *    5 -1 -1  6  5 -1 -1  6
    *    7  8  9  0  7  8  9  0
    * }}}
    *
    * @param borderPolicy Policy to use for producing border values.
    * @param shape The shape of the expanded output field.
    * @return Expanded field with new elements filled in from the nearest input
    *       edge per `borderPolicy`.
    */
  def expand(borderPolicy: BorderPolicy, shape: Shape) = CogFunctions.expand(this, borderPolicy, shape)

  /** Expand an N-dimensional field by padding it with values per the border
    * policy, maintaining the origin. The supported border policies are
    * BorderZero, BorderClamp and BorderCyclic.
    *
    * See expand(borderPolicy, shape) for a description of the border policies.
    *
    * @param borderPolicy Policy to use for producing border values.
    * @param sizes The integer dimensions of the expanded output field.
    * @return Expanded field with new elements filled in from the nearest input
    *       edge per `borderPolicy`.
    */
  def expand(borderPolicy: BorderPolicy, sizes: Int*) = CogFunctions.expand(this, borderPolicy, sizes: _*)

  /** Reduce a scalar field to a 0D scalar containing a single element which
    * is the maximum element in the input field.
    *
    * @return 0D scalar field holding the max value in input field.
    */
  def fieldReduceMax = CogFunctions.fieldReduceMax(this)

  /** Reduce a scalar field to a 0D scalar containing a single element which
    * is the minimum element in the input field.
    *
    * @return 0D scalar field holding the min value in input field.
    */
  def fieldReduceMin = CogFunctions.fieldReduceMin(this)

  /** Reduce a scalar field to a 0D scalar containing the sum of all the
    * elements in the input field.
    *
    * @return 0D scalar field holding the sum of values in input field.
    */
  def fieldReduceSum = CogFunctions.fieldReduceSum(this)

  /** Find the median value in a scalar field.
    *
    * @return 0D scalar field holding the median value of the input field.
    */
  def fieldReduceMedian = CogFunctions.fieldReduceMedian(this)

  /** Compute the forward gradient of a 2D scalar or vector field.
    *
    * This is the adjoint operator of backwardDivergence.
    *
    * @return For a scalar field input, a vector field representing the
    *         forward gradient of the input; for a vector input field, an
    *         order 4 tensor field representing the forward gradient of the
    *         input.
    */
  def forwardGradient = CogFunctions.forwardGradient(this)

  /** Invert all matrices in a matrix field using Gauss-Jordan elimination.
    *
    * This is numerically stable only for small matrices, and let's not even
    * get in to singular matrices. Be careful.
    *
    * @return A matrix field where each matrix is the (approximate) inverse of
    *         the corresponding matrix in the input matrix field.
    */
  def invertMatrices = CogFunctions.invertMatrices(this)

  /** Compute a random field based on this input field using cellular
    * automaton based RNG. The output ranges from [0 to 1] (inclusive) and is
    * uniformly distributed.
    *
    * @param bits The number of bits of precision to use
    * @return A Vector field where each point in the field is random
    */
  def random(bits:Int) = CogFunctions.random(this, bits)

  /** Find the local maximum of a neighborhood centered on each pixel in a 2D
    * scalar field.
    *
    * For each point in the input 2D scalar field, this searches a small
    * neighborhood of that point and extracts the largest scalar found.
    *
    * The `neighborhood` is defined by a matrix which is centered on the point. A
    * non-zero value in the kernel means the corresponding point in the field
    * is part of the neighborhood, while a zero implies the point should be
    * ignored.
    *
    * For example, the kernel
    * {{{
    *     1 1 0
    *     1 1 0
    *     0 0 0
    * }}}
    * specifies the 2 x 2 neighborhood for the maximum value search.
    *
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A scalar field with the same shape as the input, where each
    *        element is equal to the local max of the input field as defined
    *        by `neighborhood`.
    */
  def localMax(neighborhood: Matrix) = CogFunctions.localMax(this, neighborhood)

  /** Find the local minimum of a neighborhood centered on each pixel in a 2D
    * scalar field.
    *
    * For each point in the input 2D scalar field, this searches a small
    * neighborhood of that point and extracts the smallest scalar found.
    *
    * The `neighborhood` is defined by a matrix which is centered on the point. A
    * non-zero value in the kernel means the corresponding point in the field
    * is part of the neighborhood, while a zero implies the point should be
    * ignored.
    *
    * For example, the kernel
    * {{{
    *     1 1 0
    *     1 1 0
    *     0 0 0
    * }}}
    * specifies the 2 x 2 neighborhood for the minimum value search.
    *
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A scalar field with the same shape as the input, where each
    *        element is equal to the local min of the input field as defined
    *        by `neighborhood`.
    */
  def localMin(neighborhood: Matrix) = CogFunctions.localMin(this, neighborhood)

  /** For a scalar field, find the relative position of the local maximum of a
    * neighborhood centered on the current pixel. This is returned as vector
    * field, with each vector's tail at the current pixel and the head pointing
    * at the pixel containing the neighborhood's local maximum.
    *
    * The `neighborhood` is defined by a matrix which is centered on the point. A
    * non-zero value in the kernel means the corresponding point in the field
    * is part of the neighborhood, while a zero implies the point should be
    * ignored.
    *
    * For example, the kernel
    * {{{
    *     1 1 0
    *     1 1 0
    *     0 0 0
    * }}}
    * specifies the 2 x 2 neighborhood for the maximum value search.
    *
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A vector field with the same field shape as the input scalar
    *        field, with each vector pointing to the maximum value in the
    *        neighborhood relative to the center of the neighborhood.
    */
  def localMaxPosition(neighborhood: Matrix) = CogFunctions.localMaxPosition(this, neighborhood)

  /** For a scalar field, find the relative position of the local minimum of a
    * neighborhood centered on the current pixel. This is returned as vector
    * field, with each vector's tail at the current pixel and the head pointing
    * at the pixel containing the neighborhood's local manimum.
    *
    * The `neighborhood` is defined by a matrix which is centered on the point. A
    * non-zero value in the kernel means the corresponding point in the field
    * is part of the neighborhood, while a zero implies the point should be
    * ignored.
    *
    * For example, the kernel
    * {{{
    *     1 1 0
    *     1 1 0
    *     0 0 0
    * }}}
    * specifies the 2 x 2 neighborhood for the minimum value search.
    *
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local min search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A vector field with the same field shape as the input scalar
    *        field, with each vector pointing to the minimum value in the
    *        neighborhood relative to the center of the neighborhood.
    */
  def localMinPosition(neighborhood: Matrix) = CogFunctions.localMinPosition(this, neighborhood)

  /** Filter a 2D input scalar field with a 3 x 3 median filter.
    *
    * @return A 2D scalar field with the same field shape as the input, with
    *        each element median filtered.
    */
  def medianFilter = CogFunctions.medianFilter(this)

  /** Convert a matrix field to a vector field by stripping out one row
    * from each matrix in the field and making it a vector.
    *
    * @param index The index of the desired row to strip out of each matrix
    *        and use as a vector.
    * @return A vector field made up of the stripped out vectors from the
    *        input matrix field.
    */
  def matrixRow(index: Int) = CogFunctions.matrixRow(this, index)

  /** Find the location/position of the maximum element in a scalar field.
    *
    * @return A 0D vector field containing a vector holding the indices of the
    *        maximum element.
    */
  def maxPosition = CogFunctions.maxPosition(this)

  /** Forces locally non-maximum pixels to zero for both scalar fields and
    * vector fields.
    *
    * Locality is defined to be the 8 nearest neighbors to a given pixel. If a
    * pixel is greater than or equal to any of those neighbors, its value is
    * left intact on the output, otherwise it's set to zero. Note that border
    * pixels have only 5 nearest neighbors and corner pixels have only 3 nearest
    * neighbors, so those are the only ones checked.
    *
    * Vector fields are treated as though they were an array of scalar fields,
    * so non-maximum suppression is executed independently on each.
    *
    * @return A copy of the input field with locally non-maximum values set to
    *        zero.
    */
  def nonMaximumSuppression = CogFunctions.nonMaximumSuppression(this)

  /** Forces locally non-maximum pixels to zero for calar fields,
    * but only when comparing the pixels on either side of
    * a given pixel, using `orientation` to define where to look for the
    * two pixels for comparison.
    *
    * @param orientation A scalar field holding the local orientation of the
    *        input field at every point. Orientation ranges from -Pi/2 to Pi/2,
    *        where the corresponding orientation rotates clockwise from
     *       horizontal (-Pi/2) to vertical (0) to horizontal (Pi/2).
    * @return A copy of the input field with locally non-maximum values set to
    *        zero.
    */
  def orientedNonMaximumSuppression(orientation: Field) =
    CogFunctions.orientedNonMaximumSuppression(this, orientation)

  /** Normalize a scalar field using the L1 norm.
    *
    * @return A normalized copy of the input field.
    */
  def normalizeL1 = CogFunctions.normalizeL1(this)

  /** Normalize a scalar field using the L2 norm.
    *
    * @return A normalized copy of the input field.
    */
  def normalizeL2 = CogFunctions.normalizeL2(this)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * sum of its components.
    *
    * @return A scalar field with each element equal to the sum of components of
    *        the corresponding vector in the input field.
    */
  def reduceSum = CogFunctions.reduceSum(this)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * summing the first `factor` input vector elements to form the first output
    * vector element, and so forth.
    *
    * @return A vector field with each element equal to the sum of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceSum(factor: Int) = CogFunctions.blockReduceSum(this, factor)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * minimum of its components.
    *
    * @return A scalar field with each element equal to the minimum of
    *        the components of the corresponding vector in the input field.
    */
  def reduceMin = CogFunctions.reduceMin(this)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * taking the min() of the first `factor` input vector elements to form the
    * first output vector element, and so forth.
    *
    * @return A vector field with each element equal to the min() of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceMin(factor: Int) = CogFunctions.blockReduceMin(this, factor)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * maximum of its components.
    *
    * @return A scalar field with each element equal to the maximum of
    *        the components of the corresponding vector in the input field.
    */
  def reduceMax = CogFunctions.reduceMax(this)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * taking the max() of the first `factor` input vector elements to form the
    * first output vector element, and so forth.
    *
    * @return A vector field with each element equal to the max() of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceMax(factor: Int) = CogFunctions.blockReduceMax(this, factor)

  /** Change the shape of a scalar field without changing the number of
    * elements in it.
    *
    * This depends on the row-major ordering we use for elements in a field.
    * Elements in any scalar field, regardless of dimension, have that linear
    * ordering. Reshaping preserves that ordering; it really does nothing
    * more than change the sizes of each dimension of the field.
    *
    * @param size A sequence of new sizes for each dimensions. The product of
    *        this sequence must equal the number of elements in the input
    *        field.
    * @return The input field, reshaped, with the same elements in the same
    *        linear order.
    */
  def reshape(size: Int*) = CogFunctions.reshape(this, size: _*)

  /** Change the shape of a scalar field without changing the number of
  * elements in it.
  *
  * This depends on the row-major ordering we use for elements in a field.
  * Elements in any scalar field, regardless of dimension, have that linear
  * ordering. Reshaping preserves that ordering; it really does nothing
  * more than change the sizes of each dimension of the field.
  *
  * @param fieldShape the output field shape to use
  * @param tensorShape the output tensor shape to use
  * @param checkLegacyReshape Warn of uses of reshape that had different behaviors prior to libcog 4.3
  * @return The input field, reshaped, with the same elements in the same
  *        linear order.
  */
  def reshape(fieldShape:Shape, tensorShape:Shape,
              checkLegacyReshape: Boolean = Cog.checkLegacyReshape) =
    CogFunctions.reshape(this, fieldShape, tensorShape, checkLegacyReshape)

  /** Extract all subfields from a 2D scalar field into a 2-D vector field.
    *
    * @param diameter Sizes (rows and columns) of each subfield. Must be odd.
    * @return A 2-D vector field where the length of the vector equals the
    *        number of `diameter` x `diameter` subfields in the input.
    *        For example, a 4 x 4 input field has four subfields of size 3 x 3,
    *        so this operator would return a 2-D vector field of size 3 x 3
    *        (the size of the subfields) with vectors of length 4.
    *        Each layer of the vector field is one subfield of of the input.
    */
  def subfields(diameter: Int) = CogFunctions.subfields(this, diameter)

  /** Supersample a scalar field by 2X in each dimension, replicating pixels
    * to fill in the gaps.
    *
    * @return Input field expanded by 2X in each dimension with pixel
    * replication.
    */
  def supersample = CogFunctions.supersample(this)

  /** Perform a "domain transform" on the rows of a color field or tensor field
    * as a step in edge-aware normalized convolution.
    *
    * For details, see the paper "Domain transform
    * for edge-aware image and video processing," Gastal and Oliveira, 2011.
    * Normally this is not useful for end-users, but you're welcome to try it
    * if you would like to write your own edge-aware filters.
    *
    * @param spaceSigma Width of filter in space.
    * @param rangeSigma Width of filter in range.
    * @return The domain transform of the rows (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  def domainTransformRows(spaceSigma: Float, rangeSigma: Float) =
    CogFunctions.domainTransformRows(this, spaceSigma, rangeSigma)

  /** Perform a "domain filter" on the rows of a tensor or color field as a step
    * in edge-aware normalized convolution. This "correlates" an adaptive box
    * filter with each pixel in the rows of the color field, guided by
    * `domainTransform`.
    *
    * For details, see the paper "Domain transform
    * for edge-aware image and video processing," Gastal and Oliveira, 2011.
    * Normally this is not useful for end-users, but you're welcome to try it
    * if you would like to write your own edge-aware filters.
    *
    * @param domainTransform The cumulative domain transform for the image.
    * @param boxFilterRadius Radius of box filter.
    * @return The domain transform of the rows (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  def domainFilterRows(domainTransform: Field, boxFilterRadius: Float) =
    CogFunctions.domainFilterRows(this, domainTransform, boxFilterRadius)

  /** Perform a "domain filter" on the columns of a color field as a step in
    * edge-aware normalized convolution. This "correlates" an adaptive box
    * filter with each pixel in the columns of the color field, guided by
    * `domainTransform`.
    *
    * This operation works only on color fields and is an optimization. The
    * following code sequences, A and B, are functionally identical:
    * {{{
    *    // Prepare for filtering
    *    val rowTransform = colorImage.domainTransformRows(...)
    *    val colTransform = colorImage.transpose.domainTransformRows(...)
    *
    *    // Sequence A (slower)
    *    var smooth = colorImage
    *    smooth = smooth.domainFilterRows(rowTransform, ...)
    *    smooth = smooth.transpose.domainFilterRows(colTransform, ...).transpose
    *
    *    // Sequence B, using this operation (faster, no transposes)
    *    var smooth = colorImage
    *    smooth = smooth.domainFilterRows(rowTransform, ...)
    *    smooth = smooth.domainFilterColumns(colTransform, ...)
    * }}}
    *
    * For details, see the paper "Domain transform
    * for edge-aware image and video processing," Gastal and Oliveira, 2011.
    * Normally this is not useful for end-users, but you're welcome to try it
    * if you would like to write your own edge-aware filters.
    *
    * @param domainTransform The cumulative domain transform for the image.
    * @param boxFilterRadius Radius of box filter.
    * @return The domain transform of the columns (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  def domainFilterColumns(domainTransform: ScalarField, boxFilterRadius: Float) =
    CogFunctions.domainFilterColumns(this, domainTransform, boxFilterRadius)

  /** Transpose all the matrices in a matrix field to produce a new matrix
    * field.
    *
    * @return Copy of input matrix field with all matrices transposed.
    */
  def transposeMatrices = CogFunctions.transposeMatrices(this)

  /** Transpose each vector in a vector field to a single row matrix, thus
    * creating a matrix field from a vector field
    *
    * @return Copy of input matrix field with all vectors transposed to
    *        single row matrices.
    */
  def transposeVectors = CogFunctions.transposeVectors(this)

  /** "Trim" a field to a smaller size by clipping off indices (maintaining the
    * origin).
    *
    * @param resultShape Desired shape of the resulting field; must not be
    *        larger in any dimension than the corresponding dimension of the
    *        input field
    * @return The trimmed field.
    */
  def trim(resultShape: Shape) = CogFunctions.trim(this, resultShape)

  /** Convert a vector field to a scalar field by extracting one element of each
    * vector.
    *
    * @param index The element of each vector to be extracted.
    * @return Input field with tensor order reduced by one.
    */
  def vectorElement(index: Int) = CogFunctions.vectorElement(this, index)

  /** Convert a vector field to a smaller vector field by extracting a dynamically
    * indexed a range of elements
    *
    * @param indices
    * @return Input field with tensor order reduced by one.
    */
  def vectorElements(indices:Field) = CogFunctions.vectorElements(this, indices)

  /** Compute the "winner" of a scalar field by mapping its largest element
    * to 1.0f and the other elements to 0.0f
    *
    * This is not well-defined when multiple elements share the maximum value,
    * so beware.
    *
    * @return A scalar field with the same shape as the input, with a single
    *        element containing 1.0f and the rest containing 0.0f.
    */
  def winnerTakeAll = CogFunctions.winnerTakeAll(this)

  /** Extract the real part of a complex field as a scalar field.
    *
    * @return A scalar field with the same shape as the input, with each element
    *        equal to the real part of the corresponding complex element in the
    *        input.
    */
  def realPart = CogFunctions.realPart(this)

  /** Extract the imaginary part of a complex field as a scalar field.
    *
    * @return A scalar field with the same shape as the input, with each element
    *        equal to the imaginary part of the corresponding complex element in
    *        the input.
    */
  def imaginaryPart = CogFunctions.imaginaryPart(this)

  /** Downsample a field by taking every nth element.
    *
    * Example: for input field {1,2,3,4}
    *
    *     input.downsample(2) yields {1,3}
    *
    *     input.downsample(2,1) yields {2,4}
    *
    * @param factor The sampling factor, defaults to 2.
    * @param phase The offset within the downsampled region from which to
    *        take data, defaults to an offset of 0.
    */
  def downsample(factor:Int = 2, phase:Int = 0) = CogFunctions.downsample(this, factor, phase)

  /** Upsample with zeroes inserted between field points, increasing
    * the size of the input in all dimensions.
    *
    * Example: for input field {1,2,3}
    *
    *    input.upsample(2) yields {1,0,2,0,3,0}
    *
    *    input.upsample(2,1) yields {0,1,0,2,0,3}
    *
    * @param factor The sampling factor, defaults to 2.
    * @param phase The offset within the scaled up field to put the input
    *        data, defaults to an offset of 0.
    */
  def upsample(factor:Int = 2, phase:Int = 0) = CogFunctions.upsample(this, factor, phase)

  /** Shift a 1D scalar field left (negative `colShift`) or right (positive
    * `colShift`), pulling in zeroes where necessary.
    *
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shift(colShift: Int) = CogFunctions.shift(this, colShift)

  /** Shift a 2D scalar field in both dimensions, pulling in zeroes where
    * necessary.
    *
    * @param rowShift Number of pixels to shift field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shift(rowShift: Int, colShift: Int) = CogFunctions.shift(this, rowShift, colShift)

  /** Shift a 1D scalar field left (negative `colShift`) or right (positive
    * `colShift`), pulling in values from the opposite side where necessary.
    *
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shiftCyclic(colShift: Int) =  CogFunctions.shiftCyclic(this, colShift)

  /** Shift a 2D scalar field in both dimensions, pulling in values from the opposite
    * side where necessary. Negative shift amounts result in shifts up and to the
    * left, while positive shift amounts result in shifts down and to the right.
    *
    * @param rowShift Number of pixels to shift field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shiftCyclic(rowShift: Int, colShift: Int) = CogFunctions.shiftCyclic(this, rowShift, colShift)

  /** Transpose a 2D tensor field or color field.
    */
  def transpose = CogFunctions.transpose(this)

  /** Compute atan2 of two identically shaped scalar fields.
    *
    * @return A scalar field that's the atan2 of the two input fields.
    */
  def atan2(f2: Field) = CogFunctions.atan2(this, f2)


  /** Cross-correlate a scalar/vector/matrix field with a filter.
    *
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies.
    */
  def crossCorrelate(filter: Field,
                     borderPolicy: BorderPolicy,
                     samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution) =
    CogFunctions.crossCorrelate(this, filter, borderPolicy, samplingPolicy)

  /** Convolve a scalar/vector/matrix field with a filter.
    *
    * @param filter The filter to use for convolution; must be square and with
    *        odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along borders,
    *        implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        convolution, also helps to determine the size of output field.
    * @return Input field convolved with `filter` using the supplied border
    *        and sampling policies.
    */
  def convolve(filter: Field,
               borderPolicy: BorderPolicy,
               samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution) =
    CogFunctions.convolve(this, filter, borderPolicy, samplingPolicy)

  /** Cross-correlate a scalar/vector/matrix field with a row filter and a
    * column filter (separable convolution).
    *
    * @param rowFilter The filter to use for cross-correlating the rows
    *        with odd size in each dimension.
    * @param columnFilter The filter to use for cross-correlating the columns
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @return Input field cross-correlated with filters using the supplied
    *        border policy.
    */
  def crossCorrelateSeparable(rowFilter: Field, columnFilter: Field,
                              borderPolicy: BorderPolicy) =
    CogFunctions.crossCorrelateSeparable(this, rowFilter, columnFilter, borderPolicy)

  /** Convolve a scalar/vector/matrix field with a row filter and a
    * column filter (separable convolution).
    *
    * @param rowFilter The filter to use for convolving the rows
    *        with odd size in each dimension.
    * @param columnFilter The filter to use for convolving the columns
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along
    *        borders, implicitly helps to determine size of output field.
    * @return Input field convolved with filters using the supplied
    *        border policy.
    */
  def convolveSeparable(rowFilter: Field, columnFilter: Field,
                        borderPolicy: BorderPolicy) =
    CogFunctions.convolveSeparable(this, rowFilter, columnFilter, borderPolicy)

  /** Cross-correlate a vector field with a vector-field filter frame.
    *
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies.
    */
  def projectFrame(filter: Field,
                   borderPolicy: BorderPolicy,
                   samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                   batchSize: Int = 1) =
    CogFunctions.projectFrame(this, filter, borderPolicy, samplingPolicy, batchSize)

  /** Cross-correlate a vector field with a vector-field filter frame and block reduce
    * the output.  This functionality is only available to the cog testing framework
    * directly.  An optimizer on the user-code sequence blockReduceSum(convolve(...ProjectFrame...))
    * provides this functionality to the user without growing the COG API.
    *
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies, then block-reduced.
    */
  private[cogx] def projectFrameBlockReduceSum(filter: Field,
                   borderPolicy: BorderPolicy,
                   samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                   batchSize: Int = 1) =
    CogFunctions.projectFrameBlockReduceSum(this, filter, borderPolicy, samplingPolicy, batchSize)

  /** Convolve a vector field with a vector-field filter frame.
    *
    * @param filter The filter to use for convolution; must be square and with
    *        odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along borders,
    *        implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        convolution, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field convolved with `filter` using the supplied border
    *        and sampling policies.
    */
  def backProjectFrame(filter: Field,
               borderPolicy: BorderPolicy,
               samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                       batchSize: Int = 1) =
    CogFunctions.backProjectFrame(this, filter, borderPolicy, samplingPolicy, batchSize)

  /** Cross-correlate a vector field with a vector-field filter frame and block reduce
    * the output.  This functionality is only available to the cog testing framework
    * directly.  An optimizer on the user-code sequence blockReduceSum(convolve(...BackProjectFrame...))
    * provides this functionality to the user without growing the COG API.
    *
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies, then block-reduced.
    */
  private[cogx] def backProjectFrameBlockReduceSum(filter: Field,
                                               borderPolicy: BorderPolicy,
                                               samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                                               batchSize: Int = 1) =
    CogFunctions.backProjectFrameBlockReduceSum(this, filter, borderPolicy, samplingPolicy, batchSize)


  /** Cross-correlate a vector field with a vector-field with "filter adjoint" plane mixing.
    *
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies.
    */
  def crossCorrelateFilterAdjoint(filter: Field,
                          borderPolicy: BorderPolicy,
                          samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                          batchSize: Int = 1) =
    CogFunctions.crossCorrelateFilterAdjoint(this, filter, borderPolicy, samplingPolicy, batchSize)

  /** Convolve a vector field with a vector-field with "filter adjoint" plane mixing.
    *
    * @param filter The filter to use for convolution; must be square and with
    *        odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along borders,
    *        implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        convolution, also helps to determine the size of output field.
    * @param batchSize The number of logical (possibly multi-plane) images present in the input field.
    * @return Input field convolved with `filter` using the supplied border
    *        and sampling policies.
    */
  def convolveFilterAdjoint(filter: Field,
                    borderPolicy: BorderPolicy,
                    samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                    batchSize: Int = 1) =
    CogFunctions.convolveFilterAdjoint(this, filter, borderPolicy, samplingPolicy, batchSize)

  /** Multiply a 2D matrix field, `this`, by a 2D scalar field, `f2`, to
    * produce a 2D scalar field.
    *
    * The scalar field `f2` must have the same shape as the matrices in `this`.
    * This operator is basically pretending that the `f2` scalar field is
    * really a matrix and simply dotting that matrix with every matrix in the
    * matrix field `this`, producing a scalar in the corresponding position
    * of the result scalar field.
    *
    * @param f2 Scalar field to multiply with `this`.
    * @return "cross dot" product of `this` and `f2`.
    */
  def crossDot(f2: Field) = CogFunctions.crossDot(this, f2)

  /** Takes the inner product of two identically shaped tensor fields to
    * create a scalar field.
    *
    * The tensors in both fields must have the same shape. Dotting two tensors
    * involves multiplying corresponding elements in the two tensors and
    * summing the products.
    *
    * @param f2 Other field to "dot" with `this` field.
    * @return Scalar field where each scalar element is computed from the dot
    *       product of the tensors in the corresponding location of the two
    *       input tensor fields.
    */
  def dot(f2: Field) = CogFunctions.dot(this, f2)

  //def join(f2: Field) = BinaryOperator(JoinOp, this, f2)

  /** Select the maximum value of corresponding elements in two scalar fields.
    *
    * @param f2 Second scalar field to compare with `this`.
    * @return Scalar field where each element is the largest corresponding
    *        elements in `this` or `f2`.
    */
  def max(f2: Field) = CogFunctions.max(this, f2)

  /** Select the minimum value of corresponding elements in two scalar fields.
    *
    * @param f2 Second scalar field to compare with `this`.
    * @return Scalar field where each element is the smallest corresponding
    *        elements in `this` or `f2`.
    */
  def min(f2: Field) = CogFunctions.min(this, f2)

  //def push(f2: Field) = BinaryOperator(PushOp, this, f2)

  /** Replicate a scalar field as matrices in a matrix field (each matrix in
    * the matrix field is identical to the input scalar field).
    *
    * @param shape The shape of the resulting output field.
    * @return A matrix field with the same field shape as `f2` and tensor
    *        shape the same as the input scalar field.
    */
  def replicate(shape: Shape) = CogFunctions.replicate(this, shape)

  /** Multiply a matrix field, `this`, by a scalar field, `f2`, to produce a
    * scalar field.
    *
    * The matrix field and scalar field operands must have identical field
    * shapes. Each scalar element of the scalar field is multiplied by the
    * corresponding element in the matrix field. These products are then
    * summed to produce the scalar field result (which has the same shape as
    * the matrices in the matrix field).
    *
    * @param f2 Scalar field operand which have the same field shape as
    *        `this`.
    * @return Product of `this` and `f2`.
    */
  def reverseCrossDot(f2: Field) = CogFunctions.reverseCrossDot(this, f2)

  /** Solve Ax = b for x, where A is matrix field, `this`, and b is the
    * vector field argument.
    *
    * This actually solves the equation for each matrix / vector pair in the
    * fields `this` and `b`, producing a vector field representing x. The
    * vectors in b must be length 2 and the matrices in `this` must be 2 x 2.
    * Solves the equations using the pseudo inverse
    *
    * @param b Vector field argument.
    * @return The solution, x, to the set of linear equations.
    */
  def solve(b: Field) = CogFunctions.solve(this, b)

  /** Extracts a window from a 1D or 2D scalar, vector or matrix field, guided by
    * a 0D vector field called "the guide." The guide specifies the upper-left-most
    * (or left-most for 1D fields) point of the window, and `shape`
    * specifies the size of the window.
    *
    * A guiding vector with value (v1, v2) means a given point (row, col) extracts
    * the element at location (row + v1, col + v2) as its output. If that location
    * falls outside of the field, the BorderPolicy attached to the the opcode
    * determines how the missing value is computed. If either of the guide vector
    * components v1 and v2 is non-integral, bilinear interpolation is used to
    * to determine the approximate value.
    *
    * @param guide The guide vector.
    * @param shape Shape of the window.
    * @param border Policy for handling border processing.
    * @return Extracted window.
    */
  def subfield(guide: Field, shape: Shape, border: BorderPolicy = BorderClamp) =
    CogFunctions.subfield(this, guide, shape, border)


  /** (1) Multiply a matrix field by a matrix field to produce a matrix field; or
    * (2) multiply a matrix field by a vector field to produce a vector field.
    *
    * For case (1), corresponding matrices in `this` and `f2` are multiplied
    * using standard matrix multiplication to produce the corresponding matrix
    * in the resulting matrix field.
    *
    * For case (2), corresponding matrix/vector pair in `this` and `f2` are
    * multiplied using standard matrix/vector multiplication to produce the
    * corresponding vector (a linear transformation of the input vector)
    * in the resulting vector field.
    *
    * @param f2 Matrix field or vector field; must have the same field shape
    *       as `this`.
    * @return Matrix field or vector representing the matrix multiplications of
    *       corresponding elements in the two input fields.
    */
  def transform(f2: Field) = CogFunctions.transform(this, f2)

  /** Warps a 2D scalar, vector or matrix field, guided by a vector field
    * called "the guide." The guide must either be zero-dimensional, in which
    * case the input field is translated uniformly, or must be a 2D vector
    * field with exactly the same shape as the input.
    *
    * The guiding vector with value (v1, v2) at a given point (row, col) extracts
    * the element at location (row - v1, col - v2) as its output. If that location
    * falls outside of the field, the BorderPolicy attached to the the opcode
    * determines how the missing value is computed. If either of the guide vector
    * components v1 and v2 is non-integral, bilinear interpolation is used to
    * to determine the approximate value.
    *
    * @param guide The guiding vector field.
    * @param border Policy for handling borders.
    * @return Input field, warped by the guide.
   */
  def warp(guide: Field, border: BorderPolicy = BorderClamp) =
    CogFunctions.warp(this, guide, border)

  /** Reduce the dimensionality of a field by 1 by "slicing" along one index
    * of the first dimension.
    *
    * For example, consider this input (3 rows x 4 columns) field:
    * {{{
    *    0  1  2  3
    *    4  5  6  7
    *    8  9  0  1
    * }}}
    * The first dimension is rows, so a "slice index" of 0 would return
    * the first row:
    * {{{
    *    0  1  2  3
    * }}}
    * A slice index of 1 would return the second row:
    * {{{
    *    4  5  6  7
    * }}}
    * And a slice index of 2 would return the third row:
    * {{{
    *    8  9  0  1
    * }}}
    *
    * @param f2 A 0D scalar field supplying the index of the first dimension
    *           along which we will slice out a lower-dimensional field.
    * @return A sliced field, of dimension one less than the input field
    *       dimension.
    */
  def apply(f2: Field) = BinaryOperator(SlicePointOp, this, f2)

  // These keywords are only defined for ComplexFields, so they are listed here.
  // One side-effect of this is that a real-to-complex conversion will be
  // applied if any of these methods are attempted on a real field.

  /** Take the phase of each element in a complex field.
    *
    * This is also commonly called `arg`. This is a number in the range
    * (-Pi, Pi]
    */
  def phase = CogFunctions.phase(this)

  /** Take the magnitude of each element in a complex field */
  def magnitude = CogFunctions.magnitude(this)

  /** Take the complex conjugate of each element in a complex field */
  def conjugate = CogFunctions.conjugate(this)

  /** Compute the FFT of a complex field. */
  def fft = CogFunctions.fft(this)

  /** Compute the inverse FFT of a complex field (includes scaling). */
  def fftInverse = CogFunctions.fftInverse(this)

  /** Compute the FFT of the rows only in a 2D complex field. */
  def fftRows = CogFunctions.fftRows(this)

  /** Compute the FFT of the columns only in a 2D complex field. */
  def fftColumns = CogFunctions.fftColumns(this)

  /** Compute the inverse FFT of the rows only in a 2D complex field. */
  def fftInverseRows = CogFunctions.fftInverseRows(this)

  /** Compute the inverseFFT of the columns only in a 2D complex field. */
  def fftInverseColumns = CogFunctions.fftInverseColumns(this)

  /** Explicit conversion of a Field to a ScalarField. Since Field is abstract,
    * the only possible conversion is if the field is already a ScalarField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @return The converted field.
    */
  def toScalarField = CogFunctions.toScalarField(this)

  /** Explicit conversion of a Field to a VectorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a VectorField but stored in a Field variable. This method also
    * supports converting a ColorField to a VectorField.
    *
    * @return The converted field.
    */
  def toVectorField = CogFunctions.toVectorField(this)

  /** Explicit conversion of a Field to a MatrixField. Since Field is abstract,
    * the only possible conversion is if the field is already a MatrixField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @return The converted field.
    */
  def toMatrixField = CogFunctions.toMatrixField(this)

  /** Explicit conversion of a Field to a ComplexField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexField but stored in a Field variable. This method also
    * supports converting a ScalarField to a ComplexField.
    *
    * @return The converted field.
    */
  def toComplexField = CogFunctions.toComplexField(this)

  /** Explicit conversion of a Field to a ComplexVectorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexVectorField but stored in a Field variable. This method also
    * supports converting a VectorField to a ComplexVectorField.
    *
    * @return The converted field.
    */
  def toComplexVectorField = CogFunctions.toComplexVectorField(this)

  /** Explicit conversion of a Field to a ColorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ColorField but stored in a Field variable. This method also
    * supports converting a ScalarField or VectorField to a ColorField.
    *
    * @return The converted field.
    */
  def toColorField = CogFunctions.toColorField(this)
  
  /** Another explicit conversion, this time working on both ScalarFields and
    * VectorFields. The return type is Field though, the common base class of
    * both ComplexFields and ComplexScalarFields. */
  def toGenericComplexField = CogFunctions.toGenericComplexField(this)

  /** Takes the DCT (discrete cosine transform) of a 2D field, producing a
    * field with the same shape as the input.
    *
    * The following sequence will return the original input, within the bounds
    * of computational error:
    * {{{
    *   val field
    *   val transformed = field.dct
    *   val restored = transformed.dctInverse
    *   // restored is approximately equal to field
    * }}}
    *
    * For a somewhat faster version, see dctTransposed.
    *
    * The DCT has several requirements:
    *
    * The number of rows and columns must each be a power of 2.
    *
    * Rows and columns are restricted to the range [256, 2048]
    *
    * @return The DCT of the input.
    */
  def dct = CogFunctions.dct(this)

  /** Takes the inverse DCT (discrete cosine transform) of a 2D field, producing
    * a field with the same shape as the input.
    *
    * The following sequence will return the original input, within the bounds
    * of computational error:
    * {{{
    *   val field
    *   val transformed = field.dct
    *   val restored = transformed.dctInverse
    *   // restored is approximately equal to field
    * }}}
    *
    * For a somewhat faster version, see dctInverseTransposed.
    *
    * The DCT has several requirements:
    *
    * The number of rows and columns must each be a power of 2.
    *
    * Rows and columns are restricted to the range [256, 2048]
    *
    * @return The DCT of the input.
    */
  def dctInverse = CogFunctions.dctInverse(this)

  /** Takes the DCT (discrete cosine transform) of a 2D field and transposes it,
    * producing a field with the same shape as the input transposed.
    *
    * The following sequence will return the original input, within the bounds
    * of computational error:
    * {{{
    *   val field
    *   val transformed = field.dctTransposed
    *   val restored = transformed.dctInverseTransposed
    *   // restored is approximately equal to field
    * }}}
    *
    * This is a somewhat faster version of dct where the transpose is not
    * important to (or can be compensated within) an application.
    *
    * The DCT has several requirements:
    *
    * The number of rows and columns must each be a power of 2.
    *
    * Rows and columns are restricted to the range [256, 2048]
    *
    * @return The DCT of the input.
    */
  def dctTransposed = CogFunctions.dctTransposed(this)

  /** Takes the inverse DCT (discrete cosine transform) of a 2D field, producing
    * a field with the same shape as the input.
    *
    * The following sequence will return the original input, within the bounds
    * of computational error:
    * {{{
    *   val field
    *   val transformed = field.dctTransposed
    *   val restored = transformed.dctInverseTransposed
    *   // restored is approximately equal to field
    * }}}
    *
    * This is a somewhat faster version of dctInverse where the transpose is not
    * important to (or can be compensated within) an application.
    *
    * The DCT has several requirements:
    *
    * The number of rows and columns must each be a power of 2.
    *
    * Rows and columns are restricted to the range [256, 2048]
    *
    * @return The DCT of the input.
    */
  def dctInverseTransposed = CogFunctions.dctInverseTransposed(this)

  /** Feedback operator, denotes a field value fed back to `this`.
    *
    * This does automatic type coercion if the field fed back is larger than
    * or equal to the size (in each dimension) of the recipient of that field.
    * This should be overridden where illegal.
    *
    * @param that Field whose value will become `this` field's value on the next clock tick.
    */
  def <==(that: Field) {
    // Check that the feedback is legal:

    // 0. This must be a Constant Op.
    if (!opcode.isInstanceOf[ConstantOp])
      feedbackError("cannot feed back a signal using <== to non-constant field")

    // 1. The dimensionality of both fields must be the same.
    if (fieldType.fieldShape.dimensions != that.fieldType.fieldShape.dimensions)
      typeError(opcode, fieldType, that.fieldType)

    // 2. The tensor shape must be the same.
    if (fieldType.tensorShape != that.fieldType.tensorShape)
      typeError(opcode, fieldType, that.fieldType)

    // 3. The element type must be the same.
    if (fieldType.elementType != that.fieldType.elementType)
      typeError(opcode, fieldType, that.fieldType)

    // 4. The fed-back field must be at least as big in every dimension as the
    // receiving field, `this`.
    val fromShape = that.fieldType.fieldShape
    val toShape = this.fieldType.fieldShape
    for (i <- 0 until fromShape.dimensions)
      if (fromShape(i) < toShape(i))
        typeError(opcode, fieldType, that.fieldType)

    // Feedback is legal, add type coercion if necessary.
    if (fieldType.fieldShape != that.fieldType.fieldShape) {
      // This coerce operator here is controversial, because it could hide
      // bugs. However, the need for it is data dependent. If we revisit
      // this and remove implicit coerce, we still need the dummy copy as
      // in the else clause below for isolation between recurrence driver
      // and the recurrence input. As a compromise, we print out a warning that
      // the coerce is occurring.
      println("COG COMPILER WARNING, <== operator: \n" +
              "  The field fed back to a constant field with the <== \n" +
              "  operator is larger than that constant. This feedback field \n" +
              "  is automatically being coerced to match by trimming. You \n" +
              "  should check that this is acceptable. \n\n" +
              "  Original constant: " + fieldType.toString + "\n" +
              "  Feedback field:    " + that.fieldType.toString + "\n")
      recurrence = CogFunctions.trim(that, fieldType.fieldShape)
    } else {
      // To avoid issues with recurrences driving recurrences, we insert an
      // operation to isolate the driver of the recurrence from the
      // recurrence itself. The reason is somewhat subtle. But the key idea
      // is that you don't want the driver writing the output at the same
      // time the recurrence is reading it. There are also issues when the
      // driver writes an actuator, which is also somewhat subtle.
      // This isolation prevents the above problems.

      // The operation used to be "+ 0" but that proved to be problematic
      // if the common subexpression elimination consolidated a "forked"
      // piping chain.  A special copy opcode was created to solve this.

      // 5. If the recurrence is a pipelined sensor, do not put a copy kernel
      // between the sensor kernel and the pipe stage, since the sensor kernel
      // performs the pipestage reset initialization.

      that.opcode match {
        case op: PipelinedColorSensorOp => recurrence = that
        case op: PipelinedSensorOp => recurrence = that
        case op: PipelinedVectorSensorOp => recurrence = that
        case _ => recurrence = Field.copy(that)
      }
    }
  }


  /** The initial virtual register assigned to this field (created by code generator). */
  private var initialVirtualFieldRegister: VirtualFieldRegister = null

  /** The virtual field register that produces this field, after the optimizer has possibly
   * replaced this it without calling this.setVirtualFieldRegiister.
   *
   * Not sure why the optimizers don't replace the source directly.  Instead,
   * we currently have an approach where the Circuit/Node classes maintain a
   * mapping from a virtual field register to the virtual field register that
   * replaced it ("stole its output").
   */
  private def virtualFieldRegister: VirtualFieldRegister = {
    if (initialVirtualFieldRegister == null)
      null
    else {
      val myCircuit = initialVirtualFieldRegister.source.circuit
      myCircuit.findStolenOutput(initialVirtualFieldRegister).asInstanceOf[VirtualFieldRegister]
    }
  }

  /** Back-annotate the Field with the virtual field register that produces this
    * field. The annotation is produced by the code generator.
    */
  private[cogx] def setVirtualFieldRegister(vfr: VirtualFieldRegister) {
    require(initialVirtualFieldRegister == null)
    initialVirtualFieldRegister = vfr
  }

  /** Get the virtual field register that produces this field. */
  private[cogx] def getVirtualFieldRegister: VirtualFieldRegister =
    virtualFieldRegister

  /** Get the source kernel and output index that produces this field. */
  private[cogx] def getSource: (OpenCLAbstractKernel, Int) = {
    val sourceKernel = virtualFieldRegister.source.asInstanceOf[OpenCLAbstractKernel]
    val outputIndex = virtualFieldRegister.sourceOutputIndex match {
      case Some(i) => i
      case None => throw new RuntimeException("Compiler error: missing source kernel.")
    }
    (sourceKernel, outputIndex)
  }

  /** Remove reference to the virtual field register backing up this field. */
  private[cogx] def release() {
    initialVirtualFieldRegister = null
  }

  /** Print out, if possible, GPU code that generates this field. */
  def printKernelCode() {
    if (virtualFieldRegister == null)
      println("Field.printCode: not instantiated yet, cannot print.")
    else
    virtualFieldRegister.source match {
      case kernel: HyperKernel =>
        println("GPU kernel code:")
        println(kernel.kernelCode(0))
        println()
      case _ =>
        println("Field.printCode: non hyper-kernels cannot be printed.")
    }
  }

  /** Check if `this` has been instantiated with a virtual field register. */
  protected def isInstantiated: Boolean =
    virtualFieldRegister != null

}

/** Factory for creating constant fields.
  *
  * This object extends Intrinsics to give the ImplicitConversions trait a
  * convenient place to access the methods defined there, as in:
  * Field.vectorField(in1, in2)
  *
  * */
object Field extends CogFunctionAPI with SemanticError {
  /** Create a field of type `fieldType`. */
  def apply(fieldType: FieldType): Field = {
    fieldType.elementType match {
      case Float32 =>
        fieldType.tensorShape.dimensions match {
          case 0 =>
            ScalarField(fieldType.fieldShape)
          case 1 =>
            VectorField(fieldType.fieldShape, fieldType.tensorShape)
          case 2 =>
            MatrixField(fieldType.fieldShape, fieldType.tensorShape)
          case x =>
            throw new RuntimeException(s"tensorShape $x not implemented yet")
        }
      case Uint8Pixel =>
        require(fieldType.dimensions == 2 && fieldType.tensorShape == Shape(3),
          "ColorField construction only possible on 2D fields with tensor length 3, found: " + fieldType)
        ColorField(fieldType.rows, fieldType.columns)
      case x =>
        throw new RuntimeException("not implemented yet")
    }
  }

  /** Create a field of type `fieldType`, with supplied op and inputs */
  def apply(opcode: Opcode, in: Array[Field], fieldType: FieldType): Field = {
    fieldType.elementType match {
      case Float32 =>
        fieldType.tensorOrder match {
          case 0 =>
            new ScalarField(opcode, in, fieldType)
          case 1 =>
            new VectorField(opcode, in, fieldType)
          case 2 =>
            new MatrixField(opcode, in, fieldType)
          case _ => throw new RuntimeException("not done yet")
        }
      case Complex32 =>
        fieldType.tensorOrder match {
          case 1 =>
            new ComplexVectorField(opcode, in, fieldType)
          case 0 =>
            new ComplexField(opcode, in, fieldType)
          case x => tensorDimensionError(x)
        }
      case Uint8Pixel =>
        new ColorField(opcode, in, fieldType)
      case _ =>
        throw new RuntimeException("not done yet")
    }
  }

  /** Create a field of a given type as an output of the given operation.
    *
    * @param operation The operation, possibly multi-output, that generates the field.
    * @param resultType The type of the desired field.
    * @return The field.
    */
  def apply(operation: Operation, resultType: FieldType): Field = {
    // Create the correct type of result field.
    val resultField = resultType.elementType match {
      case Float32 =>
        resultType.tensorOrder match {
          case 0 =>
            new ScalarField(operation, resultType)
          case 1 =>
            new VectorField(operation, resultType)
          case 2 =>
            new MatrixField(operation, resultType)
          case x =>
            throw new Exception("Unsupported tensor order: " + x)
        }
      case Complex32 =>
        resultType.tensorOrder match {
          case 0 =>
            new ComplexField(operation, resultType)
          case 1 =>
            new ComplexVectorField(operation, resultType)
          case x =>
            throw new Exception("Unsupported tensor order: " + x)
        }
      case Uint8Pixel =>
        new ColorField(operation, resultType)
      case x =>
        throw new Exception("Unsupported element type: " + x)
    }
    resultField
  }

}
