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

import cogx.cogmath.algebra.complex.Complex

/** Cog symbolic operators implemented using GPUOperators.
  */
trait CogSymbolicOperators {
  self => Field


  /** Add `this` field and `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field to be added to `this`.
    * @return Sum of the two fields.
    */
  def +(that: Field): Field

  /** Subtract `that` field from `this` field to create a third field.
  *
  * This operator follows the ''algebraic binary operator rules'' for input
  * type compatibility, implicit type conversions, and output result type.
  * See the class description for these rules.
  *
  * @param that Field to be subtracted to `this`.
  * @return `this` - `that`
  */
  def -(that: Field): Field

  /** Multiply `this` field by `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field to be multiplied by `this`.
    * @return `this` * `that`
    */
  def *(that: Field): Field

  /** Divide `this` field by `that` field to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Field that `this` will be divided by.
    * @return `this` / `that`
    */
  def /(that: Field): Field

  /** Compute `this` modulo `that` to create a third field.
    *
    * This operator follows the ''algebraic binary operator rules'' for input
    * type compatibility, implicit type conversions, and output result type.
    * See the class description for these rules.
    *
    * @param that Operand for computing `this` modulo `that`.
    * @return `this` % `that`
    */
  def %(that: Field): Field

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
  def >(that: Field): Field

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
  def >=(that: Field): Field

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
  def <(that: Field): Field

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
  def <=(that: Field): Field

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
  def ===(that: Field): Field

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
  def !===(that: Field): Field

  // Outer product
  def ^(that: Field): Field

  /** Add the constant `that` to every element in `this` to produce a result
    * with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be added to every number in `this`.
    * @return A field equal to the sum of `this` and `that`, where `that` has
    *         been added to every numeric component of `this`.
    */
  def +(that: Float): Field

  /** Subtract the constant `that` from every element in `this` to produce a
    * result with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be sutracted from every number in `this`.
    * @return A field equal to the difference of `this` and `that`, where `that`
    *         has been subtracted from every numeric component of `this`.
    */
  def -(that: Float): Field

  /** Multiply every element in `this` by `that` to produce a result
    * with the same field shape and tensor shape as `this`.
    *
    * @param that Constant to be multiplied with every number in `this`.
    * @return A field equal to the product of `this` and `that`, where
    *         every numeric component of `this` has been multiplied by `that`.
    */
  def *(that: Float): Field

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
  def /(that: Float): Field

  /** For every element, `x`, in `this`, compute `x modulo that`. The result
    * has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for modulo operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x modulo that`.
    */
  def %(that: Float): Field

  /** For every element, `x`, in `this`, compute `x greaterThan that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for > operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x greaterThan that`.
    */
  def >(that: Float): Field

  /** For every element, `x`, in `this`, compute `x greaterThanEqual that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for >= operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x greaterThanEqual that`.
    */
  def >=(that: Float): Field

  /** For every element, `x`, in `this`, compute `x lessThan that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for < operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x lessThan that`.
    */
  def <(that: Float): Field

  /** For every element, `x`, in `this`, compute `x lessThanEqual that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for <= operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x lessThanEqual that`.
    */
  def <=(that: Float): Field

  /** For every element, `x`, in `this`, compute `x identicallyEquals that` where
    * 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for === operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x identicallyEquals that`.
    */
  def ===(that: Float): Field


  /** For every element, `x`, in `this`, compute `x notIdenticallyEquals that`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @param that Operand for !=== operator.
    * @return A field equal to `this` with each element, `x`, mapped to
    *         `x notIdenticallyEquals that`.
    */
  def !===(that: Float): Field

  /** Multiply each number in a field by -1.
    *
    * The resulting field has the same field shape and tensor shape as `this`.
    *
    * @return The negative of the input field.
    */
  def unary_-(): Field

  /** Add the constant `that` to every element in `this`. This will coerce
   * `this` to be complex before proceeding.
   *
   * @param that Constant to be added to number in `this`.
   * @return A field equal to the sum of `this` and `that`, where `that` has
   *         been added to every numeric component of `this`.
   */
  def +(that: Complex): Field

  /** Subtract the constant `that` from every element in `this`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Constant to be subtracted from each number in `this`.
    * @return A field equal to the difference of `this` and `that`, where `that`
    *         has been subtracted from every numeric component of `this`.
    */
  def -(that: Complex): Field

  /** Multiply every element in `this` by `that`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Multiplicand
    * @return A field equal to the product of `this` and `that`, where
    *         every numeric component of `this` has been multiplied by `that`.
    */
  def *(that: Complex): Field

  /** Divide every element in `this` by `that`. This will coerce
    * `this` to be complex before proceeding.
    *
    * @param that Divisor.
    * @return A field equal to the quotient of `this` and `that`, where
    *         every numeric component of `this` has been divided by `that`.
    */
  def /(that: Complex): Field

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
    * @param range For each dimension, a range of indices specifying where
    *        to extract the subfield.
    * @return Subfield of input as specified by the `range` parameters.
    */
  def apply(range: Range*): Field

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
  def apply(index: Int): Field

  /** Feedback operator, denotes a field value fed back to `this`.
  *
  * This does automatic type coercion if the field fed back is larger than
  * or equal to the size (in each dimension) of the recipient of that field.
  * This should be overridden where illegal.
  */
  def <==(that: Field): Unit
}
