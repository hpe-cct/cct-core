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

package cogx.api

import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform.{Inverse, Forward}
import cogx.compiler.parser.semantics.SemanticError
import cogx.parameters.Cog
import cogx.platform.types._
import cogx.platform.types.ElementTypes.{Float32, Uint8Pixel}
import cogx.compiler.parser.op._
import cogx.cogmath.algebra.real.Matrix
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.syntaxtree._


/** Function API for Cog.
  *
  * Algebraic operators are defined in CogOperatorAPI.
  */
trait CogFunctionAPI extends SemanticError with ImplicitConversions {

// When making Scaladoc for the CogFunctionAPI, it's better to do it without
// the implicit conversions or sematic error methods, since they are intermingled
// alphabetically with the function API and without their full parameter description.
// Scaladoc for the ImplicitConversions separately.

//trait CogFunctionAPI {

  /** Perform 1/x operation on every numeric element of a field.
    *
    * Somewhat dangerous to use if any element in the field could be zero, since
    * the resulting element would be NaN which does not throw an exception.
    *
    * @param field The input field.
    * @return Input field with each numeric element, x, mapped to 1/x.
    */
  def reciprocal(field: Field): Field =
     if (isComplexField(field.fieldType))
       UnaryOperator(ComplexReciprocalOp, field)
     else
       UnaryOperator(ReciprocalOp, field)

  /** Given a matrix field, compute the condition number for each matrix in
    * that field. This is currently limited to fields containing 2 x 2 matrices.
    *
    * @param field The input field.
    * @return Scalar field holding the condition numbers for the corresponding
    *         matrices in the input matrix field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def conditionNumber(field: Field): Field =
    UnaryOperator(ConditionNumberOp, field)


  /** Compute the determinant of every matrix in a matrix field.
    *
    * @param field The input field.
    * @return Scalar field holding the determinants for the corresponding
    *         matrices in the input matrix field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def determinant(field: Field): Field =
    UnaryOperator(DeterminantOp, field)

  /** Raise each number in `field` to the power `that`.
    * The resulting field has the same field shape and tensor shape as `field`.
    *
    * @param field The input field.
    * @param that The exponent.
    * @return Input field with every number raised to the power `that`.
    */
  def pow(field: Field, that: Int): Field =
    UnaryOperator(PownConstOp(that), field)  // power

  /** Raise each number in `field`  to the power `that`.
    * The resulting field has the same field shape and tensor shape as `field`.
    *
    * @param field The input field.
    * @param that The exponent.
    * @return Input field with every number raised to the power `that`.
    */
  def pow(field: Field, that: Float): Field =
    UnaryOperator(PowConstOp(that), field) // power

  /** For every element, `x`, in `field`, compute `max(x, that)`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `filed`.
    *
    * @param field The input field.
    * @param value Operand for max operator.
    * @return A field equal to `field` with each element, `x`, mapped to
    *         `max(x, that)`.
    */
  def max(field: Field, value: Float): Field =
    UnaryOperator(MaxConstOp(value), field)

  /** For every element, `x`, in `field`, compute `min(x, that)`
    * where 1.0f represents a "true" result and 0.0f represents a "false" result.
    * The resulting field has the same field shape and tensor shape as `field`.
    *
    * @param field The input field.
    * @param value Operand for min operator.
    * @return A field equal to `field` with each element, `x`, mapped to
    *         `min(x, that)`.
    */
  def min(field: Field, value: Float): Field =
    UnaryOperator(MinConstOp(value), field)


  /** Take the absolute value of every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        absoluteValue(x).
    */
  def abs(field: Field): Field =
    UnaryOperator(AbsOp, field)

  /** Take the arccosine of every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        acos(x).
    */
  def acos(field: Field): Field =
    UnaryOperator(AcosOp, field)

  /** Take the arcsine of every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        asin(x).
    */
  def asin(field: Field): Field =
    UnaryOperator(AsinOp, field)

  /** Perform bilateral filtering using `spatialFilter` for spatial filtering
    * (typically this will be a truncated Gaussian) and a Gaussian with
    * width `rangeSigma` for range filtering.
    *
    * WARNING: This is an old algorithm that is not very efficient. Consider
    * a newer algorithm such as domain transform filtering.
    *
    * @param field The input field.
    */
  @deprecated("inefficient, use domain transform filter instead", "4.1.1")
  def bilateralFilter(field: Field, spatialFilter: Matrix, rangeSigma: Float): Field =
    UnaryOperator(BilateralFilter2DOp(spatialFilter, rangeSigma), field)

  /** Take the cosine of every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        cos(x).
    */
  def cos(field: Field): Field =
    UnaryOperator(CosOp, field)

  /** Take the hyperbolic cosine of every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        cosh(x).
    */
  def cosh(field: Field): Field =
    UnaryOperator(CoshOp, field)

  /** Apply the exponential function to every numeric element in `field`. This
    * works for complex and real fields.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        exp(x).
    */
  def exp(field: Field): Field =
    if (isComplexField(field.fieldType))
      UnaryOperator(ComplexExpOp, field)
    else
      UnaryOperator(ExpOp, field)

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
    * @param field The input field.
    * @return A flipped version of the input field.
    */
  def flip(field: Field): Field =
    UnaryOperator(FlipOp, field)

  /** Map each numeric element of the input field to the largest integer
    * which is less than or equal to that element.
    *
    * @param field The input field.
    * @return Field where every number element has been "floored" to an integer.
    */
  def floor(field: Field): Field =
    UnaryOperator(FloorOp, field)

  /** Apply the natural logarithm to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        log(x).
    */
  def log(field: Field): Field =
    UnaryOperator(LogOp, field)

  /** Apply the signum operator to every numeric element in `field`.
    *
    * Signum(x) is defined to be:
    *
    *  1 if x > 0
    *
    *  0 if x: Field =
    *
    * -1 if x < 0
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        signum(x).
    */
  def signum(field: Field): Field =
    UnaryOperator(SignumOp, field)

  /** Apply the sine operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        sin(x).
    */
  def sin(field: Field): Field =
    UnaryOperator(SinOp, field)

  /** Apply the hyperbolic sine operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        sinh(x).
    */
  def sinh(field: Field): Field =
    UnaryOperator(SinhOp, field)

  /** Apply the square operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        (x * x).
    */
  def sq(field: Field): Field =
    UnaryOperator(SqOp, field)

  /** Apply the square root operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        sqrt(x).
    */
  def sqrt(field: Field): Field =
    UnaryOperator(SqrtOp, field)

  /** Apply the tangent operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        tan(x).
    */
  def tan(field: Field): Field =
    UnaryOperator(TanOp, field)

  /** Apply the hyperbolic tangent operator to every numeric element in `field`.
    *
    * @param field The input field.
    * @return Field equal to input field with each element, x, mapped to
    *        tanh(x).
    */
  def tanh(field: Field): Field =
    UnaryOperator(TanhOp, field)

  /** Compute the backward divergence of a 2D vector field.
    *
    * This is the adjoint operator of forwardGradient.
    *
    * @param field The input field.
    * @return A scalar field representing the backward divergence of an
    *        input vector field.
    */
  def backwardDivergence(field: Field): Field =
    UnaryOperator(BackwardDivergenceOp, field)

  /** Compute the backward gradient of a 2D scalar or vector field.
    *
    * @param field The input field.
    * @return For a scalar field input, a vector field representing the
    *         forward gradient of the input; for a vector input field, an
    *         order 4 tensor field representing the forward gradient of the
    *         input.
    */
  def backwardGradient(field: Field): Field =
    UnaryOperator(BackwardGradientOp, field)

  /** Compute the central gradient of a scalar of vector field.
    *
    * The central gradient at the point (x, y) is computed using the values at
    * (x - 1, y), (x, y - 1), (x, y + 1), and (x + 1, y + 1)
    *
    * @param field The input field.
    * @return For a scalar field input, returns a vectorField representing the
    *        central gradient of the input. For a vector field input, returns
    *        an order-4 tensor field representing the central gradient of the
    *        input.
    */
  def centralGradient(field: Field): Field =
    UnaryOperator(CentralGradientOp, field)

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
    * @param field The input field.
    * @param borderPolicy Policy to use for producing border values.
    * @param shape The shape of the expanded output field.
    * @return Expanded field with new elements filled in from the nearest input
    *       edge per `borderPolicy`.
    */
  def expand(field: Field, borderPolicy: BorderPolicy, shape: Shape): Field =
    UnaryOperator(ExpandBorderOp(shape, borderPolicy), field)


  /** Expand an N-dimensional field by padding it with values per the border
    * policy, maintaining the origin. The supported border policies are
    * BorderZero, BorderClamp and BorderCyclic.
    *
    * See expand(borderPolicy, shape) for a description of the border policies.
    *
    * @param field The input field.
    * @param borderPolicy Policy to use for producing border values.
    * @param sizes The integer dimensions of the expanded output field.
    * @return Expanded field with new elements filled in from the nearest input
    *       edge per `borderPolicy`.
    */
  def expand(field: Field, borderPolicy: BorderPolicy, sizes: Int*): Field =
    UnaryOperator(ExpandBorderOp(Shape(sizes: _*), borderPolicy), field)


  /** Reduce a scalar field to a 0D scalar containing a single element which
    * is the maximum element in the input field.
    *
    * @param field The input field.
    * @return 0D scalar field holding the max value in input field.
    */
  def fieldReduceMax(field: Field): Field =
    UnaryOperator(FieldReduceMaxOp, field)

  /** Reduce a scalar field to a 0D scalar containing a single element which
    * is the minimum element in the input field.
    *
    * @param field The input field.
    * @return 0D scalar field holding the min value in input field.
    */
  def fieldReduceMin(field: Field): Field =
    UnaryOperator(FieldReduceMinOp, field)

  /** Reduce a scalar field to a 0D scalar containing the sum of all the
    * elements in the input field.
    *
    * @param field The input field.
    * @return 0D scalar field holding the sum of values in input field.
    */
  def fieldReduceSum(field: Field): Field =
    UnaryOperator(FieldReduceSumOp, field)

  /** Find the median value in a scalar field.
    *
    * @param field The input field.
    * @return 0D scalar field holding the median value of the input field.
    */
  def fieldReduceMedian(field: Field): Field =
    UnaryOperator(FieldReduceMedianOp, field)

  /** Compute the forward gradient of a 2D scalar or vector field.
    *
    * This is the adjoint operator of backwardDivergence.
    *
    * @param field The input field.
    * @return For a scalar field input, a vector field representing the
    *         forward gradient of the input; for a vector input field, an
    *         order 4 tensor field representing the forward gradient of the
    *         input.
    */
  def forwardGradient(field: Field): Field =
    UnaryOperator(ForwardGradientOp, field)

  /** Invert all matrices in a matrix field using Gauss-Jordan elimination.
    *
    * This is numerically stable only for small matrices, and let's not even
    * get in to singular matrices. Be careful.
    *
    * @param field The input field.
    * @return A matrix field where each matrix is the (approximate) inverse of
    *         the corresponding matrix in the input matrix field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def invertMatrices(field: Field): Field =
    UnaryOperator(MatrixInvertOp, field)

  /** Compute a random field based on this input field using cellular
    * automaton based RNG. The output ranges from [0 to 1] (inclusive) and is
    * uniformly distributed.
    *
    * @param field The input field.
    * @param bits The number of bits of precision to use
    * @return A Vector field where each point in the field is random
    */
  def random(field: Field, bits:Int) : Field = {
    val points = field.tensorShape.points * field.fieldShape.points
    require(field.tensorShape.dimensions == 1, "must be a vector field")
    require(bits <= 24, "Currently limited to 24 bits of precision")
    //require(bits >0 && bits <= 8, "Maximum of 8 bits of precision")
    require(points >= 64, s"Field has $points points. Random method only works for fields with more than 64 points")
    UnaryOperator(RandomOp(bits), field)
  }


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
    * @param field The input field.
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A scalar field with the same shape as the input, where each
    *        element is equal to the local max of the input field as defined
    *        by `neighborhood`.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def localMax(field: Field, neighborhood: Matrix): Field = {
    val op = LocalMax2DOp(neighborhood, BorderClamp, CrossCorrelationOrientation)
    UnaryOperator(op, field)
  }


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
    * @param field The input field.
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A scalar field with the same shape as the input, where each
    *        element is equal to the local min of the input field as defined
    *        by `neighborhood`.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def localMin(field: Field, neighborhood: Matrix): Field = {
    val op = LocalMin2DOp(neighborhood, BorderClamp, CrossCorrelationOrientation)
    UnaryOperator(op, field)
  }

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
    * @param field The input field.
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local max search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A vector field with the same field shape as the input scalar
    *        field, with each vector pointing to the maximum value in the
    *        neighborhood relative to the center of the neighborhood.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def localMaxPosition(field: Field, neighborhood: Matrix): Field = {
    val op = LocalMaxPosition2DOp(neighborhood, BorderClamp, CrossCorrelationOrientation)
    UnaryOperator(op, field)
  }

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
    * @param field The input field.
    * @param neighborhood An odd-sized matrix describing the neighborhood for
    *        doing the local min search. TO DO: This will become a field rather
    *        than a matrix. XXX
    * @return A vector field with the same field shape as the input scalar
    *        field, with each vector pointing to the minimum value in the
    *        neighborhood relative to the center of the neighborhood.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def localMinPosition(field: Field, neighborhood: Matrix): Field ={
    val op = LocalMinPosition2DOp(neighborhood, BorderClamp, CrossCorrelationOrientation)
    UnaryOperator(op, field)
  }

  /** Filter a 2D input scalar field with a 3 x 3 median filter.
    *
    * @param field The input field.
    * @return A 2D scalar field with the same field shape as the input, with
    *        each element median filtered.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def medianFilter(field: Field): Field =
    UnaryOperator(MedianFilterOp, field)

  /** Convert a matrix field to a vector field by stripping out one row
    * from each matrix in the field and making it a vector.
    *
    * @param field The input field.
    * @param index The index of the desired row to strip out of each matrix
    *        and use as a vector.
    * @return A vector field made up of the stripped out vectors from the
    *        input matrix field.
    */
  def matrixRow(field: Field, index: Int): Field = {
    if (field.fieldType.tensorOrder != 2)
      tensorDimensionError(field.fieldType.tensorOrder, 2)
    UnaryOperator(TensorSliceOp(index), field)
  }

  /** Find the location/position of the maximum element in a scalar field.
    *
    * @param field The input field.
    * @return A 0D vector field containing a vector holding the indices of the
    *        maximum element.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def maxPosition(field: Field): Field =
    UnaryOperator(MaxPositionOp, field)

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
    * @param field The input field.
    * @return A copy of the input field with locally non-maximum values set to
    *        zero.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def nonMaximumSuppression(field: Field): Field =
    UnaryOperator(NonMaximumSuppressionOp, field)

  /** Forces locally non-maximum pixels to zero for calar fields,
    * but only when comparing the pixels on either side of
    * a given pixel, using `orientation` to define where to look for the
    * two pixels for comparison.
    *
    * @param field The input field.
    * @param orientation A scalar field holding the local orientation of the
    *        input field at every point. Orientation ranges from -Pi/2 to Pi/2,
    *        where the corresponding orientation rotates clockwise from
    *       horizontal (-Pi/2) to vertical (0) to horizontal (Pi/2).
    * @return A copy of the input field with locally non-maximum values set to
    *        zero.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def orientedNonMaximumSuppression(field: Field, orientation: Field): Field =
    BinaryOperator(OrientedNonMaximumSuppressionOp, field, orientation)

  /** Normalize a scalar field using the L1 norm.
    *
    * @param field The input field.
    * @return A normalized copy of the input field.
    */
  def normalizeL1(field: Field): Field =
    UnaryOperator(NormalizeL1Op, field)

  /** Normalize a scalar field using the L2 norm.
    *
    * @param field The input field.
    * @return A normalized copy of the input field.
    */
  def normalizeL2(field: Field): Field =
    UnaryOperator(NormalizeL2Op, field)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * sum of its components.
    *
    * @param field The input field.
    * @return A scalar field with each element equal to the sum of components of
    *        the corresponding vector in the input field.
    */
  def reduceSum(field: Field): Field =
    UnaryOperator(TensorReduceSumOp(field.tensorShape.points), field)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * summing the first `factor` input vector elements to form the first output
    * vector element, and so forth.
    *
    * @param field The input field.
    * @param factor The factor by which the input field tensor length is reduced.
    * @return A vector field with each element equal to the sum of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceSum(field: Field, factor: Int): Field =
    UnaryOperator(TensorReduceSumOp(factor), field)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * minimum of its components.
    *
    * @param field The input field.
    * @return A scalar field with each element equal to the minimum of
    *        the components of the corresponding vector in the input field.
    */
  def reduceMin(field: Field): Field =
    UnaryOperator(TensorReduceMinOp(field.tensorShape.points), field)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * taking the min() of the first `factor` input vector elements to form the
    * first output vector element, and so forth.
    *
    * @param field The input field.
    * @param factor The factor by which the input field tensor length is reduced.
    * @return A vector field with each element equal to the min() of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceMin(field: Field, factor: Int): Field =
    UnaryOperator(TensorReduceMinOp(factor), field)

  /** Reduce a vector field to a scalar field by mapping each vector to the
    * maximum of its components.
    *
    * @param field The input field.
    * @return A scalar field with each element equal to the maximum of
    *        the components of the corresponding vector in the input field.
    */
  def reduceMax(field: Field): Field =
    UnaryOperator(TensorReduceMaxOp(field.tensorShape.points), field)

  /** Reduce a vector field to a shorter (by factor `factor`) vector field by
    * taking the max() of the first `factor` input vector elements to form the
    * first output vector element, and so forth.
    *
    * @param field The input field.
    * @param factor The factor by which the input field tensor length is reduced.
    * @return A vector field with each element equal to the max() of `factor`
    *         components of the corresponding vector in the input field.
    */
  def blockReduceMax(field: Field, factor: Int): Field =
    UnaryOperator(TensorReduceMaxOp(factor), field)

  /** Change the shape of a scalar field without changing the number of
    * elements in it.
    *
    * This depends on the row-major ordering we use for elements in a field.
    * Elements in any scalar field, regardless of dimension, have that linear
    * ordering. Reshaping preserves that ordering; it really does nothing
    * more than change the sizes of each dimension of the field.
    *
    * @param field The input field.
    * @param size A sequence of new sizes for each dimensions. The product of
    *        this sequence must equal the number of elements in the input
    *        field.
    * @return The input field, reshaped, with the same elements in the same
    *        linear order.
    */
  def reshape(field: Field, size: Int*): Field = reshape(field, Shape(size.toArray), Shape())

  /** Change the shape of a scalar field without changing the number of
    * elements in it.
    *
    * This depends on the row-major ordering we use for elements in a field.
    * Elements in any scalar field, regardless of dimension, have that linear
    * ordering. Reshaping preserves that ordering; it really does nothing
    * more than change the sizes of each dimension of the field.
    *
    * @param field The input field.
    * @param fieldShape the output field shape to use
    * @param tensorShape the output tensor shape to use
    * @param checkLegacyReshape Warn of uses of reshape that had different behaviors prior to libcog 4.3
    * @return The input field, reshaped, with the same elements in the same
    *        linear order.
    */
  def reshape(field: Field, fieldShape:Shape, tensorShape:Shape,
              checkLegacyReshape: Boolean = Cog.checkLegacyReshape): Field =
    UnaryOperator(ReshapeOp(fieldShape, tensorShape, checkLegacyReshape), field)

  /** Extract all subfields from a 2D scalar field into a 2-D vector field.
    *
    * @param field The input field.
    * @param diameter Sizes (rows and columns) of each subfield. Must be odd.
    * @return A 2-D vector field where the length of the vector equals the
    *        number of `diameter` x `diameter` subfields in the input.
    *        For example, a 4 x 4 input field has four subfields of size 3 x 3,
    *        so this operator would return a 2-D vector field of size 3 x 3
    *        (the size of the subfields) with vectors of length 4.
    *        Each layer of the vector field is one subfield of of the input.
    */
  def subfields(field: Field, diameter: Int): Field =
    UnaryOperator(SubfieldsOp(diameter), field)

  /** Supersample a scalar field by 2X in each dimension, replicating pixels
    * to fill in the gaps.
    *
    * @param field The input field.
    * @return Input field expanded by 2X in each dimension with pixel
    * replication.
    */
  def supersample(field: Field): Field =
    UnaryOperator(SupersampleOp, field)

  /** Perform a "domain transform" on the rows of a color field or tensor field
    * as a step in edge-aware normalized convolution.
    *
    * For details, see the paper "Domain transform
    * for edge-aware image and video processing," Gastal and Oliveira, 2011.
    * Normally this is not useful for end-users, but you're welcome to try it
    * if you would like to write your own edge-aware filters.
    *
    * @param field The input field.
    * @param spaceSigma Width of filter in space.
    * @param rangeSigma Width of filter in range.
    * @return The domain transform of the rows (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def domainTransformRows(field: Field, spaceSigma: Float, rangeSigma: Float): Field =
    UnaryOperator(DomainTransformRowsOp(spaceSigma, rangeSigma), field)

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
    * @param field The input field.
    * @param domainTransform The cumulative domain transform for the image.
    * @param boxFilterRadius Radius of box filter.
    * @return The domain transform of the rows (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def domainFilterRows(field: Field, domainTransform: Field, boxFilterRadius: Float): Field =
      NaryOperator(DomainFilterRowsOp,
        Array(field, domainTransform, ScalarField(boxFilterRadius)))

  /** Perform a "domain filter" on the columns of a color field as a step in
    * edge-aware normalized convolution. This "correlates" an adaptive box
    * filter with each pixel in the columns of the color field, guided by
    * `domainTransform`.
    *
    * This operation works only on color fields and is an optimization. The
    * following code sequences, A and B, are functionally identical:
    * {{{
    *    // Prepare for filtering
    *    val rowTransform: Field =
    *    val colTransform: Field =
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
    * @param field The input field.
    * @param domainTransform The cumulative domain transform for the image.
    * @param boxFilterRadius Radius of box filter.
    * @return The domain transform of the columns (equation 11) in the
    *         Gastal paper as a scalar field.
    */
  @deprecated("will be moved to a library", "4.1.1")
  def domainFilterColumns(field: Field, domainTransform: ScalarField, boxFilterRadius: Float): Field =
    NaryOperator(DomainFilterColumnsOp,
      Array(field, domainTransform, ScalarField(boxFilterRadius)))

  /** Transpose all the matrices in a matrix field to produce a new matrix
    * field.
    *
    * @param field The input field.
    * @return Copy of input matrix field with all matrices transposed.
    */
  def transposeMatrices(field: Field): Field =
    UnaryOperator(MatrixTransposeOp, field)

  /** Transpose each vector in a vector field to a single row matrix, thus
    * creating a matrix field from a vector field
    *
    * @param field The input field.
    * @return Copy of input matrix field with all vectors transposed to
    *        single row matrices.
    */
  def transposeVectors(field: Field): Field =
    UnaryOperator(VectorTransposeOp, field)

  /** "Trim" a field to a smaller size by clipping off indices (maintaining the
    * origin).
    *
    * @param field The input field.
    * @param resultShape Desired shape of the resulting field; must not be
    *        larger in any dimension than the corresponding dimension of the
    *        input field
    * @return The trimmed field.
    */
  def trim(field: Field, resultShape: Shape): Field =
    UnaryOperator(TrimOp(resultShape), field)

  /** Convert a vector field to a scalar field by extracting one element of each
    * vector.
    *
    * @param field The input field.
    * @param index The element of each vector to be extracted.
    * @return Input field with tensor order reduced by one.
    */
  def vectorElement(field: Field, index: Int): Field = {
    if (field.fieldType.tensorOrder != 1)
      tensorDimensionError(field.fieldType.tensorOrder, 1)
    UnaryOperator(TensorSliceOp(index), field)
  }

  /** Convert a vector field to a smaller vector field by extracting a dynamically
    * indexed a range of elements
    *
    * @param field The input field.
    * @return Input field with tensor order reduced by one.
    */
  def vectorElements(field: Field, indices:Field): Field =
    BinaryOperator(VectorElementsOp, field, indices)

  /** Compute the "winner" of a scalar field by mapping its largest element
    * to 1.0f and the other elements to 0.0f
    *
    * This is not well-defined when multiple elements share the maximum value,
    * so beware.
    *
    * @param field The input field.
    * @return A scalar field with the same shape as the input, with a single
    *        element containing 1.0f and the rest containing 0.0f.
    */
  def winnerTakeAll(field: Field): Field =
    UnaryOperator(WinnerTakeAllOp, field)

  /** Extract the real part of a complex field as a scalar field.
    *
    * @param field The input field.
    * @return A scalar field with the same shape as the input, with each element
    *        equal to the real part of the corresponding complex element in the
    *        input.
    */
  def realPart(field: Field): Field =
    UnaryOperator(RealPartOp, field)

  /** Extract the imaginary part of a complex field as a scalar field.
    *
    * @param field The input field.
    * @return A scalar field with the same shape as the input, with each element
    *        equal to the imaginary part of the corresponding complex element in
    *        the input.
    */
  def imaginaryPart(field: Field): Field =
    UnaryOperator(ImaginaryPartOp, field)

  /** Downsample a field by taking every nth element.
    *
    * Example: for input field {1,2,3,4}
    * {{{
    *     input.downsample(2) yields {1,3}
    *
    *     input.downsample(2,1) yields {2,4}
    * }}}
    *
    * Further examples of downsampling on a 2-dimensional input:
    *
    * <img src="../../doc-images/downsample.png">
    *
    * @param field The input field.
    * @param factor The sampling factor, defaults to 2.
    * @param phase The offset within the scaled up field to put the input
    *        data, defaults to an offset of 0.
    */
  def downsample(field: Field, factor:Int = 2, phase:Int = 0): Field =
    UnaryOperator(DownsampleOp(factor, phase), field)

  /** Upsample with zeroes inserted between field points, increasing
    * the size of the input in all dimensions.
    *
    * Example: for input field {1,2,3}
    * {{{
    *    input.upsample(2) yields {1,0,2,0,3,0}
    *
    *    input.upsample(2,1) yields {0,1,0,2,0,3}
    * }}}
    *
    * @param field The input field.
    * @param factor The sampling factor, defaults to 2.
    * @param phase The offset within the scaled up field to put the input
    *        data, defaults to an offset of 0.
    */
  def upsample(field: Field, factor: Int = 2, phase: Int = 0): Field =
    UnaryOperator(UpsampleOp(factor, phase), field)

  /** Shift a 1D scalar field left (negative `colShift`) or right (positive
    * `colShift`), pulling in zeroes where necessary.
    *
    * @param field The input field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shift(field: Field, colShift: Int): Field =
    UnaryOperator(ShiftOp(Array(colShift), BorderZero), field)

  /** Shift a 2D scalar field in both dimensions, pulling in zeroes where
    * necessary. Negative shift amounts result in shifts up and to the left,
    * while positive shift amounts result in shifts down and to the right.
    *
    * @param field The input field.
    * @param rowShift Number of pixels to shift field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shift(field: Field, rowShift: Int, colShift: Int): Field =
    UnaryOperator(ShiftOp(Array(rowShift, colShift), BorderZero), field)

  /** Shift a 1D scalar field left (negative `colShift`) or right (positive
    * `colShift`), pulling in values from the opposite side where necessary.
    *
    * @param field The input field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shiftCyclic(field: Field, colShift: Int): Field =
    UnaryOperator(ShiftOp(Array(colShift), BorderCyclic), field)

  /** Shift a 2D scalar field in both dimensions, pulling in values from the opposite
    * side where necessary. Negative shift amounts result in shifts up and to the
    * left, while positive shift amounts result in shifts down and to the right.
    *
    * @param field The input field.
    * @param rowShift Number of pixels to shift field.
    * @param colShift Number of pixels to shift field.
    * @return Shifted input field.
    */
  def shiftCyclic(field: Field, rowShift: Int, colShift: Int): Field =
    UnaryOperator(ShiftOp(Array(rowShift, colShift), BorderCyclic), field)

  /** Transpose a 2D tensor field or color field.
    *
    * @param field The input field.
    */
  def transpose(field: Field): Field =
    UnaryOperator(Transpose2DOp, field)

  /** Compute atan2 of two identically shaped scalar fields.
    *
    * @param field The input field.
    * @return A scalar field that's the atan2 of the two input fields.
    */
  def atan2(field: Field, f2: Field): Field =
    BinaryOperator(Atan2Op, field, f2)

  /** Convolve or cross-correlate an image field with a filter field.
    *
    * Internal use only. Common code for convolve and crossCorrelate.
    */
  private[api] def convolveInternal(field: Field,
                                           filter: Field,
                                           borderPolicy: BorderPolicy,
                                           filterOrientation: FilterOrientation,
                                           samplingPolicy: ConvolutionSamplingPolicy,
                                           vectorMode: VectorMode,
                                           batchSize: Int): Field = {
    val fieldType = field.fieldType
    val filterType = filter.fieldType

    // Generally, the platform's kernels should treat DownSampling(1) and UpSampling(1) just like NoSampling,
    // but just in case, we map these degenerate cases right here.
    val sampling = samplingPolicy match {
      case UpsampleInputConvolution(1) => NoSamplingConvolution
      case DownsampleOutputConvolution(1) => NoSamplingConvolution
      case _ => samplingPolicy
    }

    def checkProjection(keyword: String) {
      if (!isRealField(fieldType))
        error(keyword + " requires a real image input," +
          "found " + fieldType)
      if (!isRealField(filterType))
        error(keyword + " requires a real filter input," +
          "found " + filterType)
      if (fieldType.tensorOrder > 1)
        tensorDimensionError(fieldType.tensorOrder, 1)
      if (filterType.tensorOrder > 1)
        tensorDimensionError(filterType.tensorOrder, 1)
    }

    vectorMode match {
      // "filter adjoint" convolution (or crossCorrelation) is where the image is held by
      // a N-plane VectorField and a K-plane image "representation" is used to reconstruct
      // a filter frame held as a K*N-plane VectorField.  To aide the coding of parameterized libraries,
      // we support the degenerate case of N or K equal to 1.
      case FilterAdjoint =>
        if (!isRealField(fieldType))
          error("convolveFilterAdjoint/crossCorrelateFilterAdjoint requires a real image input," +
            "found " + fieldType)
        if (!isRealField(filterType))
          error("convolveFilterAdjoint/crossCorrelateFilterAdjoint requires a real filter input," +
            "found " + filterType)
        if (fieldType.tensorOrder > 1)
          tensorDimensionError(fieldType.tensorOrder, 1)
        if (filterType.tensorOrder > 1)
          tensorDimensionError(filterType.tensorOrder, 1)

      // "projectFrame" crossCorrelation is where the image is held by
      // a N-plane VectorField and the frame of K filters is represented as a
      // K*N-plane VectorField.  To aide the coding of parameterized libraries,
      // we support the degenerate case of N or K equal to 1.  Conceptually, the
      // N-plane image is stacked K times with itself and the K*N-plane result
      // is plane-by-plane crossCorrelated with the filter frame
      // to produce a K*N plane result.

      case ProjectFrame => checkProjection("frameProject")

      // "projectFrameBlockReduceSum" crossCorrelation is equivalent to a ProjectFrame
      // operation, followed by a blockReduceSum(N).  The result is a K-plane result,
      // where K is the number of logical filters (note that the filter VectorField is of
      // depth K*N, where N is the number of image planes).

      case ProjectFrameBlockReduceSum => checkProjection("frameProject")

      // "backProjectFrame" convolution is where the image is held by
      // a N-plane VectorField and the frame of K filters is represented as a
      // K*N-plane VectorField.  To aide the coding of parameterized libraries,
      // we support the degenerate case of N or K equal to 1.  Conceptually, the
      // i-th image plane is convolved with the i-th block of K filters.  The
      // stack of output planes is then shuffled so that a blockReduce(N) produces
      // the desired summing of output planes for back projection.  For each
      // output plane p, the image plane used has index p % N, and the filter
      // plane used is K * (p % N) + p / N.

      case BackProjectFrame => checkProjection("frameBackProject")

      // "backProjectFrameBlockReduceSum" crossCorrelation is equivalent to a BackProjectFrame
      // operation, followed by a blockReduceSum(N).  The result is a K-plane result,
      // where K is the number of logical filters (note that the filter VectorField is of
      // depth K*N, where N is the number of image planes).

      case BackProjectFrameBlockReduceSum => checkProjection("frameBackProject")

      case PlaneByPlane =>
    }
    // Convolving a scalar field with a matrix field has a special interpretation:
    // each tensor of the matrix field is considered a 2D filter and the result
    // is a vector field.  This works on real fields only.
    if (isRealField(fieldType) && isRealField(filterType) &&
      fieldType.tensorOrder == 0 && filterType.tensorOrder == 2) {
      val opcode = ScalarMatrixConvolve2DOp(borderPolicy, filterOrientation, sampling)
      BinaryOperator(opcode, field, filter)
    }
    else {
      // We don't support FFT-based fast convolution (which must be used when we
      // have complex inputs) involving matrix fields.
      if (isComplexField(fieldType) || isComplexField(filterType)) {
        if (fieldType.tensorOrder > 1)
          tensorDimensionError(fieldType.tensorOrder, 1)
        if (filterType.tensorOrder > 1)
          tensorDimensionError(filterType.tensorOrder, 1)
      }
      val opcode = ConvolveOp(borderPolicy, filterOrientation,
        sampling, vectorMode, batchSize)
      BinaryOperator(opcode, field, filter)
    }
  }

  /** Cross-correlate a scalar/vector/matrix field with a filter.
    *
    * @param field The input field.
    * @param filter The filter to use for cross-correlation; must be square and
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        crossCorrelation, also helps to determine the size of output field.
    * @return Input field cross-correlated with `filter` using the supplied
    *        border and sampling policies.
    */
  def crossCorrelate(field: Field, 
                     filter: Field,
                     borderPolicy: BorderPolicy,
                     samplingPolicy: ConvolutionSamplingPolicy=NoSamplingConvolution): Field =
    convolveInternal(field, filter, borderPolicy,
      CrossCorrelationOrientation, samplingPolicy, PlaneByPlane, 1)

  /** Convolve a scalar/vector/matrix field with a filter.
    *
    * @param field The input field.
    * @param filter The filter to use for convolution; must be square and with
    *        odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along borders,
    *        implicitly helps to determine size of output field.
    * @param samplingPolicy Upsampling, downsampling optionally embedded in the
    *        convolution, also helps to determine the size of output field.
    * @return Input field convolved with `filter` using the supplied border
    *        and sampling policies.
    */
  def convolve(field: Field, 
               filter: Field,
               borderPolicy: BorderPolicy,
               samplingPolicy: ConvolutionSamplingPolicy=NoSamplingConvolution): Field =
    convolveInternal(field, filter, borderPolicy, ConvolutionOrientation,
      samplingPolicy, PlaneByPlane, 1)

  /** Separably convolve or cross-correlate an image field with a row filter and
    * column vector (separable convolution).
    *
    * Internal use only. Common code for convolveSeparable and
    *   crossCorrelateSeparable.
    */
  private[api] def convolveSeparableInternal(field: Field,
                                                    rowFilter: Field,
                                                    columnFilter: Field,
                                                    borderPolicy: BorderPolicy,
                                                    filterOrientation: FilterOrientation): Field =
  {
    val fieldType = field.fieldType
    // Check field constraints. Only real 2D fields for now.
    check(fieldType.dimensions == 2, "field must be 2D")
    check(fieldType.tensorOrder == 0, "field must be scalar")

    // Check filter constraints.
    check(rowFilter.fieldType.dimensions == 1, "row filter must be 1D")
    check(rowFilter.fieldType.tensorOrder == 0, "row filter must be scalar")
    val rowFilterSize = rowFilter.fieldType.columns
    check(rowFilterSize % 2 == 1, "row filter must be an odd size")

    check(columnFilter.fieldType.dimensions == 1, "column filter must be 1D")
    check(columnFilter.fieldType.tensorOrder == 0, "column filter must be scalar")
    val columnFilterSize = columnFilter.fieldType.columns
    check(columnFilterSize % 2 == 1, "column filter must be an odd size")

    // Check border policy
    check(borderPolicy == BorderClamp || borderPolicy == BorderZero ||
      borderPolicy == BorderCyclic, "unsupported border policy " +
      borderPolicy)

    // Do row processing first, then column processing.
    val rowProcessing = BinaryOperator(
      ConvolveRows2DOp(borderPolicy, filterOrientation), field, rowFilter
    )

    val columnProcessing = BinaryOperator(
      ConvolveColumns2DOp(borderPolicy, filterOrientation), rowProcessing,
      columnFilter
    )

    columnProcessing
  }

  /** Cross-correlate a scalar/vector/matrix field with a row filter and a
    * column filter (separable convolution).
    *
    * @param field The input field.
    * @param rowFilter The filter to use for cross-correlating the rows
    *        with odd size in each dimension.
    * @param columnFilter The filter to use for cross-correlating the columns
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling cross-correlation along
    *        borders, implicitly helps to determine size of output field.
    * @return Input field cross-correlated with filters using the supplied
    *        border policy.
    */
  def crossCorrelateSeparable(field: Field, 
                              rowFilter: Field, 
                              columnFilter: Field,
                              borderPolicy: BorderPolicy): Field =
    convolveSeparableInternal(field, rowFilter, columnFilter, borderPolicy,
      CrossCorrelationOrientation)

  /** Convolve a scalar/vector/matrix field with a row filter and a
    * column filter (separable convolution).
    *
    * @param field The input field.
    * @param rowFilter The filter to use for convolving the rows
    *        with odd size in each dimension.
    * @param columnFilter The filter to use for convolving the columns
    *        with odd size in each dimension.
    * @param borderPolicy Policy to use for handling convolution along
    *        borders, implicitly helps to determine size of output field.
    * @return Input field convolved with filters using the supplied
    *        border policy.
    */
  def convolveSeparable(field: Field, 
                        rowFilter: Field, 
                        columnFilter: Field,
                        borderPolicy: BorderPolicy): Field =
    convolveSeparableInternal(field, rowFilter, columnFilter, borderPolicy,
      ConvolutionOrientation)

  /** Cross-correlate a vector field with a vector-field filter frame.
    *
    * @param field The input field.
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
  def projectFrame(field: Field,
                   filter: Field,
                   borderPolicy: BorderPolicy,
                   samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                   batchSize: Int = 1): Field = {
    convolveInternal(field, filter, borderPolicy, CrossCorrelationOrientation,
      samplingPolicy, ProjectFrame, batchSize)
  }

  /** Cross-correlate a vector field with a vector-field filter frame and block reduce
    * the output.  This functionality is only available to the cog testing framework
    * directly.  An optimizer on the user-code sequence blockReduceSum(convolve(...ProjectFrame...))
    * provides this functionality to the user without growing the COG API.
    *
    * @param field The input field.
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
  private[cogx] def projectFrameBlockReduceSum(field: Field,
                   filter: Field,
                   borderPolicy: BorderPolicy,
                   samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                   batchSize: Int = 1): Field = {
    val convolveOp = convolveInternal(field, filter, borderPolicy, CrossCorrelationOrientation,
      samplingPolicy, ProjectFrameBlockReduceSum, batchSize)

    // When this field and the filter field are not small tensor vector fields, but have the
    // same vector length, a vector field of length 1 is generated by the convolve.  We transform
    // this to a ScalarField to match the result type of a blockReduceSum(convolve(...ProjectFrame...).
    if (convolveOp.tensorShape == Shape(1))
      UnaryOperator(TensorSliceOp(0), convolveOp)
    else
      convolveOp
  }

  /** Convolve a vector field with a vector-field filter frame.
    *
    * @param field The input field.
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
  def backProjectFrame(field: Field,
                       filter: Field,
                       borderPolicy: BorderPolicy,
                       samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                       batchSize: Int = 1): Field = {
    convolveInternal(field, filter, borderPolicy, ConvolutionOrientation,
      samplingPolicy, BackProjectFrame, batchSize)
  }

  /** Convolve a vector field with a vector-field filter frame. This functionality is only available
    * to the cog testing framework directly.  An optimizer on the user-code sequence
    * blockReduceSum(convolve(...BackProjectFrame...)) provides this functionality to the user without
    * growing the COG API.
    *
    * @param field The input field.
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
  private[cogx] def backProjectFrameBlockReduceSum(field: Field,
                       filter: Field,
                       borderPolicy: BorderPolicy,
                       samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                       batchSize: Int = 1): Field = {
    val convolveOp = convolveInternal(field, filter, borderPolicy, ConvolutionOrientation,
      samplingPolicy, BackProjectFrameBlockReduceSum, batchSize)

    // When this field and the filter field are not small tensor vector fields, but have the
    // same vector length, a vector field of length 1 is generated by the convolve.  We transform
    // this to a ScalarField to match the result type of a blockReduceSum(convolve(...BackProjectFrame...).
    if (convolveOp.tensorShape == Shape(1))
      UnaryOperator(TensorSliceOp(0), convolveOp)
    else
      convolveOp
  }

  /** Cross-correlate a vector field with a vector-field with "filter adjoint" plane mixing.
    *
    * @param field The input field.
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
  def crossCorrelateFilterAdjoint(field: Field,
                                  filter: Field,
                                  borderPolicy: BorderPolicy,
                                  samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                                  batchSize: Int = 1): Field = {
    convolveInternal(field, filter, borderPolicy, CrossCorrelationOrientation,
      samplingPolicy, FilterAdjoint, batchSize)
  }

  /** Convolve a vector field with a vector-field with "filter adjoint" plane mixing.
    *
    * @param field The input field.
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
  def convolveFilterAdjoint(field: Field,
                            filter: Field,
                            borderPolicy: BorderPolicy,
                            samplingPolicy: ConvolutionSamplingPolicy = NoSamplingConvolution,
                            batchSize: Int = 1): Field = {
    convolveInternal(field, filter, borderPolicy, ConvolutionOrientation,
      samplingPolicy, FilterAdjoint, batchSize)
  }

  /** Experimental keyword that invokes ConvolveMiniTiledHyperKernel directly */
  @deprecated("for cog development use only", "4.1.1")
  private[cogx] def convolveTiled(field: Field, f2: Field, localRows: Int, localColumns: Int,
                    rowsPerThread: Int, colsPerThread: Int,
                    borderPolicy: BorderPolicy, samplingPolicy: ConvolutionSamplingPolicy,
                    filterOrientation: FilterOrientation, vectorMode: VectorMode, batchSize: Int = 1) = {
    val opcode = ConvolveTiledOp(localRows, localColumns, rowsPerThread, colsPerThread, borderPolicy, filterOrientation,
      samplingPolicy, vectorMode, batchSize)

    BinaryOperator(opcode, field, f2)
  }

  /** Experimental keyword that invokes ConvolveToSmallFieldPipelinedTiledHyperKernel directly */
  @deprecated("for cog development use only", "4.1.1")
  private[cogx] def convolveToSmallFieldTiled(field: Field, f2: Field, localRows: Int, localColumns: Int,
                                rowsPerThread: Int,
                                borderPolicy: BorderPolicy, samplingPolicy: ConvolutionSamplingPolicy,
                                filterOrientation: FilterOrientation, vectorMode: VectorMode, batchSize: Int = 1) = {
    val opcode = ConvolveToSmallFieldTiledOp(localRows, localColumns, rowsPerThread, borderPolicy, filterOrientation,
      samplingPolicy, vectorMode, batchSize)

    BinaryOperator(opcode, field, f2)
  }

  /** Multiply a 2D matrix field, `field`, by a 2D scalar field, `f2`, to
    * produce a 2D scalar field.
    *
    * The scalar field `f2` must have the same shape as the matrices in `field`.
    * This operator is basically pretending that the `f2` scalar field is
    * really a matrix and simply dotting that matrix with every matrix in the
    * matrix field `field`, producing a scalar in the corresponding position
    * of the result scalar field.
    *
    * @param field The input field.
    * @param f2 Scalar field to multiply with `field`.
    * @return "cross dot" product of `field` and `f2`.
    */
  def crossDot(field: Field, f2: Field): Field =
    BinaryOperator(CrossDotOp, field, f2)

  /** Takes the inner product of two identically shaped tensor fields to
    * create a scalar field.
    *
    * The tensors in both fields must have the same shape. Dotting two tensors
    * involves multiplying corresponding elements in the two tensors and
    * summing the products.
    *
    * @param field The input field.
    * @param f2 Other field to "dot" with `field` field.
    * @return Scalar field where each scalar element is computed from the dot
    *       product of the tensors in the corresponding location of the two
    *       input tensor fields.
    */
  def dot(field: Field, f2: Field): Field =
    BinaryOperator(TensorDotOp, field, f2)

  /** Select the maximum value of corresponding elements in two scalar fields.
    *
    * @param field The input field.
    * @param f2 Second scalar field to compare with `field`.
    * @return Scalar field where each element is the largest corresponding
    *        elements in `field` or `f2`.
    */
  def max(field: Field, f2: Field): Field =
    BinaryOperator(MaxOp, field, f2)

  /** Select the minimum value of corresponding elements in two scalar fields.
    *
    * @param field The input field.
    * @param f2 Second scalar field to compare with `field`.
    * @return Scalar field where each element is the smallest corresponding
    *        elements in `field` or `f2`.
    */
  def min(field: Field, f2: Field): Field =
    BinaryOperator(MinOp, field, f2)

  /** Replicate a scalar field as matrices in a matrix field (each matrix in
    * the matrix field is identical to the input scalar field).
    *
    * @param field The input field.
    * @param shape The shape of the resulting output field.
    * @return A matrix field with the same field shape as `f2` and tensor
    *        shape the same as the input scalar field.
    */
  def replicate(field: Field, shape: Shape): Field =
    UnaryOperator(ReplicateOp(shape), field)

  /** Multiply a matrix field, `field`, by a scalar field, `f2`, to produce a
    * scalar field.
    *
    * The matrix field and scalar field operands must have identical field
    * shapes. Each scalar element of the scalar field is multiplied by the
    * corresponding element in the matrix field. These products are then
    * summed to produce the scalar field result (which has the same shape as
    * the matrices in the matrix field).
    *
    * @param field The input field.
    * @param f2 Scalar field operand which have the same field shape as
    *        `field`.
    * @return Product of `field` and `f2`.
    */
  def reverseCrossDot(field: Field, f2: Field): Field =
    BinaryOperator(ReverseCrossDotOp, field, f2)

  /** Solve Ax - b
    *
    * This actually solves the equation for each matrix / vector pair in the
    * fields `field` and `b`, producing a vector field representing x. The
    * vectors in b must be length 2 and the matrices in `field` must be 2 x 2.
    * Solves the equations using the pseudo inverse
    *
    * @param field The input field.
    * @param b Vector field argument.
    * @return The solution, x, to the set of linear equations.
    */
  def solve(field: Field, b: Field): Field =
    BinaryOperator(SolveOp, field, b)

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
    * @param field The input field.
    * @param guide The guide vector.
    * @param shape Shape of the window.
    * @param border Policy for handling border processing.
    * @return Extracted window.
    */
  def subfield(field: Field, guide: Field, shape: Shape,
               border: BorderPolicy = BorderClamp): Field =
    BinaryOperator(SubfieldOp(shape, border), field, guide)

  /** (1) Multiply a matrix field by a matrix field to produce a matrix field; or
    * (2) multiply a matrix field by a vector field to produce a vector field.
    *
    * For case (1), corresponding matrices in `field` and `f2` are multiplied
    * using standard matrix multiplication to produce the corresponding matrix
    * in the resulting matrix field.
    *
    * For case (2), corresponding matrix/vector pari in `field` and `f2` are
    * multiplied using standard matrix/vector multiplication to produce the
    * corresponding vector (a linear transformation of the input vector)
    * in the resulting vector field.
    *
    * @param field The input field.
    * @param f2 Matrix field or vector field; must have the same field shape
    *       as `field`.
    * @return Matrix field or vector representing the matrix multiplications of
    *       corresponding elements in the two input fields.
    */
  def transform(field: Field, f2: Field): Field =
    if (f2.fieldType.tensorOrder == 2)
      BinaryOperator(MatrixTransformMatrixOp(), field, f2)
    else
      BinaryOperator(MatrixTransformVectorOp, field, f2)

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
    * @param field The input field.
    * @param guide The guiding vector field.
    * @param border Policy for handling borders.
    * @return Input field, warped by the guide.
    */
  def warp(field: Field, guide: Field, border: BorderPolicy = BorderClamp): Field =
    BinaryOperator(WarpOp(border), field, guide)

  // These keywords are only defined for ComplexFields, so they are listed here.
  // One side-effect of this is that a real-to-complex conversion will be
  // applied if any of these methods are attempted on a real field.

  /** Take the phase of each element in a complex field.
    *
    * This is also commonly called `arg`. This is a number in the range
    * (-Pi, Pi]
    *
    * @param field The input field.
    * @return Phase of the input field.
    */
  def phase(field: Field): Field =
    UnaryOperator(PhaseOp, toGenericComplexField(field))

  /** Take the magnitude of each element in a complex field 
    *
    * @param field The input field.
    * @return Magnitude of the input field.
    */
  def magnitude(field: Field): Field =
    UnaryOperator(MagnitudeOp, toGenericComplexField(field))

  /** Take the complex conjugate of each element in a complex field 
    *
    * @param field The input field.
    * @return Conjugate of the input field.
    */
  def conjugate(field: Field): Field =
    UnaryOperator(ConjugateOp, toGenericComplexField(field))


  // As is traditionally done, the implementations for the FFT and its inverse
  // do not normalize so that FFT(inverseFFT(x)) ~== x. This is because the
  // normalization could be done in the forward direction, inverse direction,
  // or both. Here we do it in the backward direction to be compatible with
  // Matlab, and also to make convolution and cross-correlation in the frequency
  // domain straightforward.
  private def fftInverseNormalization(field: Field) =
    1.0f / field.fieldType.fieldShape.points
  private def fft2DInverseRowNormalization(field: Field) =
    1.0f / field.fieldType.fieldShape(0)
  private def fft2DInverseColumnNormalization(field: Field) =
    1.0f / field.fieldType.fieldShape(1)

  /**
   * Create a copy of a field.  Sometimes useful since 'outfield = infield' can cause trouble with recurrences
    * */

  /** Create a copy of a field.  This should not be used normally in a user model, but exists as a debugging tool
    * to help identify Cog core issues.  The Cog compiler uses this method internally between pipeline stages that
    * have no other computation between them.
    *
    * @param field The input field.
    * @return A copy of the input field.
    */
   def copy(field: Field): Field = {
    val copyOp =
      if (isComplexField(field.fieldType))
        UniqueComplexCopyOp()
      else
        UniqueCopyOp()

    UnaryOperator(copyOp, field)
  }

  /** Compute the FFT of a complex field. 
    *
    * @param field The input field.
    * @return FFT of the input field.
    */
  def fft(field: Field): Field = {
    // The FFT currently fails with columns == 1, so patch around this here:
    var goodType = field.fieldType
    while(goodType.dimensions > 0 && goodType.columns == 1)
      goodType = goodType.dropLast(1)

    if (goodType.fieldShape.points == 1) {
      copy(field)
    }
    else if (goodType != field.fieldType) {
      val freqField = fft(reshape(field, goodType.fieldShape, goodType.tensorShape))
      reshape(freqField, field.fieldShape, field.tensorShape)
    }
    else {
      field.fieldType.dimensions match {
        case 3 => UnaryOperator(FFT3DOp(), field)
        case 2 => UnaryOperator(FFT2DOp(), field)
        case 1 => UnaryOperator(FFT1DOp(), field)
        case x => fieldDimensionError(x)
      }
    }
  }

  /** Compute the inverse FFT of a complex field (includes scaling). 
    *
    * @param field The input field.
    * @return Inverse FFT of the input field.
    */
  def fftInverse(field: Field): Field = {
    // The FFT currently fails with columns == 1, so patch around this here:
    var goodType = field.fieldType
    while(goodType.dimensions > 0 && goodType.columns == 1)
      goodType = goodType.dropLast(1)

    if (goodType.fieldShape.points == 1) {
      copy(field)
    }
    else if (goodType != field.fieldType) {
      val freqField = fftInverse(reshape(field, goodType.fieldShape, goodType.tensorShape))
      reshape(freqField, field.fieldShape, field.tensorShape)
    }
    else {
      field.fieldType.dimensions match {
        case 3 => UnaryOperator(InverseFFT3DOp(fftInverseNormalization(field)), field)
        case 2 => UnaryOperator(InverseFFT2DOp(fftInverseNormalization(field)), field)
        case 1 => UnaryOperator(InverseFFT1DOp(fftInverseNormalization(field)), field)
        case x => fieldDimensionError(x)
      }
    }
  }

  /** Compute the FFT of a complex field that is input as two separate real and imaginary fields.
    *
    * @param real The real component of the complex field input.
    * @param imaginary The imaginary component of the complex field input.
    * @return FFT of the input, returned as a Tuple2 of the real and imaginary part fields.
    */
  def fftRI(real: Field, imaginary: Field): Tuple2[Field, Field] = {
    // The FFT currently fails with columns == 1, so patch around this here:
    var goodType = real.fieldType
    while(goodType.dimensions > 0 && goodType.columns == 1)
      goodType = goodType.dropLast(1)

    if (goodType.fieldShape.points == 1) {
      (copy(real), copy(imaginary))   // FFT of a single point value = that value (i.e. a DC component)
    }
    else if (goodType != real.fieldType) {
      val (freqReal, freqImaginary) =
        fftRI(reshape(real, goodType.fieldShape, goodType.tensorShape),
              reshape(imaginary, goodType.fieldShape, goodType.tensorShape))
      val reshapedReal = reshape(freqReal, real.fieldShape, real.tensorShape)
      val reshapedImaginary = reshape(freqImaginary, imaginary.fieldShape, imaginary.tensorShape)
      (reshapedReal, reshapedImaginary)
    }
    else {
      val outputs = real.fieldType.dimensions match {
        case 3 => MultiOutputOperator(FFT3DOpRI(), Array(real, imaginary))
        case 2 => MultiOutputOperator(FFT2DOpRI(), Array(real, imaginary))
        case 1 => MultiOutputOperator(FFT1DOpRI(), Array(real, imaginary))
        case x => fieldDimensionError(x)
      }
      (outputs(0), outputs(1))
    }
  }

  /** Compute the FFT of a real field.
    *
    * @param real The real component of the input whose imaginary component is 0.
    * @return FFT of the input, returned as a Tuple2 of the real and imaginary part fields.
    */
  def fftRI(real: Field): Tuple2[Field, Field] = {
    // The FFT currently fails with columns == 1, so patch around this here:
    var goodType = real.fieldType
    while(goodType.dimensions > 0 && goodType.columns == 1)
      goodType = goodType.dropLast(1)

    if (goodType.fieldShape.points == 1) {
      (copy(real), Field(real.fieldType))   // FFT of a single point value = that value (i.e. a DC component)
    }
    else if (goodType != real.fieldType) {
      val (freqReal, freqImaginary) =
        fftRI(reshape(real, goodType.fieldShape, goodType.tensorShape))
      val reshapedReal = reshape(freqReal, real.fieldShape, real.tensorShape)
      val reshapedImaginary = reshape(freqImaginary, real.fieldShape, real.tensorShape)
      (reshapedReal, reshapedImaginary)
    }
    else {
      val outputs = real.fieldType.dimensions match {
        case 3 => MultiOutputOperator(FFT3DOpRI(), Array(real))
        case 2 => MultiOutputOperator(FFT2DOpRI(), Array(real))
        case 1 => MultiOutputOperator(FFT1DOpRI(), Array(real))
        case x => fieldDimensionError(x)
      }
      (outputs(0), outputs(1))
    }
  }

  /** Compute the inverse FFT of a complex field represented as separate real and imaginary fields
    * (includes scaling).
    *
    * @param real The real component of the complex field input.
    * @param imaginary The imaginary component of the complex field input.
    * @return Inverse FFT of the input field, returned as a Tuple2 of the real and imaginary part fields.
    */
  def fftInverseRI(real: Field, imaginary: Field): Tuple2[Field, Field] = {
    // The FFT currently fails with columns == 1, so patch around this here:
    var goodType = real.fieldType
    while(goodType.dimensions > 0 && goodType.columns == 1)
      goodType = goodType.dropLast(1)

    if (goodType.fieldShape.points == 1) {
      (copy(real), copy(imaginary))   // FFT of a single point value = that value (i.e. a DC component)
    }
    else if (goodType != real.fieldType) {
      val (freqReal, freqImaginary) =
        fftInverseRI(reshape(real, goodType.fieldShape, goodType.tensorShape),
          reshape(imaginary, goodType.fieldShape, goodType.tensorShape))
      val reshapedReal = reshape(freqReal, real.fieldShape, real.tensorShape)
      val reshapedImaginary = reshape(freqImaginary, imaginary.fieldShape, imaginary.tensorShape)
      (reshapedReal, reshapedImaginary)
    }
    else {
      val outputs = real.fieldType.dimensions match {
        case 3 => MultiOutputOperator(InverseFFT3DOpRI(fftInverseNormalization(real)), Array(real, imaginary))
        case 2 => MultiOutputOperator(InverseFFT2DOpRI(fftInverseNormalization(real)), Array(real, imaginary))
        case 1 => MultiOutputOperator(InverseFFT1DOpRI(fftInverseNormalization(real)), Array(real, imaginary))
        case x => fieldDimensionError(x)
      }
      (outputs(0), outputs(1))
    }
  }

  /** Compute the FFT of the rows only in a 2D complex field.
    *
    * @param field The input field.
    * @return Input with each row transformed by the FFT.
    */
  def fftRows(field: Field): Field = {
    field.fieldType.dimensions match {
      case 2 => UnaryOperator(FFT2DSubOp(0, Forward), field)
      case x => fieldDimensionError(x)
    }
  }

  /** Compute the FFT of the columns only in a 2D complex field. 
    *
    * @param field The input field.
    * @return Input with each columns transformed by the FFT.
    */
  def fftColumns(field: Field): Field = {
    // The FFT currently fails with columns == 1, so patch around this here:
    if (field.fieldType.columns == 1)
      fft(field)
    else
      field.fieldType.dimensions match {
        case 2 => UnaryOperator(FFT2DSubOp(1, Forward), field)
        case x => fieldDimensionError(x)
      }
  }

  /** Compute the inverse FFT of the rows only in a 2D complex field. 
    *
    * @param field The input field.
    * @return Input with each row transformed by the inverse FFT.
    */
  def fftInverseRows(field: Field): Field = {
    field.fieldType.dimensions match {
      case 2 => UnaryOperator(FFT2DSubOp(0, Inverse,
        fft2DInverseRowNormalization(field)), field)
      case x => fieldDimensionError(x)
    }
  }

  /** Compute the inverseFFT of the columns only in a 2D complex field. 
    *
    * @param field The input field.
    * @return Input with each columns transformed by the inverse FFT.
    */
  def fftInverseColumns(field: Field): Field = {
    field.fieldType.dimensions match {
      case 2 => UnaryOperator(FFT2DSubOp(1, Inverse,
        fft2DInverseColumnNormalization(field)), field)
      case x => fieldDimensionError(x)
    }
  }

  /** Trim a ComplexField and return the real part of the result.
    *
    * Internal use only. The user API only supports same-tensor-type trims.
    * The ComplexToRealTrim kernel only supports Scalar fields (i.e. not Vector
    * or Matrix fields).
    */
  private[cogx] def trimToReal(field: Field, newShape: Shape) = {
    val trimmed = trim(field, newShape)
    UnaryOperator(RealPartOp, trimmed)
  }

  /** Enable 'realToComplexExpandBorderFill' keyword for Real-to-Complex
    * ScalarField expands, but only for internal testing.  User API only
    * supports same-tensor-type expands.  RealToComplexExpandBorder kernel
    * only supports Scalar fields (i.e. not Vector or Matrix fields).
    */
  private[cogx] def realToComplexExpandBorderFill(field: Field, rows: Int, columns: Int) = {
    require(field.fieldType.dimensions == 2)
    require(field.fieldType.fieldShape(0) <= rows)
    require(field.fieldType.fieldShape(1) <= columns)

    // Expand first, then convert to complex results in better forward merging
    val expanded = expand(field, BorderClamp, rows, columns)
    UnaryOperator(RealToComplexOp, expanded)
  }

  /** Explicit conversion of a Field to a ScalarField. Since Field is abstract,
    * the only possible conversion is if the field is already a ScalarField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toScalarField(field: Field): ScalarField = {
    fieldToScalarField(field)
  }

  /** Explicit conversion of a Field to a VectorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a VectorField but stored in a Field variable. This method also
    * supports converting a ColorField to a VectorField.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toVectorField(field: Field): VectorField = {
    if (field.fieldType.elementType == Uint8Pixel)
      // Convert color field to vector field
      UnaryOperator(ColorFieldToVectorFieldOp, field).asInstanceOf[VectorField]
    else
      fieldToVectorField(field)
  }

  /** Explicit conversion of a Field to a MatrixField. Since Field is abstract,
    * the only possible conversion is if the field is already a MatrixField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toMatrixField(field: Field): MatrixField =
    fieldToMatrixField(field)

  /** Explicit conversion of a Field to a ComplexField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexField but stored in a Field variable. This method also
    * supports converting a ScalarField to a ComplexField.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toComplexField(field: Field): ComplexField =
    fieldToComplexField(field)

  /** Explicit conversion of a Field to a ComplexVectorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexVectorField but stored in a Field variable. This method also
    * supports converting a VectorField to a ComplexVectorField.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toComplexVectorField(field: Field): ComplexVectorField =
    fieldToComplexVectorField(field)

  /** Explicit conversion of a Field to a ColorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ColorField but stored in a Field variable. This method also
    * supports converting a ScalarField or VectorField to a ColorField.
    *
    * @param field The input field.
    * @return The converted field.
    */
  def toColorField(field: Field): ColorField = {
    if (field.fieldType.elementType == Float32 && field.fieldType.tensorShape.points == 3) {
      UnaryOperator(VectorFieldToColorFieldOp, field).asInstanceOf[ColorField]
    } else
      fieldToColorField(field)
  }

  /** Another explicit conversion, this time working on both ScalarFields and
    * VectorFields. The return type is Field though, the common base class of
    * both ComplexFields and ComplexScalarFields. 
    *
    * @param field The input field.
    */
  def toGenericComplexField(field: Field): Field = {
    if (field.tensorOrder == 1)
      toComplexVectorField(field)
    else
      toComplexField(field)
  }

  /** Takes the DCT (discrete cosine transform) of a 2D field, producing a
    * field with the same shape as the input.
    *
    * The following sequence will return the original input, within the bounds
    * of computational error:
    * {{{
    *   val field
    *   val transformed: Field =
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
    * @param field The input field.
    * @return The DCT of the input.
    */
  def dct(field: Field): Field =
    UnaryOperator(DCT2DOp, field)

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
    * @param field The input field.
    * @return The DCT of the input.
    */
  def dctInverse(field: Field): Field =
    UnaryOperator(DCTInverse2DOp, field)

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
    * @param field The input field.
    * @return The DCT of the input.
    */
  def dctTransposed(field: Field): Field =
    UnaryOperator(DCTTransposed2DOp, field)

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
    * @param field The input field.
    * @return The DCT of the input.
    */
  def dctInverseTransposed(field: Field): Field =
    UnaryOperator(DCTInverseTransposed2DOp, field)


  /** Create a complex field from two scalar fields
    *
    * @param real Real part of the created complex field.
    * @param imaginary Imaginary part of the created complex field.
    * @return Complex field composed of the real and imaginary inputs
    */
  def complex(real: Field, imaginary: Field) =
    BinaryOperator(RealImaginaryToComplexOp, real, imaginary)

  /** Create a complex field from a real field.
    *
    * @param real Real input scalar field.
    * @return Complex version of real, with imaginary part set to zeros.
    */
  def complex(real: Field) =
    BinaryOperator(RealImaginaryToComplexOp, real, Field(real.fieldType))

  /** Create a complex field from polar coordinates.
    *
    * @param magnitude Scalar field where each scalar is the magnitude of the
    *        desired complex field.
    * @param phase Scalar field where each scalar is the phase of the
    *        desired complex field.
    * @return Complex field built from magnitude and phase.
    */
  def polarComplex(magnitude: Field, phase: Field) =
    BinaryOperator(PolarToComplexOp, magnitude, phase)

  /** Dynamically select a scalar field from an array of scalar fields.
    *
    * Example:
    * {{{
    *    val fields: Array[ScalarFieldExpr] = ...
    *    val index: ScalarFieldExpr
    *    val selected: ScalarFieldExpr = select(fields, index)
    * }}}
    *
    * @param index 0-dimensional scalar field supplying the index for the
    *        array to be read.
    * @return If index is in-bounds for the array, returns the selected field
    *        from that array; if index is out-of-bounds, returns a field of
    *        NaNs.
    */
  def select[T <: Field](fields: Array[T], index: Field) = {
    // The arguments are the index field followed by the array of scalar fields
    val arguments = Array.concat(Array(index), fields.asInstanceOf[Array[Field]])
    NaryOperator(FieldArraySelectOp, arguments)
  }

  /** Stack tensor fields to create a higher-order tensor field.
    *
    * For example, this can be used to stack N scalar fields to a single vector
    * field where each vector is of length N.
    *
    * @param fields The fields to be stacked.
    * @return The resulting, higher-order tensor field.
    */
  def stack(fields: Field*): Field = stack(fields.toArray)

  /** Stack tensor fields to create a higher-order tensor field.
    *
    * For example, this can be used to stack N scalar fields to a single vector
    * field where each vector is of length N.
    *
    * @param fields The fields to be stacked.
    * @return The resulting, higher-order tensor field.
    */
  def stack[T <: Field](fields: Array[T]): Field =
    NaryOperator(StackOp, fields.asInstanceOf[Array[Field]])

  /** Create a vector field from scalar fields by "stacking" the scalar fields.
    *
    * For example, this can be used to stack N scalar fields to a single vector
    * field where each vector is of length N.
    *
    * @param fields The scalar fields to be stacked into a vector field.
    * @return The resulting vector field.
    */
  def vectorField(fields: ScalarField*): VectorField = vectorField(fields.toArray)

  /** Create a vector field from scalar fields by "stacking" the scalar fields.
    *
    * For example, this can be used to stack N scalar fields to a single vector
    * field where each vector is of length N.
    *
    * @param fields The scalar fields to be stacked into a vector field.
    * @return The resulting vector field.
    */
  def vectorField(fields: Array[ScalarField]): VectorField = {
    val vectorShape = Shape(fields.length)
    NaryOperator(TensorStackOp(vectorShape), fields.asInstanceOf[Array[Field]]).
            asInstanceOf[VectorField]
  }

  /** Create a vector field from scalar fields by "stacking" the scalar fields.
    *
    * For example, this can be used to stack N scalar fields to a single vector
    * field where each vector is of length N.
    *
    * @param fields The scalar fields to be stacked into a vector field.
    * @return The resulting vector field.
    */
  def vectorField(fields: Array[Field]): VectorField = {
    fields.map(field => require(field.isInstanceOf[ScalarField]))
    val vectorShape = Shape(fields.length)
    NaryOperator(TensorStackOp(vectorShape), fields.asInstanceOf[Array[Field]]).
            asInstanceOf[VectorField]
  }

  /** Create a complex vector field by stacking complex scalar fields.
    *
    * For example, this can be used to stack N complex fields to a single
    * complex vector field where each vector is of length N.
    *
    * @param fields The complex fields to be stacked into a complex vector
    *        field.
    * @return The resulting complex vector field.
    */
  def complexVectorField(fields: ComplexField*): ComplexVectorField =
    complexVectorField(fields.toArray)

  /** Create a complex vector field by stacking complex scalar fields.
    *
    * For example, this can be used to stack N complex fields to a single
    * complex vector field where each vector is of length N.
    *
    * @param fields The complex fields to be stacked into a complex vector
    *        field.
    * @return The resulting complex vector field.
    */
  def complexVectorField(fields: Array[ComplexField]): ComplexVectorField = {
    val vectorShape = Shape(fields.length)
    NaryOperator(TensorStackOp(vectorShape), fields.asInstanceOf[Array[Field]]).
            asInstanceOf[ComplexVectorField]
  }

  /** Convert a color field to a vector field, with each vector of length
    * 3 representing, in order, the red, green and blue components of the
    * corresponding pixel.
    *
    * @param field Color field to be converted to vector field.
    * @return Vector field, with each vector holding the red, green, blue
    *        components.
    */
  def vectorField(field: ColorField): VectorField =
    toVectorField(field)

  /** Create a matrix field from vector fields by "stacking" the vector fields.
    *
    * For example, this can be used to stack N vector fields to a single matrix
    * field. If the length of the vectors in the vector fields is K, then
    * each matrix in the result will be N x K.
    *
    * @param fields The vector fields to be stacked into a matrix field.
    * @return The resulting matrix field.
    */
  def matrixField(fields: VectorField*): MatrixField = matrixField(fields.toArray)

  /** Create a matrix field from vector fields by "stacking" the vector fields.
    *
    * For example, this can be used to stack N vector fields to a single matrix
    * field. If the length of the vectors in the vector fields is K, then
    * each matrix in the result will be N x K.
    *
    * @param fields The vector fields to be stacked into a matrix field.
    * @return The resulting matrix field.
    */
  def matrixField(fields: Array[VectorField]): MatrixField = {
    val matrixShape = Shape(fields.length) concat fields(0).fieldType.tensorShape
    NaryOperator(TensorStackOp(matrixShape), fields.asInstanceOf[Array[Field]]).
            asInstanceOf[MatrixField]
  }

  /** Create a matrix field from a 2D array of scalar fields.
    *
    * For example, this can be used to stack N x K scalar fields to a single
    * matrix field. Each matrix in the result will be N x K.
    *
    * @param fields The scalar fields to be stacked into a matrix field.
    * @return The resulting matrix field.
    */
  def matrixField(fields: Array[Array[ScalarField]]): MatrixField = {
    val matrixShape = Shape(fields.length, fields(0).length)
    val inputFields = new Array[ScalarField](matrixShape.points)
    var index = 0
    for (row <- 0 until matrixShape(0); col <- 0 until matrixShape(1)) {
      inputFields(index) = fields(row)(col)
      index += 1
    }
    NaryOperator(TensorStackOp(matrixShape), inputFields.asInstanceOf[Array[Field]]).
            asInstanceOf[MatrixField]
  }

  /** Create a matrix field from a 2D array of scalar fields.
    *
    * For example, this can be used to stack N x K scalar fields to a single
    * matrix field. Each matrix in the result will be N x K.
    *
    * @param fields The scalar fields to be stacked into a matrix field.
    * @return The resulting matrix field.
    */
  def matrixField(fields: Array[Array[Field]]): MatrixField = {
    val matrixShape = Shape(fields.length, fields(0).length)
    val inputFields = new Array[ScalarField](matrixShape.points)
    var index = 0
    for (row <- 0 until matrixShape(0); col <- 0 until matrixShape(1)) {
      inputFields(index) = fields(row)(col).asInstanceOf[ScalarField]
      index += 1
    }
    NaryOperator(TensorStackOp(matrixShape), inputFields.asInstanceOf[Array[Field]]).
            asInstanceOf[MatrixField]
  }

  /** Create a color field from three scalar fields.
    *
    * @param red The red component scalar field.
    * @param green The green component scalar field.
    * @param blue The blue component scalar field.
    * @return Color field composed of (red, green, blue).
    */
  def colorField(red: Field, green: Field, blue: Field) =
    NaryOperator(MergeColorPlanesOp, Array(red, green, blue))

  /** Create a color field from either a scalar field (with an implicit
    * assumption that the field represents a grayscale image) or a vector
    * field (with the implicit assumption that the vector field represents
    * the (red, green, blue) components of the color image.
    *
    * @param field The input scalar or vector field.
    * @return Composed color field.
    */
  def colorField(field: Field) = {
    if (field.fieldType.tensorOrder == 0)
      NaryOperator(MergeColorPlanesOp, Array(field, field, field))
    else
      UnaryOperator(VectorFieldToColorFieldOp, field)
  }

  /** User mechanism for marking a field as "probed," optionally supplying a
    * name for the field.
    *
    * If a field is visible to reflection, it will be automatically named.
    * In that case supplying a `userName` is unnecessary and not recommended.
    *
    * @param field The field to be probed.
    * @param userName User's name for the field. If not supplied, the field is
    *        probed but uses system-inferred naming for the field.
    * @return The field being probed.
    */
  def probe(field: Field, userName: String = null): Field = {
    if (userName != null)
      field.setSimpleName(userName)
    field.markProbed()
    field
  }

  /** User mechanism for marking multiple fields as "probed"
    *
    * If a field is visible to reflection, it will be automatically named.
    *
    * @param fields The fields to be probed.
    */
  def probe(fields: Field*) { fields.foreach(probe(_, null.asInstanceOf[String])) }
}
