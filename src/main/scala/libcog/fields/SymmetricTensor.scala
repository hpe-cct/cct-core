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

package libcog.fields

import cogx.`package`._

/** A symmetric, positive semidefinite, order-2 tensor in 2 dimensions. See
  * the paper "An efficient method for tensor voting using steerable filters,"
  * Franken et al, 2006, for a description.
  *
  * This is implemented as a vector for efficiency because the matrix
  * representation is redundant. The matrix form of a symmetric tensor:
  * {{{
  *    Axx    Axy
  *
  *    Axy    Ayy
  * }}}
  * is represented as a length 3 vector:
  * {{{
  *    Axx
  *
  *    Axy
  *
  *    Ayy
  * }}}
  *
  * @param components The tensor components (Axx, Axy, Ayy).
  *
  * @author Greg Snider
  */
class SymmetricTensor(vector: Vector)
        extends Vector(vector(0), vector(1), vector(2))
{
  private val PI = math.Pi.toFloat

  /** Create a symmetric tensor.
    *
    * @param stickness Stickness of the tensor.
    * @param ballness Ballness of the tensor.
    * @param orientation Orientation of the tensor; must be in (0, Pi].
    */
  def this(stickness: Float, ballness: Float, orientation: Float) =
    this({
      // Compute eigenvalues
      val lambda2 = ballness
      val lambda1 = stickness + lambda2

      // Multiply eigenvalues by basis matrices. This creates a matrix
      // representation of the tensor.
      val matrix = SymmetricTensor.e1e1 * lambda1 + SymmetricTensor.e2e2 * lambda2

      // Convert the matrix to the vector representation.
      val vector = Vector(matrix(0, 0), matrix(0, 1), matrix(1, 1))

      // Rotate the dyad to the desired orientation.
      val rotated: Vector = SymmetricTensor.rotate(vector, orientation)
      rotated
    })

  /** Create a symmetric tensor from a stick tensor.
    *
    * @param stickTensor The stick tensor to convert to a symmetric tensor.
    */
  def this(stickTensor: StickTensor) =
    this(stickTensor.magnitude, 0f, stickTensor.orientation)

  /** Add two symmetric tensors.
    *
    * @param that Symmetric tensor to add to `this`.
    */
  def +(that: SymmetricTensor): SymmetricTensor = {
    new SymmetricTensor(this.asInstanceOf[Vector] + that.asInstanceOf[Vector])
  }

  /** Stickness of the tensor. */
  def stickness = math.sqrt(trace * trace - 4 * determinant).toFloat max 0

  /** Ballness of the tensor. */
  def ballness = ((trace - stickness) / 2) max 0

  /** Orientation of the tensor, in (0, Pi]. */
  def orientation = {
    val rawOrientation = Complex(vector(0) - vector(2), 2 * vector(1)).phase / 2
    if (rawOrientation <= 0)
      rawOrientation + PI
    else
      rawOrientation
  }

  /** Compute the trace of the tensor. */
  private def trace: Float =
    this(0) + this(2)

  /** Compute the determinant of the tensor. */
  private def determinant: Float =
    this(0) * this(2) - this(1) * this(1)
}

/** Helper object for creating symmetric tensors.
  *
  * @author Greg Snider
  */
object SymmetricTensor {
  val Components = 3

  // Tensor basis.
  private val e1 = new Vector(1f, 0f)
  private val e2 = new Vector(0f, 1f)
  private val e1e1: Matrix = e1 * e1.transpose
  private val e2e2: Matrix = e2 * e2.transpose

  /** Rotate the tensor by "angle", expressed in radians. */
  private def rotate(tensor: Vector, angle: Float): Vector = {
    // Create the rotation matrix.
    val rot = new Matrix(3, 3)
    val cos2 = math.cos(2 * angle).toFloat
    val sin2 = math.sin(2 * angle).toFloat
    rot(0, 0) = (1 + cos2) / 2
    rot(0, 1) = -sin2
    rot(0, 2) = (1 - cos2) / 2
    rot(1, 0) = sin2 / 2
    rot(1, 1) = cos2
    rot(1, 2) = -sin2 / 2
    rot(2, 0) = (1 - cos2) / 2
    rot(2, 1) = sin2
    rot(2, 2) = (1 + cos2) / 2
    val rotated = rot * tensor
    rotated
  }
}