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

package cogx.cogmath.algebra.real

import scala.math.{cos, sin, sqrt, atan2}

/**A two-dimensional dyad.
  *
  * The terminology of "stickness" and "ballness" is
  * described in "An efficient method for tensor voting using steerable filters,"
  * Franken et al.
  *
  * Since dyads in Cog are symmetric matrices, we implement them as vectors
  * for efficiency. The matrix representation:
  * {{{
  *   xx   xy
  *   yx   yy
  * }}}
  *
  * is therefore represented internally as a vector like this:
  * {{{
  *   xx   xy   yy
  * }}}
  *
  * @author Greg Snider
  */

class Dyad2D private (data: Array[Float])
  extends Vector(data)
  with Tensor
{
  require(data.length == 3)

  /** Create a dyad with values "xx", "xy", "yy". */
  private def this(xx: Float, xy: Float, yy: Float) = this(Array(xx, xy, yy))

  /** Create a 0 dyad. */
  def this() = this(Array(0f, 0f, 0f))

  /** The "stickness" of the dyad. */
  def stickness: Float = sqrt((sq(xx - yy) + 4 * sq(xy))).toFloat

  // The following formula for stickness is seen in the literature, but can
  // generate NaN's when the sqrt of a negative number is attempted.  The
  // negative can occur from floating point round-off error.  The above
  // equivalent form has been massaged so that the sqrt is taken of a
  // sum of two squares, so NaNs are avoided.

//  def stickness: Float = sqrt((sq(trace) - 4 * determinant)).toFloat

  /** The "ballness" of the dyad. Not sure if neg can result, so clip. */
  def ballness: Float = 0.5f * math.max(0f,(trace - stickness))

  /** The orientation of the dyad in radians. */
  def orientation: Float = {
    val deltaX = xx - yy
    val deltaY = 2 * xy
    val atan = atan2(deltaY, deltaX).toFloat
    //val angle = if (atan >= 0) atan else (atan + 2 * Pi).toFloat
    0.5f * atan
  }

  /** Rotate the dyad by "angle", expressed in radians. */
  def rotate(angle: Float): Dyad2D = {
    // Create the rotation matrix.
    val rot = new Matrix(3, 3)
    val cos2 = cos(2 * angle).toFloat
    val sin2 = sin(2 * angle).toFloat
    rot(0, 0) = (1 + cos2) / 2
    rot(0, 1) = -sin2
    rot(0, 2) = (1 - cos2) / 2
    rot(1, 0) = sin2 / 2
    rot(1, 1) = cos2
    rot(1, 2) = -sin2 / 2
    rot(2, 0) = (1 - cos2) / 2
    rot(2, 1) = sin2
    rot(2, 2) = (1 + cos2) / 2
    val rotated = rot * this
    Dyad2D(rotated)
  }

  // Dyad arithmetic
  def +(that: Dyad2D) = new Dyad2D(xx + that.xx, xy + that.xy, yy + that.yy)
  def -(that: Dyad2D) = new Dyad2D(xx - that.xx, xy - that.xy, yy - that.yy)
  def *(that: Dyad2D) = new Dyad2D(xx * that.xx, xy * that.xy, yy * that.yy)
  def /(that: Dyad2D) = new Dyad2D(xx / that.xx, xy / that.xy, yy / that.yy)
  override def unary_-(): Dyad2D = new Dyad2D(-xx, -xy, -yy)
  override def reciprocal = new Dyad2D(1f/xx, 1f/xy, 1f/yy)

  // Scalar arithmetic
  override def +(f: Float) = new Dyad2D(xx + f, xy + f, yy + f)
  override def -(f: Float) = new Dyad2D(xx - f, xy - f, yy - f)
  override def *(f: Float) = new Dyad2D(xx * f, xy * f, yy * f)
  override def /(f: Float) = new Dyad2D(xx / f, xy / f, yy / f)

  /** The trace of the dyad. */
  private def trace: Float = xx + yy

  /** The determinant of the dyad. */
  private def determinant: Float =  xx * yy - xy * xy

  /** Square a number. */
  private def sq(x: Float) = x * x

  /** The "xx" component of the dyad (see class description). */
  def xx = this(0)

  /** The "xy" component of the dyad (see class description). */
  def xy = this(1)

  /** The "yy" component of the dyad (see class description). */
  def yy = this(2)
}

/** Factory for creating Dyad2Ds.
 */
object Dyad2D {
  // Tensor basis.
  private val e1 = new Vector(1f, 0f)
  private val e2 = new Vector(0f, 1f)
  private val e1e1: Matrix = e1 * e1.transpose
  private val e2e2: Matrix = e2 * e2.transpose

  /** Create a zero-filled Dyad2D. */
  def apply(): Dyad2D = new Dyad2D(new Array[Float](3))

  /**
   * Cast a length 3 "vector" to a Dyad2D. This should only be called if the
   * vector was originally created by downcasting of a Dyad2D, because the
   * internal representation of Dyad2D is not part of the interface.
   */
  def apply(vector: Vector): Dyad2D = {
    require(vector.data.length == 3, "Illegal vector length (Dyad2D).")
    new Dyad2D(vector.data)
  }

  /** Create a dyad with properties "stickness", "ballness" and "orientation".*/
  def apply(stickness: Float, ballness: Float, orientation: Float): Dyad2D = {
    //require(stickness >= 0 && ballness >= 0,
    //  "stickness = " + stickness + ", ballness = " + ballness)
    // Compute eigenvalues
    val lambda2 = ballness max 0
    val lambda1 = (stickness max 0) + lambda2

    // Multiply eigenvalues by basis matrices. This creates a matrix
    // representation of the dyad.
    val matrix = e1e1 * lambda1 + e2e2 * lambda2

    // Convert the matrix to the vector representation.
    val dyad0 = new Dyad2D(matrix(0, 0), matrix(0, 1), matrix(1, 1))

    // Rotate the dyad to the desired orientation.
    dyad0.rotate(orientation)
  }

  /** Create a stick tensor of stickness "length" and "orientation". */
  def stick(length: Float, orientation: Float): Dyad2D =
    apply(length, 0f, orientation)

  /** Create a ball tensor of size "diameter". */
  def ball(diameter: Float): Dyad2D =
    apply(0f, diameter, 0f)

  def fromComponents(xx: Float, xy: Float, yy: Float) = new Dyad2D(xx, xy, yy)
}
