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

package cogx.cogmath.algebra.complex

import cogx.cogmath.algebra.real.PoorFloat._
import cogx.cogmath.geometry.Shape
import cogx.utilities.Random

/** Complex numbers.
  *
  * @author Greg Snider
  */
@SerialVersionUID(7029811968224931195L)
class Complex(val real: Float, val imaginary: Float)
  extends Serializable
{
  /** The norm of the complex number. */
  def normSq = real * real + imaginary * imaginary

  /** The phase (angle) in radians.
    *
    * This is a number in the range (-Pi, Pi].
    */
  def phase: Float = {
    var angle = math.atan2(imaginary, real)
    angle.toFloat
  }

  /** The length of the complex number. */
  def magnitude = math.sqrt(normSq).toFloat

  /** `this` * `that` */
  def *(that: Complex) =
    new Complex(this.real * that.real - this.imaginary * that.imaginary,
      this.real * that.imaginary + this.imaginary * that.real)

  /** `this` * `that` */
  def *(that: Float) = new Complex(this.real * that, this.imaginary * that)

  /** `this` + `that` */
  def +(that: Complex) =
    new Complex(this.real + that.real, this.imaginary + that.imaginary)

   /** `this` + `that` */
  def +(that: Float) =
    new Complex(this.real + that, this.imaginary)

  /** `this` - `that` */
  def -(that: Complex) =
    new Complex(this.real - that.real, this.imaginary - that.imaginary)

    /** `this` - `that` */
  def -(that: Float) =
    new Complex(this.real - that, this.imaginary)

  /** `this` / `that` */
  def /(that: Float) = new Complex(real / that, imaginary / that)

  /** `this` / `that` */
  def /(that: Complex): Complex = {
    val d = that.normSq
    val real = (this.real * that.real + this.imaginary * that.imaginary) / d
    val imag = (this.imaginary * that.real - this.real * that.imaginary) / d
    new Complex(real, imag)
  }
  /** -`this` */
  def unary_-() = new Complex(-(this.real), -(this.imaginary))

  /** 1/`this` */
  def reciprocal= {
    val norm = this.normSq
    new Complex(this.real/norm, -(this.imaginary)/norm)
  }

  /** e ^ this */
  def exp: Complex = {
    val x = new Complex(math.exp(this.real).toFloat, 0)
    val y = new Complex(math.cos(this.imaginary).toFloat,
      math.sin(this.imaginary).toFloat)
    x * y
  }

  /** this ^ `exponent` */
  def power(exponent: Int): Complex = {
    if (exponent == 0)
      new Complex(1, 0)
    else {
      val e = exponent.abs
      val c = if (exponent < 0) (new Complex(1, 0) / this) else this
      var result = new Complex(1, 0)
      for (i <- 0 until e)
        result *= c
      result
    }
  }

  /** Get the data in the tensor, flattened to a linear array. */
  protected def getData = new ComplexArray(this)

  /** Get the complex conjugate of this. */
  def conjugate = new Complex(real, -imaginary)

  def ~==(that: Complex) =
    (real ~== that.real) && (imaginary ~== that.imaginary)

  /** Test `this` and `other` for deep equality. */
  override def equals(other: Any): Boolean =
    other match {
      case that: Complex =>
        if (that canEqual this)
          (this.real == that.real) && (this.imaginary == that.imaginary)
        else
          false
      case that: Float =>
        this.real == that && this.imaginary == 0f
      case _ => false
    }

  /** Helper for equals. */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Complex]

  /** Required because of overriding equals. */
  override def hashCode: Int =
    real.hashCode + imaginary.hashCode

  /** Format the complex number as a String. */
  override def toString = "(%8.3f, %8.3fi) ".format(real, imaginary)

  /** Print out a complex number for debugging. */
  def print = println(toString)
}

/** Factory for creating complex numbers. */
object Complex {
  /** Random number generator. */
  private val rand = new Random

  // Shape of complex scalar tensor
  private val shape = Shape()

  /** Square root of -1. */
  val I = new Complex(0, 1)

  /** Create a complex number from the real and imaginary components. */
  def apply(real: Float, imaginary: Float) = new Complex(real, imaginary)

  /** Create a Complex number from polar coordinates. */
  def polar(r: Float, phase: Float) =
    new Complex(r * math.cos(phase).toFloat, r * math.sin(phase).toFloat)

  /** Exponential of a complex number. */
  def expc(c: Complex): Complex = {
    Complex(math.cos(c.imaginary).toFloat, math.sin(c.imaginary).toFloat) *
            math.exp(c.real).toFloat
  }

  /** Create a complex number with random real and imaginary components. */
  def random: Complex = new Complex(rand.nextFloat, rand.nextFloat)

  //implicit def floatToComplex(d: Float) = new Complex(d, 0)
  //implicit def intToComplex(i: Int) = new Complex(i.toFloat, 0)
}
