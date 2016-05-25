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

import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Vector
import java.util.Random


/** An immutable vector of complex numbers.
  *
  * @author Greg Snider
  */
class ComplexVector(private val data: ComplexArray)
{
  /** Create a complex vector from a real and imaginary vector. */
  def this(real: Vector, imaginary: Vector) = this({
    require(real.length == imaginary.length)
    val newData = new ComplexArray(real.length)
    for (i <- 0 until real.length)
      newData(i) = Complex(real(i), imaginary(i))
    newData
  })

  /** Create a zero-filled vector of size "length". */
  def this(length: Int) = this(new Vector(length), new Vector(length))

  /** Create a complex vector from a "real" vector (imaginary part = 0). */
  def this(real: Vector) = this(real.copy, new Vector(real.length))

  def this(values: Array[Complex]) = this(new ComplexArray(values))

  def length = data.length

  /** Make a copy of this complex vector. */
  def copy = new ComplexVector(data.copy)

  /** Number of elements in the vector. */
  val shape = Shape(data.length)

  /** Read element "index". */
  def apply(index: Int) = data.apply(index)

  /** Read the real part of all vector elements as a Vector. */
  def real: Vector = Vector(length, i => realAt(i))

  /** Read the imaginary part of all vector elements as a Vector. */
  def imaginary: Vector = Vector(length, i => imaginaryAt(i))

  /** Read the real part of element "index". */
  def realAt(index: Int): Float = data.real(index)

  /** Read the imaginary part of element "index". */
  def imaginaryAt(index: Int): Float = data.imaginary(index)

  /** View the complex vector as a complex array. */
  def asComplexArray = data

  def toArray: Array[Complex] = {
    val array = new Array[Complex](length)
    for (i <- 0 until length)
      array(i) = Complex(realAt(i), imaginaryAt(i))
    array
  }

  // Scalar arithmetic
  def +(d: Float) = new ComplexVector(data + d)
  def -(d: Float) = new ComplexVector(data - d)
  def *(d: Float) = new ComplexVector(data * d)
  def /(d: Float) = new ComplexVector(data / d)
  def +(c: Complex) = new ComplexVector(data + c)
  def -(c: Complex) = new ComplexVector(data - c)
  def unary_-() = this * -1.0f

  // Vector arithmetic
  def +(that: ComplexVector) = new ComplexVector(this.data + that.data)

  def -(that: ComplexVector) = new ComplexVector(this.data - that.data)

  /** Dot product of "this" and "that". */
  def dot(that: ComplexVector): Complex = {
    require(this.length == that.length)
    (data zip that.data).map(v => v._1 * v._2).reduceLeft(_ + _)
  }

  /** Reduce a complex vector to a single number. */
  def reduce(f: (Complex, Complex) => Complex): Complex = {
    var result = data(0)
    for (i <- 1 until length)
      result = f(result, data(i))
    result
  }

  /** Compare against another ComplexVector for approximate equality */
  def ~==(that: ComplexVector) =
    (real ~== that.real) && (imaginary ~== that.imaginary)

  override def equals(that: Any): Boolean = {
    that match {
      case other: ComplexVector =>
        this.data equals other.data
      case x =>
        false
    }
  }

  /** Print out the vector for debugging. */
  def print {
    for (i <- 0 until length)
      this(i).print
    println
  }
}

/** Factory for creating complex vectors.
 */
object ComplexVector {
  /** Create a ComplexVector from a list of Complex numbers. */
  def apply(elements: Complex*): ComplexVector = {
    new ComplexVector(elements.toArray)
  }

  /** Create a ComplexVector from the real and imaginary component Vectors. */
  def apply(real: Vector, imaginary: Vector) = new ComplexVector(real, imaginary)

  /** Create a ComplexVector from magnitude and phase Vectors. */
  def polar(r: Vector, phase: Vector) = {
    require(r.length == phase.length,
      "Mismatched magnitude and phase vector lengths: " +
              r.length + " and " + phase.length)
    ComplexVector(r.length, i => Complex.polar(r(i), phase(i)))
  }

  /** Create a ComplexVector of "length" initialized by "f". */
  def apply(length: Int, f: (Int) => Complex): ComplexVector = {
    val array = new ComplexArray(length)
    for (i <- 0 until length)
      array(i) = f(i)
    new ComplexVector(array)
  }

  /** Create a ComplexVector of size "length" filled with random numbers. */
  def random(length: Int): ComplexVector = {
    val random = new Random
    val values = new ComplexArray(length)
    for (i <- 0 until length)
      values(i) = new Complex(random.nextFloat, random.nextFloat)
    new ComplexVector(values)
  }
}
