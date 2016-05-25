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

import scala.util.Sorting
import cogx.cogmath.geometry.Shape
import cogx.utilities.Random
import PoorFloat._

/** A column vector.
  *
  * The interface is modeled on that provided by Scalala (which
  * in turn was modeled on Matlab), with the exception that all operations are
  * executed immediately. This provides a more natural interface, at the cost
  * of suboptimal performance since linear algebra libraries are not used.
  *
  * In the following descriptions, "v" represents a vector, "s" a scalar, "m"
  * a matrix, "b" a boolean, "i" and integer, "d" a double. The update operations,
  * such as "+=", update the elements of the vector in place.
  *
  * {{{
  * Constructors:
  *   new Vector(i)              (create vector of length i)
  *   new Vector(d*)             (create vector with elements d)
  *   copy           => v        (create a copy of a vector)
  *   length         => i        (number of elements in vector)
  *
  * Vector operations:
  *   map(f: d => d)     => v        (map a Vector by applying f to each element)
  *   mapSelf(f: d => d)			    (use f to map each element in place)
  *   reduce((d, d)=> d) => d        (reduce a Vector to a scalar)
  *   randomize                      (initialize to random values in [0, 1))
  *
  * Vector / Scalar operations:
  *   v + s   => v          (add scalar to each element to produce a new matrix)
  *   v += s                (add scalar to all elements of matrix in-place)
  *   v - s   => v
  *   v -= s
  *   v * s   => v
  *   v *= s
  *   v / s   => v
  *   v /= s
  *   -v      => v          (map each element to new matrix my multiplying by -1)
  *   v := s                (assign scalar to all elements of the matrix)
  *
  * Vector / Vector operations:
  *   v + v          => v
  *   v += v
  *   v - v          => v
  *   v -= v
  *   v :* v         => v   (element-wise multiplication)
  *   v :*= v               (element-wise multiplication in-place)
  *   v :/ v         => v   (element-wise right division)
  *   v :/= n               (element-wise right division in-place)
  *   v :\ v         => v   (element-wise left division)
  *   v :\= v               (element-wise left division in-place)
  *   v1 === v2		=> b   (m1 and m2 have same shape, identical elements)
  *   v1 !=== v2     => b
  *   v1 ~== v2
  *   v1 !~== v2
  *   v1 := v2			   (assign elements of v2 to v1)
  *
  *
  * --------------------
  *   v1 dot v2           => d   (dot product of two vectors)
  *   v1 outerProduct v2  => m   (outer product of two vectors)
  *   v concat v2         => v   (concatentate two vectors)
  *
  *
  *  -------------------------------------------------
  *
  *
  *
  *
  * Matrix conversions:
  *   shape(i, i)           => m (create matrix)
  *   rectangularize        => m (create squarest matrix possible)
  *
  * Miscellaneous:
  *   normL1     => d       (L1 norm of the vector)
  *   normL2     => d       (L2 norm of the vector)
  *   argmax     => i       (index of largest element)
  *   print                 (print out a vector for debugging)
  *
  * }}}
  * @author Greg Snider
  */
@SerialVersionUID(2098406175764110451L)
class Vector(private[real] val data: Array[Float])
  extends Tensor
  with Serializable
{
  def size = data.length

  /** Shape of the vector. */
  def shape = Shape(data.length)

  /** Tensor accessor. */
  def read(index: Int) = data(index)

  /** Create a vector of size "length". */
  def this(length: Int) = this(new Array[Float](length))

  /** Create a vector from one or more Floats. */
  def this(e: Float*) = this(e.toArray)

  /** Create a vector from a 1-dimensional tensor. */
  def this(tensor: Tensor) = this({
    require(tensor.shape.dimensions == 1)
    require(tensor.length == tensor.shape.points)
    val newData = new Array[Float](tensor.shape.points)
    for (i <- 0 until tensor.length)
      newData(i) = tensor.read(i)
    newData
  })

  def copy: Vector = {
    val dataCopy = new Array[Float](length)
    Array.copy(data, 0, dataCopy, 0, length)
    new Vector(dataCopy)
  }

  def apply(index: Int) = data(index)
  def update(index: Int, value: Float) {data(index) = value}

  /** View this Vector as an array with full read and write access. */
  final def asArray: Array[Float] = data

  /** Convert Vector to an array of doubles (copy). */
  def toArray: Array[Float] = {
    val copy = new Array[Float](data.length)
    Array.copy(data, 0, copy, 0, data.length)
    copy
  }

  // Vector operations:
  def map(f: Float => Float) = new Vector(data map f)
  def mapSelf(f: Float => Float) {
    for (i <- 0 until length)
      data(i) = f(data(i))
  }
  def randomize: Vector = {
    for (i <- 0 until length)
      this(i) = Vector.rand.nextFloat
    this
  }
  def abs = map(_.abs)
  def sgn = map((x: Float) => (if (x > 0) 1.0f else if (x < 0) -1.0f else 0.0f))
  def rectify = map(_ max 0)

  // Vector / Scalar operations:
  def +(s: Float) = map(_ + s)
  def +=(s: Float) {for (i <- 0 until length) this(i) += s}
  def -(s: Float) = map(_ - s)
  def -=(s: Float) {for (i <- 0 until length) this(i) -= s}
  def *(s: Float) = map(_ * s)
  def *=(s: Float) {for (i <- 0 until length) this(i) *= s}
  def /(s: Float) = map(_ / s)
  def /=(s: Float) {for (i <- 0 until length) this(i) /= s}
  def -() = map(_ * -1)
  def :=(s: Float) {for (i <- 0 until length) this(i) = s}

  // Boolean comparisons
  def >(v: Float) = map(x => if (x > v) 1f else 0f)
  def >=(v: Float) = map(x => if (x >= v) 1f else 0f)
  def <(v: Float) = map(x => if (x < v) 1f else 0f)
  def <=(v: Float) = map(x => if (x <= v) 1f else 0f)

  // Vector / Vector operations:
  def +(v: Vector): Vector = {
    require(length == v.length)    
    val result = copy
    for (i <- 0 until length)
      result(i) += v(i)
    result
  }
  def +=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      this(i) += v(i)
  }
  def -(v: Vector): Vector = {
    require(length == v.length)
    val result = copy
    for (i <- 0 until length)
      result(i) -= v(i)
    result
  }
  def -=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      this(i) -= v(i)
  }
  def :*(v: Vector): Vector = {
    require(length == v.length)
    val result = copy
    for (i <- 0 until length)
      result(i) *= v(i)
    result
  }
  def :*=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      this(i) *= v(i)
  }
  def :/(v: Vector): Vector = {
    require(length == v.length)
    val result = copy
    for (i <- 0 until length)
      result(i) /= v(i)
    result
  }
  def :/=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      this(i) /= v(i)
  }
  def :\(v: Vector): Vector = {
    require(length == v.length)
    val result = v.copy
    for (i <- 0 until length)
      result(i) /= this(i)
    result
  }
  def :\=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      v(i) /= this(i)
  }
  def unary_-(): Vector = {
    val negativeData = data.map(-_)
    new Vector(negativeData)
  }
  def reciprocal: Vector = {
    val reciprocalData = data.map(1f/_)
    new Vector(reciprocalData)
  }
  def ===(v: Vector): Boolean = {
    if (length == v.length) {
      for (i <- 0 until length)
        if (data(i) != v(i))
          return false
      return true
    } else
      return false
  }
  def !===(v: Vector) = !(this === v)
  def ~==(v: Vector): Boolean = {
    if (length == v.length) {
      for (i <- 0 until length) {
//        if (!approxEqual(this(i), v(i)))
        if (!(this(i) ~== v(i)))
          return false
      }
      return true
    } else
      return false
  }
  def !~==(v: Vector) = !(this ~== v)
  def :=(v: Vector) {
    require(length == v.length)
    for (i <- 0 until length)
      this(i) = v(i)
  }

  /** Get the data in the tensor, flattened to a linear array. */
  protected[cogx] def getData = data

  /** Compute the dot product of "this" and "that". */
  def dot(that: Vector): Float = {
    require(this.length == that.length)
    (data zip that.data).map(v => v._1 * v._2).reduceLeft(_ + _)
  }

  /** Reduce a Vector to a scalar using "f". */
  def reduce(f: (Float, Float) => Float): Float = data.reduceLeft(f)

  def fold(initValue: Float)(f: (Float, Float) => Float): Float =
    data.foldLeft(initValue)(f)

  def outerProduct(that: Vector): Matrix = {
    val rows = length
    val columns = that.length
    val result = new Matrix(rows, columns)
    for (row <- 0 until rows; col <- 0 until columns)
      result(row, col) = this(row) * that(col)
    result
  }

  def *(that: Matrix): Matrix = {
    require(that.rows == 1)
    outerProduct(new Vector(that.data))
  }

  def concat(that: Vector): Vector = {
    val result = new Array[Float](this.length + that.length)
    Array.copy(this.data, 0, result, 0, this.length)
    Array.copy(that.data, 0, result, this.length, that.length)
    new Vector(result)
  }

  def subvector(indexRange: Range): Vector = {
    val subdata = new Array[Float](indexRange.size)
    Array.copy(data, indexRange.start, subdata, 0, indexRange.size)
    new Vector(subdata)
  }
  
  // Matrix conversions:
  def shape(rows: Int, columns: Int): Matrix = {
    require(length == rows * columns)
    val c = this.copy
    new Matrix(rows, columns, c.data)
  }
  def rectangularize: Matrix = {
    var rows = math.sqrt(length).toInt
    while ((length / rows) * rows != length)
      rows -= 1
    val columns = length / rows
    shape(rows, columns)
  }
  def transpose: Matrix = {
    val copy = new Array[Float](length)
    Array.copy(data, 0, copy, 0, length)
    new Matrix(1, length, copy)
  }
  
  // Miscellaneous operations:
  def normL1 = data.map(_.abs).reduceLeft(_ + _)
  def normL2 = math.sqrt(data.map(e => e * e).reduceLeft(_ + _)).toFloat
  def argmax: Int = {
    var biggest = -Float.MaxValue
    var biggestIndex = -1
    for (i <- 0 until length) {
      if (this(i) > biggest) {
        biggest = this(i)
        biggestIndex = i
      }
    }
    biggestIndex
  }
  def print {
    printf("(")
    for (i <- 0 until length)
      printf(" %8.3f", this(i))
    println(")^T")
  }

  /** Sort vector elements in ascending order (creates new Vector, "this" unchanged). */
  def sort = new Vector(toArray) { Sorting.quickSort(this.data) }

  /**
   * Expand the vector, optionally extending the border into the expanded
   * region. This operation is a key part of the FFT. The new vector is of
   * size "bigSize" element (0) of this is anchored at
   * (0) in the larger vector. If "borderFill" is true, then the two
   * edges of the vector are extended evenly in all directions, as though
   * the bigger vector were actually a loop with opposite edges touching.
   */
  def expand(bigSize: Int, borderFill: Boolean): Vector =
  {
    require(bigSize >= size)
    val big = new Vector(bigSize)

    // Copy "this" into big
    for (i <- 0 until size)
      big(i) = apply(i)

    // Now copy the border along the edges if requested
    if (borderFill) {
      val nearRightApronSize = (bigSize - size) / 2
      val farRightApronSize = (bigSize - size) - nearRightApronSize
      // far edge
      val firstElement = apply(0)
      for (i <- big.size - farRightApronSize until big.size)
        big(i) = firstElement
      val lastElement = apply(size - 1)
      // near edge
      for (i <- size until (size + nearRightApronSize))
        big(i) = lastElement
    }
    big
  }

  /** Trim "this" to a Vector of length `smallSize`. */
  def trim(smallSize: Int): Vector = {
    require(smallSize <= size)
    val small = new Vector(smallSize)
    for (i <- 0 until smallSize)
      small(i) = apply(i)
    small
  }

  /** Reverses the order of the elements in `this` */
  def flip: Vector = {
    val flippedData = new Array[Float](data.length)
    for (i <- 0 until data.length)
      flippedData(i) = this.data(data.length - i - 1)
    new Vector(flippedData)
  }

  /** Rotates the elements of a vector, wrapping around the left side to the
    * right side.
    *
    * The rotation is specified by "delta"; for example, delta=2
    * would cause the element at location (0) to be moved to location (2).
    * Returns the rotated vector.  This was added in support of the
    * LowPassFunction kernel, and because Matrix has a similar method.
    * Not currently tested.  -RJC  XXX
    */
  def shiftCyclic(delta: Int): Vector = {
    val result = new Vector(length)
    var shift = delta
    while (shift < 0)
      shift += length
    for (i <- 0 until length)
      result((i + shift) % length) = this(i)
    result
  }

  override def toString: String = {
    var string = ""
    for (i <- 0 until length) {
      string += this(i)
      if (i < length - 1)
        string += " "
    }
    string
  }
}

object Vector {
  private val rand = new Random

  /** Create a vector of size "length" initialized by "f". */
  def apply(length: Int, f: (Int) => Float): Vector = {
    val data = new Array[Float](length)
    for (i <- 0 until length)
      data(i) = f(i)
    new Vector(data)
  }

  /** Create a vector using a variable list of arguments */
  def apply(elements: Float*): Vector = {
    new Vector(elements: _*)
  }

  def random(length: Int): Vector = apply(length, _ => rand.nextFloat)
}
