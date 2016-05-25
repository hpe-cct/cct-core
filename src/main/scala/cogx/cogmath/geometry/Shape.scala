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

package cogx.cogmath.geometry

/** A Shape describes the dimensions of a discrete, hyper-rectangular object.
  *
  * For historical reasons, the elements of a shape also have the following names:
  *    3D: (layers, rows, columns)
  *    2D: (rows, columns)
  *    1D: (columns)
  *
  * @param sizes The integral size of each dimension.
  *
  * @author Greg Snider
  */
@SerialVersionUID(-618254321595421426L)
final class Shape(private val sizes: Array[Int]) extends Serializable {

  /**Create a dimensional shape with zero or more dimensions. */
  @deprecated("drop 'new', use Shape(Int*) instead.", "4.0")
  def this(size: Int*) = this(size.toArray)

  /**Get the size of the indexed "dimension". */
  def apply(dimension: Int) = sizes(dimension)

  /** Get the size of the last dimension. If this is 0-dimensions, returns 0.*/
  val lastDimension: Int = {
    if (sizes.length == 0)
      0
    else
      sizes(sizes.length - 1)
  }

  /**Number of dimensions for this shape. */
  val dimensions = sizes.length

  /**Number of points in this discrete shape. */
  val points = if (dimensions == 0) 1 else sizes.reduceLeft(_ * _)

  /**Concatenate two shapes to form a higher dimensional shape. */
  def concat(that: Shape) = new Shape(Array.concat(sizes, that.sizes))


  /** Reduces the Shape by a factor in each dimension, rounding up when
    * the original size is not an integer multiple of the downsample factor.
    * The is the standard we use throughout Cog.
    */
  def downsample(factor:Int) = new Shape(toArray.map(size =>
    math.ceil(size.toFloat/factor.toFloat).toInt))

  def supersample = new Shape(toArray.map(size => 2 * size))

  /** Map this shape to another shape using "f". */
  def map(f: Int => Int) = new Shape(sizes map f)

  /** Drop the first "d" dimensions from "this", returning smaller shape. */
  def drop(d: Int): Shape = {
    require(d <= dimensions)
    val newSizes = new Array[Int](dimensions - d)
    for (i <- d until dimensions)
      newSizes(i - d) = sizes(i)
    new Shape(newSizes)
  }

  /** Drop the last "d" dimensions from "this", returning smaller shape. */
  def dropLast(d: Int): Shape = {
    require(d <= dimensions)
    val newSizes = new Array[Int](dimensions - d)
    for (i <- 0 until dimensions - d)
      newSizes(i) = sizes(i)
    new Shape(newSizes)
  }

  /** Concatenate "this" and "that" to create a higher-dimensional shape. */
  def concatenate(that: Shape): Shape = {
    new Shape(Array.concat(this.sizes, that.sizes))
  }

  /** Join "this" and "that" to create a same-dimensional shape representing
    * the two shapes abutted.  Join of 0D fields illegal. */
  def join(that: Shape): Shape = {
    require(this.drop(1) == that.drop(1))
    val newSizes = Array.tabulate(dimensions) { i =>
      if (i == 0)
        this(i) + that(i)
      else
        this(i)
    }
    Shape(newSizes)
  }

  /** Compare two shapes, returning true if every dimension of `this` is
    * smaller than or equal to the corresponding dimension in `other`.
    *
    * @param other Shape to compare with `this`.
    * @return True if `this` and `other` have the same number of dimensions and
    *       `this` is no larger in any corresponding dimension.
    */
  def <=(other: Shape): Boolean = {
    if (dimensions == other.dimensions) {
      for (i <- 0 until sizes.length)
        if (this.sizes(i) > other.sizes(i))
          return false
      true
    } else
      // Shapes have different number of dimensions, so comparison is
      // meaningless.
      false
  }

  /** Return an iterator over all possible discrete locations in the shape.
    * For example, the Shape (2, 3) has the following locations:
    * {{{
    *   (0, 0)
    *   (0, 1)
    *   (0, 2)
    *   (1, 0)
    *   (1, 1)
    *   (1, 2)
    * }}}
    */
  def indices = new Iterator[Array[Int]] {
    private val index = new Array[Int](sizes.length)
    private var notDone = true

    def hasNext = notDone

    def next(): Array[Int] = {
      val tuple = new Array[Int](sizes.length)
      for (i <- 0 until tuple.length)
        tuple(i) = index(i)
      if (tuple.length == 0)
        notDone = false
      else
        incrementIndex(tuple.length - 1)
      tuple
    }

    private def incrementIndex(i: Int) {
      index(i) += 1
      if (index(i) == sizes(i)) {
        index(i) = 0
        if (i == 0)
          notDone = false
        else
          incrementIndex(i - 1)
      }
    }
  }


  /** Convert a Shape to a String for debugging. */
  override def toString = toString("Shape")

  /**Like ordinary toString but allows for a prefix besides "Shape" */
  def toString(prefix: String) = {
    var string = prefix + "( "
    for (i <- 0 until dimensions)
      string += (this(i) + " ")
    string += ")"
    string
  }

  /** Test "this" and "other" Shape for equality. Allows "==" to work. */
  override def equals(other: Any): Boolean =
    other match {
      case that: Shape =>
        if (dimensions == that.dimensions) {
          for (i <- 0 until dimensions)
            if (sizes(i) != that.sizes(i))
              return false
          true
        } else
          false
      case _ => false
    }

  /** Required because of overriding equals. */
  override val hashCode: Int = {
    // Product-sum hash
    val Base = 31
    var factor = Base
    var sum = 0
    for (size <- sizes) {
      sum += size * factor
      factor *= Base
    }
    sum
  }

  /** Convert the shape to an array. */
  def toArray: Array[Int] = sizes.map(i => i)
}

/** Companion object for Shape.
 */
object Shape {
  /** A zero-dimensional shape. */
  val scalar = Shape()

  /** An unknown shape. */
  val unknown = Shape()

  /** Create a shape with given "size" dimensions. */
  def apply(size: Int*) = new Shape(size.toArray)

  def apply(size: Array[Int]) = new Shape(size)
}
