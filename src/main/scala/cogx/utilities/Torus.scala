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

package cogx.utilities

/** A Torus of objects (edges wrapped around for continuity).
  *
  * @author Greg Snider
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private abstract class Torus[T](val width: Int, val height: Int)(implicit m: Manifest[T])
        extends Iterable[T]
{
  private val objects = new Array2D[T](width, height)	// The objects.

  /**
   * Access the object at location ("x", "y"). Because of edge wrapping, all
   * values of x and y, positive or negative, are valid, although efficiency
   * (and practical usage) demands they be close to 0.
   */
  def apply(x: Int, y: Int): T = objects(xCoord(x), yCoord(y))

  /** Update location ("x", "y") with "obj". */
  def update(x: Int, y: Int, obj: T) {objects(xCoord(x), yCoord(y)) = obj}

  /** Return iterator over all elements in the torus. */
  def iterator = objects.iterator

  /** Extract subarray of size "width" X "height" at ("startX", "startY) */
  def subarray(startX: Int, startY: Int, width: Int, height: Int): Array2D[T] = {
    val array = new Array2D[T](width, height)
    for (x <- 0 until width; y <- 0 until height)
      array(x, y) = this(startX + x, startY + y)
    array
  }

  /** Transform torus coordinate "x" to coordinate on "objects" array. */
  private def xCoord(x: Int): Int = {
    var xx = x
    while (xx < 0)
      xx += width
    while (xx >= width)
      xx -= width
    xx
  }

  /** Transform torus coordinate "y" to coordinate on "objects" array. */
  private def yCoord(y: Int): Int = {
    var yy = y
    while (yy < 0)
      yy += height
    while (yy >= height)
      yy -= height
    yy
  }
}

/**
 * Test code for the Torus class.
 *
 * @author Greg Snider
 */
/*
object TestTorus {
  def main(args: Array[String]) {
    val Width = 3
    val Height = 2
    val torus = new Torus[Int](Width, Height)
    // test apply and update
    for (x <- 0 until Width; y <- 0 until Height)
      torus(x, y) = 10 * x + y
    for (x <- 0 until Width; y <- 0 until Height)
      require(torus(x, y) == 10 * x + y)
    // test wrap around
    for (offset <- -2 to 2) {
      for (x <- 0 until Width; y <- 0 until Height) {
        val xx = x + offset * Width
        val yy = y + offset * Height
        require(torus(xx, yy) == torus(x, y))
      }
    }
    // test subarray
    val sub = torus.subarray(2, 1, 2, 2)
    require(sub(0, 0) == 21)
    require(sub(0, 1) == 20)
    require(sub(1, 0) == 1)
    require(sub(1, 1) == 0)
    println
  }
}

object Experiment {
  private var counter = 0
  def getCount: Int = {counter += 1; counter}

  def printer(x: () => Int) {println(x())}

  def main(args: Array[String]) {
    for (i <- 0 until 10)
      printer(getCount _)
  }

}
*/