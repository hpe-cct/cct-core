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

import scala.collection.mutable.{ArrayBuffer, HashMap}

/** Represents a function as a series of (x, y, z) points and returns z = f(x, y),
  * possibly interpolated. Out-of-range x, y values return f(x, y) of nearest point.
  *
  * User: Dick Carter
  * Date: 10/6/13
  */
private [cogx] class InterpolatedFunction2D(unsortedData: Array[Tuple3[Float, Float, Float]]) {

  case class Slice(x: Float, xSlice: InterpolatedFunction1D)

  // Step 1: group the points into ArrayBuffers of same-x values:
  val grouped = new HashMap[Float, ArrayBuffer[Tuple2[Float, Float]]]()
  unsortedData.foreach(tuple => {
    val (x, y, z) = tuple
    val slice = grouped.getOrElseUpdate(x, ArrayBuffer[Tuple2[Float, Float]]())
    slice += Tuple2(y, z)
  })

  // Step 2: transform the grouped same-x data into separate 1D functions z = f(y)
  val data = grouped.mapValues(arrayBuf =>
    new InterpolatedFunction1D(arrayBuf.toArray))

  // Step 3: sort the 1D functions by increasing x value
  val slices: Array[Slice] = data.toSeq.toArray.map(s => Slice(s._1, s._2)).
          sortWith((A, B) => A.x < B.x)
  val numSlices = slices.length
  require(numSlices > 0, "Function needs 1 or more points")

  val firstSlice = slices(0)
  val lastSlice = slices(numSlices - 1)

  /** Return z = f(xVal, yVal) using bilinear interpolation */
  def f(xVal: Float, yVal: Float): Float = {
    if (xVal <= firstSlice.x)
      firstSlice.xSlice.f(yVal)
    else if (xVal >= lastSlice.x)
      lastSlice.xSlice.f(yVal)
    else {
      val (s1, s2) = getBoundingSlices(xVal, 0, numSlices - 1)
      if (s1.x == xVal)
        s1.xSlice.f(yVal)
      else {
        val z1 = s1.xSlice.f(yVal)
        val z2 = s2.xSlice.f(yVal)
        val z1z2Slope = (z2 - z1) / (s2.x - s1.x)
        z1 + (xVal - s1.x) * z1z2Slope
      }
    }
  }

  /** accept xVal and yVal as Ints */
  def f(xVal: Int, yVal: Int): Float = f(xVal.toFloat, yVal.toFloat)

  /** Find the adjacent pair of Slices that bound the requested `xVal` over
     * the x range [`lowindex`, `highIndex`]
     */
  private def getBoundingSlices(xVal: Float,
                      lowIndex: Int, highIndex: Int): Tuple2[Slice, Slice] = {
    if (highIndex - lowIndex <= 1)
      (slices(lowIndex), slices(lowIndex + 1))
    else {
      val midPointIndex = (lowIndex + highIndex)/2
      if (slices(midPointIndex).x <= xVal)
        getBoundingSlices(xVal, midPointIndex, highIndex)
      else
        getBoundingSlices(xVal, lowIndex, midPointIndex)
    }
  }
 }
