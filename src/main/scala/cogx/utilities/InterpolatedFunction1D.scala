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

/** Represents a function as a series of (x, y) points and returns f(x), possibly
  * interpolated. Out-of-range x values return f(x) of nearest point.
  *
  * User: Dick Carter
  * Date: 10/6/13
  */
private [utilities] class InterpolatedFunction1D(unsortedData: Array[Tuple2[Float, Float]]) {

  case class Point(x: Float, y: Float)

  val data = unsortedData.map(p => Point(p._1, p._2)).
          sortWith((A, B) => A.x < B.x)
  val numPoints = data.length
  require(numPoints > 0, "Function needs 1 or more points")
  val firstPoint = data(0)
  val lastPoint = data(numPoints - 1)

  /** Return y = f(xVal) using linear interpolation */
  def f(xVal: Float): Float = {
    if (xVal <= firstPoint.x)
      firstPoint.y
    else if (xVal >= lastPoint.x)
      lastPoint.y
    else {
      val (p1, p2) = getBoundingPoints(xVal, 0, numPoints - 1)
      if (p1.x == xVal)
        p1.y
      else {
        val p1p2Slope = (p2.y - p1.y) / (p2.x - p1.x)
        p1.y + (xVal - p1.x) * p1p2Slope
      }
    }
  }

  /** accept xVal as Int */
  def f(xVal: Int): Float = f(xVal.toFloat)

  /** Find the adjacent pair of Points that bound the requested `xVal` over
    * the x range [`lowindex`, `highIndex`]
    */
  private def getBoundingPoints(xVal: Float,
                       lowIndex: Int, highIndex: Int): Tuple2[Point, Point] = {
    if (highIndex - lowIndex <= 1)
      (data(lowIndex), data(lowIndex + 1))
    else {
      val midPointIndex = (lowIndex + highIndex)/2
      if (data(midPointIndex).x <= xVal)
        getBoundingPoints(xVal, midPointIndex, highIndex)
      else
        getBoundingPoints(xVal, lowIndex, midPointIndex)
    }
  }
}
