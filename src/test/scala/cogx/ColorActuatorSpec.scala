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

package cogx

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

/** Test code for Actuators.
  *
  * @author Greg Snider and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class ColorActuatorSpec
        extends FunSuite
        with MustMatchers
{

  /** A mapping from the coordinates in the color field to a Pixel value */
  def rcToPixel(r: Int, c: Int) = new Pixel(r, c, r + c)

  /** A different mapping from the coordinates in the color field to a Pixel value, used to init */
  def rcToInitPixel(r: Int, c: Int) = new Pixel(2*r, 3*c, r + 2*c)

  val zeroPixel = new Pixel(0,0,0)

  test("2 D") {
    val output = Array.ofDim[Pixel](2, 3)
    val graph = new ComputeGraph(optimize = false) {
      val field = ColorField(2, 3, (row, col) => rcToPixel(row, col))
      ColorActuator(field, output)
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == zeroPixel)
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == rcToPixel(row, col))
    }
  }

  test("2 D with non-zero init") {
    val Rows = 2
    val Columns = 3
    val output = Array.ofDim[Pixel](Rows, Columns)
    val graph = new ComputeGraph(optimize = false) {
      val field = ColorField(Rows, Columns, (row, col) => rcToPixel(row, col))
      ColorActuator(field, output, (row, col) => rcToInitPixel(row, col))
    }
    import graph._
    withRelease {
      reset
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) == rcToInitPixel(row, col))
      step
      for (row <- 0 until output.length; col <- 0 until output(0).length)
        require(output(row)(col) ==  rcToPixel(row, col))
    }
  }
}
