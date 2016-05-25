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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import scala.math._

/** Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class Dyad2DSpec extends FunSuite with MustMatchers {

  test("Constructors") {
    val dyad = Dyad2D()
    require(dyad.stickness == 0)
    require(dyad.ballness == 0)

    val Stickness = 123f
    val Ballness = 456f
    val Orientation = (Pi / 4).toFloat

    val dyad1 = Dyad2D(Stickness, Ballness, Orientation)
    require(approxEq(dyad1.stickness, Stickness))
    require(approxEq(dyad1.ballness, Ballness))
    require(approxEq(dyad1.orientation, Orientation))

    val stick = Dyad2D.stick(Stickness, Orientation)
    require(approxEq(stick.stickness, Stickness))
    require(approxEq(stick.ballness, 0))
    require(approxEq(stick.orientation, Orientation))

    val ball = Dyad2D.ball(Ballness)
    require(approxEq(ball.stickness, 0))
    require(approxEq(ball.ballness, Ballness))
  }

  test("Add") {
    // Add 2 sticks with same orientation
    val Orient1 = (Pi / 4).toFloat
    val stick1 = Dyad2D.stick(1f, Orient1)
    val stick2 = Dyad2D.stick(2f, Orient1)
    val sum12 = stick1 + stick2
    require(approxEq(sum12.stickness, 3))
    require(approxEq(sum12.ballness, 0))
    require(approxEq(sum12.orientation, Orient1))

    // Add 2 sticks with orthogonal orientation
    val Orient2 = Orient1 + (Pi / 2).toFloat
    val stick3 = Dyad2D.stick(1f, Orient2)
    val sum13 = stick1 + stick3
    require(approxEq(sum13.stickness, 0))
    require(approxEq(sum13.ballness, 1))

    // Add stick and ball
    val ball4 = Dyad2D.ball(1f)
    val sum14 = stick1 + ball4
    require(approxEq(sum14.stickness, 1))
    require(approxEq(sum14.ballness, 1))
    require(approxEq(sum14.orientation, Orient1))

    // Add two balls
    val ball5 = Dyad2D.ball(1f)
    val sum15 = ball4 + ball5
    require(approxEq(sum15.stickness, 0))
    require(approxEq(sum15.ballness, 2))

    // Add two sticks with similar orientations.
    val Delta = 0.01f
    val Orient3 = Orient1 + Delta
    val stick6 = Dyad2D.stick(1f, Orient3)
    val sum16 = stick1 + stick6
    require(approxEq(sum16.orientation, Orient1 + Delta / 2))
  }

  test("Multiply") {
    val Stickness = 123f
    val Ballness = 456f
    val Orientation = (Pi / 4).toFloat

    val dyad1 = Dyad2D(Stickness, Ballness, Orientation)
    val dyad2 = dyad1 * 2
    require(approxEq(dyad2.stickness, 2 * Stickness))
    require(approxEq(dyad2.ballness, 2 * Ballness))
    require(approxEq(dyad2.orientation, Orientation))
  }

  test("Rotate") {
    val Stickness = 123f
    val Ballness = 456f
    val Orientation = (Pi / 4).toFloat
    val Rotation = (Pi / 4).toFloat
    val dyad1 = Dyad2D(Stickness, Ballness, Orientation)
    val dyad2 = dyad1.rotate(Rotation)
    require(approxEq(dyad2.stickness, Stickness))
    require(approxEq(dyad2.ballness, Ballness))
    require(approxEq(dyad2.orientation, Orientation + Rotation))
  }

  test("CastConstructor") {
    val Stickness = 123f
    val Ballness = 456f
    val Orientation = (Pi / 4).toFloat
    val dyad1 = Dyad2D(Stickness, Ballness, Orientation)
    val vector1 = dyad1.asInstanceOf[Vector]
    val dyad2 = Dyad2D(vector1)
    require(dyad1.stickness == dyad2.stickness)
    require(dyad1.ballness == dyad2.ballness)
    require(dyad1.stickness == dyad2.stickness)
  }

  /** Return true if "x" and "y" are approximately equal. */
  private def approxEq(x: Float, y: Float): Boolean = {
    val eps = pow(2.0, -15.0)
    if (x == 0 && y.abs < 10 * eps)
      true
    else if (y == 0 && x.abs < 10 * eps)
      true
    else
      (x - y).abs < 10 * eps * (x.abs max y.abs)
  }
}