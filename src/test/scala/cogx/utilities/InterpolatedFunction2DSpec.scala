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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class InterpolatedFunction2DSpec extends FunSuite with MustMatchers {

  test("Basic") {
    val Offset = 1.0f
    val data = Array(
      (0.0f, 1.0f, Offset + 1.0f),
      (0.0f, 0.0f, Offset + 0.0f),
      (1.0f, 0.0f, Offset + 10.0f),
      (1.0f, 1.0f, Offset + 11.0f))
    val func = new InterpolatedFunction2D(data)
    data.foreach(tuple => require(func.f(tuple._1, tuple._2) == tuple._3))
    require(func.f(0.0f, 0.5f) == Offset + 0.5f)
    require(func.f(0.5f, 0.0f) == Offset + 5.0f)
    require(func.f(0.5f, 0.5f) == Offset + 5.5f)
    require(func.f(0.25f, 0.75f) == Offset + 2.5f + 0.75f)
    require(func.f(-1f, -1f) == Offset)
    require(func.f(2f, 2f) == Offset + 11.0f)
  }
}
