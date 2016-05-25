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
class InterpolatedFunction1DSpec extends FunSuite with MustMatchers {

  test("Basic") {
    val data = Array(
      (0.0f, 1.0f),
      (2.0f, 3.0f),
      (4.5f, 6.0f),
      (3.5f, -1f))
    val func = new InterpolatedFunction1D(data)
    data.foreach(tuple => require(func.f(tuple._1) == tuple._2))
    require(func.f(1.0f) == 2.0f)
    require(func.f(4.0f) == 2.5f)
    require(func.f(5.0f) == 6.0f)
    require(func.f(-1.0f) == 1.0f)
    require(func.f(2.5f) == 3.0f - 4.0f/3)
  }
}
