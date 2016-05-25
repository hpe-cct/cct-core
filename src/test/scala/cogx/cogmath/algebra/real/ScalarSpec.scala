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

/**  Test code.
  *
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ScalarSpec extends FunSuite with MustMatchers {

  test("Basic") {
    val scalar1 = new Scalar(1.0f)
    val scalar2 = new Scalar(2.5f)
    require(scalar1.shape.dimensions == 0)
    require(scalar1.shape == scalar2.shape)
    require(!(scalar1 == scalar2))
    val scalar3 = new Scalar(2.5f)
    require(scalar2 == scalar3)
  }
}