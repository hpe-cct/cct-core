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
import cogx.compiler.parser.semantics.SemanticErrorException
import cogx.test._

/** Test code for the Parser
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class ParserSpec extends FunSuite with MustMatchers with CogMatchers {

  test("binary operator input size mismatch") {
    // Add two mismatched fields - should throw exception at the offending line.
    try {
      val graph = new ComputeGraph {
        val in1 = ScalarField(1, 2)
        val in2 = ScalarField(3, 4)
        val sum1 = in1 + in2      // Should throw SemanticError exception here
        // Got to here?  That's bad
        throw new RuntimeException("SemanticError exception missing.")
        val sum2 = in1 + in1
      }
    }
    catch {
      case e: SemanticErrorException =>  println("Saw expected exception: " + e)
    }
  }

  test("deep, reconvergent network") {
    val depth = 500

    val graph = new ComputeGraph(optimize = false) {
      val one = ScalarField(1f)
      val two = ScalarField(2f)

      def build(i: Int, left: Field, right: Field): (Field, Field) = {
        if (i == 0) {
          (left, right)
        } else {
          build(i-1, (left+right+one)/two + one, (left+right-one)/two + one)
        }
      }

      val (left, right) = build(depth, one, two)
      val expectedLeft = ScalarField(2f + depth)
      val expectedRight = ScalarField(1f + depth)

      probe(left, "")
      probe(right, "")
      probe(expectedLeft, "")
      probe(expectedRight, "")
    }

    import graph._
    withRelease {
      step
      read(left) must approxEqual (read(expectedLeft))
      read(right) must approxEqual (read(expectedRight))
    }
  }
}
