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

package cogx.test

import cogx.platform.cpumemory.readerwriter.FieldReader
import org.scalatest.matchers.{MatchResult, Matcher}

/** ScalaTest matchers for Cog CPU memory types.
  *
  * @author Ben Chandler
  */
private[cogx] trait CogMatchers {
  /** Test two field memories for approximate equality by L-inf norm. */
  def approxEqual(right: FieldReader) = new Matcher[FieldReader] {
    def apply(left: FieldReader) = {
      if (left.fieldType.equals(right.fieldType)) {
        MatchResult(
          left.approxEquals(right),
          "left is not approximately equal to right",
          "left is approximately equal to right")
      } else {
        MatchResult(
          false,
          left.fieldType.toString + " is a type mis-match for " + right.fieldType.toString,
          "left and right types match")
      }
    }
  }

  /** Test two field memories for exact equality. */
  def equal(right: FieldReader) = new Matcher[FieldReader] {
    def apply(left: FieldReader) = {
      require(right != null)
      require(left != null)
      if (left.fieldType.equals(right.fieldType)) {
        MatchResult(
          left.equals(right),
          "left is not approximately equal to right",
          "left is approximately equal to right")
      } else {
        MatchResult(
          false,
          left.fieldType.toString + " is a type mis-match for " + right.fieldType.toString,
          "left and right types match")
      }
    }
  }
}
