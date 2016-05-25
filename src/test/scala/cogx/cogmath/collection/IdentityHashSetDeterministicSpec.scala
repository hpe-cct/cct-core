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

package cogx.cogmath.collection

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code.
  *
  * @author Greg Snider and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class IdentityHashSetDeterministicSpec extends FunSuite with MustMatchers {

  val Verbose = false

  /** add lots of strings, then remove half of them.  Some can be "equal" but not "eq". */
  def prepSet(size: Int, modulus: Int) = {
    val set =  new IdentityHashSetDeterministic[String]()
    val vals = new Array[String](size)
    for (i <- 0 until size) {
      vals(i) = (i % modulus).toString
      set += vals(i)
    }
    for (i <- 0 until size by 2) {
      set -= vals(i)
    }
    set
  }

  test("Basic") {
    val Length = 200
    val set1 = prepSet(Length, Length)
    val set2 = prepSet(Length, Length)
    val elementPairs = set1.zip(set2)
    if (Verbose)
      println("   " + elementPairs.toString)
    val elementPairsMatch = elementPairs.map( (s) => s._1 == s._2 ).reduceLeft( _ && _ )
    require(elementPairsMatch,"Deterministic IdentityHashSet found to be non-deterministic!")
    require(set1.size == Length / 2, "Size of resultant set wrong.")
  }

  test("Tests ignoring of object equals") {
    val Length = 100
    val set1 = prepSet(Length, 11)
    val set2 = prepSet(Length, 11)
    val elementPairs = set1.zip(set2)
    if (Verbose)
      println("   " + elementPairs.toString)
    val elementPairsMatch = elementPairs.map( (s) => s._1 == s._2 ).reduceLeft( _ && _ )
    require(elementPairsMatch,"Deterministic IdentityHashSet found to be non-deterministic!")
    require(set1.size == Length / 2, "Size of resultant set wrong.")
  }
}