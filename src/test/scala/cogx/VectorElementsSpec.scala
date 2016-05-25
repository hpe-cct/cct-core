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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import cogx.reference.RefTestInterface

/** Tests for vectorElements operator
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class VectorElementsSpec extends FunSuite{
  val input = Array(
    Array(Vector(0,1,2,3),Vector(4,5,6,7),Vector(8,9,10,11)),
    Array(Vector(12,13,14,15),Vector(16,17,18,19),Vector(20,21,22,23)),
    Array(Vector(24,25,26,27),Vector(28,29,30,31),Vector(32,33,34,35))
  )

  def testExample(index:Array[Array[Vector]], answer:Array[Array[Vector]]){
    val graph = new ComputeGraph with RefTestInterface {
      val x = VectorField(3,3,(r,c)=>input(r)(c))
      val n =
        if(index(0).length == 1 && index.length == 1)
          VectorField(index(0)(0))
        else
          VectorField(3,3,(r,c)=>index(r)(c))
      val correct = VectorField(3,3,(r,c)=>answer(r)(c))
      val y = vectorElements(x,n)

      probe(correct, y)
    }

    import graph._
    withRelease {
      step
      require(readVector(correct) == readVector(y))
    }
  }

  test("Index example 1") {
    val index = Array(Array(Vector(3,1)))
    //correct answer for vectorElements(ref, n)
    val answer = Array(
      Array(Vector(3,1),Vector(7,5),Vector(11,9)),
      Array(Vector(15,13),Vector(19,17),Vector(23,21)),
      Array(Vector(27,25),Vector(31,29),Vector(35,33))
    )
    testExample(index,answer)
  }

  test("Index example 2") {
    val index = Array(Array(Vector(2)))
    //correct answer for vectorElements(a, n2)
    val answer = Array(
      Array(Vector(2),Vector(6),Vector(10)),
      Array(Vector(14),Vector(18),Vector(22)),
      Array(Vector(26),Vector(30),Vector(34))
    )
    testExample(index,answer)
  }

  test("Index example 3") {
    val index = Array(Array(Vector(0,0)))
    //correct answer for vectorElements(a, n00)
    val answer = Array(
      Array(Vector(0,0),Vector(4,4),Vector(8,8)),
      Array(Vector(12,12),Vector(16,16),Vector(20,20)),
      Array(Vector(24,24),Vector(28,28),Vector(32,32))
    )
    testExample(index,answer)
  }

  test("Index example 4") {
    val index = Array(
      Array(Vector(1,1,2,2),Vector(0,2,1,3),Vector(3,1,2,0)),
      Array(Vector(2,1,0,3),Vector(0,3,2,1),Vector(1,0,3,2)),
      Array(Vector(2,3,0,1),Vector(3,2,1,0),Vector(1,3,0,2))
    )
    val answer = Array(
      Array(Vector(1,1,2,2),Vector(4,6,5,7),Vector(11,9,10,8)),
      Array(Vector(14,13,12,15),Vector(16,19,18,17),Vector(21,20,23,22)),
      Array(Vector(26,27,24,25),Vector(31,30,29,28),Vector(33,35,32,34))
    )
    testExample(index,answer)
  }

  test("Make sure NaN is returned for a bad index"){
    val graph = new ComputeGraph with RefTestInterface{
      val a = VectorField(Vector(1,2,3,4))
      val n = VectorField(Vector(-1,4,1,2))
      val y = vectorElements(a,n)

      probe(y)
    }

    import graph._
    withRelease {
      step
      val answer = readVector(y).asRawArray
      require(answer(0) equals Float.NaN)
      require(answer(1) equals Float.NaN)
      require(answer(2) equals 2f)
      require(answer(3) equals 3f)
    }
  }

}
