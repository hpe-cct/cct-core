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
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class PriorityQueueSpec extends FunSuite with MustMatchers {

  test("all") {
    class Element(var priority: Double)
    def priority(element: Element): Double = element.priority
    val random = new scala.util.Random
    val queue = new PriorityQueue(priority)
    val elements = new IdentityHashSet[Element]
    val Size = 10000
    for (i <- 0 until Size) {
      val element = new Element(random.nextDouble)
      elements += element
      //println(" " + element.priority)
      queue.enqueue(element)
    }

    //println("changing priorities")
    for (element <- elements) {
      element.priority = random.nextDouble
      queue.priorityChanged(element)
    }

    var lastPriority = 1000000000.0
    for (i <- 0 until Size) {
      val element = queue.dequeue
      //println(" " + element.priority)
      require(element.priority <= lastPriority, element.priority + " " + lastPriority)
      lastPriority = element.priority
    }
  }
}