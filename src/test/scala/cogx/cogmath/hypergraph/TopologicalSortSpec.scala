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

package cogx.cogmath.hypergraph

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.collection.IdentityHashSet

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class TopologicalSortSpec extends FunSuite with MustMatchers {

  test("dressing graph") {
    // Example from "Introduction to Algorithms" p. 486.
    val graph = new Hypergraph
    val undershorts = new Hypernode(graph)
    val pants = new Hypernode(graph)
    val belt = new Hypernode(graph)
    val shirt = new Hypernode(graph)
    val tie = new Hypernode(graph)
    val jacket = new Hypernode(graph)
    val socks = new Hypernode(graph)
    val shoes = new Hypernode(graph)
    val watch = new Hypernode(graph)

    // In the following statements, the source must be put on before the
    // all of the sinks.
    new Hyperedge(undershorts, Array(pants))
    new Hyperedge(pants, Array(shoes))
    new Hyperedge(socks, Array(shoes))
    new Hyperedge(pants, Array(belt, jacket))
    new Hyperedge(shirt, Array(pants, tie, jacket))
    new Hyperedge(tie, Array(jacket))
    new Hyperedge(watch, Array(shirt))
    new Hyperedge(belt, Array(jacket))

    val backEdges = new IdentityHashSet[Hyperedge]
    val sorted = TopologicalSort(graph, backEdges)

    //for (apparelNode <- sorted)
    //  println(apparelNode.proxyFor.name)

    require(occursBefore(undershorts, pants))
    require(occursBefore(pants, shoes))
    require(occursBefore(socks, shoes))
    require(occursBefore(pants, belt))
    require(occursBefore(pants, jacket))
    require(occursBefore(shirt, pants))
    require(occursBefore(shirt, tie))
    require(occursBefore(tie, jacket))
    require(occursBefore(watch, shirt))
    require(occursBefore(belt, jacket))

    def occursBefore(node1: Hypernode, node2: Hypernode): Boolean = {
      var found1 = false
      for (node <- sorted) {
        if (node eq node1) {
          if (!found1)
            found1 = true
          else
            require(false, "found 1 twice")
        } else if (node eq node2) {
          if (found1)
            return true
          return false
        }
      }
      require(false, "failed to find node1 and node2")
      false
    }
  }
}