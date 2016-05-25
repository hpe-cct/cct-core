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

/** Test code for the Hyperedge class.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class HyperedgeSpec extends FunSuite with MustMatchers {

  test("all") {
    class Graph extends Hypergraph
    class Node(owner: Graph) extends Hypernode(owner)
    val graph = new Graph
    val node0 = new Node(graph)
    val node1 = new Node(graph)
    val node2 = new Node(graph)
    val node3 = new Node(graph)

    val edge1 = new Hyperedge(node0, Array(node2, node3))
    val edge2 = new Hyperedge(node1, Array(node2))

    require(node0.outputs(0).sinks.length == 2)
    require(node1.outputs(0).sinks.length == 1)

    require(node0.outputs(0).sinks contains node2)
    require(node0.outputs(0).sinks contains node3)
    require(node1.outputs(0).sinks contains node2)

    require(node0.inputs.length == 0)
    require(node1.inputs.length == 0)
    require(node2.inputs.length == 2)
    require(node3.inputs.length == 1)

    require(node2.inputs.map(_.source) contains node0)
    require(node2.inputs.map(_.source) contains node1)
    require(node3.inputs.map(_.source) contains node0)
  }
}