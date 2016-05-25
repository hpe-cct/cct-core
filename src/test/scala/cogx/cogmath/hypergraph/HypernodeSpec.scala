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

/** Test code for the Hypernode class.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class HypernodeSpec extends FunSuite with MustMatchers {
  val Inputs = 3
  val Outputs = 2

  test("constructor") {
    class Graph extends Hypergraph
    class Node(owner: Graph) extends Hypernode(owner)

    val graph = new Graph
    val node1 = new Node(graph)
    require(node1.owner eq graph)
    val node2 = new Node(graph)
    val node3 = new Node(graph)
    val node4 = new Node(graph)
    val node5 = new Node(graph)

    val e1 = new Hyperedge(node1, Array(node2))
    val e2 = new Hyperedge(node1, Array(node3))
    val e3 = new Hyperedge(node1, Array(node4, node5))

    require(node1.outputs.length == 3)
    require(node1.outputs(0).sinks.length == 1)
    require(node1.outputs(1).sinks.length == 1)
    require(node1.outputs(2).sinks.length == 2)
    require(node1.outputs(0).sinks contains node2)
    require(node1.outputs(1).sinks contains node3)
    require(node1.outputs(2).sinks contains node4)
    require(node1.outputs(2).sinks contains node5)

    val node6 = new Node(graph)
    val node7 = new Node(graph)
    new Hyperedge(node6, Array(node1))
    new Hyperedge(node7, Array(node1))
    require(node1.inputs.length == 2)
    require(node1.inputs(0).source eq node6)
    require(node1.inputs(1).source eq node7)

  }
}