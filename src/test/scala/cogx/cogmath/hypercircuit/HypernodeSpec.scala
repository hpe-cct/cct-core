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

package cogx.cogmath.hypercircuit

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers

/** Test code for the Hypernode class.
  *
  * @author Greg Snider and Dick Carter
  */
@RunWith(classOf[JUnitRunner])
class HypernodeSpec extends FunSuite with MustMatchers {

  test("constructor") {
    class Node(inputEdges: Array[Hyperedge[Node]]) extends Hypernode[Node](inputEdges)
    class Graph extends Hypercircuit[Node]

    val graph = new Graph

    val node6 = new Node(Array[Hyperedge[Node]]())
    val node7 = new Node(Array[Hyperedge[Node]]())

    val e6 = new Hyperedge(node6)
    val e7 = new Hyperedge(node7)

    val node1 = new Node(Array(e6, e7))

    require(node1.circuit eq graph)
    require(node1.inputs.length == 2)
    require(node1.inputs(0).source eq node6)
    require(node1.inputs(1).source eq node7)

    val e1 = new Hyperedge(node1)
    val e2 = new Hyperedge(node1)
    val e3 = new Hyperedge(node1)

    val node2 = new Node(Array(e1))
    val node3 = new Node(Array(e2))
    val node4 = new Node(Array(e3))
    val node5 = new Node(Array(e3))

    require(node1.outputs.length == 3)
    require(node1.outputs(0).sinks.length == 1)
    require(node1.outputs(1).sinks.length == 1)
    require(node1.outputs(2).sinks.length == 2)
    require(node1.outputs(0).sinks contains node2)
    require(node1.outputs(1).sinks contains node3)
    require(node1.outputs(2).sinks contains node4)
    require(node1.outputs(2).sinks contains node5)

  }
}