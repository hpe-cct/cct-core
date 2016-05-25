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

/** Test code for the Hyperedge class.
  *
  * @author Greg Snider and Dick Carter
  */
@RunWith(classOf[JUnitRunner])
class HyperedgeSpec extends FunSuite with MustMatchers {

  test("all") {
    class Node(inputEdges: Array[Hyperedge[Node]]) extends Hypernode[Node](inputEdges)
    class Graph extends Hypercircuit[Node]
    val graph = new Graph
    val node0 = new Node(Array[Hyperedge[Node]]())
    val node1 = new Node(Array[Hyperedge[Node]]())

    val edge1 = new Hyperedge(node0)
    val edge2 = new Hyperedge(node1)

    val node2 = new Node(Array(edge1, edge2))
    val node3 = new Node(Array(edge1))


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