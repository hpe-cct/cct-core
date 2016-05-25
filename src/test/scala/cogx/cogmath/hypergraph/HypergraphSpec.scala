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
import scala.language.reflectiveCalls

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class HypergraphSpec extends FunSuite with MustMatchers {
  class Graph extends Hypergraph
  class Node(owner: Graph) extends Hypernode(owner)

  private def makeHypergraph = new Graph {
    val v1 = new Node(this)
    val v2 = new Node(this)
    val v3 = new Node(this)
    val v4 = new Node(this)
    val v5 = new Node(this)
    val v6 = new Node(this)

    new Hyperedge(v1, Array(v2))
    new Hyperedge(v2, Array(v2, v3, v4))
    new Hyperedge(v3, Array(v5))
    new Hyperedge(v4, Array(v3))
    new Hyperedge(v5, Array(v1, v6))
  }


  test("nodes") {
    val graph = makeHypergraph
    val nodes: Seq[Hypernode] = graph.nodes
    require(nodes.size == 6)
    require(nodes contains graph.v1)
    require(nodes contains graph.v2)
    require(nodes contains graph.v3)
    require(nodes contains graph.v4)
    require(nodes contains graph.v5)
    require(nodes contains graph.v6)
  }

  test("edges") {
    val graph = makeHypergraph
    require(graph.v1.outputs(0).sinks contains graph.v2)
    require(graph.v2.outputs(0).sinks contains graph.v2)
    require(graph.v2.outputs(0).sinks contains graph.v3)
    require(graph.v2.outputs(0).sinks contains graph.v4)
    require(graph.v3.outputs(0).sinks contains graph.v5)
    require(graph.v4.outputs(0).sinks contains graph.v3)
    require(graph.v5.outputs(0).sinks contains graph.v1)
    require(graph.v5.outputs(0).sinks contains graph.v6)
  }

  test("inputs") {
    val graph = makeHypergraph
    require(graph.inputs.length == 0)
  }

  test("outputs") {
    val graph = makeHypergraph
    require(graph.outputs.length == 1)
    require(graph.outputs contains graph.v6)
  }

  test("size") {
    val graph = makeHypergraph
    require(graph.size == 6)
  }
}