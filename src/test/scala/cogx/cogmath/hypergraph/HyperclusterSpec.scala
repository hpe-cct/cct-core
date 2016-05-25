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

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class HyperclusterSpec extends FunSuite with MustMatchers {

  test("ownership") {
    val graph = new Hypergraph
    val node1 = new Hypernode(graph)
    val node2 = new Hypernode(graph)
    val node3 = new Hypernode(graph)
    new Hyperedge(node1, Array(node2, node3))
    val cluster = Hypercluster(node2, node3)
    require(cluster.owner == graph)
    require(graph.contains(node1))
    require(!graph.contains(node2))
    require(!graph.contains(node3))
    require(graph.contains(cluster))
    require(cluster.size == 2)
    val expectedContents = Array(node2, node3)
    for (node <- cluster)
      require(expectedContents contains node)
  }

  test("edges") {
    // Create a complex hypergraph to exercise all cases of merging:
    //
    // input driving one merged node
    // input driving both merged nodes
    // one merged node driving the other plus external node
    // one merged node driving two external nodes
    //
    val graph = new Hypergraph
    val node1 = new Hypernode(graph)
    val node2 = new Hypernode(graph)
    val node3 = new Hypernode(graph)
    val node4 = new Hypernode(graph)
    val node5 = new Hypernode(graph)
    val node6 = new Hypernode(graph)
    val node7 = new Hypernode(graph)
    val node8 = new Hypernode(graph)
    val node9 = new Hypernode(graph)
    val node10 = new Hypernode(graph)

    val edge1 = new Hyperedge(node1, Array(node4))
    val edge2 = new Hyperedge(node3, Array(node2, node4, node5))
    val edge3 = new Hyperedge(node4, Array(node5, node6, node10))
    val edge4 = new Hyperedge(node9, Array(node5))
    val edge5 = new Hyperedge(node5, Array(node7, node8))
    val edge6 = new Hyperedge(node5, Array(node10))

    val cluster = Hypercluster(node4, node5)
    require(edge1.sinks.length == 1)
    require(edge2.sinks.length == 2)
    require(edge3.sinks.length == 2)
    require(edge4.sinks.length == 1)
    require(edge5.sinks.length == 2)

    val sinks1 = Seq(cluster)
    val sinks2 = Seq(node2, cluster)
    val sinks3 = Seq(node6, node10)
    val sinks4 = Seq(cluster)
    val sinks5 = Seq(node7, node8)
    val sinks6 = Seq(node10)

    edge1.sinks.foreach(sink => require(sinks1 contains sink))
    edge2.sinks.foreach(sink => require(sinks2 contains sink))
    edge3.sinks.foreach(sink => require(sinks3 contains sink))
    edge4.sinks.foreach(sink => require(sinks4 contains sink))
    edge5.sinks.foreach(sink => require(sinks5 contains sink))
    edge6.sinks.foreach(sink => require(sinks6 contains sink))

  }
}
