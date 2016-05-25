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

/** Test code.
  *
  * @author Greg Snider and Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class HypercircuitSpec extends FunSuite with MustMatchers {
  class Node(inputEdges: Array[Hyperedge[Node]]) extends Hypernode[Node](inputEdges)
  class Graph extends Hypercircuit[Node] {
    val v1 = new Node(Array[Hyperedge[Node]]())
    val e1 = new Hyperedge(v1)
    val v2 = new Node(Array(e1))
    val e2 = new Hyperedge(v2)
    val v4 = new Node(Array(e2))
    val e4 = new Hyperedge(v4)
    val v3 = new Node(Array(e2, e4))
    val e3 = new Hyperedge(v3)
    val v5 = new Node(Array(e3))
    val e5 = new Hyperedge(v5)
    val v6 = new Node(Array(e5))
  }

  test("nodes") {
    val graph = new Graph
    val nodes = graph.flattenPreorder
    require(nodes.size == 6)
    require(nodes contains graph.v1)
    require(nodes contains graph.v2)
    require(nodes contains graph.v3)
    require(nodes contains graph.v4)
    require(nodes contains graph.v5)
    require(nodes contains graph.v6)
  }

  test("edges") {
    val graph = new Graph
    require(graph.v1.outputs(0).sinks contains graph.v2)
    require(graph.v2.outputs(0).sinks contains graph.v3)
    require(graph.v2.outputs(0).sinks contains graph.v4)
    require(graph.v3.outputs(0).sinks contains graph.v5)
    require(graph.v4.outputs(0).sinks contains graph.v3)
    require(graph.v5.outputs(0).sinks contains graph.v6)
  }

  test("size") {
    val graph = new Graph
    require(graph.size == 6)
  }

  class NamedNode(name: String, inputEdges: Array[Hyperedge[NamedNode]]) extends Hypernode[NamedNode](inputEdges) {
    override def toString() = name
  }
  class DressingGraph extends Hypercircuit[NamedNode] {
    val undershorts = new NamedNode("undershorts", Array[Hyperedge[NamedNode]]())
    val socks = new NamedNode("socks", Array[Hyperedge[NamedNode]]())
    val watch = new NamedNode("watch", Array[Hyperedge[NamedNode]]())
    val shirt = new NamedNode("shirt", Array(new Hyperedge(watch)))
    val e1 = new Hyperedge(shirt)
    val pants = new NamedNode("pants", Array(e1, new Hyperedge(undershorts)))
    val tie = new NamedNode("tie", Array(e1))
    val shoes = new NamedNode("shoes", Array(new Hyperedge(socks), new Hyperedge(pants)))
    val e2 = new Hyperedge(pants)
    val belt = new NamedNode("belt", Array(e2))
    val jacket = new NamedNode("jacket", Array(new Hyperedge(tie), e1, e2, new Hyperedge(belt)))
  }

  test("dressing graph") {
    // Example from "Introduction to Algorithms" p. 486.
    val graph = new DressingGraph


    val sorted = graph.flatten

    import graph._

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

    def occursBefore(node1: NamedNode, node2: NamedNode): Boolean = {
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

  test("improper mixing of two graphs" )  {
    try {
      val graph1 = new DressingGraph
      val graph2 = new DressingGraph

      val e6_graph1 = new Hyperedge(graph1.jacket)
      val e6_graph2 = new Hyperedge(graph2.jacket)
      val mixedUpNode = new NamedNode("mixedUpNode", Array(e6_graph1, e6_graph2))
      // Got to here?  That's bad.
      throw new RuntimeException("MixedHypercircuitException exception missing.")
    }
    catch {
      case e: MixedHypercircuitException =>  println("Saw expected exception: " + e)
    }
  }

  test("thread isolation" )  {
    val graph1 = new DressingGraph
    new Thread {
      override def run {
        val graph2 = new DressingGraph
      }
    }.start
    Thread.sleep(1000)
    // The new Thread has probably made its circuit, now add to our thread's graph1
    val hat = new NamedNode("hat", Array[Hyperedge[NamedNode]]())
    val e6_graph1 = new Hyperedge(graph1.jacket)
    val e7_graph1 = new Hyperedge(hat)
    // A prior version of the Cog compiler would throw an exception here, since we're
    // adding to graph1 after graph2 was started (in a different thread).
    val umbrella = new NamedNode("umbrella", Array(e6_graph1, e7_graph1))
    require(true)
  }
}
