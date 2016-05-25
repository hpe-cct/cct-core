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

import cogx.cogmath.collection.{IdentityHashMap, IdentityHashSet}
import scala.collection.mutable.ArrayBuffer

/** Maps an acyclic hypergraph onto a total ordering.
  *
  * The algorithm is described in "Introduction to Algorithms" by Cormen,
  * Leiserson and Rivest, p 485 ("Topological Sort").
  *
  * @author Greg Snider
  */
private [hypergraph] object TopologicalSort {

  /** Map graph onto total ordering. If graph is cyclic, the back edges must
    * be listed in `backEdges` so that they can be ignored during the sort.
    *
    * Uses the `color` attribute of nodes.
    *
    * @param graph The graph to be sorted.
    * @param backEdges The edges in the graph which are ignored during the sort;
    *        removing these edges from graph must result in an acyclic graph.
    * @return A list of the total ordering (starting at inputs).
    */
  def apply(graph: Hypergraph, backEdges: IdentityHashSet[Hyperedge]): Seq[Hypernode] =
  {
    val sortedNodes = new ArrayBuffer[Hypernode]
    val nodeColor = new IdentityHashMap[Hypernode, NodeColor]

    for (node <- graph.nodes)
      nodeColor(node) = White

    // Now visit all of the nodes in arbitrary order.
    for (node <- graph.nodes) {
      if (nodeColor(node) == White)
        visit(node, nodeColor, sortedNodes, backEdges)
    }
    sortedNodes.reverse
  }

  /**
    * Visit and mark a single node.
    *
    * @param node the node to visit.
    */
  private def visit(node: Hypernode,
                    nodeColor: IdentityHashMap[Hypernode, NodeColor],
                    sortedNodes: ArrayBuffer[Hypernode],
                    backEdges: IdentityHashSet[Hyperedge])
  {
    require(nodeColor(node) == White)
    nodeColor(node) = Grey
    val sinks = new ArrayBuffer[Hypernode] {
      for (edge <- node.outputs) {
        if (!backEdges.contains(edge)) {
          edge.sinks.foreach(this += _)
        }
      }
    }
    for (sink <- sinks) {
      if (nodeColor(sink) == White)
        visit(sink, nodeColor, sortedNodes, backEdges)
      else if (nodeColor(sink) == Grey)
        require(false, "back edge found: graph not acyclic")
    }
    nodeColor(node) = Black
    sortedNodes += node
  }
}