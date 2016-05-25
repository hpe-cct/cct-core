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

/** Levelize a hypergraph from inputs to outputs.
  *
  * @author Greg Snider
  */
private [cogx] object Levelize {

  /** Levelize a hypergraph from inputs to outputs.
    *
    * The graph must be acyclic. To make a cyclic
    * graph acyclic, the `backEdges` parameter should be passed in with the set
    * of all back edges in the graph--these are ignored in the levelization.
    *
    * @param graph The directed hypergraph to levelized.
    * @param backEdges The edges to be ignored in the traversal since induce
    *        loops in the graph.
    * @return Map of node in graph to its level
    */
  def apply(graph: Hypergraph,
            backEdges: IdentityHashSet[Hyperedge] = new IdentityHashSet[Hyperedge]):
     IdentityHashMap[Hypernode, Int] =

  {
    val nodeLevel = new IdentityHashMap[Hypernode, Int]
    // Initialize levels
    for (node <- graph)
      nodeLevel(node) = 0
    val ordered: Seq[Hypernode] = TopologicalSort(graph, backEdges)
    markLevels(ordered, nodeLevel, backEdges)
    nodeLevel
  }

  /** Levelize a hypergraph from inputs to outputs.
    *
    * The graph must be acyclic. To make a cyclic
    * graph acyclic, the `backEdges` parameter should be passed in with the set
    * of all back edges in the graph--these are ignored in the levelization.
    *
    * @param graph The directed hypergraph to levelized.
    * @param backEdges The edges to be ignored in the traversal since induce
    *        loops in the graph.
    *
    * @return Sequence of lists, one list per level (starting at level 0)
    */
  def toLists(graph: Hypergraph,
              backEdges: IdentityHashSet[Hyperedge]): Seq[List[Hypernode]] =
  {
    val nodeToLevel = apply(graph, backEdges)
    val levelList = new ArrayBuffer[List[Hypernode]]
    for (node <- graph.nodes) {
      val level = nodeToLevel(node)
      if (level >= levelList.length)
        for (i <- levelList.length to level + 1)
          levelList += List[Hypernode]()
      levelList(level) ::= node
    }
    levelList
  }

  /** Mark the level of each Vertex in the graph.
    *
    * The level is equivalent to the worst case "arrival time" of inputs plus
    * the propagation `delay` of the Node.
    *
    * @param sortedNodes Topologically sorted nodes.
    * @param backEdges Edges to be ignored in marking levels.
    */
  private def markLevels(sortedNodes: Seq[Hypernode],
                         nodeLevel: IdentityHashMap[Hypernode, Int],
                         backEdges: IdentityHashSet[Hyperedge])
  {
    for (node <- sortedNodes) {
      var maxInputTime = 0
      for (inEdge <- node.inputs) {
        if (!backEdges.contains(inEdge)) {
          val source = inEdge.source
          val sourceTime = nodeLevel(source)
          require(sourceTime >= 0, "internal error")
          if (sourceTime > maxInputTime)
            maxInputTime = sourceTime
        }
      }
      nodeLevel(node) = 1 + maxInputTime
    }
  }
}

