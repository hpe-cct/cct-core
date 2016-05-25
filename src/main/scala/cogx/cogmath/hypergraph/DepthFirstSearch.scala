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

import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}
import scala.collection.mutable.ArrayBuffer
import java.util.{Comparator, TreeSet}

/** Implements depth first search of a hypergraph. "Labels" each node with
  * "discovery" and "finishing" times. See "Introduction to Algorithms" by
  * Cormen, Leiserson and Rivest (ISBN 0-07-013143-0 McGraw-Hill) for a
  * description of the algorithm.
  *
  * @param graph The graph to be searched.
  * @param forward True for a forward search, false for a reverse search (where
  *        edges in the graph are assumed to point in the opposite direction).
  * @param nodeSearchOrder Optional specification of the order in which to
  *        search nodes.
  *
  * @author Greg Snider
  */
private [cogx] class DepthFirstSearch(graph: Hypergraph,
                       forward: Boolean = true,
                       nodeSearchOrder: Seq[Hypernode] = null)
{
  /** Nonrecursive mode not working... */
  private val Nonrecursive = false
  private val _predecessor = new IdentityHashMap[Hypernode, Hypernode]
  private val _discoveryTime = new IdentityHashMap[Hypernode, Int]
  private val _finishingTime = new IdentityHashMap[Hypernode, Int]

  /** Source of timestamps on nodes. */
  private var time = 0
  /** All backedges found in search */
  private val backEdges = new IdentityHashSet[Hyperedge]
  /** Topologically sorted nodes. */
  private var levelizedNodes = new ArrayBuffer[Hypernode]
  /** Order in which to search the nodes. */
  val searchOrder: Seq[Hypernode] = if (nodeSearchOrder != null) {
    // User-defined search order
    nodeSearchOrder
  } else {
    // Visit all of the nodes in arbitrary order.
    /*
    new ArrayBuffer[Hypernode] {
      for (node <- graph)
        this += node
    }
    */
    new ArrayBuffer[Hypernode] {
      val nodes = graph.nodes
      nodes.filter(_.inputs.length == 0).foreach(this += _)
      nodes.filter(_.inputs.length != 0).foreach(this += _)
    }
  }
  private val nodeColor = new IdentityHashMap[Hypernode, NodeColor]

  // Initialize
  for (node <- graph)
    nodeColor(node) = White

  /** Nodes ordered by depth-first traversal. */
  val subgraphRoots: Seq[Hypernode] = if (forward)
    visitNodes(searchOrder)
  else
    reverseVisitNodes(searchOrder)

  /** Get the discovery time of a node in a graph that's been searched.
    *
    * @param node the node being queried.
    * @return the discovery time of that node.
    */
  def discoveryTime(node: Hypernode): Int =
    _discoveryTime(node)

  /** Get the finishing time of a node in a hypergraph that's been searched.
    *
    * @param node the node being queried.
    * @return the finishing time of that node.
    */
  def finishingTime(node: Hypernode): Int =
    _finishingTime(node)

  /**
   * Get the predecessor of a node in a hypergraph that's been searched.
   *
   * @param node the node of interest.
   * @return node's predecessor.
   */
  def predecessor(node: Hypernode): Hypernode =
    _predecessor(node)



  /** Search the nodes in the order supplied by an iterator.
    *
    * @param nodes Ordered nodes to be visited.
    * @return the roots of the subtrees found in the visiting.
    */
  private def visitNodes(nodes: Seq[Hypernode]): Seq[Hypernode] = {
    val treeRoots = new ArrayBuffer[Hypernode]
    for (node <- nodes) {
      if (nodeColor(node) == White) {
//        if (Nonrecursive)
//          visitNonrecursive(node)
//        else
          visit(node)
        treeRoots += node
      }
    }
    treeRoots
  }

  /** Search the nodes in the order supplied by an iterator. However
    * the graph is searched as though the direction of every edge
    * were reversed.
    *
    * @param nodes Ordered nodes to be visited.
    * @return the roots of the subtrees found in the visiting.
    */
  private def reverseVisitNodes(nodes: Seq[Hypernode]): Seq[Hypernode] = {
    val treeRoots = new ArrayBuffer[Hypernode]
    for (node <- nodes) {
      if (nodeColor(node) == White) {
//        if (Nonrecursive)
//          reverseVisitNonrecursive(node)
//        else
          reverseVisit(node)
        treeRoots += node
      }
    }
    treeRoots
  }

  private def visitNonrecursive(node: Hypernode) {
    if (nodeColor(node) == White) {
      nodeColor(node) = Grey
      time += 1
      _discoveryTime(node) = time
      val stack = new scala.collection.mutable.Stack[Hypernode]
      stack.push(node)
      while (!stack.isEmpty) {
        val node = stack.pop
        for (edge <- node.outputs) {
          for (sink <- edge.sinks) {
            if (nodeColor(sink) == White) {
              nodeColor(sink) = Grey
              time += 1
              _discoveryTime(sink) = time
              _predecessor(sink) = node
              stack.push(sink)
            } else
              backEdges.add(edge)
          }
        }
        nodeColor(node) = Black
        time += 1
        _finishingTime(node) = time
        levelizedNodes prepend node
      }
    }
  }

  /** Visit and mark a single node.
    *
    * @param node the node to visit.
    */
  private def visit(node: Hypernode) {
    nodeColor(node) = Grey
    time += 1
    _discoveryTime(node) = time

    // Explore all sinks of this node.  This is equivalent to
    // exploring every edge sourced by this node.
    for (edge <- node.outputs) {
      for (sink <- edge.sinks) {
        if (nodeColor(sink) == White) {
          _predecessor(sink) = node
          visit(sink)
        } else if (nodeColor(sink) == Grey)
          backEdges.add(edge)
      }
    }

    // We've finished.
    nodeColor(node) = Black
    time += 1
    _finishingTime(node) = time
    levelizedNodes prepend node
  }


  private def reverseVisitNonrecursive(node: Hypernode) {
    if (nodeColor(node) == White) {
      nodeColor(node) = Grey
      time += 1
      _discoveryTime(node) = time
      val stack = new scala.collection.mutable.Stack[Hypernode]
      stack.push(node)
      while (!stack.isEmpty) {
        val node = stack.pop
        // Explore all *sources* of this node.  This is equivalent to
        // exploring every edge sunk by this node.
        for (edge <- node.inputs) {
          val source = edge.source
          if (nodeColor(source) == White) {
            nodeColor(node) = Grey
            time += 1
            _discoveryTime(node) = time
            _predecessor(source) = node
            stack.push(source)
          }
        }
        nodeColor(node) = Black
        time += 1
        _finishingTime(node) = time
      }
    }
  }

  /** Visit and mark a single node *in reverse order*.  This means that
    * instead of chasing outgoing edges, we chase incoming edges.
    *
    * @param node the node to visit.
    */
  private def reverseVisit(node: Hypernode) {
    nodeColor(node) = Grey
    time += 1
    _discoveryTime(node) = time
    // Explore all *sources* of this node.  This is equivalent to
    // exploring every edge sunk by this node.
    for (edge <- node.inputs) {
      val source = edge.source
      if (nodeColor(source) == White) {
        _predecessor(source) = node
        reverseVisit(source)
      }
    }

    // We've finished.
    nodeColor(node) = Black
    time += 1
    _finishingTime(node) = time
  }

  /** Return the set of back edges, if any, found in the search.
    *
    * @return set of edges.
    */
  def getBackEdges: IdentityHashSet[Hyperedge] =
    backEdges
}


private [cogx] object TestHyperDepthFirstSearch extends App {
  /**
* This class allows us to compare the finishing times of two nodes.
*/
  class NodeFinishingTimeComparator(search: DepthFirstSearch)
          extends Comparator[Hypernode]
  {
    /**
     * Since we want to order in decreasing finishing time we:
     *   1. return -1 if node1 has a later finishing time than node2.
     *   2. return 0 if both nodes have the same finishing time.
     *   3. return 1 if node1 has an earlier finishing time than node2.
     */
    def compare(node1: Hypernode, node2: Hypernode): Int = {
      val finish1 = search.finishingTime(node1)
      val finish2 = search.finishingTime(node2)
      if (finish1 > finish2)
        return -1
      else if (finish1 == finish2)
        return 0
      else
        return 1
    }
  }


  val graph = createTestHypergraph
  val search = new DepthFirstSearch(graph, true)
  /*
  for (node <- graph) {
    printf("discoveryTime = %d, finishingTime = %d\n",
      search.discoveryTime(node), search.finishingTime(node))
  }
  */
  val orderedSet = new TreeSet(new NodeFinishingTimeComparator(search))
  for (node <- graph)
    orderedSet.add(node)
  val orderedSeq = new ArrayBuffer[Hypernode] {
    val iter = orderedSet.iterator
    while (iter.hasNext)
      this += iter.next
  }

  val rsearch = new DepthFirstSearch(graph, false, orderedSeq)
  for (node <- orderedSeq) {
    printf("discoveryTime = %d, finishingTime = %d\n",
      rsearch.discoveryTime(node), rsearch.finishingTime(node))
  }

  /**
   * Creates a hypergraph with known connectivity and
   * results (hypergraph comes from the text mentioned at the beginning of the
   * class, section 22.5 (Strongly connected components) figure 22.9 (p. 553 in
   * the edition I have).
   */
  private def createTestHypergraph: Hypergraph = {
    val graph = new Hypergraph
    val a = new Hypernode(graph)
    val b = new Hypernode(graph)
    val c = new Hypernode(graph)
    val d = new Hypernode(graph)
    val e = new Hypernode(graph)
    val f = new Hypernode(graph)
    val g = new Hypernode(graph)
    val h = new Hypernode(graph)
    new Hyperedge(a, Array(b))

    new Hyperedge(e, Array(a))
    new Hyperedge(b, Array(e))
    new Hyperedge(b, Array(f))
    new Hyperedge(b, Array(c))
    new Hyperedge(e, Array(f))
    new Hyperedge(f, Array(g))
    new Hyperedge(g, Array(f))
    new Hyperedge(c, Array(d))
    new Hyperedge(d, Array(c))
   new Hyperedge(g, Array(h))
    new Hyperedge(d, Array(h))

    graph
  }
}
