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

import java.util.Comparator
import java.util.TreeSet
import scala.collection.mutable.ArrayBuffer
import cogx.cogmath.collection.IdentityHashMap

/** Implements the finding of "strongly connected components" in a
  * hypergraph.
  *
  * See "Introduction to Algorithms" by
  * Cormen, Leiserson and Rivest (ISBN 0-07-013143-0 McGraw-Hill) for a
  * description of the algorithm.
  *
  * @author Greg Snider
  */

private [cogx] object StronglyConnectedComponents {

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

  /**
   * Create a searcher of strongly connected components. The constructor
   * actually finds the components, and the remaining methods can be used
   * to find out what those components are.
   */
  def apply(graph: Hypergraph): Seq[ArrayBuffer[Hypernode]] = {
    // Do a forward depth first search to label all nodes with
    // finishing times.
    val searcher = new DepthFirstSearch(graph)

    // Order the nodes in order of decreasing finishing time.
    val orderedSet = new TreeSet(new NodeFinishingTimeComparator(searcher))
    for (node <- graph)
      orderedSet.add(node)
    val orderedSeq = new ArrayBuffer[Hypernode] {
      val iter = orderedSet.iterator
      while (iter.hasNext)
        this += iter.next
    }

    // Now do a reverse search using the ordered nodes from the first search.
    val reverseSearcher = new DepthFirstSearch(graph, forward = false, orderedSeq)
    val subgraphRoots: Seq[Hypernode] = reverseSearcher.subgraphRoots

    // Create a hypercluster for each subtree and install it in the subtrees map.
    val subgraphs = new IdentityHashMap[Hypernode, ArrayBuffer[Hypernode]] {
      for (root <- subgraphRoots)
        this(root) = new ArrayBuffer[Hypernode]
    }

    val nodeToRootMap = new IdentityHashMap[Hypernode, Hypernode]
    for (root <- subgraphRoots) {
      nodeToRootMap(root) = root
      subgraphs(root) += root
    }

    // Now put each node into the proper hypercluster.
    for (node <- graph)
      sortNode(reverseSearcher, node, subgraphs, nodeToRootMap)

    subgraphs.values.toSeq
  }

  private def sortNode(search: DepthFirstSearch,
                       node: Hypernode,
                       clusters: IdentityHashMap[Hypernode, ArrayBuffer[Hypernode]],
                       nodeToRoot: IdentityHashMap[Hypernode, Hypernode]): Hypernode =
  {
    if (nodeToRoot contains node) {
      return nodeToRoot(node) // It's already been sorted.
    } else {
      val predecessor = search.predecessor(node)
      require(predecessor != null)
      val root =
        if (nodeToRoot.contains(predecessor))
          nodeToRoot(predecessor)
        else
          sortNode(search, predecessor, clusters, nodeToRoot) // recursion
      nodeToRoot(node) = root
      val cluster: ArrayBuffer[Hypernode] = clusters(root)
      cluster += node
      return root
    }
  }
}
