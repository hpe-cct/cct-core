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

package cogx.utilities

/** Base class for a directed graph that automatically wires up Edges
  * and Nodes as they are created. Subclasses are expected to assign
  * values to Edge and Node (which will correspond to subclasses for
  * EdgeBase and NodeBase respectively).
  *
  * @author Greg Snider
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private abstract class DirectedGraph {
  type Edge <: EdgeBase
  type Node <: NodeBase
  private var nodes = List[Node]()
  private var edges = List[Edge]()
  
  /** Get a list of all nodes in the graph. */
  def allNodes = nodes
  
  /** Get a list of all edges in the graph. */
  def allEdges = edges

  /**
   * Base class for an Edge that connects the "from" node to the "to" node.
   * Note that this manages the connection lists of the nodes as well.
   */
  abstract class EdgeBase(from: Node, to: Node) {
    self: Edge =>
    edges = this :: edges
    def source = from
    def sink = to
    from.outputs = this :: from.outputs
    to.inputs = this :: to.inputs
  }
  
  /**
   * Base class for a Node.
   */
  abstract class NodeBase {
    self: Node =>
    nodes = this :: nodes
    var inputs = List[Edge]()
    var outputs = List[Edge]()
  }

  /**
   * Return a topologically sorted list of nodes in the graph. The element at the
   * head of the sorted list will have no edges terminating on it.
   * WARNING: this will fail if graph is not acyclic.
   */  
  def totalOrder: List[Node] = {
    import scala.collection.mutable.Set
    var visitedNodes = Set[Node]()

    def sort(nodes: List[Node]) : List[Node] = {
      visitedNodes = Set[Node]()
      var sortedNodes = List[Node]()
      for (node <- nodes)
        if (!(visitedNodes contains node))
          sortedNodes = visit(node, sortedNodes)
      sortedNodes
    }
  
    def visit(node: Node, sortedNodes: List[Node]): List[Node] = {
      visitedNodes += node
      var sorted = sortedNodes
      for (outEdge <- node.outputs; sink = outEdge.sink)
        if (!(visitedNodes contains sink))
          sorted = visit(sink, sortedNodes)
      node :: sorted
    }
    
    sort(allNodes)
  }
}

