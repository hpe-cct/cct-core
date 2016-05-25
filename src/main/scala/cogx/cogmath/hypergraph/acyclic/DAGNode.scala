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

package cogx.cogmath.hypergraph.acyclic

import cogx.cogmath.collection.IdentityHashSet
import cogx.cogmath.hypergraph.{NodeColor, White}


/** A node in a DAG (directed, acyclic graph); Each node has 0 or more
  * children, and is the root of a subDAG.
  *
  * @author Greg Snider
  */

private [cogx] trait DAGNode[T <: DAGNode[T]] {
  // Attributes for common graph algorithms
  var color: NodeColor = White
  var level = -1

  /** Seq of children of this node. */
  def children: Array[T]

  /** Count the number of children for this node. */
  def childCount: Int = {
    var count = 0
    for (child <- children)
      count += 1
    count
  }

  /** Count the number of nodes in the DAG rooted at "this". */
  def subDAGSize: Int = {
    var count = 0
    def f(t: T) {count += 1}
    traversePreorder(f)
    count
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[hypergraph] def traversePreorder(f: T => Unit, _visited: IdentityHashSet[T] = null) {
    val visited = if (_visited == null) new IdentityHashSet[T] else _visited
    if (!visited.contains(this.asInstanceOf[T])) {
      visited += this.asInstanceOf[T]
      f(this.asInstanceOf[T])
      for (child <- children) {
        child.traversePreorder(f, visited)
      }
    }
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[hypergraph] def traversePostorder(f: T => Unit, _visited: IdentityHashSet[T] = null) {
    val visited = if (_visited == null) new IdentityHashSet[T] else _visited
    if (!visited.contains(this.asInstanceOf[T])) {
      visited += this.asInstanceOf[T]
      for (child <- children) {
        child.traversePostorder(f, visited)
      }
      f(this.asInstanceOf[T])
    }
  }

  /** Print out the DAG rooted at "this" using an in-order traversal. */
  def print() {
    val visited = new IdentityHashSet[DAGNode[T]]
    this.print(0, visited)
  }

  private[hypergraph] def print(visited: IdentityHashSet[DAGNode[T]]) {
    this.print(0, visited)
  }

  /** Recursive printing of the DAG using "toString" for each node. */
  private def print(level: Int, visited: IdentityHashSet[DAGNode[T]]) {
    if (!visited.contains(this)) {
      visited += this
      // Indent 2 spaces for each level
      for (i <- 0 until level * 2)
        printf(" ")
      println(this.toString)
      for (child <- children)
        child.print(level + 1, visited)
    }
  }
}
