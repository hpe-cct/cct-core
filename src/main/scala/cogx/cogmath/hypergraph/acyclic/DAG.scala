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

import scala.collection.mutable.ArrayBuffer

/** A directed, acyclic graph, with one or more roots.
  *
  * @param _roots The roots of the DAG.
  *
  * @author Greg Snider
  */

private [cogx] class DAG[T <: DAGNode[T]](_roots: Array[T])(implicit val m: Manifest[T])
{
  /** Get the root of a single-rooted DAG. */
  def root: T = {
    require(_roots.length == 1)
    _roots(0)
  }

  /** Get all the roots for the DAG. */
  def roots: Array[T] = _roots

  /** The number of nodes in the DAG. */
  def size: Int = {
    root.subDAGSize
  }

  /** Traverse DAG preorder, executing "f" for each node not already visited. */
  def traversePreorder(f: T => Unit) {
    val visited = new IdentityHashSet[T]
    for (root <- roots)
      root.traversePreorder(f, visited)
  }

  /** Traverse DAG postorder, executing "f" for each node not already visited.*/
  def traversePostorder(f: T => Unit) {
    val visited = new IdentityHashSet[T]
    for (root <- roots)
      root.traversePostorder(f, visited)
  }

  /** Flatten the DAG to an array using a preorder traversal. */
  def flattenPreorder: Array[T] = {
    val nodes = new ArrayBuffer[T]
    val visited = new IdentityHashSet[T]
    for (root <- roots)
      root.traversePreorder(nodes += _, visited)
    nodes.toArray
  }

  /** Flatten the DAG to an array using a postorder traversal. */
  def flattenPostorder: Array[T] = {
    val nodes = new ArrayBuffer[T]
    val visited = new IdentityHashSet[T]
    for (root <- roots)
      root.traversePostorder(nodes += _, visited)
    nodes.toArray
  }

  /** Print out the DAG, preorder, with each node only printed once. */
  def print() {
    val visited = new IdentityHashSet[DAGNode[T]]
    for (root <- roots)
      root.print(visited)
  }
}
