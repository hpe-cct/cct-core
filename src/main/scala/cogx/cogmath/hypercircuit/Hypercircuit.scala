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

import scala.collection.mutable.ArrayBuffer
import cogx.cogmath.collection.{IdentityHashMap, IdentityHashSetDeterministic, IdentityHashSet}

/** A circuit is a directed, acyclic graph of Hypernodes, where Hypernodes have
  * zero or more ordered inputs and zero or more ordered outputs.
  *
  * Note that the Hyper{circuit,node,edge} classes are templated on the
  * actual type of the node.  This allows the methods of the classes to
  * return the actual node type where applicable.  One could also have
  * templated the classes on the actual edge type, but it was thought that
  * the added complexity was not worth the benefit.  As a result, methods
  * returning an edge may need to have their result cast to the actual type
  * as in: returnedEdge.asInstanceOf[ActualEdgeType].
  *
  * Note that this is mutable, so be careful.
  *
  * @author Dick Carter and Greg Snider
  */

private [cogx] class Hypercircuit[T <: Hypernode[T]] {
  Hypercircuit.setCurrent(this)
  /** Inputs to the circuit. */
  protected val inputs = new ArrayBuffer[T]
  /** Map of oldEdge --> newEdge, where oldEdge is a node output (an edge) that
    * was removed by the Node.stealOutputFrom operator and given to newEdge.
    */
  private val stolenOutputMap = new IdentityHashMap[Hyperedge[T], Hyperedge[T]]

  /** Called by Edge whenever a new Node is created that steals an output edge
    * from an existing node. This keeps track of all the stealing.
    */
  private[hypercircuit] def outputStolen(from: Hyperedge[T], newOwner: Hyperedge[T]) {
    stolenOutputMap(from) = newOwner
  }

  /** Find the edge which has stolen the sinks of another edge.
    *
    * If the output has been stolen multiple times, it tracks that and returns
    * the final owner. If the output has never been stolen, it simply returns
    * the edge.
    *
    * @param originalOwner The edge that originally drove the sink.
    * @return Edge that currently drives the sink originally driven by
    *        originalOwner.
    *
    */
  def findStolenOutput(originalOwner: Hyperedge[T]): Hyperedge[T] = {
    var newOwner = originalOwner
    while (stolenOutputMap contains newOwner)
      newOwner = stolenOutputMap(newOwner)
    newOwner
  }

  /** Add a primary input to the circuit. */
  private def addInput(node: T) {
    inputs += node
  }

  /** Remove a primary input of the circuit, using object identity */
  def removeInput(node: T) {
    for (i <- inputs.length - 1 to 0 by -1) {
      if (inputs(i) eq node)
        inputs.remove(i)
    }
  }

  /** Get the number of unique nodes in the circuit. */
  def size: Int = {
    var count = 0
    traversePostorder(node => {count += 1})
    count
  }

  /** Get the number of unique nodes in the circuit that satisfy the
    * filter function. */
  def filteredSize(f: (T) => Boolean): Int = {
    var count = 0
    traversePostorder(node => if (f(node)) {count += 1})
    count
  }

  /** Check if `this` contains `node`. */
  def contains(node: T): Boolean = {
    var found = false
    def compareWith(n: T) {
      if (n eq node)
        found = true
    }
    traversePreorder(compareWith)
    found
  }

  /** Get the roots (outputs) of the circuit. A root node is defined as one
    * that drives no other nodes on any of its outputs.*/
  def roots: Seq[T] = {
    val roots = new IdentityHashSetDeterministic[T]
    val visited = new IdentityHashSetDeterministic[T]
    for (input <- inputs) {
      findRoots(input, roots, visited)
    }
    roots.toSeq
  }

  /** Get the leaves (inputs) of the circuit. */
  def leaves: Seq[T] = {
    inputs.seq
  }

  /** Helper method to determine the roots of the circuit */
  private def findRoots(from: T,
                        roots: IdentityHashSetDeterministic[T],
                        visited: IdentityHashSetDeterministic[T])
  {
    val stack = new scala.collection.mutable.ArrayStack[T]
    stack.push(from)

    while (stack.nonEmpty) {
      val cursor = stack.pop()
      if (!visited.contains(cursor)) {
        visited += cursor
        if (!cursor.drivesOtherNodes)
          roots += cursor
        else
          for (outputEdge <- cursor.outputs)
            outputEdge.sinks.foreach(stack.push _)
      }
    }
  }


  /** Traverse DAG preorder, executing "f" for each node not already visited. */
  def traversePreorder(f: T => Unit) {
    val visited = new IdentityHashSet[T]
    for (root <- roots)
      root.traversePreorder(f, visited)
  }

  /** Traverse DAG postorder, executing "f" for each node not already visited.*/
  def traversePostorder(f: T => Unit) {
    val visited = new IdentityHashSetDeterministic[T]
    for (root <- roots)
      root.traversePostorder(f, visited)
  }

  /** Print out a circuit representation for debugging. */
  def print() {
    System.out.print(this)
  }

  /** Create a string representation of the circuit for debugging. */
  override def toString(): String = {
    val visited = new IdentityHashSetDeterministic[T]
    val s = new StringBuilder()
    for (root <- roots) {
      root.toStringWithFanIn(0, visited, 0 until root.numOutputs, s)
    }
    s.toString
  }

  /** Flatten the circuit to an array. Default is by postorder traversal. */
  def flatten: Seq[T] = {
    val buffer = new ArrayBuffer[T]
    traversePostorder(node => {buffer += node})
    buffer.toSeq
  }

  /** Flatten the circuit to an array using a preorder traversal. */
  def flattenPreorder: Seq[T] = {
    val buffer = new ArrayBuffer[T]
    traversePreorder(node => {buffer += node})
    buffer.toSeq
  }
}


/** Companion object for Circuit, maintains a context for building circuits.
  *
  * This object formerly had the field:
  *
  * private var _current: Hypercircuit[_] = null
  *
  * We now use a thread-local version to enable simultaneous Cog compilation from multiple threads.
  */
private[cogx] object Hypercircuit {
  /** The "current" circuit being built, each thread getting its own instance starting with null. */
  private val _current =  new ThreadLocal[Hypercircuit[_]]
  private def current = _current.get()
  private def current_=(newCircuit: Hypercircuit[_]) { _current.set(newCircuit) }

  /** Add `node` as an element of the current circuit. If no circuit exists,
    * one is built.
    */
  def add[T <: Hypernode[T]](node: T): Hypercircuit[T] = {
    if (current == null)
      current = new Hypercircuit[T]
    if (node.inputs.length == 0)
      current.asInstanceOf[Hypercircuit[T]].addInput(node)
    current.asInstanceOf[Hypercircuit[T]]
  }

  /** Set the current circuit. */
  private[cogx] def setCurrent(tree: Hypercircuit[_]): Unit = {
    current = tree
  }
}