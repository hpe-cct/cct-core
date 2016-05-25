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

package cogx.cogmath.circuit

import scala.collection.mutable.ArrayBuffer
import cogx.cogmath.collection.{IdentityHashMap, IdentityHashSetDeterministic, IdentityHashSet}

/** A circuit is a directed, acyclic graph of Nodes, where Nodes have zero or
  * more ordered inputs and zero or more ordered outputs.
  *
  * Note that this is mutable, so be careful.
  *
  * @author Greg Snider
  */

private [cogx] class Circuit[T <: Node[T]] {
  Circuit.setCurrent(this)
  /** Inputs to the circuit. */
  protected val inputs = new ArrayBuffer[T]
  /** Map of oldNode --> newNode, where oldNode is a node that was removed
    * by the Node.stealOutputFrom operator and given to newNode.
    */
  private val stolenOutputMap = new IdentityHashMap[T, T]

  /** Called by Node whenever a new Node is created that steals an output
    * from an existing node. This keeps track of all the stealing.
    */
  private[circuit] def outputStolen(from: T, newOwner: T) {
    stolenOutputMap(from) = newOwner
  }

  /** Find the node which has stolen the output of an another node.
    *
    * If the output has been stolen multiple times, it tracks that and returns
    * the final owner. If the output has never been stolen, it simply returns
    * the node.
    *
    * @param originalOwner The node that originally drove the output.
    * @return Node that currently drives the output originally driven by
    *        originalOwner.
    *
    */
  def findStolenOutput(originalOwner: T): T = {
    var newOwner = originalOwner
    while (stolenOutputMap contains newOwner)
      newOwner = stolenOutputMap(newOwner)
    newOwner
  }

  /** Add a primary input to the circuit. */
  private def addInput(node: T) {
    inputs += node
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

  /** Get the roots (outputs) of the circuit. */
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
        if (cursor.outputs.length == 0)
          roots += cursor
        else
          for (output <- cursor.outputs)
            stack.push(output)
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

  /** Print out a circuit for debugging. */
  def print() {
    val visited = new IdentityHashSetDeterministic[T]
    for (root <- roots) {
      root.print(0, visited)
    }
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
  * private var _current: Circuit[_] = null
  *
  * We now use a thread-local version to enable simultaneous Cog compilation from multiple threads.
  */
private [cogx] object Circuit {
  /** The "current" circuit being built, each thread getting its own instance starting with null. */
  private val _current =  new ThreadLocal[Circuit[_]]
  private def current = _current.get()
  private def current_=(newCircuit: Circuit[_]) { _current.set(newCircuit) }

  /** Add `node` as an element of the current circuit. If no circuit exists,
    * one is built.
    */
  def add[T <: Node[T]](node: T): Circuit[T] = {
    if (current == null)
      current = new Circuit[T]
    if (node.inputs.length == 0)
      current.asInstanceOf[Circuit[T]].addInput(node)
    current.asInstanceOf[Circuit[T]]
  }

  /** Set the current circuit. */
  private def setCurrent(tree: Circuit[_]) {
    current = tree
  }
}