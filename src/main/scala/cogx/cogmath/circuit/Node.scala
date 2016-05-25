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

import cogx.cogmath.collection.{IdentityHashSetDeterministic, IdentityHashSet}
import scala.collection.mutable.ArrayBuffer

/** A Node is a circuit element that sources and sinks signals. It has zero or
  * more outputs, called Sources, and zero or more inputs, called Sinks.
  *
  * @param in Input sources driving the inputs to this node.
  *
  * @author Greg Snider
  */

private [cogx] abstract class Node[T <: Node[T]](private val in: Array[T]) {

  // Sources driven by this node. Initially these sources have no sinks.
  private val _sinks = new ArrayBuffer[T]

  /** All nodes are created alive; when removed from circuit, they are dead. */
  private var dead = false

  /** This instance, case to type T. */
  private val me = this.asInstanceOf[T]

  // Inputs of this node, connect them to their sources.
  for (i <- 0 until in.length)
    in(i)._sinks += me

  /** Circuit that this Node belongs to. If the node has no inputs, it must
    * get the owning circuit from the Circuit object (which, as has been noted
    * above, is not thread safe). If it has inputs, it gets the owning circuit
    * from the first input.
    */
  val circuit: Circuit[T] =
    if (in.length == 0)
      Circuit.add(me)
    else
      in(0).circuit
  in.foreach(source => require(source.circuit == circuit))

  /** Check if node is dead (removed from circuit). */
  def isDead: Boolean = dead

  /** Get the sources driven by this node. */
  def outputs: Seq[T] = _sinks.toSeq

  /** Get the inputs consumed by this node. */
  def inputs: Seq[T] = in.toSeq

  /** Remove `node` from the ArrayBuffer of sinks.  This method should
    * be called in preference to _sinks -= node because of its use of
    * object identity (eq) over object equality (equals)
    */
  private def removeSink(node: T) {
    // Remove from back to front to preserve indexing as we remove elements
    val lastIndex = _sinks.length - 1
    for (i <- lastIndex to 0 by -1) {
      if (_sinks(i) eq node) {
        _sinks.remove(i)
      }
    }
  }

  /** Steal all sinks from another node, `from`, and add them to `this.`
    *
    * This makes the `from` useless since whatever signal it drives is not
    * used by any other node. Consequently `from` is removed from the circuit.
    *
    * @param from The node from which to steal outputs; this node will then be
    *        removed from the circuit after the theft is complete.
    */
  def stealOutputsFrom(from: T) {
    // Transfer sinks from `from` to `this`.
    while (from._sinks.length > 0) {
      // This is subtle because because nodes can be multiply-connected, e.g.
      // a single node can appear on two inputs of another node. Compounding
      // this is that we must distinguish between "eq" and "equals" operators
      // when putting nodes into collections. ArrayBuffers use "equals" but we
      // really want "eq" when adding or subtracting, so we must use our own
      // methods for those operations.
      val sink = from._sinks.head

      /** Is node in buffer (using 'eq' instead of 'equals')? */
      def containsObject(buffer: ArrayBuffer[T], node: T): Boolean = {
        for (i <- 0 until buffer.length)
          if (buffer(i) eq node)
            return true
        return false
      }

      from.removeSink(sink)
      if (!containsObject(this._sinks, sink))
        this._sinks += sink

      // Find `from` on the sink and replace it with `this`, its new source.
      // Since nodes can be multiply-connected, we must replace all instances
      // of `from` on the sink.
      for (index <- 0 until sink.in.length)
        if (sink.in(index) eq from)
          sink.in(index) = me
    }
    require(from._sinks.length == 0)
    // Disconnect `from` from its sources
    removeFromCircuit(from)
    circuit.outputStolen(from, me)
  }

  /** Remove `node` from circuit, recursively removing inputs also that are no
    * longer used because of the removal of `node`. Note that
    */
  private def removeFromCircuit(node: T) {
    assume(node._sinks.length == 0)
    node.in.foreach(_.removeSink(node))
    node.dead = true

    // Recursion. If any input no longer has sinks, it needs to be removed also.
    // However, primary inputs (nodes with no inputs) may NOT be removed since
    // they form the foundation of the Circuit.
    node.in.foreach {
      input =>
        if ((input._sinks.length == 0) && (input.in.length > 0)) {
          removeFromCircuit(input)
        }
    }
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[circuit] def traversePreorder(f: T => Unit, visited: IdentityHashSet[T]) {
    if (!visited.contains(me)) {
      visited += me
      f(me)
      for (child <- inputs) {
        child.traversePreorder(f, visited)
      }
    }
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[circuit] def traversePostorder(f: T => Unit, visited: IdentityHashSetDeterministic[T]) {
    System.out.flush
    if (!visited.contains(me)) {
      visited += me
      for (child <- inputs) {
        child.traversePostorder(f, visited)
      }
      f(me)
    }
  }

  /** Recursive printing of the DAG using "toString" for each node. */
  private[circuit] def print(level: Int, visited: IdentityHashSetDeterministic[T]) {
    val alreadyVisited = visited.contains(me)
    visited += me
    // Put a leading "|" if this has already been printed
    if (alreadyVisited)
      printf("|")
    else
      printf(" ")
    // Indent 2 spaces for each level
    for (i <- 0 until level * 2)
      printf(" ")
    println(this.toString)
    // If this has already been visited, we don't bother to print out the
    // children--redundant and creates an overly verbose printout for highly
    // reconvergent circuits.
    if (!alreadyVisited)
      for (child <- inputs)
        child.print(level + 1, visited)
  }

  override def toString = "Node!!!"
}
