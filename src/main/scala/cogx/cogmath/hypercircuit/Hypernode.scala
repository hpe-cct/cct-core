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

import cogx.cogmath.collection.{IdentityHashSetDeterministic, IdentityHashSet}
import scala.collection.mutable.ArrayBuffer

/** A Node is a circuit element that sources and sinks signals (a.k.a. edges).
  * It has zero or more outputEdges, for which it is considered the source, and
  * zero or more inputEdges, on which it is listed as a sink.
  *
  * Note that the Hyper{circuit,node,edge} classes are templated on the
  * actual type of the node.  This allows the methods of the classes to
  * return the actual node type where applicable.  One could also have
  * templated the classes on the actual edge type, but it was thought that
  * the added complexity was not worth the benefit.  As a result, methods
  * returning an edge may need to have their result cast to the actual type
  * as in: returnedEdge.asInstanceOf[ActualEdgeType].
  *
  * @param inputEdges Hyperedges connected to the inputs of this node.
  *
  * @author Dick Carter
  */

private [cogx] abstract class Hypernode[T <: Hypernode[T]](private val inputEdges: Array[Hyperedge[T]]) {

  /** Output edges driven by this node. Initially none.*/
  private val outputEdges = new ArrayBuffer[Hyperedge[T]]

  /** All nodes are created alive; when removed from circuit, they are dead. */
  private var dead = false

  // The following self-reference (as a 'val') created an infinite-loop in the field-discovery mechanism
  // of cogx.runtime.debugger.UserFieldNames, as exercised by cogio.fieldstate.VectorQueueActuatorSpec.
  // Thus, keep this a 'def'.

  /** This instance, cast to type T. */
  private def me = this.asInstanceOf[T]

  // Inputs of this node, connect them to their sources.
  for (i <- 0 until inputEdges.length)  {
    if (i > 0 && inputEdges(0).source.circuit != inputEdges(i).source.circuit) {
      val errMsg =
        s"\nAttempt to create an operation '$this' as part of the following two Hypercircuits:\n" +
        "-----------------------------------------------------------------------------------------------------\n" +
        "1st Hypercircuit = \n" +
        inputEdges(0).source.circuit.toString()  +
        "-----------------------------------------------------------------------------------------------------\n" +
        "2nd Hypercircuit = \n" +
        inputEdges(i).source.circuit.toString() +
        "-----------------------------------------------------------------------------------------------------"
      throw new MixedHypercircuitException(errMsg)
    }
    inputEdges(i).addSink(me)
  }

  /** Circuit that this Node belongs to. If the node has no inputs, it must
    * get the owning circuit from the Circuit object (which, as has been noted
    * above, is not thread safe). If it has inputs, it gets the owning circuit
    * from the first input.
    */
  val circuit: Hypercircuit[T] =
    if (inputEdges.length == 0)
      Hypercircuit.add(me)
    else {
      for (inputEdge <- inputEdges)
        if (inputEdge.source.circuit.isSealed)
          throw new RuntimeException(s"Attempted addition of node $this to a Hypercircuit after it has been 'sealed'.")
      inputEdges(0).source.circuit
    }


  /** Check if node is dead (removed from circuit). */
  def isDead: Boolean = dead

  /** Get the sources driven by this node. */
  def outputs: Seq[Hyperedge[T]] = outputEdges.toSeq

  /** Get the inputs consumed by this node. */
  def inputs: Seq[Hyperedge[T]] = inputEdges.toSeq

  /** Get the inputs consumed by this node. */
  def replaceInput(i: Int, edge: Hyperedge[T]) { inputEdges(i) = edge }

  /** Add `sink` node to this hyperedge. */
  def addOutput(edge: Hyperedge[T]) {
    outputEdges += edge
  }

  /** Get the number of input hyperedges driving this hypernode. */
  def numInputs = inputEdges.length

  /** Get the number of output hyperedges driven by this hypernode. */
  def numOutputs = outputEdges.length

  /** Get the output index of a hyperedge on the source. */
  def outputIndex(edge: Hyperedge[T]): Option[Int] = {
    for(i <- 0 until outputEdges.length)
      if (outputEdges(i) eq edge)
        return Some(i)
    return None
  }

  /** Does this node drive any other nodes? */
  private[hypercircuit] def drivesOtherNodes: Boolean = {
    outputEdges.exists(_.sinks.length > 0)
  }


  /** Steal all sinks from another node, `from`, and add them to `this.`
    *
    * This makes the `from` useless since whatever signal it drives is not
    * used by any other node. Consequently `from` is removed from the circuit.
    *
    * @param from The node from which to steal outputs; this node will then be
    *        removed from the circuit after the theft is complete.
    *
    */
  def stealOutputsFrom(from: T) {
    // Transfer sinks from `from` to `this`.
    require(from.outputEdges.length == this.outputEdges.length,
      "Mismatch of node outputs size: " + from.outputEdges.length + " and " +
                                          this.outputEdges.length)
    for (i <- 0 until outputEdges.length)
      outputEdges(i).stealSinksFrom(from.outputEdges(i))

    // Disconnect `from` from its sources
    from.removeFromCircuit(mustDo = true)
  }

  /** Remove `node` from circuit, recursively removing input nodes that are no
    * longer used because the removal of `node` removed the last sink on all
    * outputs.
    *
    * @param mustDo Throw an exception if the node is output-connected
    * @param recursive After removing `this`, recursively remove the inputs if possible
    */
  def removeFromCircuit(mustDo: Boolean, recursive: Boolean = true) {
    val node = me
    val canRemove = node.outputEdges.filter(_.sinks.length > 0).isEmpty
    require(canRemove || !mustDo,
      "Removal of output-connected node not possible.")

    node.inputEdges.foreach(_.removeSink(node))
    if (node.inputEdges.length == 0)
      node.circuit.removeInput(me)
    node.dead = true

    // Recursion. If any input no longer has sinks, it needs to be removed also.
    // However, primary inputs (nodes with no inputs) may NOT be removed since
    // they form the foundation of the Circuit.
    if (recursive)
      node.inputEdges.foreach {
        inputEdge =>
          if ((inputEdge.sinks.length == 0)) {
            inputEdge.source.removeFromCircuit(mustDo = false, recursive)
          }
      }
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[hypercircuit] def traversePreorder(f: T => Unit, visited: IdentityHashSet[T]) {
    if (!visited.contains(me)) {
      visited += me
      f(me)
      for (child <- inputs) {
        child.source.traversePreorder(f, visited)
      }
    }
  }

  /** Traverse tree, executing "f" for each node not already visited. */
  private[hypercircuit] def traversePostorder(f: T => Unit, visited: IdentityHashSetDeterministic[T]) {
    System.out.flush
    if (!visited.contains(me)) {
      visited += me
      for (child <- inputs) {
        child.source.traversePostorder(f, visited)
      }
      f(me)
    }
  }

  /** Convert and input index into a Tuple of the sourcing node and the output index of the sourcing node. */
  private[hypercircuit] def sourceInfo(inputIndex: Int) = {
    val inputEdge = inputs(inputIndex)
    val driver = inputEdge.source
    val driverOutputIndex = driver.outputIndex(inputEdge) match {
      case Some(outputIndex) => outputIndex
      case None => throw new RuntimeException("Hypercircuit inconsistency detected")
    }

    (driver, driverOutputIndex)
  }

  /** A helper function to printInputsInGroups() to determine the index of the first input not in the same group
    * as the input with startInputIndex.
    * @param startInputIndex The starting input index of the group.
    * @param untilInputIndex One past the largest possible ending index of the group.
    * @return
    */
  private[hypercircuit] def calcOutOfSequenceInputIndex(startInputIndex: Int, untilInputIndex: Int): Int = {
    val (startInputNode, startInputNodeOutputIndex) = sourceInfo(startInputIndex)
    for (inputIndex <- startInputIndex + 1 until untilInputIndex) {
      val (inputNode, inputNodeOutputIndex) = sourceInfo(inputIndex)
      if (!(inputNode eq startInputNode) ||
           (inputIndex - startInputIndex) != (inputNodeOutputIndex - startInputNodeOutputIndex))
        return inputIndex
    }
    untilInputIndex
  }

  /** Adds strings for the inputs of a node as groups, for conciseness.  So, rather than:
    *
    *   FourInputKernel_37
    *     TwoOutputKernel_93[0]
    * |   TwoOutputKernel_93[1]
    *     TwoOutputKernel_61[0]
    * |   TwoOutputKernel_61[1]
    *
    * We instead print:
    *
    *   FourInputKernel_37
    *     TwoOutputKernel_93[0,1]
    *     TwoOutputKernel_61[0,1]
    *
    * @param level A parameter to control the indenting of the Node description.
    * @param visited  Which nodes have been already printed in detail.
    * @param startInputIndex The starting input index over the range of node inputs to print.
    * @param untilInputIndex One past the ending input index over the range of node inputs to print.
    * @param s Accumulated string of the node and the tree of nodes that fanin
    */
  private[hypercircuit] def inputGroupsToString(level: Int, visited: IdentityHashSetDeterministic[T],
                                                startInputIndex: Int, untilInputIndex: Int, s: StringBuilder) {
    if (startInputIndex < untilInputIndex) {
      val (inputNode, startInputNodeOutputIndex) = sourceInfo(startInputIndex)
      val outOfSequenceInputIndex = calcOutOfSequenceInputIndex(startInputIndex, untilInputIndex)
      val (lastInputNode, lastInputNodeOutputIndex) = sourceInfo(outOfSequenceInputIndex - 1)
      inputNode.toStringWithFanIn(level + 1, visited, startInputNodeOutputIndex to lastInputNodeOutputIndex, s)
      inputGroupsToString(level, visited, outOfSequenceInputIndex, untilInputIndex, s)
    }
  }

  /** Recursively create a string representation of a Node and all its fan-in nodes.
    *
    * @param level
    * @param visited
    * @param outputRange
    * @param s Accumulated string of the node and the tree of nodes that fanin
    */
   private[hypercircuit] def toStringWithFanIn(level: Int, visited: IdentityHashSetDeterministic[T], outputRange: Range, s: StringBuilder) {
    val alreadyVisited = visited.contains(me)
    visited += me
    // Put a leading "|" if this has already been printed
    if (alreadyVisited)
      s append "|"
    else
      s append " "
    // Indent 2 spaces for each level
    for (i <- 0 until level * 2)
      s append " "

    if (this.numOutputs == 0 || this.numOutputs == 1 && outputRange == (0 until 1))
      s append this.toString + "\n"
    else
      s append this.toString + outputRange.mkString("[", ",", "]") + "\n"
    // If this has already been visited, we don't bother to print out the
    // children--redundant and creates an overly verbose printout for highly
    // reconvergent circuits.
    if (!alreadyVisited)
      inputGroupsToString(level, visited, 0, inputs.length, s)
  }

  override def toString = "Node"
}
