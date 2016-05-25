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

/** A hyperedge in a hypergraph of hypernodes.
  *
  * Note that the Hyper{circuit,node,edge} classes are templated on the
  * actual type of the node.  This allows the methods of the classes to
  * return the actual node type where applicable.  One could also have
  * templated the classes on the actual edge type, but it was thought that
  * the added complexity was not worth the benefit.  As a result, methods
  * returning an edge may need to have their result cast to the actual type
  * as in: returnedEdge.asInstanceOf[ActualEdgeType].
  *
  * @param _source The hypernode driving this hyperedge.
  *
  * @author Dick Carter and Greg Snider
  */
private [cogx] class Hyperedge[T <: Hypernode[T]](_source: T)
{
  /** The node that sources this hyperedge. */
  private var sourceNode = _source
  /** The nodes driven by this hyperedge. */
  private val sinkNodes = new ArrayBuffer[T]
  /** Every hyperedge has a weight which is application defined. */
  var weight = 1.0

  // Initialize
  sourceNode.addOutput(this)

  /** Get the source of this hyperedge. */
  def source: T = sourceNode

  /** Get the output index of this hyperedge on the source. */
  def sourceOutputIndex: Option[Int] = sourceNode.outputIndex(this)

  /** Get the sinks of this hyperedge. */
  def sinks: Seq[T] = sinkNodes

  /** Add `sink` node to this hyperedge. */
  private[hypercircuit] def addSink(sink: T) {
    if (!containsObject(sinkNodes,sink))
      sinkNodes += sink
  }

  /** Remove `sink` node from this hyperedge. Uses object identity.  */
  private[hypercircuit] def removeSink(node: T) {
    for (i <- sinkNodes.length - 1 to 0 by -1)
      if (sinkNodes(i) eq node)
        sinkNodes.remove(i)
  }

  /** Is node in buffer (using 'eq' instead of 'equals')? */
  private[hypercircuit] def containsObject(buffer: Seq[T], node: T): Boolean = {
    for (i <- 0 until buffer.length)
      if (buffer(i) eq node)
        return true
    return false
  }

  /** Steal sinks from another edge, `from`, and add them to `this.` One can
    * avoid stealing sinks that are a single `exceptSink` node.  This is useful in
    * merging a source node into a sink node, where the source -> sink connection
    * is probed or has additional sinks outside the merged kernel.  In that case,
    * the output of the merged kernel wants to steal the sinks of the source -> sink
    * connection edge, but does not want to include the sink.
    *
    * This makes the `from` useless since it is no longer used by any other node.
    *
    * @param from The edge from which to steal outputs;
    * @param exceptSink The sink to not transfer from `from` to `this`;
    */
  def stealSinksFrom(from: Hyperedge[T], exceptSink: T = null.asInstanceOf[T]) {

    // Is sink the node that we don't want to steal?
    def isException(sink: T) = (exceptSink != null) && (exceptSink eq sink)

    // Transfer sinks from `from` to `this`.
    val fromSinks = from.sinks
    while (fromSinks.length > 0) {
      // This is subtle because because nodes can be multiply-connected, e.g.
      // a single node can appear on two inputs of another node. Compounding
      // this is that we must distinguish between "eq" and "equals" operators
      // when putting nodes into collections. ArrayBuffers use "equals" but we
      // really want "eq" when adding or subtracting, so we must use our own
      // methods for those operations.
      val sink = fromSinks.head

      from.removeSink(sink)

      if (!isException(sink)) {
        if (!containsObject(this.sinks, sink))
          this.addSink(sink)

        // Find `from` on the sink and replace it with `this`, its new source.
        // Since nodes can be multiply-connected, we must replace all instances
        // of `from` on the sink.
        for (i <- 0 until sink.inputs.length)
          if (sink.inputs(i) eq from)
            sink.replaceInput(i, this)
      }
    }
    source.circuit.outputStolen(from, this)
  }

  /** A string representation of the edge, based on the node that drives it */
  override def toString = sourceOutputIndex match {
    case Some(outIndex) =>
      if (sourceNode.numOutputs > 1)
        s"$sourceNode[$outIndex]"
      else
        s"$sourceNode"
    case None =>
      s"$sourceNode[?]"
  }
}
