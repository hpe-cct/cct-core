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

import scala.collection.mutable.ArrayBuffer

/** A hyperedge in a hypergraph of hypernodes.
  *
  * @param _source The hypernode driving this hyperedge.
  * @param _sinks The hypernodes driven by this hyperedge.
  *
  * @author Greg Snider
  */
private [cogx] class Hyperedge(_source: Hypernode, _sinks: Iterable[Hypernode])
{
  /** The node that sources this hyperedge. */
  private var sourceNode = _source
  /** The nodes driven by this hyperedge. */
  private val sinkNodes = new ArrayBuffer[Hypernode]
  /** Every hyperedge has a weight which is application defined. */
  var weight = 1.0

  // Initialize
  sourceNode.addOutput(this)
  _sinks.foreach(sink => addSink(sink))

  /** Get the source of this hyperedge. */
  def source: Hypernode = sourceNode

  /** Get the sinks of this hyperedge. */
  def sinks: Seq[Hypernode] = sinkNodes

  /** Add `sink` node to this hyperedge. */
  private[hypergraph] def addSink(sink: Hypernode) {
    if (!sinkNodes.contains(sink)) {
      sinkNodes += sink
      sink.addInput(this)
    }
  }

  /** Remove `sink` node from this hyperedge. */
  private[hypergraph] def removeSink(node: Hypernode): Hyperedge = {
    if (sinkNodes contains node) {
      sinkNodes -= node
      node.removeInput(this)
    }
    this
  }

  /** Replace the source of this hyperedge with `newSource`. */
  private[hypergraph] def replaceSource(newSource: Hypernode) {
    removeSource()
    sourceNode = newSource
    sourceNode.addOutput(this)
  }

  private[hypergraph] def removeSource() {
    sourceNode.removeOutput(this)
  }
}
