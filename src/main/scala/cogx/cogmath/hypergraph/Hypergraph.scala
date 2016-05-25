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
import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}

/** A Hypergraph is similar to a directed graph in that it is composed of
  * hypernodes interconnected with hyperedges. There are two main differences:
  *
  * 1. A Hyperedge has a single source but one or more sinks. This is very much
  * like a signal in an electronic circuit.
  *
  * 2. A Hypernode can be driven by multiple input hyperedges and may drive
  * multiple hyperedges, also very much like a component in an electronic
  * circuit. The input and output hyperedges are ordered, in the same order
  * that they are connected to the hypernode.
  *
  * @author Greg Snider
  */
private [cogx] class Hypergraph extends Iterable[Hypernode] {
  private val hypernodes = new IdentityHashSet[Hypernode]

  /** Add `node` to the hypergraph. */
  private[hypergraph] def +=(node: Hypernode) {
    hypernodes += node
  }

  /** Remove `node` from the hypergraph. */
  private[hypergraph] def -=(node: Hypernode) {
    hypernodes -= node
  }

  /** Get all nodes in the hypergraph. */
  def nodes: Seq[Hypernode] = hypernodes.toSeq

  /** Get nodes which are inputs to the hypergraph. */
  def inputs: Seq[Hypernode] = hypernodes.toSeq.filter(_.inputs.length == 0)

  /** Get nodes which are outputs from the hypergraph. */
  def outputs: Seq[Hypernode] = hypernodes.toSeq.filter(_.outputs.length == 0)

  /** Get an iterator over all nodes in the hypergraph. */
  def iterator: Iterator[Hypernode] = hypernodes.iterator

  /** Get all edges in the hypergraph. */
  def edges: Seq[Hyperedge] = {
    val edges = new IdentityHashSet[Hyperedge]
    for (node <- this) {
      for (input <- node.inputs)
        edges += input
      for (output <- node.outputs)
        edges += output
    }
    edges.toSeq
  }

  /** Check if `node` is owned by this hypergraph. */
  def contains(node: Hypernode): Boolean =
    hypernodes contains node

  /** Number of hypernodes in the hypergraph. */
  override def size: Int = hypernodes.size
}
