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
import cogx.cogmath.collection.IdentityHashSet

/** Represents a hypernode in a directed hypergraph.
  *
  * Note that this uses some mix-in / generic magic that seems like it can't
  * possibly work, yet appears to be a Scala idiom. See TestHypernode for an
  * example of proper inheritance of this trait.
  *
  * @param owner The hypergraph in which this hypernode exists.
  *
  * @author Greg Snider
  */
private [cogx] class Hypernode(val owner: Hypergraph)
{
  /** Edges which are inputs to this hypernode. */
  private val inputEdges = new ArrayBuffer[Hyperedge]
  /** Edges which are outputs of (driven by) this hypernode. */
  private val outputEdges = new ArrayBuffer[Hyperedge]
  /** Every hypernode has a "weight" which is application defined. */
  var weight = 1.0
  /** Cluster that this hypernode belongs to, application defined. */
  var inCluster: Hypercluster = null
  /** Attraction of hypernode to something else, application defined. */
  var attraction: Double = 0.0
  /** Level of node, application defined. */
  var level = 0

  // Initialize
  owner += this

  /** Add an input `edge` to this. */
  protected[hypergraph] def addInput(edge: Hyperedge) {
    inputEdges += edge
  }

  /** Add an output `edge` to this. */
  protected[hypergraph] def addOutput(edge: Hyperedge) {
    outputEdges += edge
  }

  /** Remove an input `edge` from this */
  protected[hypergraph] def removeInput(edge: Hyperedge) {
    inputEdges -= edge
  }

  /** Remove an output `edge` from this. */
  protected[hypergraph] def removeOutput(edge: Hyperedge) {
    outputEdges -= edge
  }

  /** Get the input hyperedges driving this hypernode. */
  def inputs: Seq[Hyperedge] = inputEdges.toSeq

  /** Get the output hyperedges driven by this hypernode. */
  def outputs: Seq[Hyperedge] = outputEdges.toSeq
}
