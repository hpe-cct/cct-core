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

/** A Hypercluster is a hypernode that contains hypernodes from a different
  * hypergraph. This is used to create hierarchies of hypergraphs.
  *
  * @author Greg Snider
  */
private [hypergraph] class Hypercluster private(owner: Hypergraph)
        extends Hypernode(owner)
        with Iterable[Hypernode]
{
  /** Nodes in this cluster. */
  private val contents = new ArrayBuffer[Hypernode]

  /** Add `node` to the cluster. Note that the weight of the cluster
    * equals the sum of the weights of the nodes in the cluster.
    *
    * @param node Hypernode to be added to this cluster.
    */
  private[Hypercluster] def +=[T <: Hypernode](node: T) {
    require(node.owner == owner)
    contents += node
    weight += node.weight
    node.inCluster = this
  }

  /** An iterator over all nodes in the cluster. */
  def iterator =
    contents.iterator

  /** Number of nodes in the hypercluster. */
  override def size: Int =
    contents.size

  override def toString = "Hypercluster_" + hashCode.toString
}


/** Factory for creating Hyperclusters.
  */
private [cogx] object Hypercluster {

  /** Merge two hypernodes into a hypercluster.
    *
    * This will remove the two nodes from the hypergraph, replacing it with
    * the new cluster which will be connected in to the hypergraph like any
    * other node. The weight of the cluster will equal the sum of the
    * weights of the two nodes.
    *
    * @param node1 First node to merge.
    * @param node2 Second node to merge.
    * @return The hypercluster with the two nodes.
    */
  def apply(node1: Hypernode, node2: Hypernode): Hypercluster = {
    // Create the cluster, absorbing the nodes and removing them from the
    // owning hypergraph.
    val cluster = new Hypercluster(node1.owner) {
      this += node1
      this += node2
      node1.owner -= node1
      node2.owner -= node2

      // The inputs and outputs of the two absorbed nodes become the inputs
      // and outputs of the cluster, except for nodes that are swallowed by
      // the merging. The rules:
      //
      // 1. An input to either node becomes an input to the cluster, except when
      // that input comes from one of the two nodes.
      //
      val inputSignals = new IdentityHashSet[Hyperedge]
      node1.inputs.foreach {
        signal =>
          if (signal.source != node2)
            inputSignals += signal
      }
      node2.inputs.foreach {
        signal =>
          if (signal.source != node1)
            inputSignals += signal
      }
      inputSignals.foreach(_.removeSink(node1).removeSink(node2).addSink(this))

      // 2. An output from either node becomes an output from the cluster,
      // except when the only sinks are the two nodes.

      val outputSignals = new IdentityHashSet[Hyperedge]
      node1.outputs.foreach {
        signal =>
          if (signal.removeSink(node1).removeSink(node2).sinks.length > 0)
            outputSignals += signal
      }
      node2.outputs.foreach {
        signal =>
          if (signal.removeSink(node1).removeSink(node2).sinks.length > 0)
            outputSignals += signal
      }
      outputSignals.foreach(_.replaceSource(this))

      require(node1.inputs.size == 0)
      if (node1.outputs.size > 0)
        for (output <- node1.outputs) {
          if (output != null) {
            output.removeSource()
            require(output.sinks.length == 0)
          }
        }
      require(node2.inputs.size == 0)
      if (node2.outputs.size > 0)
        for (output <- node2.outputs) {
          if (output != null) {
            output.removeSource()
            require(output.sinks.length == 0)
          }
        }
    }

    for (net <- cluster.inputs) {
      require(net.sinks.length > 0)
      require(net.source != null)
    }
    for (net <- cluster.outputs) {
      require(net.sinks.length > 0)
      require(net.source != null)
    }

    cluster
  }
}