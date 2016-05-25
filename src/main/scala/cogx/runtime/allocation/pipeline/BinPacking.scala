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

package cogx.runtime.allocation.pipeline

import cogx.cogmath.hypergraph.{Hypergraph, Hyperedge, Hypercluster, Hypernode}
import scala.collection.mutable.ArrayBuffer
import cogx.cogmath.collection.IdentityHashSet

/** Packs hypernodes into bins using a variation of the Best Fit Decreasing
  * heuristic.
  *
  * @author Greg Snider
  */
private[cogx]
object BinPacking {

  /** Groups nodes into a minimal number of clusters (or bins) such that no
    * cluster exceeds a maximum weight or bandwidth. This actually changes
    * the hypergraph containing the nodes, replacing the nodes with
    * hyperclusters.
    *
    * @param nodes The nodes to be clustered into a minimal number of bins.
    * @param maxWeight The maximum weight allowed in a bin; this equals the
    *        sum of the weights of the nodes in that bin.
    * @param maxBandwidth Maximum bandwidth of the bin.
    */
  def apply(nodes: List[Hypernode],
            maxWeight: Double,
            maxBandwidth: Double): Seq[Hypernode] =
  {
    val bins = new ArrayBuffer[Hypernode]
    val sortedNodes = nodes.sortWith(_.weight > _.weight)
    for (node <- sortedNodes) {
      //println("node weight = " + node.weight)
      if (!placeInFirstBin(node)) {
        // Create a new bin
        bins += node
      }
    }

     /* Place `node` in first bin that it fits in.
      *
      * @param node The node to be placed in a bin
      * @return True is successful, false if it doesn't fit in any bin.
      */
    def placeInFirstBin(node: Hypernode): Boolean = {
      for (bin <- bins) {
        if (fits(node, bin, maxWeight, maxBandwidth)) {
          Hypercluster(node, bin)
          return true
        }
      }
      false
    }

    bins
  }

  /** Check if `node` will fit in `bin`.
    *
    * @param node Candidate node to be merged into bin.
    * @param bin The hypercluster / bin that node would be merged with.
    * @param maxWeight Maximum weight allowed for any bin.
    * @param maxBandwidth Maximum bandwidth allowed for any bin.
    */
  private def fits(node: Hypernode,
                   bin: Hypernode,
                   maxWeight: Double,
                   maxBandwidth: Double): Boolean =
  {
    // First check the weight.
    if (node.weight + bin.weight > maxWeight)
      false
    else {
      // Weight OK, check the bandwidth.
      val nets = new IdentityHashSet[Hyperedge]
      node.inputs.foreach {nets += _}
      node.outputs.foreach {nets += _}
      bin.inputs.foreach {nets += _}
      bin.outputs.foreach {nets += _}
      val bandwidth = nets.toSeq.map(_.weight).reduceLeft(_ + _)
      bandwidth <= maxBandwidth
    }
  }
}

/** Test code for bin packing. */
private[cogx]
object TestBinPacking extends App {
  // Create a test hypergraph.
  val graph = new Hypergraph
  // Input layer.
  val node0 = new Hypernode(graph) {weight = 1.0}
  // Layer to be packed into bins.
  val node1 = new Hypernode(graph) {weight = 1.0}
  val node2 = new Hypernode(graph) {weight = 2.0}
  val node3 = new Hypernode(graph) {weight = 3.0}
  val node4 = new Hypernode(graph) {weight = 3.0}
  // Output layer.
  val node5 = new Hypernode(graph) {weight = 3.5}
  val node6 = new Hypernode(graph) {weight = 3.5}
  // Connections
  new Hyperedge(node0, Array(node1, node2)) {weight = 1.0}
  new Hyperedge(node0, Array(node3, node4)) {weight = 2.0}
  new Hyperedge(node1, Array(node5)) {weight = 1.0}
  new Hyperedge(node2, Array(node6)) {weight = 1.0}
  new Hyperedge(node3, Array(node5, node6)) {weight = 1.0}

  // nodes to be packed.
  val nodesToPack = List(node1, node2, node3, node4)

  val MaxWeight = 5.0
  val MaxBandwidth = 5.0

  val bins: Seq[Hypernode] = BinPacking(nodesToPack, MaxWeight, MaxBandwidth)
  println("bins: " + bins.length)
}