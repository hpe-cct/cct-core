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

import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}
import cogx.cogmath.hypergraph._

/** Maintains the pipeline schedule for a directed, acyclic hypergraph.
  *
  * Heuristic: levelizes the graph. When a node is merged, it is merged with
  * only those input nodes that exist in the next lower level. The clustered
  * nodes are replaced with a cluster at the same level as the inputs. The
  * schedule may be compressed downward using the compress method.
  *
  * @param dag The hypergraph being scheduled.
  *
  * @author Greg Snider
  */
private[cogx]
class Schedule(dag: Hypergraph) {
  /** Debug flag for verbose output. */
  val Debug = true
  /** Hypernodes in dag, levelized (each level is a list of nodes). */
  private val levelNodes: Seq[IdentityHashSet[Hypernode]] =
    levelizeDAG(dag)
  /** Initial weight of all nodes in dag. */
  private val initialDagWeight = dag.nodes.map(_.weight).reduceLeft(_ + _)

  /** Check that all nodes in the graph are legally levelized. */
  def check() {
    var totalWeight = 0.0
    for (level <- 0 until levels) {
      for (node <- levelNodes(level)) {
        //require(node.weight <= maxWeight, " illegal node weight: " + node.weight)
        totalWeight += node.weight
        for (signal <- node.inputs)
          require(signal.source.level < level,
            " level: " + level + ", input level: " + signal.source.level)
      }
    }

    val totalDagWeight = dag.nodes.map(_.weight).reduceLeft(_ + _)
    require(totalDagWeight == totalWeight, "levels not right, " +
      "initial weight: " + initialDagWeight +
              " totalDagWeight: " + totalDagWeight +
              " levels weight: " + totalWeight)

    require(totalWeight == initialDagWeight,
      "total weight: " + totalWeight + ", initial DAG weight: " + initialDagWeight)
  }

  /** Number of levels in schedule (first level is level 1). */
  def levels = levelNodes.length

  /** Get the nodes at a given level in the schedule.
    *
    * @param level The level in the schedule
    * @return A sequence of all hypernodes at the given level.
    */
  def nodesAtLevel(level: Int): Seq[Hypernode] =
    levelNodes(level).toSeq

  /** Merge a node with its inputs, subject to constraints.
    *
    * @param node The node to be merged.
    * @param maxWeight The maximum total weight allowed in the merged cluster.
    * @param maxBandwidth The maximum bandwidth on inputs and outputs allowed
    *        in the merged cluster.
    * @return The merged cluster if successful, null if merge violated a
    *        constraint.
    */
  def merge(node: Hypernode,
            maxWeight: Double,
            maxBandwidth: Double): Hypernode =
  {
    val mergeableNodes = new IdentityHashSet[Hypernode]
    mergeableNodes += node
    for (inputEdge <- node.inputs) {
      val source = inputEdge.source
      require(source.level < node.level, "bad levelization")
      if (source.level == node.level - 1)
        mergeableNodes += source
    }
    var load = 0.0
    for (node <- mergeableNodes)
      load += node.weight
    if (load > maxWeight)
      return null

    val nets = new IdentityHashSet[Hyperedge]
    for (node <- mergeableNodes) {
      node.inputs.foreach(nets += _)
      node.outputs.foreach(nets += _)
    }
    var bandwidth = nets.toSeq.map(_.weight).foldLeft(0.0)(_ + _)
    // Subtract out bandwidth of nets that have been buried in the cluster
    for (edge <- node.inputs)
      if (edge.sinks.length == 1)
        bandwidth -= edge.weight
    if (bandwidth > maxBandwidth)
      return null


    // We are now committed to the merge
    for (node <- mergeableNodes)
      levelNodes(node.level) -= node
    val cluster = mergeableNodes.reduceLeft(Hypercluster(_, _))
    cluster.level = node.level - 1
    cluster.weight = load
    levelNodes(cluster.level) += cluster
    require(cluster.weight <= maxWeight)
    cluster
  }

  /** Compress the schedule downward, moving nodes down earlier in the
    * schedule when possible.
    *
    * @param startingLevel The starting level for doing the compression, no
    *        compression done below this level.
    * @return True if schedule was compressed, false if not.
    */
  def compress(startingLevel: Int): Boolean = {
    var compressed = false
    for (level <- startingLevel until levels) {
      val levelSet = levelNodes(level)
      for (node <- levelSet) {
        val maxInputLevel = node.inputs.map(_.source.level).foldLeft(0)(_ max _)
        if (maxInputLevel <= level - 2) {
          val oldLevel = node.level
          val newLevel = maxInputLevel + 1
          node.level = newLevel
          levelNodes(oldLevel) -= node
          levelNodes(newLevel) += node
          compressed = true
        }
      }
    }
    compressed
  }

  /** Pack nodes at each level into bins. */
  def pack(maxWeight: Double, maxBandwidth: Double) {
    for (level <- 2 until levels) {
      var nodesToPack = List[Hypernode]()
      for (node <- levelNodes(level)) {
        levelNodes(level) -= node
        nodesToPack = node :: nodesToPack
      }
      val bins = BinPacking(nodesToPack, maxWeight, maxBandwidth)
      for (bin <- bins)
        levelNodes(level) += bin
    }
  }

  /** Print a summary of the schedule for debugging. */
  def print() {
    var totalClusters = 0
    for (level <- 0 until levelNodes.length) {
      val levelSet = levelNodes(level)
      val clusters = levelSet.size
      val levelWeight = levelSet.toSeq.map(_.weight).foldLeft(0.0)(_ + _)
      val maxWeight = levelSet.toSeq.map(_.weight).foldLeft(0.0)(_ max _)
      val avgWeight = levelWeight / clusters
      totalClusters += clusters
      printf("  stage %d: %d clusters, avg wt = %f, max wt = %f\n",
        level, clusters, avgWeight, maxWeight)
    }
    println("total clusters:  " + totalClusters)
  }

  /** Levelize `dag`.
    *
    * @return A sequence of hypernode sets, one set per level in the dag; note
    *         that level 0 is always empty.
    */
  private def levelizeDAG(dag: Hypergraph): Seq[IdentityHashSet[Hypernode]] = {
    if (Debug)
      println("levelizeDAG:")
    val nodeToLevel: IdentityHashMap[Hypernode, Int] = Levelize(dag)
    val levels: Seq[IdentityHashSet[Hypernode]] = {
      val maxLevel = nodeToLevel.values.foldLeft(0)(_ max _)
      val levelized = Array.tabulate(maxLevel + 1) {
        _ => new IdentityHashSet[Hypernode]
      }
      for (node <- dag) {
        node.level = nodeToLevel(node)
        levelized(nodeToLevel(node)) += node
      }
      levelized
    }
    if (Debug) {
      println("  levels = " + levels.length)
      for (level <- 0 until levels.length)
        println("    level " + level + ": " + levels(level).size + " nodes")
    }
    checkLevelizedDAG(dag)
    levels
  }

  /** Verify that `dag` is legally levelized. */
  private def checkLevelizedDAG(dag: Hypergraph) {
    for (node <- dag.nodes) {
      val level = node.level
      for (signal <- node.inputs)
        require(signal.source.level < level)
    }
  }
}
