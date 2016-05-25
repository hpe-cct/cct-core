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

import cogx.cogmath.collection.PriorityQueue
import scala.collection.mutable.ArrayBuffer
import cogx.utilities.Time
import cogx.cogmath.hypergraph.{StronglyConnectedComponents, Hypernode, Hypergraph, Hypercluster}

/** Pipelines a computation, trying for as must horizontal parallelism (number
  * of cluster in each pipeline stage) and minimal vertical parallelism (number
  * of pipeline stages) while trying to bury as much bandwidth into clusters
  * as possible.
  *
  * @param hypergraph The hypergraph to be pipelined. The nodes in this
  *        hypergraph must be annotated with "weight" which denotes the
  *        computational load. The edges must also be annotated with "weight"
  *        which is the bandwidth (in bytes per clock tick) carried by that
  *        edge.
  *
  * @author Greg Snider
  */
private[cogx]
class Pipeliner(hypergraph: Hypergraph, constraints: ClusterConstraints) {
  /** Directed, acyclic graph of SCCs in hypergraph. */
  private val dag: Hypergraph = clusterSCCs(hypergraph)
  /** Schedule of (pipelined) hypernodes in hypergraph. */
  private val schedule = new Schedule(dag)
  /** We prefer to cluster nodes that swallow edges with low fanout; this
    * controls that bias.
    */
  private val LowFanoutBias = 0.6

  // Do the pipelining.
  pipeline(dag)

  /** Pipeline the dag. */
  private def pipeline(dag: Hypergraph) {
    val startingEdges = dag.edges.size
    val startingEdgeWeight =
      dag.edges.map(edge => edge.weight * (1 + edge.sinks.length)).reduceLeft(_ + _)
    for (stage <- 3 until schedule.levels) {
      do {
        println("stage: " + stage)
        //schedule.check()
        clusterStage(stage)
      } while (schedule.compress(stage))
    }
    val endingEdges = dag.edges.size
    val endingEdgeWeight =
      dag.edges.map(edge => edge.weight * (1 + edge.sinks.length)).reduceLeft(_ + _)

    // Now pack
    schedule.pack(constraints.maxLoad, constraints.maxBandwidth)

    println("edges")
    println("   start: " + startingEdges)
    println("     end: " + endingEdges)

    println("edge weight")
    println("   start: " + startingEdgeWeight)
    println("     end: " + endingEdgeWeight)
  }

  /** Print out the pipeline schedule. */
  def print() {
    schedule.print
  }

  /** Attempt to clusters nodes a given level in the schedule by merging
    * them with input nodes in the next lower level.
    *
    * @param level The schedule level to be clustered.
    * @return True if at least one node was clustered.
    */
  private def clusterStage(level: Int): Boolean = {
    var clustered = 0
    val priorityQueue = prioritizeLevel(level)
    while (!priorityQueue.isEmpty) {
      val node = priorityQueue.dequeue
      val cluster =
        schedule.merge(node, constraints.maxLoad, constraints.maxBandwidth)
      if (cluster != null)
        clustered += 1
    }
    clustered > 0
  }


  /** Prioritize all hypernodes at a given level in the schedule.
    *
    * @param level The scheduling level of hypernodes to be prioritized.
    * @return A priority queue of those hypernodes sorted by priority.
    */
  private def prioritizeLevel(level: Int): PriorityQueue[Hypernode] = {
    // Put all nodes at level into a priority queue. Priority for a node is
    // the attraction of that node to the nodes in the next lower levels.
    new PriorityQueue[Hypernode](_.attraction) {
      for (node <- schedule.nodesAtLevel(level)) {
        node.attraction = connectivityOf(node)
        this.enqueue(node)
      }
    }
  }

  /** Connectivity of a node in one level to the nodes in the levels below.
    *
    * First we define:
    *
    *   bandwidth = edge.weight
    *
    *   sinks = number of sinks on an edge
    *
    * Then we sum over the inputs:
    *
    *   connectivity = bandwidth / (sinks - LowFanoutBias)
    *
    */
  private def connectivityOf(node: Hypernode): Double = {
    node.inputs.map(edge => edge.weight / (edge.sinks.length - LowFanoutBias)).
            foldLeft(0.0)(_ + _)
  }

  /** Take a directed, cyclic hypergraph and convert it to a coarser,
    * DAG by collapsing each SCC (strongly connected component) to a
    * hypercluster.
    *
    * @param graph The cyclic graph to be
    */
  private def clusterSCCs(graph: Hypergraph): Hypergraph = {
    val components: Seq[ArrayBuffer[Hypernode]] =
      StronglyConnectedComponents(hypergraph)
    for (component <- components) {
      component.foldLeft(component.head)(Hypercluster(_, _))
    }
    graph
  }
}
