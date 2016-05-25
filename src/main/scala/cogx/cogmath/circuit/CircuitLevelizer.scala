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

import scala.collection.mutable.ArrayBuffer
import cogx.cogmath.collection.IdentityHashMap

/** Function which takes a circuit and levelizes it, with inputs at level 0.
  *
  * @author Greg Snider
  */

private [cogx] object CircuitLevelizer {

  /** Levelize `circuit`, returning a list of nodes at each level. */
  def apply[T <: Node[T]](circuit: Circuit[T]): Array[List[T]] = {
    // Get a list of all nodes in the circuit.
    val nodes = new ArrayBuffer[T]
    circuit.traversePostorder(nodes += _)

    // Find nodes which are inputs to the circuit (have no inputs themselves).
    val sources = new ArrayBuffer[T]
    nodes.foreach(k => {
      if (k.inputs.length == 0)
        sources += k
    })

    // Find sinks for each node
    val sinks = new IdentityHashMap[T, List[T]]()
    nodes.foreach(
      node => {sinks(node) = List[T]()}
    )
    for (node <- nodes) {
      for (input <- node.inputs)
        sinks(input) ::= node
    }

    // Propagate levels up from bottom, the source nodes
    val nodeToLevel = new IdentityHashMap[Node[T], Int]()
    nodes.foreach(nodeToLevel(_) = -1)

    def propagateLevel(node: T, newLevel: Int) {
      var currentLevel = nodeToLevel(node)
      if (newLevel > currentLevel) {
        nodeToLevel(node) = newLevel
        currentLevel = newLevel
        for (sink <- sinks(node))
          propagateLevel(sink, currentLevel + 1)
      }
    }

    for (source <- sources) {
      propagateLevel(source, 0)
    }

    // Find the highest level in the levelized circuit.
    val highestLevel = circuit.roots.map(nodeToLevel(_)).foldLeft(0)(_ max _)

    // Create the levelized data: array of lists, one list per level
    val levelized = Array.tabulate(highestLevel + 1) {
      (level) => List[T]()
    }
    nodes.foreach(k => levelized(nodeToLevel(k)) ::= k)
    levelized
  }
}