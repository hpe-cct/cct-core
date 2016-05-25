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

import cogx.cogmath.hypergraph.{Hypernode, Hypergraph, Hyperedge}

/**
  *
  *
  * @author Greg Snider
  */
private[cogx]
class LinearCircuit(val levels: Int) extends Hypergraph {

  create()

  def create() {
    val nodes = Array.tabulate(levels) {
      _ => new Hypernode(this)
    }
    for (level <- 0 until (levels - 1))
      new Hyperedge(nodes(level), Array(nodes(level + 1)))
  }
}