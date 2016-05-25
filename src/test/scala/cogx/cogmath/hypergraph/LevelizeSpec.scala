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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import cogx.cogmath.collection.{IdentityHashMap, IdentityHashSet}
import scala.language.reflectiveCalls

/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class LevelizeSpec extends FunSuite with MustMatchers {
  /*
   * Here's the graph we will test. Node names are in the boxes.
   *
   *
   *                        +---+
   *                        | 5 |
   *                        +---+
   *                        ^   ^
   *                        |   |
   *              +---------+   +------+
   *              |                    |
   *            +---+                +---+
   *            | 4 |                | 2 |
   *            +---+                +---+
   *            ^   ^                  ^
   *            |   |                  |
   *      +-----+   +---------+ +------+
   *      |                   | |
   *    +---+                +---+
   *    | 3 |                | 1 |
   *    +---+                +---+
   *      ^                    ^
   *      |                    |
   *      +------+ +-----------+
   *             | |
   *            +---+
   *            | 0 |
   *            +---+
   *
   *
   */
  def createGraph = new Hypergraph {
    val node0 = new Hypernode(this)
    val node1 = new Hypernode(this)
    val node2 = new Hypernode(this)
    val node3 = new Hypernode(this)
    val node4 = new Hypernode(this)
    val node5 = new Hypernode(this)

    val edge03 = new Hyperedge(node0, Array(node3, node1))
    val edge34 = new Hyperedge(node3, Array(node4))
    val edge14 = new Hyperedge(node1, Array(node4, node2))
    val edge45 = new Hyperedge(node4, Array(node5))
    val edge25 = new Hyperedge(node2, Array(node5))
  }

  test("algorithm") {
    val graph = createGraph
    val backEdges = new IdentityHashSet[Hyperedge]
    val nodeLevel: IdentityHashMap[Hypernode, Int] = Levelize(graph, backEdges)
    require(nodeLevel(graph.node0) == 1)
    require(nodeLevel(graph.node1) == 2)
    require(nodeLevel(graph.node2) == 3)
    require(nodeLevel(graph.node3) == 2)
    require(nodeLevel(graph.node4) == 3)
    require(nodeLevel(graph.node5) == 4)
  }
}