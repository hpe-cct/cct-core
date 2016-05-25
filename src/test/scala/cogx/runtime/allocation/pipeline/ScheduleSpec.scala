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

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

import cogx.api.ImplicitConversions
import cogx.cogmath.hypergraph.{Hypernode, Hypergraph, Hyperedge}

/** Test code.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class ScheduleSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
{
  test("all") {
 /*
 * Here's the DAG we will test. Node names are in the boxes.
 *
 *
 *                        +---+
 *                        | 5 |                    Level 4
 *                        +---+
 *                        ^   ^
 *                        |   |
 *              +---------+   +------+
 *              |                    |
 *            +---+                +---+
 *            | 4 |                | 2 |           Level 3
 *            +---+                +---+
 *            ^   ^                ^   ^
 *            |   |                |   |
 *      +-----+   +----------+-----+   |
 *      |                    |         |
 *    +---+                +---+       |
 *    | 3 |                | 1 |       |           Level 2
 *    +---+                +---+       |
 *      ^                    ^         |
 *      |                    |         |
 *      +-------+------------+         |
 *              |                      |
 *            +---+                  +---+
 *            | 0 |                  | 6 |         Level 1
 *            +---+                  +---+
 *
 *
 */
    val graph = new Hypergraph {
      val node0 = new Hypernode(this) {weight = 1}
      val node1 = new Hypernode(this) {weight = 1}
      val node2 = new Hypernode(this) {weight = 1}
      val node3 = new Hypernode(this) {weight = 1}
      val node4 = new Hypernode(this) {weight = 1}
      val node5 = new Hypernode(this) {weight = 1}
      val node6 = new Hypernode(this) {weight = 1}

      val edge03 = new Hyperedge(node0, Array(node3, node1)) {weight = 1}
      val edge34 = new Hyperedge(node3, Array(node4)) {weight = 1}
      val edge14 = new Hyperedge(node1, Array(node4, node2)) {weight = 1}
      val edge45 = new Hyperedge(node4, Array(node5)) {weight = 1}
      val edge25 = new Hyperedge(node2, Array(node5)) {weight = 1}
      val edge62 = new Hyperedge(node6, Array(node2)) {weight = 1}
    }

    val schedule = new Schedule(graph)

    // test levels
    require(schedule.nodesAtLevel(1) contains graph.node0)
    require(schedule.nodesAtLevel(1) contains graph.node6)
    require(schedule.nodesAtLevel(2) contains graph.node1)
    require(schedule.nodesAtLevel(2) contains graph.node3)
    require(schedule.nodesAtLevel(3) contains graph.node4)
    require(schedule.nodesAtLevel(3) contains graph.node2)
    require(schedule.nodesAtLevel(4) contains graph.node5)

    // test max weight
    require(schedule.merge(graph.node4, maxWeight = 2.0, maxBandwidth = 1.0) == null)

    // test max bandwidth
    require(schedule.merge(graph.node4, maxWeight = 3.0, maxBandwidth = 2.0) == null)

    // barely meets weight and bandwidth constraints
    var cluster1 =
      schedule.merge(graph.node4, maxWeight = 3.0, maxBandwidth = 3.0)
    require(cluster1 != null)
    require(cluster1.weight == 3.0)
    require(cluster1.level == 2)

    require(schedule.nodesAtLevel(1) contains graph.node0)
    require(schedule.nodesAtLevel(1) contains graph.node6)
    require(!schedule.nodesAtLevel(2).contains(graph.node1))
    require(!schedule.nodesAtLevel(2).contains(graph.node3))
    require(!schedule.nodesAtLevel(3).contains(graph.node4))
    require(schedule.nodesAtLevel(2).contains(cluster1))
    require(schedule.nodesAtLevel(3) contains graph.node2)
    require(schedule.nodesAtLevel(4) contains graph.node5)

    val cluster2 =
      schedule.merge(graph.node2, maxWeight = 4.0, maxBandwidth = 4.0)
    require(cluster2 != null)
    require(cluster2.weight == 4.0)
    require(cluster2.level == 2)

    require(schedule.nodesAtLevel(1) contains graph.node0)
    require(schedule.nodesAtLevel(1) contains graph.node6)
    require(!schedule.nodesAtLevel(2).contains(graph.node1))
    require(!schedule.nodesAtLevel(2).contains(graph.node3))
    require(!schedule.nodesAtLevel(3).contains(graph.node4))
    require(!schedule.nodesAtLevel(2).contains(cluster1))
    require(schedule.nodesAtLevel(2).contains(cluster2))
    require(!schedule.nodesAtLevel(3).contains(graph.node2))
    require(schedule.nodesAtLevel(4) contains graph.node5)

    schedule.compress(3)

    require(schedule.nodesAtLevel(1) contains graph.node0)
    require(schedule.nodesAtLevel(1) contains graph.node6)
    require(!schedule.nodesAtLevel(2).contains(graph.node1))
    require(!schedule.nodesAtLevel(2).contains(graph.node3))
    require(!schedule.nodesAtLevel(3).contains(graph.node4))
    require(!schedule.nodesAtLevel(2).contains(cluster1))
    require(schedule.nodesAtLevel(2).contains(cluster2))
    require(!schedule.nodesAtLevel(3).contains(graph.node2))
    require(schedule.nodesAtLevel(3) contains graph.node5)
  }
}