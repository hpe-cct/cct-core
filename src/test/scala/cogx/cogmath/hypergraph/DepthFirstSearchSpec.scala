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
import cogx.cogmath.collection.IdentityHashSet

/** Test code for the Hyperedge class.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class DepthFirstSearchSpec extends FunSuite with MustMatchers {

  test("back edges") {
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
    val graph = new Hypergraph
    val n0 = new Hypernode(graph)
    val n1 = new Hypernode(graph)
    val n2 = new Hypernode(graph)
    val n3 = new Hypernode(graph)
    val n4 = new Hypernode(graph)
    val n5 = new Hypernode(graph)

    new Hyperedge(n0, Array(n1, n3))
    new Hyperedge(n3, Array(n4))
    new Hyperedge(n1, Array(n4, n2))
    new Hyperedge(n4, Array(n5))
    new Hyperedge(n2, Array(n5))

    val search = new DepthFirstSearch(graph)
    val backEdges: IdentityHashSet[Hyperedge] = search.getBackEdges
    require(backEdges.size == 0, "back edges: " + backEdges.size)
  }
}