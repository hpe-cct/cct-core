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

package cogx.cogmath.hypergraph.acyclic

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers


/** Test code.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class DAGNodeSpec extends FunSuite with MustMatchers {
  val Verbose = false

  /** Node class with identity "index" for testing. */
  class MyNode(val index: Int, kids: Array[MyNode] = Array()) extends DAGNode[MyNode] {
//    private val kids = ArrayBuffer[MyNode]()
//    def addChild(child: MyNode) {kids += child}
    def children = kids
    override def toString = "node " + index
    // We force a goofy equality, all MyNodes are equal, to test that
    // foreach and print traverse the tree regardless of how subclasses
    // of DAGNode have defined equality.
    override def equals(that: Any) = true
    override def hashCode = 1
  }

  /** Build a simple DAG.
    * {{{
    *                5
    *              /   \
    *             4     2
    *            / \  /
    *           3   1
    *            \ /
    *             0
    * }}}
    */
  private def buildTree: MyNode = {
    val node0 = new MyNode(0)
    val node1 = new MyNode(1, Array(node0))
//    node1.addChild(node0)
    val node2 = new MyNode(2, Array(node1))
//    node2.addChild(node1)
    val node3 = new MyNode(3, Array(node0))
//    node3.addChild(node0)
    val node4 = new MyNode(4, Array(node3, node1))
//    node4.addChild(node3)
//    node4.addChild(node1)
    val node5 = new MyNode(5, Array(node4, node2))
//    node5.addChild(node4)
//    node5.addChild(node2)

    val root = node5
    root
  }

  test("PreorderTraversal") {
    val root = buildTree
    // Count nodes in tree.
    require(root.subDAGSize == 6)
    // Test for pre-order traversal.
    val traversalOrder = Array(5, 4, 3, 0, 1, 2)
    var index = 0
    root.traversePreorder((n: MyNode) => {
      val node = n.asInstanceOf[MyNode]
      require(node.index == traversalOrder(index))
      index += 1
    })
  }

  test("PostorderTraversal") {
    val root = buildTree
    // Count nodes in tree.
    require(root.subDAGSize == 6)
    // Test for pre-order traversal.
    val traversalOrder = Array(0, 3, 1, 4, 2, 5)
    var index = 0
    root.traversePostorder((n: MyNode) => {
      val node = n.asInstanceOf[MyNode]
      require(node.index == traversalOrder(index))
      index += 1
    })
  }

  test("Print") {
    if (Verbose)
      buildTree.print
  }
}