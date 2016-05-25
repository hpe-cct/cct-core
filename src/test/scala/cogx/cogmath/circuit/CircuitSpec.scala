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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import scala.language.reflectiveCalls

/** Test code for the circuit package.
  *
  * Because the circuit classes are mutually recursive and abstract (a problem
  * with graph-like structures in general), the package is tested as a unit.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class CircuitSpec extends FunSuite with MustMatchers {

  /** Test version of Node subclass. */
  class TestNode(val index: Int, in: TestNode*)
          extends Node[TestNode](in.toArray)
  {
    override def toString = "node " + index
  }

  /** Test version of Circuit subclass. */
  class TestCircuit extends Circuit[TestNode]

  val Verbose = false

  /** A simple circuit with 2 roots, inputs at bottom
    * {{{
    *                5    8
    *              /   \ / \
    *             4     2   \
    *            / \  /      \
    *           3   1         7
    *            \ /         /
    *             0         6
    * }}}
    */
  val circuit = new TestCircuit {
    val node0 = new TestNode(0)
    val node1 = new TestNode(1, node0)
    val node2 = new TestNode(2, node1)
    val node3 = new TestNode(3, node0)
    val node4 = new TestNode(4, node3, node1)
    val node5 = new TestNode(5, node4, node2)   // root
    val node6 = new TestNode(6)
    val node7 = new TestNode(7, node6)
    val node8 = new TestNode(8, node2, node7)   // root
  }

  test("roots") {
    require(circuit.roots.length == 2, "expected 2 roots")
    require(circuit.roots(0) == circuit.node5)
    require(circuit.roots(1) == circuit.node8)
  }

  test("preorder traversal") {
    // Test for pre-order traversal.
    val traversalOrder = Array(5, 4, 3, 0, 1, 2, 8, 7, 6)
    var index = 0
    circuit.traversePreorder((node: TestNode) => {
      require(node.index == traversalOrder(index), "node " + node.index)
      index += 1
    })
  }

  test("postorder traversal") {
    // Test for pre-order traversal.
    val traversalOrder = Array(0, 3, 1, 4, 2, 5, 6, 7, 8)
    var index = 0
    circuit.traversePostorder((node: TestNode) => {
      require(node.index == traversalOrder(index))
      index += 1
    })
  }

  test("print") {
    if (Verbose)
      circuit.print
  }

  test("steal output") {
    // Replace node 4 with a new node, node 9.
    // Node 9 connects to the same inputs as node 4 then steals its outputs
    val node9 = new TestNode(9, circuit.node3, circuit.node1)
    node9.stealOutputsFrom(circuit.node4)
    if (Verbose) {
      println("after stealing")
      circuit.print
    }

    // Test for pre-order traversal. Note the substitution of 9 for 4
    val traversalOrder = Array(5, 9, 3, 0, 1, 2, 8, 7, 6)
    var index = 0
    circuit.traversePreorder((node: TestNode) => {
      require(node.index == traversalOrder(index), "node " + node.index)
      index += 1
    })

    // Now remove node 3 by having node 1 steal its outputs
    require(circuit.size == 9)
    circuit.node1.stealOutputsFrom(circuit.node3)
    require(circuit.size == 8)

    // Test for pre-order traversal.
    val traversalOrder2 = Array(5, 9, 1, 0, 2, 8, 7, 6)
    index = 0
    circuit.traversePreorder((node: TestNode) => {
      require(node.index == traversalOrder2(index), "node " + node.index)
      index += 1
    })
  }

  test("steal internal") {
    val circuit = new TestCircuit {
      val in1 = new TestNode(0)
      val in2 = new TestNode(1)
      val sum1 = new TestNode(2, in1, in2)
      val sum2 = new TestNode(3, in1, in2)
      val topSum = new TestNode(4, sum1, sum2)
    }
    require(circuit.size == 5)
    //println("CircuitSpec: before stealing")
    //circuit.print

    circuit.sum1.stealOutputsFrom(circuit.sum2)
    require(circuit.size == 4)
    //println("CircuitSpec: after stealing")
    //circuit.print
    circuit.traversePreorder {
      node => require(!(node eq circuit.sum2))
    }
  }

  test("steal recursive") {
    val circuit = new TestCircuit {
      val n0 = new TestNode(0)
      val n1 = new TestNode(1, n0)
      val n2 = new TestNode(2, n1)
    }
    require(circuit.size == 3)
    val n3 = new TestNode(3, circuit.n0)
    require(circuit.size == 4)
    require(circuit.contains(circuit.n0))
    require(circuit.contains(circuit.n1))
    require(circuit.contains(circuit.n2))
    require(circuit.contains(n3))
    n3 stealOutputsFrom circuit.n2
    // n2 is now useless, as is n1. n0 is primary input and cannot be removed.
    require(circuit.contains(circuit.n0))
    require(!circuit.contains(circuit.n1))
    require(!circuit.contains(circuit.n2))
    require(circuit.contains(n3))
  }
}