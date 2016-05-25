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

package cogx.runtime

import cogx.platform.types.KernelCodeTypes.CogNone
import cogx.platform.types.KernelTypes.UnrestorableKernelType

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith

import cogx.api.{CogFunctionAPI, ImplicitConversions}
import cogx.compiler.parser.syntaxtree.ScalarField
import cogx.platform.types._
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.codegenerator.KernelCircuit
import cogx.cogmath.geometry.Shape
import cogx.runtime.debugger.{ProbedField, ProbedCircuit}

/** Test code for auto naming.
  *
  * @author Greg Snider
  */

@RunWith(classOf[JUnitRunner])
class ProbedCircuitSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with CogFunctionAPI
{
  /** Create a complex kernelCircuit, probe selected nodes, and verify
    * that only those survive to the probed circuit with correct dependences.
    * {{{
    *                5    8
    *              /   \ / \
    *             4     2   \
    *            / \  /      \
    *           3   1         7
    *            \ /         /
    *             0         6
    * }}}
    *
    * We mark nodes {0, 6, 4, 5, 8} as probed.
    */
  test("probes") {
    val field0D = new FieldType(Shape(), Shape(), Float32)

    class Kernel(val index: Int, in: Array[AbstractKernel], fieldType: FieldType = field0D)
            extends AbstractKernel(new Opcode("junk") {}, in.map(_.outputs(0)),
              Array(fieldType))
    {
      val kernelCodeType = CogNone
      /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
      val kernelType = UnrestorableKernelType
      // Add the output VirtualFieldRegister
      new VirtualFieldRegister(fieldType, this)

      override def toString = {
        "kernel " + index + (if (probed) " probed" else "")
      }

      /** Create a clone of this kernel that uses a new set of virtual field registers
        *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
      def copyWithNewInputs(inputs: Array[VirtualFieldRegister]) =
        new Kernel(index, inputs.map(_.source))
    }

    val circuit = new KernelCircuit {
      val node0 = new Kernel(0, Array())
      val node1 = new Kernel(1, Array(node0))
      val node2 = new Kernel(2, Array(node1))
      val node3 = new Kernel(3, Array(node0))
      val node4 = new Kernel(4, Array(node3, node1))
      val node5 = new Kernel(5, Array(node4, node2))   // root
      val node6 = new Kernel(6, Array())
      val node7 = new Kernel(7, Array(node6))
      val node8 = new Kernel(8, Array(node2, node7))   // root
    }
    circuit.node0.markProbed
    circuit.node6.markProbed
    circuit.node4.markProbed
    circuit.node5.markProbed
    circuit.node8.markProbed

    circuit.print

    val probedKernels = Array(
      circuit.node0,
      circuit.node6,
      circuit.node4,
      circuit.node5,
      circuit.node8
    )
    val probedCircuit = new ProbedCircuit(circuit)
    probedCircuit.traversePostorder {
      probedField =>
        require(probedKernels contains probedField.kernel)
        val dependencies: Seq[ProbedField] = probedField.dependsOn
        probedField.kernel.asInstanceOf[Kernel].index match {
          case 0 =>
            require(dependencies.length == 0)
          case 6 =>
            require(dependencies.length == 0)
          case 4 =>
            require(dependencies.length == 1)
            require(dependencies(0).kernel == circuit.node0)
          case 5 =>
            val nodes = Array(circuit.node0, circuit.node4)
            require(dependencies.length == 2)
            require(nodes contains dependencies(0).kernel)
            require(nodes contains dependencies(1).kernel)
          case 8 =>
            val nodes = Array(circuit.node0, circuit.node6)
            require(dependencies.length == 2)
            require(nodes contains dependencies(0).kernel)
            require(nodes contains dependencies(1).kernel)
        }
    }
  }

  /** test that when the HyperKernel merger replaces a field's output generating
    * kernel the field is still probeable.  This test is less than ideal- it hangs
    * the test suite if there's a problem because another thread throws the "not
    * probable" exception.
    */
  test("probe of field with merger-replaced output kernel") {
    val cg = new ComputeGraph {
      val x = ScalarField(Shape(10,10))
      val y = 2f*x
      val z = 2f*x
      val zero = y-z
      probe(zero, "")
    }
    try {
      cg.step
      cg.read(cg.zero)
    }
    finally
      cg.release
  }
}
