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

package cogx.runtime.debugger

import cogx.cogmath.circuit.Circuit
import cogx.compiler.codegenerator.KernelCircuit
import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}
import cogx.platform.types.VirtualFieldRegister
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel

/** An optimized computation with a simple interface, primarily intended
  * for writing debuggers. The structure of the circuit can be analyzed using
  * the inherited Circuit methods.
  *
  * While the KernelCircuit has been moved to the Hypercircuit/Hypernode classes
  * to support multi-output kernels, this class still uses Circuit/Node.  Thus,
  * it is not set up yet for probing of multi-output kernels.
  *
  * @param circuit A Circuit of AbstractKernels that represents the optimized
  *                computation.
  *
  * @author Greg Snider
  */
class ProbedCircuit(circuit: KernelCircuit)
        extends Circuit[ProbedField]
{
  init(circuit)

  /** Add ProbedFields to `this` that reflect the probed kernels in circuit.
    */
  private def init(circuit: KernelCircuit) {

    // For each virtual field register, we need to create a set of the other
    // virtual field registers it depends on.
    val dependencies = new IdentityHashMap[VirtualFieldRegister,
            IdentityHashSet[VirtualFieldRegister]]
    circuit.traversePostorder { kernel =>
      kernel.outputs.foreach(dependencies(_) = new IdentityHashSet[VirtualFieldRegister])
    }

    // While a user writes apps in terms of fields, the compiler deals not only
    // with fields but the kernels that generate them. An individual field may
    // actually be the product of several kernels, but only the kernel that
    // actually produces the user's field inhereits the field name. As a
    // result, the kernel circuit may wind up containing a bunch of unnamed
    // helper kernels, each outputting a field that wasn't explicitly defined
    // by the user. We don't want these fields to show up in the debugger even
    // if the user issues a probeAll command - the extra kernels are a compiler
    // implementation detail and will likely only serve to confuse users and
    // clutter the debugger. Hence, all the traversals and matches below check
    // not only to see if a kernel has been marked probed, but also that it has
    // inherited a name from the user's ComputeGraph implementation - unnamed
    // kernels are assumed to be compiler fluff and will not be added to the
    // ProbedCircuit.

    // Now, deduce the transitive closure of dependencies for each probed
    // kernel, bottom up.
    circuit.traversePostorder { kernel =>
      for (input <- kernel.inputs) {
        if (input.probed && input.name != "")
          kernel.outputs.foreach(dependencies(_) += input)
        else {
          kernel.outputs.foreach(dependencies(_).putAll(dependencies(input)))
        }
      }
    }

    // Create the probed fields
    val vfrToProbedField = new IdentityHashMap[VirtualFieldRegister, ProbedField]
    circuit.traversePostorder {
      kernel => {
        kernel.outputs.foreach( output => {
          if (output.probed && output.name != "") {
            val inputs = dependencies(output).toArray.map(
              vfr => vfrToProbedField(vfr))
            val probedField = new ProbedField(inputs, output)
            vfrToProbedField(output) = probedField
          }
        })
      }
    }

    // Mark recurrences on the probed fields.
    circuit.traversePostorder {
      case kernel: RecurrentFieldKernel if kernel.name != "" =>
        require(kernel.outputs.length == 1)
        val recurrenceField = vfrToProbedField(kernel.outputs(0))
        val feedbackVfr = kernel.recurrence

        val feedbackFields: Seq[ProbedField] =
          if (vfrToProbedField contains feedbackVfr)
            // The kernel driving the recurrence input is probed, so
            // no problem.
            Array(vfrToProbedField(feedbackVfr)).seq
          else {
            // The kernel driving the recurrence input isn't probed.
            // Chase back to find all probed fields that feedback kernel
            // depends on. In this case the recurrence will appear to have
            // feedback from one or more probed kernels (which is correct).
            dependencies(feedbackVfr).toSeq.map(vfr => vfrToProbedField(vfr))
          }
        recurrenceField.setFeedbackFrom(feedbackFields)
      case _ =>
    }
  }
}
