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

package cogx.compiler.codegenerator.opencl.fragments

import cogx.platform.types.{VirtualFieldRegister, Opcode, FieldType}

/**
  * A class representing merged hyperkernels that ensures that no two merged
  * kernels can be considered equal by the default mechanism established by
  * AbstractKernel, which compares the Opcode, field number and sizes.
  *
  * Technically, this class is not required, since the common subexpression
  * elimination is run before hyperkernel merging.  If someone wants to start
  * comparing merged hyperkernels for functional equality they can replace this
  * simple test with a more elaborate one.
  *
  * @param opcode Opcode for this particular kernel.
  * @param in Array of virtual field registers driving this kernel.
  * @param resultType Types of fields produced by this kernel.
  * @param address Addressing mode; note that for scalar fields,
  *        SmallTensorAddressing and TensorElementAddressing are equivalent.
  * @param samplingMode True if positions outside the domain of the field should
  *        return the value of the nearest border, otherwise return 0. This is
  *        valid only for image fields.
  *
  * @author Dick Carter
  */
private[cogx]
class CompoundHyperKernel(opcode: Opcode,
                  in: Array[VirtualFieldRegister],
                  resultType: Array[FieldType],
                  address: AddressingMode,
                  samplingMode: SamplingMode)
        extends HyperKernel(opcode, in, resultType, address, samplingMode)
{
  // We assume that two compound hyperkernels are different, even if they
  // have the same inputs and opcode.
  override def canEqual(other: Any): Boolean = false

  /** Create sensible name instead of "anon" */
  override def toString: String = {
    "CompoundHyperKernel" + "_" + id + "(" + opcode + ") => " + fieldTypesString +
      " '" + name + "'" + ", inputs = " + inputFieldTypesString
  }
}

