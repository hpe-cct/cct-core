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

package cogx.compiler.codegenerator.opencl.cpukernels

import cogx.compiler.parser.op.{ConstantScalar1DOp, ConstantOp}
import cogx.platform.types.{VirtualFieldRegister, FieldType}
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Vector

/** A kernel that simply sources a fixed vector. This is typically
  * used to create constant data for kernels.
  *
  * @author Dick Carter
  */
private[cogx]
class FixedVectorKernel private (fieldType: FieldType, opcode: ConstantOp)
        extends ConstantFieldKernel(fieldType, opcode){

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  override def copyWithNewInputs(inputs: Array[VirtualFieldRegister]) =
    new FixedVectorKernel(fieldType, opcode)

}

private[cogx]
object FixedVectorKernel {

  def apply(vector: Vector) = {
    val resultType = new FieldType(vector.shape, Shape(), Float32)
    val opcode = ConstantScalar1DOp(i => vector(i))

    new FixedVectorKernel(resultType, opcode)
  }
}
