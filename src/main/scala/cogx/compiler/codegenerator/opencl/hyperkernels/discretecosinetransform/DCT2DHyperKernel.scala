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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.cogmath.algebra.real.Logarithm
import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.compiler.parser.op._
import cogx.platform.types.{FieldType, VirtualFieldRegister}


/** Factory object for creating a chain of kernels to perform a 2D DCT
  */
private[cogx]
object DCT2DHyperKernel extends Logarithm {
  val DeviceMaxWorkItemsPerWorkGroup = 256

  /** Create a kernel DAG for a 2D DCT or its inverse.
    *
    * @param in The virtual field register of the input field to be 2D FFT'd.
    * @param operation The opcode for this operation, with dir and dim info.
    * @param resultType The FieldType of the result of this kernel.
    * @return The synthesized hyperkernel.
    */
  def apply(in: VirtualFieldRegister, operation: DCTOpcode, resultType: FieldType):
  HyperKernel =
  {
    // True for forward transform, false for inverse.
    var forward = true
    // True for transposed output (both forward and inverse), false for normal.
    var transposed = true
    operation match {
      case DCT2DOp =>
        forward = true
        transposed = false
      case DCTInverse2DOp =>
        forward = false
        transposed = false
      case DCTTransposed2DOp =>
        forward = true
        transposed = true
      case DCTInverseTransposed2DOp =>
        forward = false
        transposed = true
    }

    // The shape of the transposed field.
    val transposedType =
      new FieldType(Shape(in.fieldType.columns, in.fieldType.rows),
        in.fieldType.tensorShape, in.fieldType.elementType)

    // Compute the chain of kernels needed to the requested variant of the DCT.
    if (forward) {
      // Forward DCT
      val interleave =
        DCTInterleaveHyperKernel(in, DCTInterleaveOp, in.fieldType)
      val dctRows =
        new DCTRowsHyperKernel(interleave.outputs(0), DCTRowsOp, interleave.fieldType)
      val flipped =
        DCTTransposeHyperKernel(dctRows.outputs(0), DCTTransposeFieldOp, transposedType)
      val dctColumns =
        new DCTRowsHyperKernel(flipped.outputs(0), DCTRowsOp, transposedType)
      if (transposed)
        dctColumns
      else
        DCTTransposeHyperKernel(dctColumns.outputs(0), DCTTransposeFieldOp, in.fieldType)
    } else {
      // Inverse DCT
      val dctRows = new DCTRowsHyperKernel(in, DCTInverseRowsOp, in.fieldType)
      val flipped =
        DCTTransposeHyperKernel(dctRows.outputs(0), DCTTransposeFieldOp, transposedType)
      val dctColumns =
        new DCTRowsHyperKernel(flipped.outputs(0), DCTInverseRowsOp, transposedType)
      val deinterleave =
        DCTDeinterleaveHyperKernel(dctColumns.outputs(0), DCTDeinterleaveOp, transposedType)
      if (transposed)
        deinterleave
      else
        DCTTransposeHyperKernel(deinterleave.outputs(0), DCTTransposeFieldOp, in.fieldType)
    }
  }
}
