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

package cogx.compiler.gpu_operator.expression

import cogx.compiler.gpu_operator.declaration.{ConstantExpression, NamedVariable}
import cogx.compiler.gpu_operator.types.GPUType
import cogx.compiler.parser.semantics.SemanticError
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Int32

/** An expression generated from a "_readTensor()" call.
  *
  * @param elementType The type of the tensor elementbeing read.
  * @param usesLocalAddressing True if the read operation uses local,
  *                            implicit addressing (_layer, _row, _column),
  *                            false if it uses explicit, nonlocal addressing.
  */
class ReadTensorElementExpression private (elementType: GPUType,
                                           val usesLocalAddressing: Boolean)
  extends GPUExpression(new Operator("readTensorElement"), elementType, Array())


/** Factory for creating ReadTensorElementExpressions.
 */
object ReadTensorElementExpression extends SemanticError {

  /** Create an expression for a "_readTensor" call.
    *
    * @param fieldType The type of the field being read.
    * @param elementType The type of tensor element being read.
    * @param fieldIndex The index of the input tensor field, starting from 0.
    *                   For a given GPU kernel, unique inputs are designated by
    *                   unique field indices.
    * @param indices Optional indices for nonlocal addressing of the desired
    *                tensor. If None, this uses implicit (_layer, _row, _column)
    *                addressing. Note that if the field is 0D, though, it reads
    *                the single tensor in the field.
    * @param tensorElementIndexIsLocal The user-specified tensor element index permits local addressing.
    */
   def apply(fieldType: FieldType,
             elementType: GPUType,
             fieldIndex: Int,
             indices: Option[Array[GPUExpression]],
             tensorElementIndexIsLocal: Boolean): ReadTensorElementExpression =
  {
    indices match {

      // Nonlocal reads.
      case Some(indices: Array[GPUExpression]) =>
        if (indices.length != fieldType.dimensions)
          error("indices of readTensorElement function do not match field dimensions")
        for (index <- indices) {
          if (!index.gpuType.isInt)
            error("integer expression required for field index")
          if (index.gpuType.elements > 1)
            error("non-vector expression required for field index")
        }

        // Hyperkernels require hardcoded names for indexing, namely "layer",
        // "row" and "column". We declare these using named variables. However,
        // since they may have already been declared previously, this should
        // be called within an anonymous block to avoid name clashes.
        indices.length match {
          case 0 =>
          case 1 =>
            val column = NamedVariable(GPUType(Int32, 1), "column")
            column := indices(0)
          case 2 =>
            val row = NamedVariable(GPUType(Int32, 1), "row")
            val column = NamedVariable(GPUType(Int32, 1), "column")
            row := indices(0)
            column := indices(1)
          case 3 =>
            val layer = NamedVariable(GPUType(Int32, 1), "layer")
            val row = NamedVariable(GPUType(Int32, 1), "row")
            val column = NamedVariable(GPUType(Int32, 1), "column")
            layer := indices(0)
            row := indices(1)
            column := indices(2)
          case x => internalError("unexpected index dimensionality: " + x)
        }
        new ReadTensorElementExpression(elementType, usesLocalAddressing = false) {
          override def exprString: String = " _readTensorElementNonlocal(@in" + fieldIndex + ")"
        }

        // Local field point reads (although not necessarily local reads because the tensorElement might not be local).
        // The following code is a bit unfortunate: we generate the right code by supplying the needed _layer, _row,
        // and _column variables for the field being read.  But who says they exist for the workfield (which might
        // be of lower dimension).  As a result, in the Prolog, we had to define _layer, _row and _column for all
        // workfield types.
      case None =>
        import ConstantExpression._
        if (tensorElementIndexIsLocal) {
          new ReadTensorElementExpression(elementType, usesLocalAddressing = true) {
            override def exprString: String = " _readTensorElementLocal(@in" + fieldIndex + ")"
          }
        }
        else fieldType.dimensions match {
          case 0 =>
            apply(fieldType, elementType, fieldIndex, Some(Array()), tensorElementIndexIsLocal)
          case 1 =>
            apply(fieldType, elementType, fieldIndex, Some(Array(_column)), tensorElementIndexIsLocal)
          case 2 =>
            apply(fieldType, elementType, fieldIndex, Some(Array(_row, _column)), tensorElementIndexIsLocal)
          case 3 =>
            apply(fieldType, elementType, fieldIndex, Some(Array(_layer, _row, _column)), tensorElementIndexIsLocal)
        }
    }
  }
}
