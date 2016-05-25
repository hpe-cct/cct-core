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

package cogx.compiler.gpu_operator.function

import cogx.compiler.parser.syntaxtree.Field
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.declaration.{GPUArrayVariable, GPUVariable}

/** Functions that create variables like the tensors found in a tensor field.
  *
  * @author Greg Snider
  */
trait FieldVariableFunctions {

  /** Create a variable of type defined by the tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @return The value of the tensor as an expression.
    */
  def _tensorVar(f: Field): GPUVariable = {
    val buffer = FieldBuffer.create(f)
    buffer.tensorVar()
  }

  /** Create a 1D array of tensors, where the tensor type is defined the
    * type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param columns Length of tensor array.
    * @return The declaration for the array.
    */
  def _tensorArray(f: Field,
                   columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorArray(Array(columns))
  }

  /** Create a 2D array of tensors, where the tensor type is defined the
    * type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param rows Rows in tensor array.
    * @param columns Columns in tensor array.
    * @return The declaration for the array.
    */
  def _tensorArray(f: Field,
                   rows: GPUExpression,
                   columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorArray(Array(rows, columns))
  }

  /** Create a 3D array of tensors, where the tensor type is defined the
    * type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param layers Layers in tensor array.
    * @param rows Rows in tensor array.
    * @param columns Columns in tensor array.
    * @return The declaration for the array.
    */
  def _tensorArray(f: Field,
                   layers: GPUExpression,
                   rows: GPUExpression,
                   columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorArray(Array(layers, rows, columns))
  }

  /** Create an element variable of type defined by the tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @return The value of the tensor as an expression.
    */
  def _tensorElementVar(f: Field): GPUVariable = {
    val buffer = FieldBuffer.create(f)
    buffer.tensorElementVar()
  }

  /** Create a 1D array of tensor elements, where the element type is defined
    * by the type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param columns Length of tensor array.
    * @return The declaration for the array.
    */
  def _tensorElementArray(f: Field,
                          columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorElementArray(Array(columns))
  }

  /** Create a 2D array of tensor elements, where the element type is defined
    * by the type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param rows Rows in tensor array.
    * @param columns Columns in tensor array.
    * @return The declaration for the array.
    */
  def _tensorElementArray(f: Field,
                          rows: GPUExpression,
                          columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorElementArray(Array(rows, columns))
  }

  /** Create a 3D array of tensor elements, where the element type is defined
    * by the type of tensors in a field.
    *
    * @param f The tensor field to be analyzed.
    * @param layers Layers in tensor array.
    * @param rows Rows in tensor array.
    * @param columns Columns in tensor array.
    * @return The declaration for the array.
    */
  def _tensorElementArray(f: Field,
                          layers: GPUExpression,
                          rows: GPUExpression,
                          columns: GPUExpression): GPUArrayVariable =
  {
    val buffer = FieldBuffer.create(f)
    buffer.tensorElementArray(Array(layers, rows, columns))
  }
}
