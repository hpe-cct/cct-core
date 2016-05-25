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

import cogx.compiler.gpu_operator.declaration.ConstantExpression
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.statement.{WriteTensorElementNonlocal, WriteTensorElement, WriteTensorNonlocal, WriteTensor}
import cogx.compiler.gpu_operator.types.OutFieldIndex

/** Functions that write tensor fields.
  *
  * @author Greg Snider
  */
trait FieldWriteFunctions {

  /** Write a tensor to an output field using the default location for the
    * current thread (_layer, _row, _column). Note that if this is a 0D field,
    * though, the tensor will be written to the single location in that field
    * regardless of the (_layer, _row, _column) thread variables.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor to be written
    */
  def _writeTensor(index: OutFieldIndex, value: GPUExpression) {
    new WriteTensor(index, value)
  }

  /** Write a tensor to an indexed location in a 1D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor to be written
    * @param column The column index of the location to be written with the
    *               tensor.
    */
  def _writeTensor(index: OutFieldIndex,
                   value: GPUExpression,
                   column: GPUExpression)
  {
    new WriteTensorNonlocal(index, value, Array(column))
  }

  /** Write a tensor to an indexed location in a 2D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor to be written
    * @param row The row index of the location to be written with the
    *        tensor.
    * @param column The column index of the location to be written with the
    *        tensor.
    */
  def _writeTensor(index: OutFieldIndex,
                   value: GPUExpression,
                   row: GPUExpression,
                   column: GPUExpression)
  {
    new WriteTensorNonlocal(index, value, Array(row, column))
  }

  /** Write a tensor to an indexed location in a 3D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor to be written
    * @param layer The layer index of the location to be written with the
    *        tensor.
    * @param row The row index of the location to be written with the
    *        tensor.
    * @param column The column index of the location to be written with the
    *        tensor.
    */
  def _writeTensor(index: OutFieldIndex,
                   value: GPUExpression,
                   layer: GPUExpression,
                   row: GPUExpression,
                   column: GPUExpression)
  {
    new WriteTensorNonlocal(index, value, Array(layer, row, column))
  }

  /** Write a tensor element to an output field using the default location for
    * the current thread (_layer, _row, _column). Note that if this is a 0D
    * field, though, the tensor will be written to the single location in that
    * field regardless of the (_layer, _row, _column) thread variables.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor element to be written
    * @param tensorElementIndex The index of the element to be written; this
    *                           will most commonly be _tensorElement, but it
    *                           is not restricted to that.
    */
  def _writeTensorElement(index: OutFieldIndex,
                          value: GPUExpression,
                          tensorElementIndex: GPUExpression)
  {
    if (tensorElementIndex == ConstantExpression._tensorElement)
      new WriteTensorElement(index, value)
    else
      new WriteTensorElementNonlocal(index, value, Array[GPUExpression](), tensorElementIndex, pointLocal = true)
  }

  /** Write a tensor element to an indexed location in a 1D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor element to be written
    * @param column The column index of the location to be written with the
    *        tensor element.
    * @param tensorElementIndex The index of the element to be written; this
    *                           will most commonly be _tensorElement, but it
    *                           is not restricted to that.
    */
  def _writeTensorElement(index: OutFieldIndex,
                          value: GPUExpression,
                          column: GPUExpression,
                          tensorElementIndex: GPUExpression)
  {
    new WriteTensorElementNonlocal(index, value,
      Array(column), tensorElementIndex)
  }

  /** Write a tensor element to an indexed location in a 2D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor element to be written
    * @param row The row index of the location to be written with the
    *        tensor element.
    * @param column The column index of the location to be written with the
    *        tensor element.
    * @param tensorElementIndex The index of the element to be written; this
    *                           will most commonly be _tensorElement, but it
    *                           is not restricted to that.
    */
  def _writeTensorElement(index: OutFieldIndex,
                          value: GPUExpression,
                          row: GPUExpression,
                          column: GPUExpression,
                          tensorElementIndex: GPUExpression)
  {
    new WriteTensorElementNonlocal(index, value,
      Array(row, column), tensorElementIndex)
  }

  /** Write a tensor element to an indexed location in a 3D output field.
    *
    * @param index The index of the output field to be written (e.g. _out0)
    * @param value The tensor element to be written
    * @param layer The layer index of the location to be written with the
    *        tensor element.
    * @param row The row index of the location to be written with the
    *        tensor element.
    * @param column The column index of the location to be written with the
    *        tensor element.
    * @param tensorElementIndex The index of the element to be written; this
    *                           will most commonly be _tensorElement, but it
    *                           is not restricted to that.
    */
  def _writeTensorElement(index: OutFieldIndex,
                          value: GPUExpression,
                          layer: GPUExpression,
                          row: GPUExpression,
                          column: GPUExpression,
                          tensorElementIndex: GPUExpression)
  {
    new WriteTensorElementNonlocal(index, value,
      Array(layer, row, column), tensorElementIndex)
  }

}
