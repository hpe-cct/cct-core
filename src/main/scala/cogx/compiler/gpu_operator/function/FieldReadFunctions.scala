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

import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.parser.syntaxtree.Field
import cogx.compiler.gpu_operator.expression.GPUExpression


/** Functions that read or access tensor fields.
  *
  * @author Greg Snider
  */
trait FieldReadFunctions extends SemanticError {

  /** Read the "current" tensor (addressed by _layer, _row, _column).
    *
    * @param f The tensor field to be read.
    * @return The value of the tensor as an expression.
    */
  def _readTensor(f: Field): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensor()
  }

  /** Read the tensor in a 1D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param column The column address of the tensor.
    * @return The value of the tensor as an expression.
    */
  def _readTensor(f: Field,
                  column: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensor(Array(column))
  }

  /** Read the tensor in a 2D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param row The row address of the tensor.
    * @param column The column address of the tensor.
    * @return The value of the tensor as an expression.
    */
  def _readTensor(f: Field,
                  row: GPUExpression,
                  column: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensor(Array(row, column))
  }

  /** Read the tensor in a 3D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param layer The layer address of the tensor.
    * @param row The row address of the tensor.
    * @param column The column address of the tensor.
    * @return The value of the tensor as an expression.
    */
  def _readTensor(f: Field,
                  layer: GPUExpression,
                  row: GPUExpression,
                  column: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensor(Array(layer, row, column))
  }

  /** Read the "current" tensor (addressed by _layer, _row, _column).
    *
    * @param f The tensor field to be read.
    * @param element The element (index) of the tensor to be read.
    * @return The value of the tensor as an expression.
    */
  def _readTensorElement(f: Field,
                         element: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensorElement(element)
  }

  /** Read the tensor in a 1D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param column The column address of the tensor.
    * @param element The element (index) of the tensor to be read.
    * @return The value of the tensor as an expression.
    */
  def _readTensorElement(f: Field,
                         column: GPUExpression,
                         element: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensorElement(Array(column), element)
  }

  /** Read the tensor in a 2D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param row The row address of the tensor.
    * @param column The column address of the tensor.
    * @param element The element (index) of the tensor to be read.
    * @return The value of the tensor as an expression.
    */
  def _readTensorElement(f: Field,
                         row: GPUExpression,
                         column: GPUExpression,
                         element: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensorElement(Array(row, column), element)
  }

  /** Read the tensor in a 3D field using explicit addressing.
    *
    * @param f The tensor field to be read.
    * @param layer The layer address of the tensor.
    * @param row The row address of the tensor.
    * @param column The column address of the tensor.
    * @param element The element (index) of the tensor to be read.
    * @return The value of the tensor as an expression.
    */
  def _readTensorElement(f: Field,
                         layer: GPUExpression,
                         row: GPUExpression,
                         column: GPUExpression,
                         element: GPUExpression): GPUExpression = {
    val buffer = FieldBuffer.create(f)
    buffer.readTensorElement(Array(layer, row, column), element)
  }
}