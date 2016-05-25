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

package cogx.compiler.gpu_operator.statement

import cogx.compiler.gpu_operator.declaration.ConstantExpression._
import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.types.OutFieldIndex
import cogx.platform.types.FieldType

/** Write a tensor element at an explicit (layer, row, column, tensorIndex)
  * address.
  *
  * @param index The index of the output field to be written (e.g. _out0)
  * @param value The tensor element value to be written
  * @param fieldIndices Address (layer, row, column) of tensor to be written.
  * @param tensorElementIndex Index of the tensor element to be written.
  */
class WriteTensorElementNonlocal(val index: OutFieldIndex,
                                 value: GPUExpression,
                                 fieldIndices: Array[GPUExpression],
                                 tensorElementIndex: GPUExpression, pointLocal: Boolean = false)
  extends WriteStatement
{

  /** Generate a string that implements the write statement.
    *
    * @param outputFieldType Type of the output field being written.
    * @return String for write statement
    */
  def toString(outputFieldType: FieldType): String = {
    // If pointLocal == true, supply the default field point indices per the field dimension
    val indices =
      if (!pointLocal)
        fieldIndices
      else {
        outputFieldType.dimensions match {
          case 0 => Array[GPUExpression]()
          case 1 => Array(_column)
          case 2 => Array(_row, _column)
          case 3 => Array(_layer, _row, _column)
          case x => throw new RuntimeException("Unsupported field dimension must be <= 3: " + x)
        }
      }
    require(outputFieldType.dimensions == indices.length,
      "illegal _writeTensorElement call: number of indices doesn't match " +
        "dimensionality of field being written. Field shape: " +
        outputFieldType.fieldShape + ", indices: " + indices.length + "."
    )
    val buffer = new StringBuffer()
    // We declare new layer, row, column variables within the scope of this write to avoid disturbing
    // the values of these variables in an outer scope.
    buffer append "{ "
    indices.length match {
      case 0 =>
      case 1 =>
        buffer append "int column = " + indices(0).exprString + "; "
      case 2 =>
        buffer append "int row = " + indices(0).exprString + "; "
        buffer append "int column = " + indices(1).exprString + "; "
      case 3 =>
        buffer append "int layer = " + indices(0).exprString + "; "
        buffer append "int row = " + indices(1).exprString + "; "
        buffer append "int column = " + indices(2).exprString + "; "
      case x => throw new Exception("unexpected index length: " + x)
    }

    buffer append "int tensorElement = " + tensorElementIndex.exprString + "; "
    buffer append "_writeTensorElementNonlocal" + index.index + " = " + value.exprString + "; }"
    buffer.toString
  }
}
