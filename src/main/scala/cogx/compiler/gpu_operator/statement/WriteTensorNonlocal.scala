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

import cogx.compiler.gpu_operator.expression.GPUExpression
import cogx.compiler.gpu_operator.types.OutFieldIndex
import cogx.platform.types.FieldType

/** Statement to write a tensor to a field at an explicitly provided address.
  *
  * @param index Index of the output field to be written.
  * @param value The tensor value to be written.
  * @param fieldIndices The (layer, row, column) address of the tensor to be
  *                     written.
  */
class WriteTensorNonlocal(val index: OutFieldIndex,
                          value: GPUExpression,
                          fieldIndices: Array[GPUExpression])
  extends WriteStatement
{
  /** Generate a string that implements the write statement.
    *
    * @param outputFieldType Type of the output field being written.
    * @return String for write statement
    */
  def toString(outputFieldType: FieldType): String = {
    require(outputFieldType.dimensions == fieldIndices.length,
      "illegal _writeTensor call: number of indices doesn't match " +
        "dimensionality of field being written"
    )
    val buffer = new StringBuffer()
    fieldIndices.length match {
      case 0 =>
      case 1 =>
        buffer append "column = " + fieldIndices(0).exprString + "; "
      case 2 =>
        buffer append "row = " + fieldIndices(0).exprString + "; "
        buffer append "column = " + fieldIndices(1).exprString + "; "
      case 3 =>
        buffer append "layer = " + fieldIndices(0).exprString + "; "
        buffer append "row = " + fieldIndices(1).exprString + "; "
        buffer append "column = " + fieldIndices(2).exprString + "; "
      case x => throw new Exception("unexpected index length: " + x)
    }
    val protection: String = outputFieldType.dimensions match {
      case 3 =>
        "if (layer < _layers && row < _rows && column < _columns) "
      case 2 =>
        "if (row < _rows && column < _columns) "
      case 1 =>
        "if (column < _columns) "
      case 0 =>
        ""
    }
    buffer append protection
    buffer append "_writeTensorNonlocal" + index.index + " = " + value.exprString + "; "
    buffer append "\n"
    buffer.toString
  }
}
