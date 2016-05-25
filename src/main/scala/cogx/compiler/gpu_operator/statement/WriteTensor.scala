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

/** Statement to write a tensor in an output field.
  *
  * @param index Index of the output field to be written.
  * @param value The tensor value to be written.
  */
private[gpu_operator]
class WriteTensor(val index: OutFieldIndex, value: GPUExpression)
  extends WriteStatement
{
  constructed()

  /** Generate a string that implements the write statement.
    *
    * @param outputFieldType Type of the output field being written.
    * @return String for write statement
    */
  def toString(outputFieldType: FieldType): String = {
    require(outputFieldType != null)
    // Block illegal threads from writing "out of bounds"
    val buffer = new StringBuffer
    val protection: String = outputFieldType.dimensions match {
      case 3 =>
        "if (_layer < _layers &&_row < _rows && _column < _columns) "
      case 2 =>
        "if (_row < _rows && _column < _columns) "
      case 1 =>
        "if (_column < _columns) "
      case 0 =>
        ""
    }
    buffer append protection
    buffer append "_writeTensorLocal" + index.index + " = " + value.exprString
    buffer.toString
  }
}
