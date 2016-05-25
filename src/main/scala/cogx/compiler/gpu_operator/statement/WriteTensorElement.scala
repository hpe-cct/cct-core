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

/** Write a tensor element to a field at the default (_layer, _row, _column)
  * field point address and default tensor element address _tensorElement..
  *
  * @param index The index of the output field to be written (e.g. _out0)
  * @param value The tensor element value to be written
  */
class WriteTensorElement(val index: OutFieldIndex,
                         value: GPUExpression)
  extends WriteStatement
{

  /** Generate a string that implements the write statement.
    *
    * @param outputFieldType Type of the output field being written.
    * @return String for write statement
    */
  def toString(outputFieldType: FieldType): String = {
    val buffer = new StringBuffer
    buffer append "{ _writeTensorElementLocal" + index.index + " = " + value.exprString + "; }"
    buffer.toString
  }
}
