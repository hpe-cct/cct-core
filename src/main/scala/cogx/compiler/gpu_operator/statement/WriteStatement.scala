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

import cogx.compiler.gpu_operator.types.OutFieldIndex
import cogx.platform.types.FieldType

/** Base for write statements (writing a tensor or tensor element in a field).
  *
  * @author Greg Snider
  */
abstract class WriteStatement extends Statement {
  /** Index of the field being written by this write statement. */
  val index: OutFieldIndex

  /** Generate a string that implements the write statement.
    *
    * @param outputFieldType Type of the output field being written.
    * @return String for write statement
    */
  def toString(outputFieldType: FieldType): String
}
