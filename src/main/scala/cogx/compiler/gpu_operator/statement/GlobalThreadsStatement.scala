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

import cogx.platform.types.FieldType

/** Statement to override global thread allocation.
  *
  * @param fieldType The type of field to pattern thread allocation after;
  *                  typically this will be one thread per tensor.
  */
case class GlobalThreadsStatement(fieldType: FieldType) extends Statement {
  constructed()

  /** This statement does not generate executable code. */
  override def toString =
    "// Overriding global threads: " + fieldType.toString
}
