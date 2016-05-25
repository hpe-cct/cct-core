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

package cogx.compiler.parser.syntaxtree

import cogx.compiler.parser.op.MultiOpcode

/** Base class for all MultiField operators.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class MultiOperator(val opcode: MultiOpcode, val inputs: Array[Field])
{
  val fieldType = null
  def outputs: Array[_]

}

/*
class MultiOperator1D(operation: MultiOp, inputs: Array[FieldNode], columns: Int)
        extends MultiOperator(operation, inputs)
{

  def outputs = Array.tabulate(columns) {
    col => MultiSelect(this, Shape(col))
  }

}

class MultiOperator2D(operation: MultiOp, inputs: Array[FieldNode],
                      rows: Int, columns: Int)
        extends MultiOperator(operation, inputs)
{

  def outputs = Array.tabulate(rows, columns) {
  (row, col) => MultiSelect(this, Shape(row, col))
}

}

class MultiOperator3D(operation: MultiOp, inputs: Array[FieldNode],
                    layers: Int, rows: Int, columns: Int)
      extends MultiOperator(operation, inputs)
{
def outputs = Array.tabulate(layers, rows, columns) {
(layer, row, col) => MultiSelect(this, Shape(layer, row, col))
}
}
*/