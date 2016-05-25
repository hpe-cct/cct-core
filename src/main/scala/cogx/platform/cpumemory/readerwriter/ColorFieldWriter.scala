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

package cogx.platform.cpumemory.readerwriter

import cogx.platform.types.Pixel
import cogx.cogmath.geometry.Shape

/** The interface for writing a color field on the CPU.
  *
  * @author Greg Snider
  */
trait ColorFieldWriter extends FieldWriter
{
  /** Set the shape of the color field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param fieldShape The desired shape of the color field for writing
    */
  def setShape(fieldShape: Shape) {}

  /** Set the shape of the color field for writing.
    *
    * @param rows Rows in the color field.
    * @param columns Columns in the color field.
    */
  def setShape(rows: Int, columns: Int) {
    setShape(Shape(rows, columns))
  }

  /** Write `out` from a 2D color field at (`row`, `col`). */
  def write(row: Int, col: Int, out: Pixel): Unit
}