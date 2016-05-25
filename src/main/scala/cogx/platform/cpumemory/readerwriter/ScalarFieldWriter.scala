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

import cogx.cogmath.geometry.Shape

/** The interface for writing a scalar field on the CPU.
  *
  * @author Greg Snider
  */
trait ScalarFieldWriter extends FieldWriter
{
  /** Set the shape of the scalar field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param shape The desired shape of the scalar field for writing
    */
  def setShape(shape: Shape) {}

  /** Set the shape to be 0D. */
  def setShape() {
    setShape(Shape())
  }

  /** Set the shape to `columns`. */
  def setShape(columns: Int) {
    setShape(Shape(columns))
  }

  /** Set the shape to `rows` x `columns`. */
  def setShape(rows: Int, columns: Int) {
    setShape(Shape(rows, columns))
  }

  /** Set the shape to `layers` x `rows` x `columns`. */
  def setShape(layers: Int, rows: Int, columns: Int) {
    setShape(Shape(layers, rows, columns))
  }

  /** Write `value` to a 0D scalar field. */
  def write(value: Float): Unit

  /** Write `value` at (`col`) in a 1D scalar field. */
  def write(col: Int, value: Float): Unit

  /** Write `value` at (`row`, `col`) in a 2D scalar field. */
  def write(row: Int, col: Int, value: Float): Unit

  /** Write `value` at (`layer`, `row`, `col`) in a 3D scalar field. */
  def write(layer: Int, row: Int, col: Int, value: Float ): Unit
}