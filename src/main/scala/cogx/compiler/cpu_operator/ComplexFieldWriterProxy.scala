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

package cogx.compiler.cpu_operator

import cogx.platform.cpumemory._
import cogx.cogmath.geometry.Shape
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Complex32
import cogx.cogmath.algebra.complex.Complex
import cogx.platform.cpumemory.readerwriter.ComplexFieldWriter

/** A proxy for a ComplexFieldWriter that lets the actual writer be changed at
  * runtime.
  *
  * @author Greg Snider
  */
private[cogx]
class ComplexFieldWriterProxy
        extends FieldWriterProxy
        with ComplexFieldWriter
{
  /** The writer to which all write commands are directed. */
  private var actualWriter: ComplexFieldWriter = null

  /** Change the proxied ScalarFieldWriter to `writer`. */
  private[cpu_operator] def setWriter(writer: ComplexFieldWriter) {
    actualWriter = writer
  }

  private var _fieldType: FieldType = null
  def fieldType = _fieldType

  /** Set the shape of the complex field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param fieldShape The desired shape of the complex field for writing
    */
  override def setShape(fieldShape: Shape) {
    if (actualWriter == null) {
      _fieldType = new FieldType(fieldShape, Shape(), Complex32)
      actualWriter = FieldMemory.indirect(_fieldType).asInstanceOf[ComplexFieldMemory]
    }
  }

  /** Write `value` to a 0D complex field. */
  def write(value: Complex) {
    actualWriter.write(value)
  }

  /** Write `value` at (`col`) in a 1D complex field. */
  def write(col: Int, value: Complex) {
    actualWriter.write(col, value)
  }

  /** Write `value` at (`row`, `col`) in a 2D complex field. */
  def write(row: Int, col: Int, value: Complex) {
    actualWriter.write(row, col, value)
  }

  /** Write `value` at (`layer`, `row`, `col`) in a 3D complex field. */
  def write(layer: Int, row: Int, col: Int, value: Complex) {
    actualWriter.write(layer, row, col, value)
  }
}
