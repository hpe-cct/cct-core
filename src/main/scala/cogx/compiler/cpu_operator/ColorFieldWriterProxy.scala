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
import cogx.platform.types.{Pixel, FieldType}
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.platform.cpumemory.readerwriter.ColorFieldWriter

/** A proxy for a ColorFieldWriter that lets the actual writer be changed at
  * runtime.
  *
  * @author Greg Snider
  */
private[cogx]
class ColorFieldWriterProxy
        extends FieldWriterProxy
        with ColorFieldWriter
{
  /** The writer to which all write commands are directed. */
  private var actualWriter: ColorFieldWriter = null

  /** Change the proxied ScalarFieldWriter to `writer`. */
  private[cpu_operator] def setWriter(writer: ColorFieldWriter) {
    actualWriter = writer
  }

  private var _fieldType: FieldType = null
  def fieldType = _fieldType

  /** Set the shape of the color field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param fieldShape The desired shape of the color field for writing
    */
  override def setShape(fieldShape: Shape) {
    if (actualWriter == null) {
      _fieldType = new FieldType(fieldShape, Shape(3), Uint8Pixel)
      actualWriter = FieldMemory.indirect(_fieldType).asInstanceOf[ColorFieldMemory]
    }
  }

  /** Write `value` at (`row`, `col`) in a 2D color field. */
  def write(row: Int, col: Int, value: Pixel) {
    actualWriter.write(row, col, value)
  }
}

