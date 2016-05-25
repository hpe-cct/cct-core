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

import cogx.platform.cpumemory.{FieldMemory, VectorFieldMemory}
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Vector
import cogx.platform.cpumemory.readerwriter.VectorFieldWriter

/** A proxy for a VectorFieldWriter that lets the actual writer be changed at
  * runtime.
  *
  * @author Greg Snider
  */
private[cogx]
class VectorFieldWriterProxy
        extends FieldWriterProxy
        with VectorFieldWriter
{
  /** The writer to which all write commands are directed. */
  private var actualWriter: VectorFieldWriter = null

  /** Change the proxied ScalarFieldWriter to `writer`. */
  private[cpu_operator] def setWriter(writer: VectorFieldWriter) {
    actualWriter = writer
  }

  /** Type of the field writer, initially undefined. */
  private var _fieldType: FieldType = null

  /** Type of the field writer. */
  def fieldType = _fieldType

  /** Set the shape of the scalar field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param fieldShape The desired shape of the scalar field for writing
    */
  override def setShape(fieldShape: Shape, vectorShape: Shape) {
    if (actualWriter == null) {
      _fieldType = new FieldType(fieldShape, vectorShape, Float32)
      actualWriter = FieldMemory.indirect(_fieldType).asInstanceOf[VectorFieldMemory]
    }
  }

  /** Write `value` to a 0D scalar field. */
  def write(value: Vector) {
    actualWriter.write(value)
  }

  /** Write `value` at (`col`) in a 1D scalar field. */
  def write(col: Int, value: Vector) {
    actualWriter.write(col, value)
  }

  /** Write `value` at (`row`, `col`) in a 2D scalar field. */
  def write(row: Int, col: Int, value: Vector) {
    actualWriter.write(row, col, value)
  }

  /** Write `value` at (`layer`, `row`, `col`) in a 3D scalar field. */
  def write(layer: Int, row: Int, col: Int, value: Vector) {
    actualWriter.write(layer, row, col, value)
  }
}

