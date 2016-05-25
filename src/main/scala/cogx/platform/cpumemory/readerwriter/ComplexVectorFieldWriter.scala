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
import cogx.cogmath.algebra.complex.ComplexVector

/** The interface for writing a vector field on the CPU.
  *
  * @author Greg Snider
  */
trait ComplexVectorFieldWriter extends FieldWriter
{
  /** Set the shape of the vector field for writing.
    *
    * If the field already has a defined shape, this does nothing. Subclasses
    * that allow field shape to be defined must override this method.
    *
    * @param fieldShape The desired shape of the scalar field for writing
    */
  def setShape(fieldShape: Shape, vectorShape: Shape) {}

  /** Write `out` to a 0D vector field. */
  def write(out: ComplexVector): Unit

  /** Write `out` to a 1D vector field at (`col`). */
  def write(col: Int, out: ComplexVector): Unit

  /** Write `out` to a 2D vector field at (`row`, `col`). */
  def write(row: Int, col: Int, out: ComplexVector): Unit

  /** Write `out` to a 3D vector field at (`row`, `col`). */
  def write(layer: Int, row: Int, col: Int, out: ComplexVector): Unit
}