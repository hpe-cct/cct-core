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

import cogx.cogmath.algebra.complex.Complex

/** The interface for reading a complex field on the CPU.
  *
  * @author Greg Snider
  */
trait ComplexFieldReader
        extends FieldReader
{
  /** Read the single value in a 0D vector field into `out`. */
  def read(): Complex

  /** Read the value at (`col`) in a 1D vector field into `out`. */
  def read(col: Int): Complex

  /** Read the value at (`row`, `col`) in a 2D vector field into `out`. */
  def read(row: Int, col: Int): Complex

  /** Read the value at (`layer`, `row`, `col`) in a 3D vector field into `out`. */
  def read(layer: Int, row: Int, col: Int): Complex
}