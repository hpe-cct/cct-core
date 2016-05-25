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

package cogx.cogmath.algebra.complex

import scala.Serializable

/** Implicit conversions for complex numbers.
  *
  * @author Greg Snider
  */
trait ComplexImplicits extends Serializable {
  import scala.language.implicitConversions

  /** Square root of -1 */
  val I = cogx.cogmath.algebra.complex.Complex.I
  /** Float --> Complex */
  implicit def floatToComplex(d: Float): Complex = new Complex(d, 0)
  /** Int --> Complex */
  implicit def intToComplex(i: Int): Complex = new Complex(i.toFloat, 0)
}
