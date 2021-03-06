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

import scala.language.postfixOps
import cogx.cogmath.algebra.complex.Complex

/** A wrapper for Complex that allows commutative operations between fields and
  * floats.
  *
  * @author Greg Snider
  */
private[cogx]
class CogComplex(value: Complex) extends Complex(value.real, value.imaginary) {
  def +(that: Field) = that + value
  def -(that: Field) = (that * -1f) + value
  def *(that: Field) = that * value
  def /(that: Field) = CogFunctions.reciprocal(that) * value

  // Might be worth adding:
//  def ===(that: Field) = that === value
//  def !===(that: Field) = that !=== value
}