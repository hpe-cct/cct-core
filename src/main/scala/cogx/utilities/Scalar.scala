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

package cogx.utilities

import scala.language.implicitConversions

/** An object version of a mutable Double. Accessing the value of the scalar
  * requires (), but writes can be direct. For example:
  * {{{
  *   val x = new Scalar		// Create a scalar.
  *   x = 1.234				// Assign value to scalar.
  *   var y = 25.3			// Ordinary double.
  *   val sum = y + x()		// Accessing value of x.
  *   val sum = y + x.value	// Alternate way of accessing value of x.
  *   val z = new Scalar
  *   z = x					// Assign one scalar to another
  * }}}
  *
  * @author Greg Snider
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private class Scalar(initialValue: Double) {
  var value = initialValue
  
  def this() = this(0.0)
  def apply() = value
  def update(newValue: Double) {value = newValue}
  def update(that: Scalar) {value = that()}
  
  def +(that: Double) = new Scalar(value + that)
  def -(that: Double) = new Scalar(value - that)
  def *(that: Double) = new Scalar(value * that)
  def /(that: Double) = new Scalar(value / that)
  
  def +(that: Scalar) = new Scalar(value + that())
  def -(that: Scalar) = new Scalar(value - that())
  def *(that: Scalar) = new Scalar(value * that())
  def /(that: Scalar) = new Scalar(value / that())
  
  override def toString = "(" + value.toString + ")"
}

@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private object Scalar {
  //import scala.language.implicitConversions

  /*** Implicitly convert an Array2D[Double] to a Matrix. */
  implicit def doubleWrapper(d: Double): Scalar = new Scalar(d)
}