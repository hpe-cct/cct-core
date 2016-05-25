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

package cogx.cogmath.algebra.real

import cogx.cogmath.geometry.Shape


/** A zero-dimensional Tensor holding a single real value.
  *
  * @author Greg Snider
  */

class Scalar(val value: Float) extends Tensor {
  def shape = Scalar.shape

  def toFloat = value

  /** Read element at "index" in the flattened tensor. */
  def read(index: Int) = value
}

/** Companion object for the Scalar class.
 */
object Scalar {
  /** Shape of all scalars. */
  val shape = Shape()
}
