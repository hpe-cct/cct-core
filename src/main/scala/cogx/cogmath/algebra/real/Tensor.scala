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


/** A Tensor is used to represent general algebraic objects more complex than
  * scalars: vectors, matrices, tensors, pixels, and so on.
  *
  * It is represented
  * as a shaped, multidimensional array of numbers which are represented
  * internally as a flat array of Floats. The numbers are ordered in the flat
  * array using conventional C semantics: the rightmost index varies the fastest.
  *
  * @author Greg Snider
  */
@SerialVersionUID(7583272980775728570L)
trait Tensor extends Serializable {
  /** The shape of the tensor. A Shape of () represents a scalar. */
  def shape: Shape

  /** Read element at "index" in the flattened tensor. */
  def read(index: Int): Float

  /** The number of "numbers" held in the tensor. */
  def length = shape.points

 /** Test "this" and "other" for deep equality. Allows "==" to work. */
  override def equals(other: Any): Boolean =
    other match {
      case that: Tensor =>
        if ((this canEqual that) && (this.shape == that.shape)) {
          for (i <- 0 until shape.points)
            if (read(i) != that.read(i))
              return false
          true
        } else
          false
      case _ => false
    }

  /** Helper for equals. */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Tensor]

  /** Required because of overriding equals. */
  override def hashCode: Int = {
    // Since Tensors aren't likely to go into sets, we do a simple hash.
    shape.points
  }
}
