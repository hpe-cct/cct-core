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

/** A low-precision floating point number, necessary because some GPUs offer
  * a reduced precision mode for multiply-adds that is faster.
  *
  * This supplies
  * the "approximately equal" functionality needed to compare results on the
  * GPU with results on the CPU (which generally has higher precision). The
  * implicit defined in the companion object lets you write code like this:
  * {{{
  *    val a: Float = ...
  *    val b: Float = ...
  *    val approxEqual: Boolean = (a ~== b)
  * }}}
  *
  * To access this:
  * {{{
  *    import cog.algebra.real.PoorFloat._
  * }}}
  *
  * User: snider1
  */

class PoorFloat(value: Float) {

  /** Return true if "this.value" and "y" are approximately equal. */
  def ~==(y: Float): Boolean = {
    // eps exponent was -15 .  Old GT200 architecture failed with -13 on log test.  -RJC
    val eps = math.pow(2.0, -12.0)
    if (value == 0 && y.abs < 10 * eps)
      true
    else if (y == 0 && value.abs < 10 * eps)
      true
    else
      (value - y).abs < (10 * eps * (value.abs max y.abs))
  }
}

/** Library for accessing PoorFloat functionality. */
object PoorFloat {
  import scala.language.implicitConversions

  type PoorFloat = cogx.cogmath.algebra.real.PoorFloat
  implicit def floatToPoorFloat(value: Float): PoorFloat = new PoorFloat(value)
}
