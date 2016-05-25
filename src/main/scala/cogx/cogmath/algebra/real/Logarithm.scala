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

/** Base 2 logarithms.
  *
  * @author Greg Snider
  */

trait Logarithm {
  /** Take the logarithm of "value" base 2. */
  def log2(value: Float): Float = (math.log(value) / math.log(2)).toFloat

  /** Return true if "value" is an exact power of 2. */
  def isPowerOf2(value: Float): Boolean = {
    val log = log2(value).toInt
    return value == (1 << log)
  }

  def roundUpPowerOf2(value: Float): Int = {
    if (isPowerOf2(value))
      value.toInt
    else
      1 << (log2(value).toInt + 1)
  }

  def roundDownPowerOf2(value: Float): Int = {
    if (isPowerOf2(value))
      value.toInt
    else
      1 << log2(value).toInt
  }
}

object Logarithm extends Logarithm
