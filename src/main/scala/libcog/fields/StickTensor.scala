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

package libcog.fields

import libcog._

/** A stick tensor in 2 dimensions, encoded as a complex number.
  *
  * Because orientation extends only from 0 to Pi while phase of a complex
  * number extends from 0 to 2 Pi, we *define* orientation to be half the phase
  * of the complex number. This means when creating the stick tensor from a
  * given orientation and magnitude, the orientation must be doubled.
  *
  * @param magnitude Magnitude of tensor.
  * @param orient Orientation of tensor (0 to Pi].
  * @return A stick tensor, represented as a complex number.
  *
  * @author Greg Snider
  */
class StickTensor(val stickness: Float, val orientation: Float)
  extends Complex(stickness * math.cos(2 * orientation).toFloat,
    stickness * math.sin(2 * orientation).toFloat)
{
  require(orientation > 0 && orientation <= math.Pi.toFloat,
    "illegal orientation for stick tensor: " + orientation +
            ". Must be in (0, Pi].")
}


object StickTensor {

  /** Cast a complex number to a stick tensor. This presumes that the complex
    * number represents a legitimate
    */
  def apply(complex: Complex): StickTensor = {
    val stickness = computeStickness(complex)
    val orientation = computeOrientation(complex)
    new StickTensor(stickness, orientation)
  }

  /** Compute orientation from complex representation, for testing. */
  def computeOrientation(stickTensor: Complex): Float = {
    val rawOrientation = stickTensor.phase / 2
    if (rawOrientation <= 0)
      rawOrientation + math.Pi.toFloat
    else
      rawOrientation
  }

  /** Compute orientation from complex representation, for testing. */
  def computeStickness(stickTensor: Complex): Float =
    stickTensor.magnitude
}