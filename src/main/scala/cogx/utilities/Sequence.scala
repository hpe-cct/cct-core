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


/** A Sequence is an array or vector of real values, but the data storage
  * for the values may not be stored contiguously. This allows us to "pull"
  * vectors out of higher-order tensors and examine (but not modify)
  * them in place.
  *
  * @author Greg Snider
  */

@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private trait Sequence {
  val length: Int
  def apply(index: Int): Double
}