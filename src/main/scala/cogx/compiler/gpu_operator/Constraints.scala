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

package cogx.compiler.gpu_operator

/** Implementation-imposed constraints on various parameters
 *
 * @author Greg Snider
 */
private[cogx]
trait Constraints {
  /** Maximum size of tensor, minimizing register pressure for kernel fusion.*/
  val MaxTensorSize = 4
  /** Upper bound on number of dimensions for an array. */
  val MaxArrayDimensions = 3
}
