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

package cogx.platform.constraints

/** Parameters about fields that must be enforced by the compiler.
  *
  * @author Greg Snider
  */
private[cogx]
trait FieldLimits {

  /** Maximum number of dimensions in tensor fields. */
  val MaxFieldDimensions = 3

  /** Maximum supported tensor order for tensors in field. */
  val MaxTensorOrder = 2
}