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

package cogx.runtime

/** Unique identifier for a field.
  *
  * A field is produced by a kernel, but a kernel can have multiple outputs,
  * each of which produces a field. The field is thus uniquely identified by
  * the (kernel, output) which produced it.
  *
  * @param kernelID The ID for the kernel which produces this field.
  * @param kernelOutput The index of the kernel output that produces this field.
  *
  * @author Greg Snider
  */
private[cogx]
case class FieldID(kernelID: Int, kernelOutput: Int)
