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

package cogx.platform.opencl

import cogx.platform.types.FieldRegister

/** A register than holds the data for a field.
  *
  * The register can act like either a master/slave flip-flop or a latch,
  * depending on how it's constructed. Externally it always looks like a
  * flip-flop (since it has both master and slave methods), but for latches
  * the master and slave are the same.
  *
  * @param buffer0 First buffer, needed by both flip-flops and latches.
  * @param buffer1 Second buffer, needed only by flip-flops.
  *
  * @author Greg Snider
  */
private[cogx]
class OpenCLFieldRegister(buffer0: OpenCLBuffer[_], buffer1: OpenCLBuffer[_] = null)
  extends FieldRegister[OpenCLBuffer[_]](buffer0, buffer1)
