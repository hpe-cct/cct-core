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

package cogx.runtime.execution

import cogx.runtime.FieldID
import cogx.platform.cpumemory.AbstractFieldMemory

/** Messages used by all supervisor actors.
  *
  * @author Greg Snider
  */
private[runtime]
object SupervisorMessages {

  /** Request (down) to advance simulation one step. */
  case object Step

  /** Response (up) that Step request is done. */
  case class StepDone(e: Option[Exception])

  /** Request used internally by Evaluator to implement running. */
  case object Run

  /** Request (down) to reset simulation state. */
  case object Reset

  /** Response (up) that reset has been done. */
  case class ResetDone(e: Option[Exception])

  /** Request (down) for current state of field. */
  case class  ProbeField(id: FieldID)

  /** Response (up) with current state of field. */
  case class  ProbeData(id: FieldID, data: AbstractFieldMemory)

  /** Field data exchanged between distributed actors. */
  case class  FieldData(id: FieldID, data: AbstractFieldMemory)

  // Temporary, for debugging actor hierarchy.
  case object DebugPrint
}