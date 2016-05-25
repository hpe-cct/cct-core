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

/** Interface to a state machine that can be "clocked" (advance one state) and
  * "reset" (return to an initial state).
  *
  * User: Greg Snider
  * Date: May 6, 2010
  * Time: 2:03:17 PM
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private [utilities] trait Clockable {
  /** Advance state machine one step. */
  def clock: Unit

  /** Reset state machine to an initial state. */
  def reset: Unit
}