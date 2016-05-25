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

package cogdebugger.ui.fieldvisualizations

import libcog._
import scala.swing.Panel

/** Interface for a subpanel within a larger panel.
  *
  * @author Greg Snider
  */
trait Subpanel extends Zoomable {
  /** Update the subpanel's display.
    *
    * @param src The owner of the subpanel.
    * @param data The new field data to be displayed.
    * @param time Current simulation time.
    */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long): Unit

//  /** Change zooming level by factor `delta` */
//  def changeZoomLevel(delta: Float): Unit
}