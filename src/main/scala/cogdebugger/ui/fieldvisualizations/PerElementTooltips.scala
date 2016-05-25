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

import cogdebugger.ui.components.DoubleBufferedImagePanel
import javax.swing.JPanel
import java.awt.event.MouseEvent

/** A mixin for DoubleBufferedImagePanel that enables dynamic tooltips.
  *
  * You can set a static tooltip for a Swing component that will be displayed
  * any time the mouse comes to rest somewhere over that component. This mixin
  * does things slightly different - the tooltip String that pops up can vary
  * depending on the coordinates of the mouse relative to the component.
  *
  * Created by gonztobi on 4/22/2014.
  */
trait PerElementTooltips {
  self: DoubleBufferedImagePanel =>

  // We have to set `tooltip` to a non-null String in order for Swing to enable
  // tooltips. The actual text that gets shown is determined by the `popupText`
  // method.
  tooltip = "Enable"

  /** Optionally returns a String whose contents can vary depending on the
    * coordinates of the `mouseEvent` argument. No tooltip of any kind is
    * displayed by Swing in the event that this method returns None. */
  def popupText(mouseEvent: MouseEvent): Option[String]

  override lazy val peer = new JPanel with SuperMixin {
    override def getToolTipText(mouseEvent: MouseEvent): String = {
      popupText(mouseEvent).getOrElse(null)
    }
  }

}
