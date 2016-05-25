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

package cogdebugger.ui.components

import swing._

/** A panel that displays an infinite progress bar at its center, with an
  * optional label.
  *
  * Drop this panel into your GUI where you want to put components that take a
  * long time to initialize. Once said expensive component has finished its
  * initialization, swap it with your LoadingAnimationPanel. */
class LoadingAnimationPanel(label: String) extends BoxPanel(Orientation.Vertical) {
  def this() = this(null)

  contents += Swing.VGlue

  if (label != null)
    contents += new Label(label) {
      horizontalAlignment = Alignment.Center
      peer.setAlignmentX(0.5f)
    }

  val hBox = new BoxPanel(Orientation.Horizontal)
  hBox.contents += Swing.HGlue
  hBox.contents += Component.wrap(new javax.swing.JProgressBar() { setIndeterminate(true) })
  hBox.contents += Swing.HGlue

  contents += hBox
  contents += Swing.VGlue
}
