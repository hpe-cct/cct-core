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

package cogdebugger.coggui3ports

import swing.{BorderPanel, SimpleSwingApplication, MainFrame, Label}
import cogdebugger.ui.components.ToolBar

/**
 *
 * @author Greg Snider
 */

class ZoomingToolbar(title: String, zoomIn: => Unit, zoomOut: => Unit)
    extends ToolBar {
  if (title != "")
    add(new Label(title + "    "))
  add(new ClearButton("+", zoomIn))
  add(new ClearButton("-", zoomOut))

  def this(zoomIn: => Unit, zoomOut: => Unit) =
    this("", zoomIn, zoomOut)
}

/**
 * Test code for the ZoomingToolbar class.
 */
object TestZoomingToolbar extends SimpleSwingApplication {
  override lazy val top = new MainFrame {
    def zoomIn() { println("zoom in") }
    def zoomOut() { println("zoom out") }
    title = "Test ZoomingToolbar"
    contents = new BorderPanel {
      add(new ZoomingToolbar("Toolbar", zoomIn(), zoomOut()),
        BorderPanel.Position.North)
    }
  }

  override def main(args: Array[String]) { super.main(args) }
}