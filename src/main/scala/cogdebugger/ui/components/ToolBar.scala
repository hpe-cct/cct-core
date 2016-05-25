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

import javax.swing.{JToolBar, SwingConstants}
import scala.swing._

/**
 * A wrapper for a JToolBar.
 * @author Tobin Gonzalez
 */
class ToolBar(title: String, orientation: Orientation.Value)
        extends Component with SequentialContainer.Wrapper {

  /** Builds a horizontal toolbar with no title. */
  def this() = this(null, Orientation.Horizontal)

  /** Builds a horizontal toolbar with the given title. */
  def this(title: String) = this(title, Orientation.Horizontal)

  /** Builds a toolbar with the given orientation and no title. */
  def this(orientation: Orientation.Value) = this(null, orientation)

  override lazy val peer =
    new JToolBar(title, orientation.id) with SuperMixin // Mixin necessary?

  /** Adds a separator to the end of the toolbar */
  def addSeparator() { peer.addSeparator() }
  def addSeparator(size: Dimension) { peer.addSeparator(size) }

  /** Enables/disables re-docking of the toolbar */
  def floatable_=(floatable: Boolean) { peer.setFloatable(floatable) }
  def floatable = peer.isFloatable

  /** Enables/disables button rollover animations */
  def rollover_=(rollover: Boolean) { peer.setRollover(rollover) }
  def rollover = peer.isRollover

  def borderPainted_=(painted: Boolean) { peer.setBorderPainted(painted) }
  def borderPainted = peer.isBorderPainted
  
  def add(components: Component*) { add(components.toArray) }
  def add(components: Array[Component]) { for (component <- components) contents += component }

  def remove(component: Component) { contents -= component }

  def margin_=(insets: Insets) { peer.setMargin(insets) }
  def margin = peer.getMargin
}

object TestToolbar {//extends SimpleSwingApplication {
  import scala.swing._
  
  //override lazy val top = new MainFrame {
  lazy val top = new MainFrame {
    title = "Test ToolBar.scala ["+getClass.getPackage.getName+"]"

    val toolbar = new ToolBar("Title") {
      contents += new Label("<== Drag this handle!")
      addSeparator(new Dimension(50, 0))
      contents += new Button("Button1")
      contents += new Button("Button2")
    }
    
    val view = new Label("VIEW") { preferredSize = new Dimension(640, 480) }
    
    contents = new BorderPanel() {
      add(toolbar, BorderPanel.Position.North)
      add(view, BorderPanel.Position.Center)
    }
  }

  def main(args: Array[String]) {
    Swing.onEDT {
      top.pack()
      top.visible = true
    }
  }
}