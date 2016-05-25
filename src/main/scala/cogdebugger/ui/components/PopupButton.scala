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

import scala.swing._
import javax.swing.JPopupMenu
import javax.swing.event.PopupMenuListener

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 2/4/13
 * Time: 3:46 PM
 *
 * An attempt to build a custom drop-down menu button. Swing already has
 * dropdown menus, but they can only be added to a JMenuBar, which must be
 * installed at the top of the window/frame. I'd rather make use of space on
 * our existing toolbars, and allow for custom menu items to be installed.
 */

/** A button that, when clicked, pops up a menu anchored at this button (as
  * opposed to the mouse's current position).
  * 
  * Add scala.swing.Menu Menus and scala.swing.MenuItem MenuItems to
  * it just like you would an ordinary menu. */
class PopupButton(label: String) extends Button(label) {
  val menu = new PopupMenu
  action = Action(label) {
    menu.show(PopupButton.this, 0, PopupButton.this.bounds.height)
  }
  def contents = menu.contents
}


// The below classes were submitted for integration with the Scala Swing
// library, but hadn't been accepted by the time 2.9.2 was released. They'll
// probably show up in 2.10.

// Bizarrely, PopupMenu was part of 2.10.1-RC1, but not the final 2.10.1

object PopupMenu {
  private[PopupMenu] trait JPopupMenuMixin { def popupMenuWrapper: PopupMenu }
}

/**
 * A popup menu.
 *
 * Example usage:
 *
 * {{{
 * val popupMenu = new PopupMenu {
 *   contents += new Menu("menu 1") {
 *     contents += new RadioMenuItem("radio 1.1")
 *     contents += new RadioMenuItem("radio 1.2")
 *   }
 *   contents += new Menu("menu 2") {
 *     contents += new RadioMenuItem("radio 2.1")
 *     contents += new RadioMenuItem("radio 2.2")
 *   }
 * }
 * val button = new Button("Show Popup Menu")
 * reactions += {
 *   case e: ButtonClicked => popupMenu.show(button, 0, button.bounds.height)
 * }
 * listenTo(button)
 * }}}
 *
 * @see javax.swing.JPopupMenu
 */
class PopupMenu extends Component with SequentialContainer.Wrapper with Publisher {
  override lazy val peer: JPopupMenu = new JPopupMenu with PopupMenu.JPopupMenuMixin with SuperMixin {
    def popupMenuWrapper = PopupMenu.this
  }

  peer.addPopupMenuListener(new PopupMenuListener {
    def popupMenuWillBecomeVisible(e: javax.swing.event.PopupMenuEvent) {
      publish(PopupMenuWillBecomeVisible(PopupMenu.this))
    }
    def popupMenuWillBecomeInvisible(e: javax.swing.event.PopupMenuEvent) {
      publish(PopupMenuWillBecomeInvisible(PopupMenu.this))
    }
    def popupMenuCanceled(e: javax.swing.event.PopupMenuEvent) {
      publish(PopupMenuCanceled(PopupMenu.this))
    }
  })

  def show(invoker: Component, x: Int, y: Int) { peer.show(invoker.peer, x, y) }

  def margin: Insets = peer.getMargin
  def label: String = peer.getLabel
  def label_=(s: String) { peer.setLabel(s) }
}

abstract class PopupMenuEvent extends scala.swing.event.ComponentEvent

case class PopupMenuWillBecomeVisible(source: PopupMenu) extends PopupMenuEvent

case class PopupMenuWillBecomeInvisible(source: PopupMenu) extends PopupMenuEvent

case class PopupMenuCanceled(source: PopupMenu) extends PopupMenuEvent

