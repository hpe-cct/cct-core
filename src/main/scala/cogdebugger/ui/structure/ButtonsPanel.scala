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

package cogdebugger.ui.structure

import libcog.ComputeGraph
import cogdebugger.ui.components.WrapPanel
import scala.swing.{Button, Dimension}
import scala.swing.event.MouseClicked
import java.awt.event.MouseEvent.{BUTTON1, BUTTON3}

/**
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 7/26/13
 * Time: 2:22 PM
 */

/** A simpler alternative to [[cogdebugger.ui.structure.GraphViewer]]. Just a
  * big list of buttons, one per ProbedField in the given Cog model/app.
  * Buttons are labelled with the long name of the field they represent, and
  * appear sorted by that name. Clicking a button raises an
  * [[cogdebugger.ui.structure.InteractiveGraphEvent]] event indicating the
  * clicked field. */
class ButtonsPanel(computeGraph: ComputeGraph) extends WrapPanel {
  val buttons = collection.mutable.Buffer[Button]()
  computeGraph.probedCircuit.traversePreorder(field => {
    val b = new Button(field.name.mkString("."))
    b.listenTo(b.mouse.clicks)
    b.reactions += {
      case mc: MouseClicked => mc.peer.getButton match {
        case BUTTON1 => ButtonsPanel.this.publish(VertexLeftClick(field))
        case BUTTON3 => ButtonsPanel.this.publish(VertexRightClick(field))
        case _ =>
      }
    }
    buttons += b
  })

  // Make all buttons the same width
  val maxWidth = buttons.foldLeft(0)((maxWidth, button) => maxWidth max button.preferredSize.width)
  buttons foreach { button =>
    val oldSize = button.preferredSize
    button.preferredSize = new Dimension(maxWidth, oldSize.height)
  }

  contents ++= buttons.sortBy(_.text)
}
