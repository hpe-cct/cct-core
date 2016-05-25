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

import javax.swing.SwingUtilities
import scala.swing._
import java.awt.{Toolkit, AWTEvent, Cursor}
import java.awt.event.{MouseEvent, AWTEventListener}

/** A ScrollPane that scrolls in response to mouse drags on its viewport, in
  * addition to the standard interactions with its scrollbars. The captured
  * mouse drag events act on the scrollbars directly, so mouse-pan and
  * scrollbar-pan should stay in sync.
  *
  * ---
  *
  * Note: This is the most robust implementation so far, in that children with
  * tooltips enabled won't interfere with the mouse drag. It's built using an
  * AWTEventListener, which gets ALL mouse events produced by the app,
  * regardless of whether another listener somewhere is getting the events
  * first (e.g. a tooltip enabled child). One small quirk of this approach is
  * that you can initiate a pan by dragging from the blank corner formed by the
  * horizontal and vertical scrollbars.
  *
  * Created by gonztobi on 3/11/14.
  */
class PanPane extends ScrollPane {

  def this(c: Component) = {
    this()
    contents = c
  }

  private var mouseDown = false
  private var mouseDownAt: Point = null
  private var mouseLastAt: Point = null

  private val listener = new AWTEventListener {
    override def eventDispatched(event: AWTEvent): Unit = {
      event match {
        case me: MouseEvent =>
          val convertedPoint = SwingUtilities.convertPoint(me.getComponent, me.getPoint, peer)
          if (mouseDown) {
            if (SwingUtilities.isLeftMouseButton(me) && me.getID == MouseEvent.MOUSE_RELEASED) {
              mouseDown = false
              cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
            } else if (SwingUtilities.isLeftMouseButton(me) && me.getID == MouseEvent.MOUSE_DRAGGED) {
              // Getting mouse events directly from the child while it's moving makes for jittery
              // movement. Converting to the root pane's coordinate system first makes things much
              // smoother.
              val dx = convertedPoint.x - mouseLastAt.x
              val dy = convertedPoint.y - mouseLastAt.y
              horizontalScrollBar.value -= dx
              verticalScrollBar.value   -= dy
              mouseLastAt = convertedPoint
            }

          } else if ((me.getComponent eq peer) || peer.getViewport.isAncestorOf(me.getComponent)) {
            if (SwingUtilities.isLeftMouseButton(me) && me.getID == MouseEvent.MOUSE_PRESSED) {
              mouseDown = true
              mouseDownAt = convertedPoint
              mouseLastAt = mouseDownAt
              cursor = Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR)
            }
          }
        case _ =>
      }
    }
  }

  // I wanted to remove the event listener when it's not needed (no scrollbars)
  // but it doesn't play well with mouse-drag-to-zoom. It looks like the
  // listener gets registered several times when scrollbars reappear, despite
  // the containment checks.
//  private val (hScrollBar, vScrollBar) =
//    (horizontalScrollBar, verticalScrollBar)
//
//  listenTo(hScrollBar, vScrollBar)
//  reactions += {
//    case swing.event.UIElementShown(`hScrollBar`) => installListener()
//    case swing.event.UIElementShown(`vScrollBar`) => installListener()
//    case swing.event.UIElementHidden(`hScrollBar`) =>
//      if (!vScrollBar.visible) { removeListener() }
//    case swing.event.UIElementHidden(`vScrollBar`) =>
//      if (!hScrollBar.visible) { removeListener() }
//  }
//
//  if (hScrollBar.visible || vScrollBar.visible)
//    installListener()

  installListener()

  private def installListener() {
    if (!Toolkit.getDefaultToolkit.getAWTEventListeners.contains(listener))
      Toolkit.getDefaultToolkit.addAWTEventListener(listener, PanPane.EventMask)
  }

  private def removeListener() {
    Toolkit.getDefaultToolkit.removeAWTEventListener(listener)
  }

}

object PanPane {
  private[cogdebugger] val EventMask: Long =
    AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK
}