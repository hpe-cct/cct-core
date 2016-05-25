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
import cogdebugger.ui.fieldvisualizations.Zoomable
import java.awt.event.{MouseEvent, AWTEventListener}
import java.awt.{Toolkit, AWTEvent}
import javax.swing.{JComponent, SwingUtilities}
import scala.swing.event.{UIElementShown, UIElementHidden}
import java.awt.geom.Point2D

/** A mixin that enables right-click-and-drag to adjust the magnification of
  * a Zoomable component.
  *
  * This style of zoom level control lends itself to non-integral
  * multiplicative zoom changes - e.g. each pixel of drag makes the view 10%
  * larger, as opposed to 1x, 2x, 3x normal size. Note however that not every
  * Swing component (including some of our own visualizations) draw with
  * anti-aliasing enabled - this will become very noticable with non-integral
  * zoom.
  *
  * Created by gonztobi on 3/31/2014.
  */
trait MouseDragZoom {
  self: Component with Zoomable =>

  /** True if right mouse button is currently held down, false otherwise. */
  private var rightMouseDown = false
  /** Screen space location of the last right-mouse button event. */
  private var rightMouseLastAt: Point = null

  /** Component that generated the mouse down event. */
  private var targetComp: java.awt.Component = null
  /** Screen space coordinate of where the mouse down event occured. */
  private var mouseDownAt: Point = null
  /** Location of the mouse down event inside the target component. */
  private val normalizedCenterOfZoom = new Point2D.Float()

  /* We use an AWTEventListener because it will receive mouse events
   * regardless of whether or not mouse listeners are registered on child
   * components. This lets us see mouse events for zooming purposed even if
   * tooltips are enabled on children.
   */
  private val zoomListener = new AWTEventListener {
    override def eventDispatched(event: AWTEvent): Unit = {
      event match {
        case mouseEvent: MouseEvent =>
          if (SwingUtilities.isRightMouseButton(mouseEvent) &&
              (peer.isAncestorOf(mouseEvent.getComponent) ||
              (peer eq mouseEvent.getComponent))) {
            mouseEvent.getID match {
              case MouseEvent.MOUSE_PRESSED  => startDrag(mouseEvent)
              case MouseEvent.MOUSE_DRAGGED  => drag(mouseEvent)
              case MouseEvent.MOUSE_RELEASED => endDrag(mouseEvent)
              case _ =>
            }
            rightMouseLastAt = mouseEvent.getLocationOnScreen
          }
        case _ =>
      }
    }
  }
  
  // Add/remove the zoom listener as the component is shown and hidden
  listenTo(mouse.moves, mouse.clicks, self)
  reactions += {
    case UIElementHidden(`self`) => uninstallListener()
    case UIElementShown(`self`)  => installListener()
  }

  installListener()

  private def uninstallListener() {
    Toolkit.getDefaultToolkit.removeAWTEventListener(zoomListener)
  }

  private def installListener() {
    if (!Toolkit.getDefaultToolkit.getAWTEventListeners(PanPane.EventMask).contains(zoomListener))
      Toolkit.getDefaultToolkit.addAWTEventListener(zoomListener, PanPane.EventMask)
  }

  private def startDrag(mouseEvent: MouseEvent) {
    rightMouseDown = true
    mouseDownAt = mouseEvent.getLocationOnScreen

    // We need to store where the mouse was clicked in a size invariant way
    // so that we can correctly translate the view after scaling it.
    targetComp = mouseEvent.getComponent
    val targetSize = targetComp.getSize
    val halfPixelPitchNormalizedWidth  = 1f / targetSize.width  / 2
    val halfPixelPitchNormalizedHeight = 1f / targetSize.height / 2
    normalizedCenterOfZoom.setLocation(
      mouseEvent.getPoint.getX / targetSize.width  + halfPixelPitchNormalizedWidth,
      mouseEvent.getPoint.getY / targetSize.height + halfPixelPitchNormalizedHeight
    )
    
    cursor = java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.N_RESIZE_CURSOR)
  }

  private def drag(mouseEvent: MouseEvent) {
    // Apply zoom based on mouse movement
    val point = mouseEvent.getLocationOnScreen
    val dy = point.getY - rightMouseLastAt.y
    if (dy > 0) zoomIn()
    else if (dy < 0) zoomOut()

    // Zooming will have likely moved stuff around. Find where the center of //
    // zoom has moved to and translate it back to its starting point.        //

    // Use `preferredSize` rather than `size` - the latter won't change until
    // the panel has finished revalidating and repainting (when that happens is
    // up to Swing), but preferredSize can be calculated immediately.
    val newSize = preferredSize
    val newX = normalizedCenterOfZoom.x * newSize.width
    val newY = normalizedCenterOfZoom.y * newSize.height
    val newPoint = new Point(math.round(newX), math.round(newY))
    SwingUtilities.convertPointToScreen(newPoint, targetComp)
    val offsetX = newPoint.x - mouseDownAt.x
    val offsetY = newPoint.y - mouseDownAt.y
    peer.setLocation(location.x - offsetX, location.y - offsetY)
    peer.getParent.doLayout()
  }
  
  private def endDrag(mouseEvent: MouseEvent) {
    rightMouseDown = false
    cursor = java.awt.Cursor.getPredefinedCursor(java.awt.Cursor.DEFAULT_CURSOR)
  }

}