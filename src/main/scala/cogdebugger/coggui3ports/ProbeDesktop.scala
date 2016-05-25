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

import org.interactivemesh.scala.swing.{InternalFrame, InternalDesktopPane}
import org.interactivemesh.scala.swing.LayeredPane.LayerConstraints
import cogdebugger.ui.fieldvisualizations.Zoomable
import scala.swing._
import cogdebugger.ProbeManager

/**
 * A desktop on which to place internal frames displaying Probe data.
 * <p>
 * There are some subtleties here that arise from the way Swing is implemented.
 * All Swing GUI events are processed on the "Swing event thread," and that
 * includes all methods of this object. This should be obvious for most
 * methods since they are called from GUI components such as Buttons.
 * <p>
 * The basic protocol of this module is simple. When a new Probe is requested
 * by a user command (such as a button click), the ProbeDesktop creates a
 * ProbeFrame where that Probe can be viewed.
 *
 * @author Greg Snider
 */
class ProbeDesktop(probeManager: ProbeManager)
    extends BorderPanel {

  protected val toolbar = new ZoomingToolbar("Probes", zoomIn(), zoomOut()) {
    floatable = false
    val closeAllButton = Button("Close All") {
      for (frame <- frames )
        frame.closed = true
      tiler.reset(desktop.size)
    }
    closeAllButton.tooltip =
            "Close all frames open on the desktop and reset the frame tiler."

    val readProbesButton = Button("Read probes") { probeManager.readProbes() }
    readProbesButton.tooltip = "Request that any installed Probes be read out."

    add(closeAllButton)
    add(readProbesButton)
  }

  protected lazy val desktop = new WrappedInternalDesktopPane

  def desktopWidth = desktop.size.width
  def desktopHeight = desktop.size.height

  /** All the InternalFrames currently on the desktp. */
  def frames = desktop.contents.collect { case f: InternalFrame => f }

  desktop.dragMode = InternalDesktopPane.DragMode.Outline

  // Initialize
  add(toolbar, BorderPanel.Position.North)
  add(desktop, BorderPanel.Position.Center)

  // Init the ProbeFrame tiler to the current desktop size. Lazily evaluated so
  // the desktop has actually established its size before we build the tiler.
  lazy val tiler = new Tiler(desktop.size)

  /** Subscribe the tiler to `frame` and add the frame to the desktop. */
  def addFrame(frame: InternalFrame) {
    tiler.listenTo(frame.frame)
    desktop.publicAdd(frame, new LayerConstraints)
    frame.visible = true
  }

  /** Zoom any zoomable frames on the desktop. Here zoomable frames are those
    * that mix in [[cogdebugger.ui.fieldvisualizations.ZoomProperty]] directly
    * or those that have a Zoomable as their first child component.
    */
  private def zoomIn() {
    frames.collect {
      case z: Zoomable => z.zoomIn()
      case f: InternalFrame if f.contents.head.isInstanceOf[Zoomable] =>
        f.contents.head.asInstanceOf[Zoomable].zoomIn()
    }
  }

  /** Unzoom any zoomable frames on the desktop. Here Zoomable frames are those
    * that implement [[cogdebugger.ui.fieldvisualizations.Zoomable]] directly
    * or those that have a Zoomable as their first child component.
    */
  private def zoomOut() {
    frames.collect {
      case z: Zoomable => z.zoomOut()
      case f: InternalFrame if f.contents.head.isInstanceOf[Zoomable] =>
        f.contents.head.asInstanceOf[Zoomable].zoomOut()
    }
  }

}