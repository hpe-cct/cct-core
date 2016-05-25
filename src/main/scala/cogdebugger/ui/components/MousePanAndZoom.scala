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
import java.awt.geom.Point2D
import scala.swing.event._
import java.awt.{BasicStroke, Graphics2D}
import javax.swing.SwingConstants
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased
import scala.swing.event.UIElementResized
import scala.swing.event.MouseDragged
import cogdebugger.ui.fieldvisualizations.Zoomable

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 7/1/13
 * Time: 2:30 PM
 */

/** Augments a panel with mouse pan and zoom capability (sort of like your
  * favorite online maps application).
  *
  * Rather than mixing in this trait directly to some panel displaying the
  * visual that you want to pan and zoom, you should wrap that panel in another
  * panel and mix in this trait to the wrapper. This helps guarantee clean
  * redraws of the panel (otherwise a screen clear can "miss" due to the
  * applied pan and zoom). */
trait MousePanAndZoom extends Component with Zoomable {

  protected var (panOffsetX, panOffsetY) = (0.0, 0.0) // Pan offset
  protected var zoom = 1.0                            // Magnification level of image
  var preserveLineThickness = true
  var stroke = new BasicStroke(1 / zoom.toFloat)

  protected val mouseDownLoc = new Point2D.Double()  // Location where mouse clicked
  protected val lastMouseLoc = new Point2D.Double()  // Last location of mouse
  protected var rightMouseDown = false    // Last mouse down was/wasn't a right-click

  //protected val lastSize = new Dimension(-1, -1)
  protected var lastSize: Option[Dimension] = None

  var zDelta = 1.1f
  def zoomLevel = zoom
  def zoomLevel_=(zoom: Int) { this.zoom = zoom; stroke = new BasicStroke(1 / zoom.toFloat) }
  override def zoomIn() { zoomView(zDelta) }
  override def zoomOut() { zoomView(1 / zDelta) }
  def changeZoomLevel(delta: Float) {
    // Our viewers that don't use mouse pan and zoom usually add or subtract
    // the delta to their current zoom level. To zoom in, you use a positive
    // delta, and zoom out with negative. This class however, multiplies
    // in the delta, so we have to handle the delta a little differently.
    // We also don't want to multiply zoom by 0, ever.
    if (delta > 0)
      zoomView(delta)
    else if (delta < 0)
      zoomView(1 / (-delta))
//    val centerOfZoom = new Point2D.Float(0, 0)
//    if (delta > 0)
//      zoomView(centerOfZoom, delta)
//    else if (delta < 0)
//      zoomView(centerOfZoom, 1 / (-delta))
  }

  def resetView() {
    // TODO Center image instead of just setting panX/Y to zero
    panOffsetX = 0; panOffsetY = 0
    zoom = 1
    repaint()
  }

  /** Sets current zoom factor; zooms about screen center */
  def setZoom(zoomFactor: Double) {
    zoomView(zoomFactor / zoom)
  }

  /** Multiplies current zoom level by amt; zooms about screen center */
  def zoomView(amt: Double) {
    val centerX = size.getWidth / 2
    val centerY = size.getHeight / 2
    zoomView(new Point2D.Double(centerX, centerY), amt)
  }

  /** Zooms about last mouse-down location; zoom amount is determined by the
    *  delta between the current mouse location and the mouse down location. */
  def zoomView(newMouseX: Float, newMouseY: Float) {
    val dz = if (newMouseY - lastMouseLoc.getY.toFloat > 0)
      1.1f
    else if (newMouseY - lastMouseLoc.getY.toFloat < 0)
      1 / 1.1f
    else
      1

    // Translate mouse coords into image coords
    val mousex = mouseDownLoc.getX// - panOffsetX
    val mousey = mouseDownLoc.getY// - panOffsetY

    zoomView(new Point2D.Double(mousex, mousey), dz)
  }

  /** Multiplies current zoom level by amt; zooms about the given point in
    * image coordinates. */
  def zoomView(aboutPoint: Point2D, amt: Double) {

    // Effective Panel coords
    val px = aboutPoint.getX - panOffsetX
    val py = aboutPoint.getY - panOffsetY

    // Adjust pan amount to account for zoom
    panOffsetX -= px * amt - px
    panOffsetY -= py * amt - py

    zoom *= amt

    repaint()
  }

  /** Pans the view according to mouse movement. */
  def panView(newMouseX: Double, newMouseY: Double) {
    panOffsetX += newMouseX - lastMouseLoc.getX
    panOffsetY += newMouseY - lastMouseLoc.getY
    repaint()
  }

  listenTo(this, mouse.clicks, mouse.moves)
  reactions += {

    // Implement mouse pan/zoom ///////////////////////////////////////////////
    case e: MousePressed => {
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
        rightMouseDown = true
      else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON2)
        resetView()
      mouseDownLoc.setLocation(e.point)
      lastMouseLoc.setLocation(e.point)
    }
    case MouseDragged(src, point, mods) => {
      if (rightMouseDown)
        zoomView(point.x, point.y)
      else
        panView(point.x, point.y)
      lastMouseLoc.setLocation(point)
      repaint()
    }
    case e: MouseReleased => {
      if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3)
        rightMouseDown = false
    }

    // Manage additional/reduced space from window resizing ///////////////////

    case UIElementResized(source) if source == this => {
      // Keep the view centered in the panel as it resizes.
      lastSize match {
        case Some(dim) =>
          val (dx, dy) = (size.width - dim.width, size.height - dim.height)
          dim.setSize(size)
          panOffsetX += dx / 2f
          panOffsetY += dy / 2f
        case None =>
          lastSize = Option(new Dimension(size))
      }
    }
  }

  /**
   * Computes which cells are visible at the current pan and zoom settings
   * given a cell size in pixels. Cells are assumed to be square.
   * @param cellSize Width (or height) of a the square cell, in pixels
   * @param rows Number of rows of cells
   * @param cols Number of columns of cells
   * @return (firstRow, firstCol, lastRow, lastCol)
   */
  def getVisibleCellBounds(cellSize: Int, rows: Int, cols: Int) = {

    val x = panOffsetX
    val y = panOffsetY
    val w = bounds.width
    val h = bounds.height
    val ecs = cellSize * zoom // effective cell size

    val firstRow = scala.math.floor(-y / ecs).toInt
    val firstCol = scala.math.floor(-x / ecs).toInt

    val lastRow =
      if (y <= 0)
        scala.math.ceil(h / ecs).toInt + 1 + firstRow
      else
        scala.math.ceil((h - y) / ecs).toInt
    val lastCol =
      if (x <= 0)
        scala.math.ceil(w / ecs).toInt + 1 + firstCol
      else
        scala.math.ceil((w - x) / ecs).toInt

    // Clamp values before returning
    //(firstRow max 0, firstCol max 0, lastRow min rows, lastCol min cols)

    // Clamping as above makes it difficult to blit from a BufferedImage
    // (a draw buffer) instead of using g.draw* calls.
    (firstRow, firstCol, lastRow, lastCol)
  }


//  /**
//   * Classes that implement this trait and override paintComponent should call
//   * super.paintComponent immediately to apply the pan and zoom. Alternatively,
//   * if you don't want to call super.paintComponent, you can immediately apply
//   * the following transforms to the Graphics2D object to do the pan and zoom:
//   * <pre>
//   *   g.translate(panOffsetX, panOffsetY)
//   *   g.transform(zoomTransform)
//   * </pre>
//   * @param g Graphics2D object to draw with
//   */
//  abstract override def paintComponent(g: Graphics2D) {
//
//    println(f"in PAINTCOMPONENT! [pan($panOffsetX%.2f, $panOffsetY%.2f), zoom($zoomLevel%.2f)]")
//
//    // Clear any previous drawing in this panel
//    super.paintComponent(g)
//
//    // Apply transforms to the graphics object, which will be passed to child
//    // components when they do their painting.
//    g.translate(panOffsetX, panOffsetY)
//    g.scale(zoom, zoom)
//    if (preserveLineThickness) g.setStroke(new BasicStroke(1 / zoom.toFloat))
//  }

  override def paintChildren(g: Graphics2D) {

    // Our translating/zooming means children don't know exactly what part of
    // the screen needs clearing, so we have to do it ourselves.
    val oldColor = g.getColor
    g.setColor(background)
    g.fillRect(0, 0, bounds.width, bounds.height) // Clear old drawings
    g.setColor(oldColor)

    g.translate(panOffsetX, panOffsetY)
    g.scale(zoom, zoom)
    if (preserveLineThickness) g.setStroke(stroke)
    super.paintChildren(g)
  }

  // TODO: Maybe add scroll bars and things?
  // Probably better to just drop your drawing inside a scrollpane in that
  // case, no? Just make sure preferredSize is set correctly.

}

object MousePanAndZoom {
  def wrap(panel: Panel) = {
    val p = new BorderPanel with MousePanAndZoom
    p.layout(panel) = BorderPanel.Position.Center
    p
  }
}

object Test {

  def main(args: Array[String]) {
    Swing.onEDT {
      val frame = top
      frame.visible = true
      println("Wrapper size after showing: "+frame.wrapper.size)
    }
  }

  lazy val top = new MainFrame() {
    title = "Test"

    val panel = new Panel {
      preferredSize = new Dimension(100, 100)
      override def paintComponent(g: Graphics2D) {
        super.paintComponent(g)
        g.fillRect(10, 10, 80, 80)
      }
    }

    //val wrapper = MousePanAndZoom.wrap(panel)
    val wrapper = MousePanAndZoom.wrap(panel) //new PanAndZoomPanel(panel)
    println("wrapper size: "+wrapper.size)
    contents = wrapper
  }
}

object TestingStuff extends SimpleSwingApplication {
  lazy val top = new MainFrame() {

    println("Creating new MainFrame.")
    println("  Using layout manager: "+peer.getLayout)

//    val label = new Label("TEST TEXT")
//    contents = new CustomPanel(label)

//    contents = new ColoredSquarePanel(20, new Color(0, 255, 0))

//    contents = new GridPanel(1, 1) with MousePanAndZoom {
//      contents += new ColoredSquarePanel(20, new Color(255, 0, 0))
////      contents += new ColoredSquarePanel(20, new Color(0, 255, 0))
//    }

    //contents = new PanAndZoomPanel(new ColoredSquarePanel(20, new Color(0, 255, 0)))
    contents = MousePanAndZoom.wrap(new ColoredSquarePanel(20, new Color(0, 255, 0)))

//    contents = new BoxPanel(Orientation.Horizontal) {
//      contents += Swing.HGlue
//      contents += new ColoredSquarePanel(20, new Color(0, 255, 0))
//      contents += Swing.HGlue
//    }



  }

  class CustomPanel(wrapped: Component) extends GridPanel(1, 1) {
    background = new Color(255, 0, 0)
    contents += wrapped
  }

  class ColoredSquarePanel(size: Int, color: Color) extends Panel {

    println("Creating new ColoredSquarePanel.")
    println("  Using layout manager: "+peer.getLayout)

    preferredSize = new Dimension(size, size)
    //maximumSize = new Dimension(size, size)

    println("  Preferred size is: "+preferredSize)

    //background = new Color(200, 0, 100) // Pink background for debugging

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      g.setColor(color)
      g.fillRect(0, 0, size, size)
    }
  }

}
