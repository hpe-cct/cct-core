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

import libcog._
import scala.swing.{FlowPanel, Panel}
import scala.language.reflectiveCalls
import java.awt.{Color, BasicStroke, Graphics2D, Dimension}
import java.awt.RenderingHints._
import java.awt.geom.AffineTransform
import cogdebugger.PropertyValueChanged

/**
 * Created with IntelliJ IDEA.
 * User: gonzatob
 * Date: 9/6/12
 * Time: 2:22 PM
 *
 * This abstract class takes care of much of the drawing logic needed to render
 * fields that can be visualized as a grid of distinct elements (such as vector
 * fields or dyad fields). It supports pan, zoom, and save/restore
 * functionality, and can handle 0D, 1D, 2D, and 3D fields.
 *
 * Concrete viewer implementations will need to define three methods. The first
 * is update, which runs before any drawing is done to allow a client to do
 * such things as compute min/max values for normalization. The second is
 * drawElement, where the client must supply the code to draw a single element
 * of the field. Lastly is getXmlTagName, which just defines a name unique to
 * this viewer type for use in the workspace configuration XML files.
 */
abstract class TileBasedFieldPanel[T <: FieldReader](
      fieldShape: Shape,
      antialiasing: Boolean = true,
      preserveLineThickness: Boolean = true)
    extends FlowPanel(FlowPanel.Alignment.Center)()//BorderPanel
    with ZoomProperty {

  protected var data: Option[T] = None

  val dim = fieldShape.dimensions
  val (layers, rows, cols) = dim match {
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case 3 => (fieldShape(0), fieldShape(1), fieldShape(2))
    case x => throw new RuntimeException("Unsupported field dimension: "+x)
  }
  val layersInARow = math.sqrt(layers).ceil.toInt
  protected var margin = 5

  protected def drawElements(data: T, g: Graphics2D) {
    data.fieldShape.dimensions match {
      case 0 => draw0D(data, g)
      case 1 => draw1D(data, g)
      case 2 => draw2D(data, g)
      case 3 => draw3D(data, g)
      case _ => throw new RuntimeException("Only 0-3D fields supported.")
    }
  }

  // Zoom fields & methods //
  /** The width and height (in pixels) of each cell at default zoom. */
  zoomType = ZoomableProperty.ZoomType.Multiplicative
  val cellSize = 8
  val zoomIncrement = 1.1f
  protected var stroke = new BasicStroke()
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      if (newValue > 0) {
        stroke = new BasicStroke(1 / newValue)
        canvas.scaleXform.setToScale(newValue, newValue)
        canvas.preferredSize = canvas.computeCanvasSize
        canvas.revalidate()
        canvas.repaint()
      }
  }

  def layerWidth = cols * cellSize
  def layerHeight = rows * cellSize

  protected val canvas = new Panel {
    val scaleXform = new AffineTransform()

    preferredSize = computeCanvasSize

    // This offscreen rendering is not well implmented and needs to be
    // re-thought. It doesn't provide a useful speed increase for large fields
    // and is much worse for small ones. Writing to and reading from the
    // offscreen buffer is not properly syncrhonized either, causing visible
    // tearing and blanking, especially when zooming to large magnifications.
//    private var offScreenBuffer =
//      new BufferedImage(preferredSize.width, preferredSize.height, BufferedImage.TYPE_BYTE_GRAY)
//
//    listenTo(this)
//    reactions += {
//      case scala.swing.event.UIElementResized(src) =>
//        offScreenBuffer = new BufferedImage(size.width, size.height, BufferedImage.TYPE_BYTE_GRAY)
//        renderToOffscreenBuffer()
//    }

    def getScale = scaleXform.getScaleX // Assumes square scale aspect ratio

    def computeCanvasSize = {
      val width = Math.ceil((layersInARow * (cellSize * cols + margin)) * getScale + margin).toInt
      val height = Math.ceil((layersInARow * (cellSize * rows + margin)) * getScale + margin).toInt
      new Dimension(width, height)
    }

    /** This method is to return a range indicating which cells are visible
      * based on the current zoom and pan. The intent is that it will be used
      * to prevent overdrawing. Right now, always indicates that every cell is
      * visible. */
    def getVisibleCellBounds(cellSize: Int, rows: Int, cols: Int) = {
      // TODO
      (0, 0, rows, cols)
    }

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      //g.drawImage(offScreenBuffer, 0, 0, null)
      render(g, size.width, size.height)
    }

//    def renderToOffscreenBuffer() {
//      val g = offScreenBuffer.createGraphics()
//      render(g, offScreenBuffer.getWidth, offScreenBuffer.getHeight)
//      g.dispose()
//    }

    def render(g: Graphics2D, w: Int, h: Int) {
      // Clear the image
      g.setColor(background)
      g.fillRect(0, 0, w, h)

      if (antialiasing) g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)
      if (preserveLineThickness) g.setStroke(stroke)
      g.transform(scaleXform)
      // We need to store a reference to the current data field so that it
      // doesn't get changed out from under us while drawing.
      data foreach { data => drawElements(data, g) }
    }
  }

  //layout(canvas) = BorderPanel.Position.Center
  contents += canvas

  /** Update the data being viewed with new "data". */
  def update(target: AnyRef, data: T) {
    this.data = Option(data)
    updateStatistics(data)
//    canvas.renderToOffscreenBuffer()
    canvas.repaint()
  }

  // TODO: Add button to toggle grid on/off
  // Defaults to off, which creates a less cluttered display, better keeping
  // with Tufte's display principles (max information to ink ratio).
  var drawGrid = false

  def draw0D(data: T, g: Graphics2D) {
    g.translate(margin, margin)

    // Draw background rectangle
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, layerWidth, layerHeight)

    // Draw Grid
    if (drawGrid) {
      g.setColor(new Color(225, 225, 225))
      g.drawRect(0, 0, cellSize * cols, cellSize)
    }

    g.setColor(java.awt.Color.BLACK)

    drawElement(data, g)
  }

  def draw1D(data: T, g: Graphics2D) {
    g.translate(margin, margin)

    // Draw background rectangle
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, layerWidth, layerHeight)

    if (drawGrid) {
      g.setColor(new Color(225, 225, 225))
      g.drawRect(0, 0, cellSize * cols, cellSize)
      for (c <- 0 until cols) g.drawLine(c * cellSize, 0, c * cellSize, cellSize)
    }

    g.setColor(java.awt.Color.BLACK)

    for (c <- 0 until cols) {
      // Save the transform so we can undo any client translations.
      val t = g.getTransform
      drawElement(data, g, c)
      g.setTransform(t)
      g.translate(cellSize, 0) // Move to next column
    }
  }

  def draw2D(data: T, g: Graphics2D) {
    g.translate(margin, margin)

    // Draw background rectangle
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, layerWidth, layerHeight)

    // Reduce overdraw by only drawing visible cells
    val (sr, sc, er, ec) = // startRow, startCol, endRow, endCol
      canvas.getVisibleCellBounds(cellSize, rows, cols)
    val startRow = sr max 0
    val startCol = sc max 0
    val endRow = er min rows
    val endCol = ec min cols
    val shownRows = endRow - startRow
    val shownCols = endCol - startCol

    if (drawGrid) {
      g.setColor(new Color(225, 225, 225))
      g.drawRect(0, 0, cellSize * cols, cellSize)
      for (c <- 0 until cols) g.drawLine(c * cellSize, 0, c * cellSize, rows * cellSize)
      for (r <- 0 until rows) g.drawLine(0, r * cellSize, cols * cellSize, r * cellSize)
    }

    g.setColor(java.awt.Color.BLACK)

    g.translate(cellSize * startCol, cellSize * startRow)
    //for (r <- 0 until data.rows) {
    for (r <- startRow until endRow) {
      //for (c <- 0 until data.columns) {
      for (c <- startCol until endCol) {
        val t = g.getTransform
        drawElement(data, g, r, c)
        g.setTransform(t) // Undo any client translations.
        g.translate(cellSize, 0)
      }
      g.translate(-cellSize * shownCols, cellSize) // Translate to start of next row
    }
  }

  def draw3D(data: T, g: Graphics2D) {

    // TODO Reduce overdraw as in 2D case

    g.translate(margin, margin)
    for (l <- 0 until layers) {

      if (l > 0 && l % layersInARow == 0)
        g.translate(-(layerWidth + margin) * layersInARow, layerHeight + margin)

      // Draw background rectangle
      g.setColor(java.awt.Color.WHITE)
      g.fillRect(0, 0, layerWidth, layerHeight)

      if (drawGrid) {
        g.setColor(new Color(225, 225, 225))
        g.drawRect(0, 0, layerWidth, layerHeight)
        for (c <- 0 until cols) g.drawLine(c * cellSize, 0, c * cellSize, rows * cellSize)
        for (r <- 0 until rows) g.drawLine(0, r * cellSize, cols * cellSize, r * cellSize)
      }

      g.setColor(java.awt.Color.BLACK)

      // Draw elements
      for (r <- 0 until rows) {
        for (c <- 0 until cols) {
          val t = g.getTransform
          drawElement(data, g, l, r, c)
          g.setTransform(t) // Undo any client transforms.
          g.translate(cellSize, 0) // Next cell
        }
        g.translate(-cellSize * cols, cellSize) // Translate to start of next row
      }
      g.translate(layerWidth + margin, -cellSize * rows) // Next layer
    }
  }

  // Abstract methods /////////////////////////////////////////////////////////

  /** Compute any needed statistics on your data (for display or to help draw
    * later). Called after new data has been received (in updateData) but
    * before drawing begins for that new data. */
  def updateStatistics(data: T)

  /** Draw the single field element at the given indices.
    *
    * Called after the pen has been moved to the top-left corner of the cell to
    * draw in. Clients should take care to draw within the bounds of the cell,
    * which is a cellSize by cellSize square.
    *
    * It would be nice if we could give an element as an argument instead of a
    * bunch of indices, but the Field class doesn't currently define any means
    * to actually extract elements from the field; that's left to subclasses.
    * Further complicating things, the panel classes don't know in advance how
    * many dimensions the data field has, so we have to use a variable length
    * indices parameter. If your field class has a read(indices: Int*) method,
    * you can get the current element, regardless of field dimensions, like so:
    * {{{
    *   val element = read(indices: _*)
    * }}}
    */
  def drawElement(data: T, g: Graphics2D, indices: Int*)

}
