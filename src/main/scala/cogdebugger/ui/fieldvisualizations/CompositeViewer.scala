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
import cogdebugger.ui.components.{DoubleBufferedImagePanel, ToolBar}
import scala.swing._
import java.awt.image.BufferedImage

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/18/13
 * Time: 4:15 PM
 */

/** An experiment to see if I can't set up a visualization that combines the
  * data from two fields into a single visualization.
  *
  * The idea stems from Matthew + Ben's tracker app, where they had to render
  * a circular target on top of a webcam feed. Originally, that required a lot
  * of hacking and pixel twiddling; but with the right probe framework, it
  * should be simple to define a viewer that does the same sort of thing, e.g.
  * by interpreting the contents of fieldA as an image and the contents of
  * fieldB as the coordinates of a target that needs to be overlaid on top.
  *
  * ---
  *
  * Sudden realization - how do we actually build one of these in response to
  * user input? They can only click on one field at a time... */
class CompositeViewer(
        backgroundField: AnyRef,
        targetField: AnyRef,
        initBackground: ScalarFieldReader)
    extends BorderPanel
    with EventDrivenViewer {

  require(initBackground.fieldShape.dimensions == 2)
  // TODO Enforce proper shape on target field as well

  /** The (Cog4) fields this viewer is visualizing. */
  val targets = Set(backgroundField, targetField)

  protected var (x, y, r) = (0f, 0f, 0f) // Target location and size
  protected var rScale = 10

  def rows = initBackground.fieldShape(0)
  def cols = initBackground.fieldShape(1)

  val circle = new java.awt.geom.Ellipse2D.Float()
  val circleColor = new Color(0, 255, 0)
  val imgPanel = new DoubleBufferedImagePanel(new Dimension(cols, rows), BufferedImage.TYPE_BYTE_GRAY) {
    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
      circle.setFrame(x * cols - r * rScale / 2, y * rows - r * rScale / 2,
                      r * rScale,                r * rScale)
      g.setColor(circleColor)
      g.fill(circle)
    }
  }

  /** Zoom in/out step size */
  //val sDelta = 1

  //protected def scaleXform = imgPanel.scaleXform
  //protected def transXform = imgPanel.transXform

  protected val toolbar = new ToolBar("Scalar Field Tools")
  toolbar.floatable = false
  toolbar.contents += Button("+") {
    //val (sx, sy) = (scaleXform.getScaleX, scaleXform.getScaleY)
    //scaleXform.setToScale(sx + sDelta, sy + sDelta)
    //imgPanel.repaint()
    imgPanel.zoomIn()
  }
  toolbar.contents += Button("-") {
    //val (sx, sy) = (scaleXform.getScaleX, scaleXform.getScaleY)
    //// This conditional prevents zooming out beyond 1:1, which may be
    //// undesirable (e.g. large field that doesn't otherwise fit in panel).
    //if (sx > sDelta && sy > sDelta) {
    //  scaleXform.setToScale(sx - sDelta, sy - sDelta)
    //  imgPanel.repaint()
    //}
    if (imgPanel.zoomLevel > imgPanel.zoomIncrement)
      imgPanel.zoomOut()
  }


  add(toolbar, BorderPanel.Position.North)
  add(imgPanel, BorderPanel.Position.Center)

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 0) => update(src, data.asInstanceOf[ScalarFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
//    FieldMemoryPool.release(data)
  }

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: ScalarFieldReader) {
    throw new RuntimeException("must be updated for new ScalarFieldMemory.read")
    /*
    src match {
      case `background` =>
        // The first field provides the background image.
        for (r <- 0 until rows; c <- 0 until cols)
          img.getRaster.setSample(c, r, 0, data.read(r, c))
        imgPanel.repaint()
      case `target` =>
        // Extract coordinates and radius of target from second field.
        //x = data.read(0); y = data.read(1); r = data.read(2)
        x = data.read(0, 0); y = data.read(0, 1); r = data.read(0, 2)
        //println(s"Updating fieldB: x=$x, y=$y, r=$r")
        imgPanel.repaint()
      case _ => throw new RuntimeException("Got data from a field we shouldn't have.")
    }
    */
  }
}
