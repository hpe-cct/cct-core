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

package cogdebugger.ui.fieldvisualizations.color

import libcog._
import cogdebugger.RestorableState
import cogdebugger.ui.components.DoubleBufferedImagePanel
import cogdebugger.ui.fieldvisualizations.{Zoomable, UnsupportedDimensionException, EventDrivenViewer}
import cogdebugger.ui.fieldvisualizations.ZoomableProperty.ZoomType
import scala.swing._
import scala.xml.{Node, Elem}
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent
import javax.swing.JPanel

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/26/13
 * Time: 4:39 PM
 */

/** A visualization for fields containing color images - that is,
  * two-dimensional fields of cogx.platform.types.ElementTypes.Uint8Pixels.
  *
  * At this time, three-dimensional color fields (stacks of two-D images)
  * aren't supported on NVidia hardware, so this viewer has no support for
  * such fields either.
  */
class ColorFieldMemoryView(target: AnyRef, fShape: Shape)
        extends FlowPanel
        with EventDrivenViewer
        with Zoomable
        with RestorableState {

  require(fShape.dimensions == 2)

  /** The (Cog4) fields this viewer is visualizing. */
  def targets = Set(target)

  private val (layers, rows, cols) = fShape.dimensions match {
    case 2 => (1, fShape(0), fShape(1))
    case 3 => (fShape(0), fShape(1), fShape(2))
    case x => throw new UnsupportedDimensionException(x)
  }

  private val dim = new Dimension(cols, rows)
  private val imgPanel =
    new DoubleBufferedImagePanel(dim, BufferedImage.TYPE_3BYTE_BGR) {
      override lazy val peer = new JPanel with SuperMixin {
        override def getToolTipText(event: MouseEvent) = {
          val row = math.floor(event.getY / zoomLevel).toInt
          val col = math.floor(event.getX / zoomLevel).toInt
          popupText(row, col)
        }
      }
    }

  // Setting a String here enables tooltips, but the displayed tooltip is
  // actually determined by our popupText method defined below.
  imgPanel.tooltip = "Enable"

  // Pass through zoom methods to the imgPanel
  val ZoomProperty = imgPanel.ZoomProperty
  def zoomType = imgPanel.zoomType
  def zoomType_=(zType: ZoomType) { imgPanel.zoomType = zType }
  def zoomIncrement = imgPanel.zoomIncrement
  def zoomIncrement_=(incr: Float) { imgPanel.zoomIncrement = incr }
  def zoomIn() { imgPanel.zoomIn() }
  def zoomOut() { imgPanel.zoomOut() }
  zoomIncrement = 1.1f
  zoomType = ZoomType.Multiplicative

  // Add ZoomProperty to props list for save/restore
  properties += ZoomProperty

  contents += imgPanel

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Uint8Pixel, 1) => update(src, data.asInstanceOf[ColorFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  private var lastData: Option[ColorFieldReader] = None

  /** Updates the visualization based on the contents of `data`. */
  private val tmpPixel = new Pixel(0, 0, 0)
  def update(src: AnyRef, data: ColorFieldReader) {
    fShape.dimensions match {
      case 2 =>
        imgPanel.update(raster => {
          for (r <- 0 until rows; c <- 0 until cols) {
            data.read(r, c, tmpPixel)
            raster.setSample(c, r, 0, tmpPixel.red)
            raster.setSample(c, r, 1, tmpPixel.green)
            raster.setSample(c, r, 2, tmpPixel.blue)
          }
        })
        lastData = Some(data)

      /*
       * NOTE: 3D color fields are not supported by Nvidia hardware, so we can't
       * support them here.
      case 3 =>
        for (l <- 0 until layers) {
          val img = imgs(l)
          val raster = img.getRaster
          for (r <- 0 until rows; c <- 0 until cols) {
            data.read(l, r, c, tmpPixel)
            raster.setSample(c, r, 0, tmpPixel.red)
            raster.setSample(c, r, 1, tmpPixel.green)
            raster.setSample(c, r, 2, tmpPixel.blue)
            raster.setSample(c, r, 3, 255)
          }
          imgPanels(l).repaint()
        }
      */
      case x => throw new UnsupportedDimensionException(x)
    }
  }

  // Need a separate pixel to read into since the update method can be called
  // on a background thread, while this method should always exectute on the
  // EDT.
  private val tmpPixel2 = new Pixel(0, 0, 0)
  private def popupText(row: Int, col: Int) = lastData match {
    case Some(colorField) =>
      colorField.read(row, col, tmpPixel2)
      "RGB (%d, %d): %d, %d, %d".format(row, col,
        tmpPixel2.redInt, tmpPixel2.greenInt, tmpPixel2.blueInt)
    case None => null
  }

  def save: Elem =
    <ColorFieldMemoryView>
      { propertiesTag }
    </ColorFieldMemoryView>

  def restore(tag: Node) {
    (tag \ "ColorFieldMemoryView" \ "properties").headOption.foreach(xmlToProperties)
  }

}
