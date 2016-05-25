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

package cogdebugger.ui.fieldvisualizations.scalar

import libcog._
import cogdebugger.{PropertyValueChanged, RestorableState}
import cogdebugger.ui.components.DoubleBufferedImagePanel
import cogdebugger.ui.fieldvisualizations._
import scala.swing._
import scala.xml.{Node, Elem}
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent

/** A viewer for ScalarFields that assumes that they're holding grayscale
  * images. That is, the values in the ScalarField are in the range [0f, 1f],
  * with values <= 0 to be represented as black, and values >= 1 as pure white.
  *
  * The view presented can look quite similar to that of the ScalarMemoryView,
  * but because this view makes assumptions about the possible range of values,
  * it's potentially more performant.
  *
  * Only accepts ScalarFields of 2 dimensions or lower.
  */
class GrayscaleImagePanel(target: AnyRef, fShape: Shape)
    extends FlowPanel
    with EventDrivenViewer
    with ZoomProperty
    with RestorableState {

  val (rows, cols) = fShape.dimensions match {
    case 0 => (1, 1)
    case 1 => (1, fShape(0))
    case 2 => (fShape(0), fShape(1))
    case x => throw new UnsupportedDimensionException(x)
  }

  /** Reference to the most recent field data handed to this view through its
    * update method. Used for tooltips. */
  private var lastData: ScalarFieldReader = null

  protected val imgPanel =
    new DoubleBufferedImagePanel(new Dimension(cols, rows),
                                 BufferedImage.TYPE_BYTE_GRAY) with PerElementTooltips {
      /** Optionally returns a String whose contents can vary depending on the
        * coordinates of the `mouseEvent` argument. No tooltip of any kind is
        * displayed by Swing in the event that this method returns None. */
      override def popupText(mouseEvent: MouseEvent): Option[String] = {
        val row = math.floor(mouseEvent.getY / zoomLevel).toInt
        val col = math.floor(mouseEvent.getX / zoomLevel).toInt
        if (lastData != null &&
            row >= 0 && row < rows &&
            col >= 0 && col < cols) {
          val datum = getElement(lastData, 0, row, col)
          Some(s"($row, $col): $datum")
        } else None
      }
    }

  /** Zoom increment. This value is added/subtracted to the current `zoomLevel`
    * when zooming in or out. At a zoomLevel of 1, the image is rendered suh
    * that a single scalar element maps to exactly 1x1 pixel. A zoomLevel > 1
    * magnifies the image by that factor; e.g. a zoomLevel of 3 renders the
    * image such that each scalar element maps to 3x3 pixels. */
  var zoomIncrement = 1f
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      imgPanel.zoomLevel = newValue
  }

  contents += imgPanel

  /** Update the view with new data. This method schedules a repaint of the
    * view. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 0) => update(src, data.asInstanceOf[ScalarFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  /** Update the view with new data. This method schedules a repaint of the
    * view. */
  def update(src: AnyRef, data: ScalarFieldReader) {
    val iter = data.iterator
    imgPanel.update(raster =>
      for (r <- 0 until rows; c <- 0 until cols)
        raster.setSample(c, r, 0, iter.next * 255f min 255f)
    )
    lastData = data
  }

  /** Read a single element out of the given field. Though this method requires
    * three indices into the field (layer, row, and column), it will work with
    * fields of less than 3 dimensions - the extra indices are ignored in that
    * case. */
  private def getElement(field: ScalarFieldReader, layer: Int, row: Int, col: Int) =
    fShape.dimensions match {
      case 0 => field.read()
      case 1 => field.read(col)
      case 2 => field.read(row, col)
      case 3 => field.read(layer, row, col)
      case x => throw new UnsupportedDimensionException(x)
    }

  def save: Elem =
    <GrayscaleImagePanel>
      { RestorableState.getTagFor(imgPanel, "zoomLevel") }
    </GrayscaleImagePanel>

  def restore(savedState: Node) {
    (savedState \ "GrayscaleImagePanel").headOption foreach { myNode =>
      RestorableState.restoreTag(imgPanel, myNode \ "zoomLevel")
    }
  }

}
