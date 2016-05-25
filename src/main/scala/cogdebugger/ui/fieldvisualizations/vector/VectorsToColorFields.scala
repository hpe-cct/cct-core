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

package cogdebugger.ui.fieldvisualizations.vector

import cogx._
import cogdebugger.ui.fieldvisualizations.{ZoomProperty, EventDrivenViewer}
import cogdebugger.{PropertyValueChanged, RestorableState}
import scala.swing._
import java.awt.image.BufferedImage
import cogdebugger.ui.components.{WrapPanel, DoubleBufferedImagePanel}
import cogdebugger.ui.fieldvisualizations.ZoomableProperty.ZoomType
import javax.swing.JPanel
import java.awt.event.MouseEvent

/** A special purpose viewer for 2-D vector fields having vector lengths that
  * are a multiple of 3.
  *
  * If you think of the vector field as a stack of planes, one plane per
  * vector element index, then every three planes are taken to the RGB color
  * planes of a single image.
  *
  * All values in the vector field are assumed to be on the range [0, 1]. Zero
  * represents no intensity, and one represent maximum intensity.
  *
  * E.g.
  * {{{
  *
  * Vector element planes -->  color images
  *     _______
  *   /  r    / ------
  * /_______/          \      +---------------+
  *     _______          \    |               |
  *   /  g    / ------------- | color image 1 |
  * /_______/            /    |               |
  *     _______        /      +---------------+
  *   /  b    / ------
  * /_______/
  *     _______
  *   /  r    / ------
  * /_______/          \      +---------------+
  *     _______          \    |               |
  *   /  g    / ------------- | color image 2 |
  * /_______/            /    |               |
  *     _______        /      +---------------+
  *   /  b    / ------
  * /_______/
  *
  * }}}
  *
  * Created by gonztobi on 11/12/2014.
  */
class VectorsToColorFields(targetFieldType: FieldType)
  extends BorderPanel
  with EventDrivenViewer
  with ZoomProperty
  with RestorableState {
  
  private val fieldShape = targetFieldType.fieldShape  
  private val dimensions = fieldShape.dimensions
  private val (layers, rows, columns) = dimensions match {
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case 3 => (fieldShape(0), fieldShape(1), fieldShape(2))
  }
  private val vectorLength = targetFieldType.tensorShape(0)
  
  require(vectorLength % 3 == 0, "Vector lengths must be a multiple of 3.")
  
  private val imgPanels = Array.tabulate(vectorLength / 3) { imgIdx => buildImagePanel(imgIdx) }
  private val wrapPanel = new WrapPanel(); wrapPanel.contents ++= imgPanels

  val zoomIncrement = 1.1f
  zoomType = ZoomType.Multiplicative
  properties += ZoomProperty
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      println("Changing zoom level from "+oldValue+" to "+newValue)
      imgPanels.foreach(_.zoomLevel = newValue)
      wrapPanel.revalidate()
      wrapPanel.repaint()
  }

  imgPanels.foreach(panel => panel.tooltip = "enable")
  add(wrapPanel, BorderPanel.Position.Center)

  final def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) => update(src, data.asInstanceOf[VectorFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data (not a vector field)")
    }
  }

  // Pre-allocate space for copying vectors into to avoid constant object
  // allocations
  private val tmpVectors = Array.tabulate(rows) {
    _ => Vector(vectorLength, _ => 0f)
  }

  private var lastData: Option[VectorFieldReader] = None

  private def update(src: AnyRef, data: VectorFieldReader) {
    // Note the use of a ParRange here in the outermost loop to update all the
    // rows in parallel.
    for (row <- (0 until rows).par) {
      val tmpVector = tmpVectors(row)
      for (col <- 0 until columns) {
        data.read(row, col, tmpVector)
        updateImages(row, col, tmpVector)
      }
    }
    // Images are updated one pixel at a time. When we're finally done with all
    // of them, we do a no-op update, which has the side-effect of swapping the
    // front and back buffers of the image panels.
    for (imgPanel <- imgPanels) imgPanel.update(_ => Unit)

    lastData = Some(data)
  }

  /** Updates the back buffer pixel at (row, col) in every image using the
    * values contained in the `data` vector. */
  private def updateImages(row: Int, col: Int, data: Vector) {
    for (i <- 0 until vectorLength / 3) {
      val imgPanel = imgPanels(i)
      val r = (data(i * 3 + 0) * 255).toInt
      val g = (data(i * 3 + 1) * 255).toInt
      val b = (data(i * 3 + 2) * 255).toInt
      val argb = 0xFF000000 | (r << 16) | (g << 8) | b
      imgPanel.back.setRGB(col, row, argb)
    }
  }

  /** Builds an image panel for holding the `index`th image. */
  private def buildImagePanel(index: Int) = {
    val dim = dimensions match {
      case 0 => new Dimension(1, 1)
      case 1 => new Dimension(fieldShape(0), 1)
      case 2 => new Dimension(fieldShape(1), fieldShape(0))
      case 3 => new Dimension(fieldShape(2), fieldShape(1))
      case x => throw new RuntimeException("Unsupported field dimension: "+x)
    }
    new DoubleBufferedImagePanel(dim, BufferedImage.TYPE_3BYTE_BGR) {
      override lazy val peer = new JPanel with SuperMixin {
        override def getToolTipText(event: MouseEvent) = {
          val row = math.floor(event.getY / zoomLevel).toInt
          val col = math.floor(event.getX / zoomLevel).toInt
          popupText(index, row, col)
        }
      }
    }
  }

  private val tmpVector = Vector(vectorLength, _ => 0f)
  private def popupText(imgIdx: Int, row: Int, col: Int): String = {
    lastData match {
      case Some(data) =>
        data.read(row, col, tmpVector)
        val r = tmpVector(imgIdx * 3 + 0)
        val g = tmpVector(imgIdx * 3 + 1)
        val b = tmpVector(imgIdx * 3 + 2)
        "(%d, %d): %f, %f, %f".format(row, col, r, g, b)
      case None => null
    }
  }

  def save: scala.xml.Elem =
    <VectorsToColorFields>
      { propertiesTag }
    </VectorsToColorFields>

  def restore(savedState: scala.xml.Node) {
    (savedState \ "VectorsToColorFields" \ "properties").headOption.foreach(xmlToProperties)
  }
  
}
