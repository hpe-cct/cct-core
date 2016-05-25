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
import java.awt.geom.AffineTransform
import java.awt.Dimension
import cogdebugger.ui.fieldvisualizations.{ZoomProperty, Zoomable}
import cogdebugger.PropertyValueChanged

/** A panel that displays an image. Preferred size is initially set to the
  * current image's unscaled dimensions. */
class ImagePanel(protected var _image: Image) extends Panel with ZoomProperty {

  /** Panning transformation */
  val transXform = new AffineTransform()

  /** Zooming transformation */
  val scaleXform = new AffineTransform()

  def image = _image

  /** Sets this panel's displayed image to `img` and updates preferredSize to
    * be equal to `img`'s unscaled dimensions. */
  def image_=(img: Image) {
    _image = img
    resetSize()
  }

  resetSize() // Initialize panel size

  val lock = new java.util.concurrent.locks.ReentrantReadWriteLock()

  val xform = new AffineTransform()
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    //val xform = new AffineTransform(scaleXform)
    xform.setTransform(scaleXform)
    xform.concatenate(transXform)
    //g.drawImage(image, xform, null)
    g.transform(xform)

    lock.readLock().lock()
    g.drawImage(image, null, null)
    lock.readLock.unlock()
  }

  protected def resetSize() {
    preferredSize = new Dimension(image.getWidth(null), image.getHeight(null))
    minimumSize = preferredSize
    maximumSize = preferredSize
  }

  var zoomIncrement: Float = 1f
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(`ZoomProperty`, _, _) =>
      val zoom = zoomLevel
      // Ignore trying to set the zoom to zero (or a negative value)
      if (zoom > 0) {
        scaleXform.setToScale(zoom, zoom)
        preferredSize = new Dimension(math.ceil(image.getWidth(null) * zoom).toInt,
          math.ceil(image.getHeight(null) * zoom).toInt)
        minimumSize = preferredSize
        maximumSize = preferredSize
        revalidate()
        repaint()
      }
  }
//  def changeZoomLevel(delta: Float) { zoomLevel = zoomLevel + delta }
//  def zoomLevel = scaleXform.getScaleX
//  def zoomLevel_=(zoom: Double) {
//    // Ignore trying to set the zoom to zero (or a negative value)
//    if (zoom > 0) {
//      scaleXform.setToScale(zoom, zoom)
//      preferredSize = new Dimension(math.ceil(image.getWidth(null) * zoom).toInt,
//        math.ceil(image.getHeight(null) * zoom).toInt)
//      minimumSize = preferredSize
//      maximumSize = preferredSize
//      revalidate()
//      repaint()
//    }
//  }

}

import java.awt.image.{BufferedImage, WritableRaster}

/** A double-buffered variant of ImagePanel. Meant to alleviate some of the
  * screen-tearing seen using the old implementation. Note that there is
  * currently no way to resize the internal buffers once they've been created.
  *
  * The `imgType` argument should be one of the constants defined in
  * java.awt.image.BufferedImage, e.g. BufferedImage.TYPE_3BYTE_BGR.
  *
  * @param dim     The dimensions of the internal image buffers
  * @param imgType The desired type of the internal buffers
  */
class DoubleBufferedImagePanel(dim: Dimension, imgType: Int)
    extends Panel
    with ZoomProperty {

  protected val scaleXform = new AffineTransform()
  protected val img1 = new BufferedImage(dim.width, dim.height, imgType)
  protected val img2 = new BufferedImage(dim.width, dim.height, imgType)
  protected var _front = img1
  protected var _back = img2
  protected val swapLock = new java.util.concurrent.Semaphore(1)

  def front = _front
  def back  = _back

  // Initial size of panel ought to exactly match image dimensions.
  preferredSize = dim

  /** Updates the back buffer by applying `op` to its raster, then swaps
    * buffers and schedules a repaint. */
  def update(op: WritableRaster => Unit) {
    op(back.getRaster)
    swapBuffers()
    repaint()
  }

  /** Paints this component. Buffer swapping is prevented while the front
    * buffer is being painted. */
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.transform(scaleXform)
    swapLock.acquire()
    g.drawImage(front, null, null)
    swapLock.release()
  }

  /** Swaps the front and back buffers. */
  private def swapBuffers() {
    swapLock.acquire()
    val tmp = back
    _back = front
    _front = tmp
    swapLock.release()
  }

  /** Default magnification step size when zooming in and out. */
  var zoomIncrement: Float = 1f

  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(`ZoomProperty`, _, _) =>
      val zoom = zoomLevel
      if (zoom > 0) {
        scaleXform.setToScale(zoom, zoom)
        preferredSize = new Dimension(math.ceil(dim.width  * zoom).toInt,
          math.ceil(dim.height * zoom).toInt)
        revalidate()
        repaint()
      }
  }
//  def changeZoomLevel(delta: Float) {
//    zoomType match {
//      case ZoomType.Additive       => zoomLevel += delta
//      case ZoomType.Multiplicative => zoomLevel *= delta
//    }
//  }
//  def zoomLevel = scaleXform.getScaleX
//
//  /** Changes the scalefactor of the image to `zoom`, adjust preferredSize
//    * accordingly, and then revalidates and repaints the panel. Ignores
//    * attempts to set a negative or zero zoom level.
//    */
//  def zoomLevel_=(zoom: Double) {
//    if (zoom > 0) {
//      scaleXform.setToScale(zoom, zoom)
//      preferredSize = new Dimension(math.ceil(dim.width  * zoomLevel).toInt,
//                                    math.ceil(dim.height * zoomLevel).toInt)
//      revalidate()
//      repaint()
//    }
//  }
}
