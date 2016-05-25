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

import java.awt.image.BufferedImage
import java.awt.geom.AffineTransform
import scala.swing._
import cogdebugger.ui.fieldvisualizations.Zoomable

/** A non-automated test to check that the pan and zoom panes work as epxected.
  * I must admit that I've confounded this test a bit by experimenting a bit
  * (e.g. enabling anti-aliasing on the image panel).
  *
  * Created by gonztobi on 3/25/2014.
  */
object PanPaneTest {

  val ImgWidth  = 1920
  val ImgHeight = 1080

  val GridSpacing = 100
  val XGrids = 10
  val YGrids = 10

  def main(args: Array[String]) {

    val gradientImage = new BufferedImage(ImgWidth, ImgHeight, BufferedImage.TYPE_BYTE_GRAY)
    val g = gradientImage.createGraphics()

    // Draw a grid in the image, surrounded by a rounded box border
    g.drawRoundRect(10, 10, ImgWidth - 20, ImgHeight - 20, 10, 10)
    val xGridWidth  = XGrids * GridSpacing
    val yGridHeight = YGrids * GridSpacing
    val xMargin = (ImgWidth - xGridWidth) / 2
    val yMargin = (ImgHeight - yGridHeight) / 2
    for (y <- 0 to YGrids)
      g.drawLine(xMargin, yMargin + y * GridSpacing, ImgWidth - xMargin, yMargin + y * GridSpacing)
    for (x <- 0 to XGrids)
      g.drawLine(xMargin + x * GridSpacing, yMargin, xMargin + x * GridSpacing, ImgHeight - yMargin)

    g.dispose()

    Swing.onEDT {
      // The child component
      val imgPanel = new SimpleImagePanel(gradientImage) {
        import java.awt.RenderingHints

        // Anti-aliasing is helpful for preserving detail when zooming way out,
        // but less useful when zoomin in. This probably needs to be turned
        // on/off depending on the current zoom level. Or, maybe there's an
        // API for setting up the magnification/minification filters like in
        // OpenGL?
        override def paintComponent(g: Graphics2D) {
          g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
            RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
          g.setRenderingHint(RenderingHints.KEY_RENDERING,
            RenderingHints.VALUE_RENDER_QUALITY)
          super.paintComponent(g)
        }
      }

      // Toolips have broken the mouse event mechanism in the past; we need to
      // be sure our solution works in the presence of tooltips.
      imgPanel.tooltip = "An image panel"

      // Pan/zoom should work even with multiple levels of nested components.
      val innerWrapper = new FlowPanel(imgPanel)
      innerWrapper.hGap = 20
      innerWrapper.vGap = 20
      innerWrapper.background = java.awt.Color.RED

      // We sometimes compose several of our visualization classes together in
      // a single parent/container panel. For zooming to work, we need to mixin
      // the zoom trait to this wrapper, and just call through to the zoom
      // methods of its children.
      val wrapper = new FlowPanel(innerWrapper) with Zoomable with MouseDragZoom {
        //require(contents(0).asInstanceOf[FlowPanel].contents(0) eq imgPanel)
        //println("Wrapper is: "+this)
        //println("Child is:   "+contents(0))
        //println("GChild is:  "+contents(0).asInstanceOf[FlowPanel].contents(0))
        def zoomIn()  { imgPanel.zoomLevel *= 1.1f }
        def zoomOut() { imgPanel.zoomLevel /= 1.1f }
      }
      wrapper.hGap = 20
      wrapper.vGap = 20

      // The PanPane is smaller than the child so that the scrollbars appear
      // and there's something to pan
      val panPane = new PanPane(wrapper)
      panPane.preferredSize = new Dimension(640, 480)

      // Toolbar to host zoom controls
      val toolbar = new BoxPanel(Orientation.Horizontal)
      toolbar.contents += Button("+") { imgPanel.zoomLevel *= 1.1f }
      toolbar.contents += Button("-") { imgPanel.zoomLevel /= 1.1f }

      val bp = new BorderPanel()
      bp.layout(toolbar) = BorderPanel.Position.North
      bp.layout(panPane) = BorderPanel.Position.Center

      val frame = new MainFrame()
      frame.title = "Test: Mouse Pan and Zoom"
      frame.contents = bp

      frame.visible = true
    }

  }

}

/** Simple panel that displays the given image. Can be zoomed in/out; zoom
  * operations adjust preferredSize to match zoomed image size and request
  * a revalidate. Note that images drawn at sizes smaller that 1:1 tend to
  * exhibit some nasty aliasing.
  */
class SimpleImagePanel(img: BufferedImage) extends Component {
  preferredSize = new Dimension(img.getWidth, img.getHeight)

  private val scaleXform = new AffineTransform()
  var _zoomLevel = 1f
  def zoomLevel = _zoomLevel
  def zoomLevel_=(zoom: Float) {
    scaleXform.setToScale(zoom, zoom)
    _zoomLevel = zoom
    preferredSize =
            new Dimension(math.ceil(img.getWidth * zoom).toInt,
              math.ceil(img.getHeight * zoom).toInt)
    revalidate()
    repaint()
  }

  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.transform(scaleXform)
    g.drawImage(img, 0, 0, null)
  }
}