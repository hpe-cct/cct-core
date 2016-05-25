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

package cogdebugger.ui.fieldvisualizations.complex

import libcog._
import scala.swing._
import cogdebugger.ui.fieldvisualizations.Zoomable
import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView

/** A panel which displays a complex field as a color flow field. The direction
  * is denoted by color and the amplitude by brightness.
  *
  * NOT DONE YET.
  *
  * @param fieldType Shape of the complex field being displayed
  *
  * @author Greg Snider
  */
class ComplexColorFlowSubpanel(fieldType: FieldType)
        extends FlowPanel
        with Zoomable
{

  def fieldShape = fieldType.fieldShape

  require(fieldShape.dimensions == 2, "Only 2D complex fields supported now")
  val rows = fieldShape(0)
  val columns = fieldShape(1)
  private val complexToColorMap = new ComplexToColorMap(1.0f)

  /** Panel displaying magnitude part of complex field. */
  private val magnitudePanel = new ScalarMemoryView(fieldType)
  /** Panel displaying phase part of complex field. */
  private val phasePanel = new ScalarMemoryView(fieldType)
  /** Zooming? */
  var zDelta = 1f

  // Initialize
  setFloatingMax(magnitudePanel)
  setFloatingMax(phasePanel)
  contents += wrapPanel(magnitudePanel, "magnitude")
  contents += wrapPanel(phasePanel, "phase")

  /** Update the display with a new complex field.
    *
    * @param src Requester of the update (?).
    * @param data The new complex field data
    * @param time Current simulation time.
    */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    // We need to split the complex fields into two scalar fields.
    val complexData = data.asInstanceOf[ComplexFieldMemory]
    val magnitudeData = Array.ofDim[Float](rows, columns)
    val phaseData = Array.ofDim[Float](rows, columns)
    var maxMagnitude = 0.000001f  // Avoid division by 0
    for (row <- 0 until rows; col <- 0 until columns) {
      val complex: Complex = complexData.read(row, col)
      magnitudeData(row)(col) = complex.magnitude
      phaseData(row)(col) = complex.phase
      if (complex.magnitude > maxMagnitude)
        maxMagnitude = complex.magnitude
    }
    // Scale magnitude to lie in range [0, 1]
    for (row <- 0 until rows; col <- 0 until columns) {
      magnitudeData(row)(col) /= maxMagnitude
    }
    magnitudePanel.update(Seq(magnitudeData))
    phasePanel.update(Seq(phaseData))
  }

  def zoomIn() {
    magnitudePanel.zoomIn()
    phasePanel.zoomIn()
  }
  def zoomOut() {
    magnitudePanel.zoomOut()
    phasePanel.zoomOut()
  }

  /** Force floating max to be true. */
  private def setFloatingMax(panel: ScalarMemoryView) {
    //panel.viewerTools.floatMaxButton.selected = true
    panel.FloatingMaxProperty.value = true
  }

  /** Build a small panel which shows color --> direction information. */
  private def buildColorKey(size: Int): Panel = {
    /*
    val rows = size | 1; val cols = size | 1
    val centerRow = rows / 2; val centerCol = cols / 2
    val maxLen = centerRow min centerCol
    val normalize = 1f / maxLen

    val img = new BufferedImage(cols, rows, BufferedImage.TYPE_3BYTE_BGR)
    val raster = img.getRaster
    for (row <- 0 until rows; col <- 0 until cols) {
      val v = Vector((row - centerRow) * normalize, (col - centerCol) * normalize)
      val px = complexToColorMap.map(v)
      val (r, g, b) = (px.red, px.green, px.blue)

      if (v.normL2 <= 1f ) {
        raster.setSample(col, row, 0, b)
        raster.setSample(col, row, 1, g)
        raster.setSample(col, row, 2, r)
      } else {
        val rgb = background.getRGB
        raster.setSample(col, row, 0, rgb >> 0  & 0xFF)
        raster.setSample(col, row, 1, rgb >> 8  & 0xFF)
        raster.setSample(col, row, 2, rgb >> 16 & 0xFF)
      }
    }

    // Draw grid
    val g = img.getGraphics
    g.setColor(java.awt.Color.BLACK)
    g.drawLine(centerRow, 0, centerRow, rows)
    g.drawLine(0, centerCol, cols, centerCol)
    g.dispose()

    new ImagePanel(img)
    */
    null
  }

  /** Wraps a panel with another panel, removing the wrapped panel's toolbar
    * while adding a title and legend.
    *
    * @param panel The panel being wrapped.
    * @param title The title displayed for the wrapped panel.
    */
  private def wrapPanel(panel: ScalarMemoryView, title: String): BorderPanel = {
    //panel.layout -= panel.viewerTools // Hide the panel's toolbar
    val label = new Label(title)
    val legend = new BoxPanel(Orientation.Horizontal)
    //legend.contents ++= panel.viewerTools.legendComps
    legend.contents ++= panel.toolbarComponents(panel.legendGroupIdx).components
    new BorderPanel() {
      border = Swing.LineBorder(java.awt.Color.gray)
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  }


  class ComplexToColorMap(vectorClipLength: Float, private val MaxIndex: Int = 100) {
    import scala.math._
    private val MapSize = 2 * MaxIndex + 1
    private val map = Array.ofDim[Pixel](MapSize, MapSize)
    private val Span = (2 * Pi / 3).toFloat

    // Initialize the lookup table.
    for (row <- -MaxIndex to MaxIndex; col <- -MaxIndex to MaxIndex) {
      // Convert (row, col) to polar coordinates with angles in (-Pi, Pi)
      var angle = atan2(row, col).toFloat
      while (angle < 0)
        angle += 2 * Pi.toFloat
      val value = sqrt(row * row  + col * col).toFloat
      val saturation = 1.0f

      // This doesn't match the paper, but the transformation is not specified.
      // Good enough for now.
      val (red, green, blue) = hsv2rgb(angle, saturation, value)
      update(row, col, new Pixel(red, green, blue))
    }

    /** Read map at ("row", "col") using indices on (-MaxIndex, MaxIndex). */
    private def apply(row: Int, col: Int): Pixel = {
      val r = (row min MaxIndex) max -MaxIndex
      val c = (col min MaxIndex) max -MaxIndex
      map(r + MaxIndex)(c + MaxIndex)
    }

    /** Write ("row", "col") with "color", indices on (-MaxIndex, MaxIndex). */
    private def update(row: Int, col: Int, color: Pixel) {
      map(row + MaxIndex)(col + MaxIndex) = color
    }

    /** Convert HSV encoding to RGB. See the Wikipedia article on HSL and HSV for
      * a description of the encoding and the algorithm.
      */
    private def hsv2rgb(h: Float, s: Float, v: Float): (Float, Float, Float) = {
      require(h >= 0 && h <= 2 * Pi)
      require(s >= 0 && s <= 1)
      require(v >= 0 && v <= 1)
      val hDegrees = (h / (2 * Pi)) * 360
      val hi = math.floor(hDegrees / 60.0).toInt % 6
      val f =  ((hDegrees / 60.0) - math.floor(hDegrees / 60.0)).toFloat
      val p = v * (1.0f - s)
      val q = v * (1.0f - (f*s))
      val t = v * (1.0f - ((1.0f - f) * s))
      hi match {
        case 0 => (v, t, p)
        case 1 => (q, v, p)
        case 2 => (p, v, t)
        case 3 => (p, q, v)
        case 4 => (t, p, v)
        case 5 => (v, p, q)
      }
    }

    /** Map complex number to a Pixel.
     *
      * @param complex The complex number to be represented.
      * @return Pixel
      */
    def map(complex: Complex): Pixel = {
      val row = (complex.imaginary * MaxIndex).toInt
      val column = (complex.real * MaxIndex).toInt
      val pixel = apply(row, column)
      require(pixel != null)
      pixel
    }
  }

}