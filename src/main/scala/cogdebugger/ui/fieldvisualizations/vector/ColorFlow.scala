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

import libcog.{Shape, AbstractFieldMemory, VectorFieldReader, Vector, Pixel}
import cogdebugger.PropertyValueChanged
import cogdebugger.ui.fieldvisualizations.{ZoomProperty, EventDrivenViewer}
import cogdebugger.ui.components.{DoubleBufferedImagePanel, ToolBar, ImagePanel}
import java.awt.image.BufferedImage
import javax.swing.event.ChangeEvent
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.math._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 8/18/13
 * Time: 7:58 PM
 */
class ColorFlow(target: AnyRef, fieldShape: Shape, tensorShape: Shape)
        extends BorderPanel
        with EventDrivenViewer
        with ZoomProperty {

  require(fieldShape.dimensions == 2)
  require(tensorShape == Shape(2))

  val targets = Set(target)

  private val rows = fieldShape(0)
  private val cols = fieldShape(1)

  private var _vectorClipLength = 1f
  private val vectorToColorMap: VecToColorMap =
    ColorFlow.MiddleburyColorKey
  private def vectorClipLength = _vectorClipLength
  private def vectorClipLength_=(length: Float) {
    _vectorClipLength = length
  }

  /** Temp storage for input field memory's read methods. */
  private val tmpVector = Vector(0, 0)
  /** Temp storage for rgb color information from our Vector -> Color map. */
  private val rgb = Array.ofDim[Int](3)
  private def red   = rgb(0)
  private def green = rgb(1)
  private def blue  = rgb(2)

  private val imgPanel = new DoubleBufferedImagePanel(new Dimension(cols, rows), BufferedImage.TYPE_3BYTE_BGR)
  private val toolbar = new ColorFlowToolBar()
  private val key = buildColorKey(64)

  val zoomIncrement: Float = 1f
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      imgPanel.zoomLevel = newValue
  }
  properties += ZoomProperty

  add(imgPanel, BorderPanel.Position.Center)
  add(toolbar, BorderPanel.Position.North)

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    data match {
      case data: VectorFieldReader =>
        imgPanel.update(raster => {
          for (r <- 0 until rows; c <- 0 until cols) {
            data.read(r, c, tmpVector)
            vectorToColorMap(tmpVector, rgb)
            raster.setSample(c, r, 0, red)
            raster.setSample(c, r, 1, green)
            raster.setSample(c, r, 2, blue)
          }
        })
      case x => throw new RuntimeException("Unsupported data type: "+x)
    }
  }

  def toggleKeyVisibility() {
    if (toolbar.showKeyButton.selected)
      layout(key) = BorderPanel.Position.East
    else
      layout -= key
    revalidate()
  }

  private def buildColorKey(size: Int) = {
    val rows = size | 1; val cols = size | 1
    val centerRow = rows / 2; val centerCol = cols / 2
    val maxLen = centerRow min centerCol
    val normalize = vectorClipLength / maxLen

    val img = new BufferedImage(cols, rows, BufferedImage.TYPE_3BYTE_BGR)
    val raster = img.getRaster
    for (row <- 0 until rows; col <- 0 until cols) {
      val dx = (col - centerCol) * normalize
      val dy = (row - centerRow) * normalize
      val v = Vector(dy, dx)
      ColorFlow.MiddleburyColorKey.computeColor(dx, dy, rgb)

      if (v.normL2 <= vectorClipLength) {
        raster.setSample(col, row, 0, red)
        raster.setSample(col, row, 1, green)
        raster.setSample(col, row, 2, blue)
      } else {
        val rgb = background.getRGB
        raster.setSample(col, row, 0, rgb >> 16 & 0xFF)
        raster.setSample(col, row, 1, rgb >> 8  & 0xFF)
        raster.setSample(col, row, 2, rgb >> 0  & 0xFF)
      }
    }

    // Draw grid
    val g = img.getGraphics
    g.setColor(java.awt.Color.BLACK)
    g.drawLine(centerRow, 0, centerRow, rows)
    g.drawLine(0, centerCol, cols, centerCol)
    g.dispose()

    new ImagePanel(img)
  }

  class ColorFlowToolBar extends ToolBar("Color Flow Tools") {

    val model = new javax.swing.SpinnerNumberModel(1, 0, 100, 1)
    val jSpinner = new javax.swing.JSpinner(model)
    jSpinner.addChangeListener(new javax.swing.event.ChangeListener() {
      def stateChanged(e: ChangeEvent) { vectorClipLength = jSpinner.getValue.asInstanceOf[Int] }
    })
    val spinner = Component.wrap(jSpinner)

    val showKeyButton = new ToggleButton("Show key")
    listenTo(showKeyButton)
    reactions += { case ButtonClicked(`showKeyButton`) => toggleKeyVisibility() }

    contents += Button("+") { zoomIn() }
    contents += Button("+") { zoomOut() }
    contents += Swing.HStrut(10)
    contents += new Label("Clamp to:")
    contents += Swing.HStrut(5)
    contents += spinner
    contents += Swing.HGlue
    contents += showKeyButton
  }

}

object ColorFlow {
  object MiddleburyColorKey extends VecToColorMap {
    private val map = makeColorWheel
    def mapSize = map.length

    /** Map the given angle (in degrees) to a color. The vector is assumed to
      * be the maximum allowable length, so the returned color is maximally
      * saturated.
      * @param deg Angle in degrees
      * @param rgb An array of at least length 3 in which to store color
      *            information. Red at index 0, green at 1, and blue at 2. */
    def apply(deg: Float, rgb: Array[Int]) = {
      var d = deg % 360
      while (d < 0) d += 360

      // Our angle probably falls between two "buckets" - we dither the color
      // between the two.
      val normalizedAngle = d / 360f * (mapSize - 1)
      val idx0 = normalizedAngle.toInt // First bucket
      val idx1 = (idx0 + 1) % mapSize  // Second bucket
      val f = normalizedAngle - idx0   // How far into the first bucket?
      // TODO `rgb` shouldn't be created every time we need to map a vector
      // but it simplifies threading (the debugger does some drawing in
      // background threads; it's possible multiple ColorFlow views are going
      // in parallel)
      for (colorBand <- 0 until 3) {
        val col0 = map(idx0)(colorBand) / 255.0
        val col1 = map(idx1)(colorBand) / 255.0
        val col = (1 - f) * col0 + f * col1 // Mix buckets; like alpha blending
        rgb(colorBand) = (255.0 * col).toInt
      }
      rgb
    }

    /** Map a vector to a color, storing the color in the given array. Note
      * that the y component is expected to be the first vector component, and
      * x the second. Vectors with a magnitude > 1 are considered "out of
      * range" and while the returned color will have a hue appropriate for the
      * vector's angle, the brightness will be only 75% of max (a holdover from
      * the orignal Middlebury implementation of the color key). Also keep in
      * mind that in a (row, column) coordinate system (like Cog's) y inceases
      * in the ''downward'' direction.
      * @param v A 2-dimensional vector, ordered (y, x)
      * @param rgb An array of at least length three in which to store the
      *            mapped colors rgb components
      */
    def apply(v: Vector, rgb: Array[Int]) {
      computeColor(v(1), v(0), rgb)
    }

    /** As `apply(Vector, Array[Int])`, but this method allocates and returns
      * a Color object.
      * @param v A 2-dimensional vector, ordered (y, x)
      * @return The color representing the given vector, based on its
      *         orientation and magnitude.
      */
    def apply(v: Vector) = {
      require(v.size == 2, "Not a 2D vector.")
      val rgb = Array.ofDim[Int](3)
      computeColor(v(1), v(0), rgb)
      new Color(rgb(0), rgb(1), rgb(2))
    }

    /** Map a vector with the given x and y components to a color. Vectors
      * with a magnitude > 1 are considered "out of range" and while the
      * returned color will have a hue appropriate for the vector's angle,
      * the brightness will be only 75% of max (a holdover from the orignal
      * Middlebury implementation of the color key).
      * @param fx x component of the vector
      * @param fy y component of the vector
      * @param rgb An array of at least length three in which to store the rgb
      *            components of the mapped color
      */
    def computeColor(fx: Float, fy: Float, rgb: Array[Int]) {
      val rad = sqrt(fx * fx + fy * fy)
      val a = atan2(-fy, -fx) / Pi
      val fk = (a + 1.0) / 2.0 * (mapSize - 1)
      val k0 = fk.toInt
      val k1 = (k0 + 1) % mapSize
      val f = fk - k0
      //f = 0; // uncomment to see original color wheel
      for (b <- 0 until 3) {
        val col0 = map(k0)(b) / 255.0
        val col1 = map(k1)(b) / 255.0
        var col = (1 - f) * col0 + f * col1
        if (rad <= 1)
          col = 1 - rad * (1 - col) // increase saturation with radius
        else
          col *= .75 // out of range
        rgb(b) = (255.0 * col).toInt
      }
    }

    /** Creates the color wheel that maps a vector to a color based on its
      * orientation and magnitude. This mapping resembles the a straightforward
      * mapping of vector angle to hue in an HSV color space, but the lengths
      * of the transitions from one color to the next are tweaked based on the
      * human eye's limitations in distinguishing shades of certain colors.
      */
    private def makeColorWheel = {
      // relative lengths of color transitions:
      // these are chosen based on perceptual similarity
      // (e.g. one can distinguish more shades between red and yellow
      //  than between yellow and green)
      val multiplier = 1
      val RY = 15 * multiplier
      val YG = 6 * multiplier
      val GC = 4 * multiplier
      val CB = 11 * multiplier
      val BM = 13 * multiplier
      val MR = 6 * multiplier
      val nCols = RY + YG + GC + CB + BM + MR

      val map = Array.ofDim[Int](nCols, 3)
      def setCols(r: Int, g: Int, b: Int, k: Int) {
        map(k)(0) = r
        map(k)(1) = g
        map(k)(2) = b
      }

      var k = 0
      for (i <- 0 until RY) {
        setCols(255, 255*i/RY, 0, k)
        k += 1
      }
      for (i <- 0 until YG) {
        setCols(255-255*i/YG, 255, 0, k)
        k += 1
      }
      for (i <- 0 until GC) {
        setCols(0, 255, 255*i/GC, k)
        k += 1
      }
      for (i <- 0 until CB) {
        setCols(0, 255-255*i/CB, 255, k)
        k += 1
      }
      for (i <- 0 until BM) {
        setCols(255*i/BM, 0, 255, k)
        k += 1
      }
      for (i <- 0 until MR) {
        setCols(255, 0, 255-255*i/MR, k)
        k += 1
      }

      map
    }
  }
}

trait VecToColorMap {
  /** Return a Color corresponding to the given vector based on its
    * orientation and magnitude. */
  def apply(v: Vector): Color
  /** Gets the color corresponding to the given vector based on its
    * orientation and magnitude, and fills the (first three elements of) the
    * `rgb` array with the color components - red at index 0, green at 1, and
    * blue at index 3. */
  def apply(v: Vector, rgb: Array[Int])
}

class VectorToHSVColorMap(vectorClipLength: Float, private val MaxIndex: Int = 100)
        extends VecToColorMap {
  private val MapSize = 2 * MaxIndex + 1
  private val map = Array.ofDim[Pixel](MapSize, MapSize)
  private val Span = (2 * Pi / 3).toFloat

  // Initialize the lookup table.
  for (row <- -MaxIndex to MaxIndex; col <- -MaxIndex to MaxIndex) {
    // Convert (row, col) to polar coordinates with angles in (-Pi, Pi)
    var angle = atan2(row, col).toFloat
    while (angle < 0)
      angle += 2 * Pi.toFloat
    val length = sqrt(row * row  + col * col).toFloat
    val saturation = (length / MaxIndex) min 1.0f

    // This doesn't match the paper, but the transformation is not specified.
    // Good enough for now.
    val (red, green, blue) = hsv2rgb(angle, saturation, 1)
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

  /** Map "vector" to a Pixel. */
  def map(vector: Vector): Pixel = {
    val row = (vector(0) * MaxIndex / vectorClipLength).toInt
    val col = (vector(1) * MaxIndex / vectorClipLength).toInt
    val pixel = apply(row, col)
    require(pixel != null)
    pixel
  }

  def apply(vector: Vector, rgb: Array[Int]) = {
    val px = map(vector)
    rgb(0) = px.redInt
    rgb(1) = px.greenInt
    rgb(2) = px.blueInt
  }
  
  def apply(vector: Vector) = {
    val px = map(vector)
    new Color(px.redInt, px.greenInt, px.blueInt)
  }
}
