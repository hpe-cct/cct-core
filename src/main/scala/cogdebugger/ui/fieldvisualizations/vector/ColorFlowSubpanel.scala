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

import libcog._
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.components._
import java.awt.image.BufferedImage
import scala.swing._
import javax.swing.{SpinnerNumberModel, JPanel}
import java.awt.event.MouseEvent
import cogdebugger._
import scala.xml.{Node, Elem}
import cogdebugger.ui.fieldvisualizations.ComponentGroup
import cogdebugger.PropertyValueChanged
import java.awt.Color
import scala.swing.Color

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 8/18/13
 * Time: 7:58 PM
 */

/** A visualization for VectorFields of two dimensions containing vectors of
  * two dimensions. Renders the field as a color image, one pixel per vector
  * in the field, with the color hue of a pixel denoting the orientation of
  * its corresponding vector and color saturation denoting magnitude. By
  * default, shorter vectors appear desaturated (white), and longer vectors
  * appear more saturated.
  *
  * To speed up rendering, this visualization does '''not''' attempt to
  * discover or report the minimum and maximum lengths of vectors in the field.
  * Instead, the `toolbarComponents` method provides a control that allows
  * users to specify a 'clip length' denoting the magnitude at which vectors
  * should appear maximally saturated in the visualization. Vectors longer than
  * this clip length are considered "out of range" and rendered as black.
  * Vectors shorter than this clip length will appear more and more desaturated
  * as their length approaches zero (how desaturated is proportional to their
  * distance between the clip length and zero), with vectors of length zero
  * rendering as pure white.
  *
  * The color encoding used by this visualization is based on the encoding used
  * in the Middlebury studies on Optical Flow. Rather than a more
  * straightforward translation from from vector orientation to hue in an HSV
  * color space, the Middlebury institude modified their transformation on the
  * basis that certain colors are difficult for human eyes to differentiate.
  * Thus, in the HSV color space, the rate at which colors ''appear'' to vary
  * with orientation is non-linear. The Middlebury color key stretches some
  * color transitions while compressing others in attempt to make the color
  * space appear more linear to human vision and thus make the vector display
  * easier to interpret.
  *
  * @param fieldType A FieldType describing the shape of the field and its
  *                  vectors.
  */
class ColorFlowSubpanel(fieldType: FieldType)
        extends BorderPanel
        with Viewer
        with ZoomProperty
        with RestorableState
        with ToolbarItems
{

  private def fieldShape  = fieldType.fieldShape
  private def tensorShape = fieldType.tensorShape

  require(fieldShape.dimensions == 2)
  require(tensorShape == Shape(2))

  private val rows = fieldShape(0)
  private val cols = fieldShape(1)

  /** Default zoom step size. */
  zoomType = ZoomableProperty.ZoomType.Multiplicative
  val zoomIncrement: Float = 1.1f

  private val tmpVector = Vector(0, 0)
  private val rgb = Array.ofDim[Int](3)
  private def red   = rgb(0)
  private def green = rgb(1)
  private def blue  = rgb(2)

  /** Vectors longer than this value are clipped to it. */
  val VectorClipProperty = new FloatProperty("Vector Clip Length", 1f)
  private val vectorToColorMap: VecToColorMap = {
//    // The stock Middlebury color key. Renders out of range vectors at 75%
//    // brightness. Runs slow.
//    ColorFlow.MiddleburyColorKey

//    // Modified Middlebury color key. Renders out of range vectors as black.
//    // Still slow.
//    new VecToColorMap {
//      /** Gets the color corresponding to the given vector based on its
//        * orientation and magnitude, and fills the (first three elements of) the
//        * `rgb` array with the color components - red at index 0, green at 1, and
//        * blue at index 3. */
//      override def apply(v: Vector, rgb: Array[Int]) {
//        if (v.normL2 > VectorClipProperty.value) {
//          rgb(0) = 0; rgb(1) = 0; rgb(2) = 0
//        } else {
//          ColorFlow.MiddleburyColorKey.computeColor(v(1), v(0), rgb)
//        }
//      }
//
//      /** Return a Color corresponding to the given vector based on its
//        * orientation and magnitude. */
//      override def apply(v: Vector): Color = {
//        apply(v, rgb)
//        new Color(rgb(0), rgb(1), rgb(2))
//      }
//    }

    /** The Middlebury color flow key as originally implemented does a bunch
      * of math for each color lookup (largely due to color dithering for
      * angles that map "between" two buckets) and runs slow compared to our
      * original HSV color key. This lookup table avoids dithering and so
      * should be faster, and as long as its large enough, any color
      * differences won't be detectable by human eyes.
      */
    new VecToColorMap {
      val Size = 100 | 1
      val Center = Size / 2
      val map = Array.ofDim[Color](Size, Size)
      for (r <- 0 until Size; c <- 0 until Size) {
        val dx = (c.toFloat - Center) / Center
        val dy = (r.toFloat - Center) / Center
        //ColorFlow.MiddleburyColorKey.computeColor(dx, dy, rgb)
        //map(r)(c) = new Color(red, green, blue)
        val angle = math.atan2(dy, dx).toFloat
        val len   = math.sqrt(dx * dx + dy * dy)
        if (len < 1) {
          ColorFlow.MiddleburyColorKey.computeColor(dx, dy, rgb)
          map(r)(c) = new Color(red, green, blue)
        } else {
          ColorFlow.MiddleburyColorKey(angle, rgb)
          map(r)(c) = new Color(red, green, blue)
        }
      }

      /** Return a Color corresponding to the given vector based on its
        * orientation and magnitude. Out of range vectors (those with
        * magnitude > 1) are rendered as black.
        */
      def apply(v: Vector): Color = {
        // We used to use v.normL2 here, but it's quite slow compared to
        // manually computing vector length.
        val len = math.sqrt(v(0) * v(0) + v(1) * v(1))
        if (len > 1) {
          Color.BLACK
        } else {
          val y = math.round(v(0) * Center) + Center
          val x = math.round(v(1) * Center) + Center
          val r = y max 0 min (Size - 1)
          val c = x max 0 min (Size - 1)
          map(r)(c)
        }
      }

      /** Gets the color corresponding to the given vector based on its
        * orientation and magnitude and fills the (first three elements of)
        * the `rgb` array with the color components - red at index 0, green at
        * 1, and blue at index 3. Out of range vectors (those with magnitude >
        * 1) are rendered as black.
        */
      def apply(v: Vector, rgb: Array[Int]) {
        val color = apply(v)
        rgb(0) = color.getRed
        rgb(1) = color.getGreen
        rgb(2) = color.getBlue
      }

    }

  }

  val ShowKeyProperty = new BooleanProperty("Show Key", false)

  private val imgPanel = new DoubleBufferedImagePanel(new Dimension(cols, rows), BufferedImage.TYPE_3BYTE_BGR) {
    override lazy val peer = new JPanel with SuperMixin {
      override def getToolTipText(event: MouseEvent) = {
        super.getToolTipText(event)
        // It may be impossible to get the tooltip for certain points in the
        // field if zoomLevel is < 1
        val row = math.floor(event.getY / zoomLevel).toInt
        val col = math.floor(event.getX / zoomLevel).toInt
        popupText(row, col)
      }
    }
    override def maximumSize = {
      val w = math.ceil(cols * zoomLevel).toInt
      val h = math.ceil(rows * zoomLevel).toInt
      new Dimension(w, h)
    }
  }
  private val key = buildColorKey(64)

  // We have to set the text to something other than null for Swing to turn on
  // tooltips. It doesn't matter what the String contains; our panel
  // implementation overrides getToolTipText to build a String on the fly.
  imgPanel.tooltip = "Enable"

  listenTo(VectorClipProperty, ZoomProperty, ShowKeyProperty)
  reactions += {
    case PropertyValueChanged(VectorClipProperty, oldValue, newValue: Float) =>
      if (previousData != null) update(this, previousData, 0L)

    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      if (newValue > 0) imgPanel.zoomLevel = newValue

    case PropertyValueChanged(ShowKeyProperty, oldValue, newValue: Boolean) =>
      if (newValue)
        layout(key) = BorderPanel.Position.East
      else
        layout -= key
      revalidate()
  }
  properties += ZoomProperty
  properties += VectorClipProperty
  properties += ShowKeyProperty

  add(new WrapPanel(imgPanel), BorderPanel.Position.Center)

  private var previousData: AbstractFieldMemory = null

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    previousData = data
    val clipLen = VectorClipProperty.value // Capture clip length
    data match {
      case data: VectorFieldReader =>
        imgPanel.update(raster => {
          for (r <- 0 until rows; c <- 0 until cols) {
            data.read(r, c, tmpVector)
            //tmpVector *= (1 / clipLen)
            tmpVector(0) = tmpVector(0) / clipLen
            tmpVector(1) = tmpVector(1) / clipLen
            vectorToColorMap(tmpVector, rgb)
            raster.setSample(c, r, 0, red)
            raster.setSample(c, r, 1, green)
            raster.setSample(c, r, 2, blue)
          }
        })
      case data: ComplexFieldReader =>
        imgPanel.update(raster => {
          for (r <- 0 until rows; c <- 0 until cols) {
            val complex = data.read(r, c)
            tmpVector(0) = complex.real / clipLen
            tmpVector(1) = complex.imaginary / clipLen
            //tmpVector *= (1 / clipLen)
            vectorToColorMap(tmpVector, rgb)
            raster.setSample(c, r, 0, red)
            raster.setSample(c, r, 1, green)
            raster.setSample(c, r, 2, blue)
          }
        })
      case x => throw new RuntimeException("Unsupported data type: "+x)
    }
  }

  val tooltipTmpVector = cogx.Vector(0f, 0f)
  protected def popupText(row: Int, col: Int): String = {
    if (previousData != null && row < rows && col < cols) {
      previousData match {
        case vfr: VectorFieldReader =>
          vfr.read(row, col, tooltipTmpVector)
          s"($row, $col): ${tooltipTmpVector.toString}"
        case cfr: ComplexFieldReader =>
          s"($row, $col): ${cfr.read(row, col)}"
      }
    } else
      null // No tooltip to display
  }

  def save: Elem =
    <ColorFlowSubpanel>
      { propertiesTag }
    </ColorFlowSubpanel>

  def restore(tag: Node) {
    (tag \ "ColorFlowSubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  private def buildColorKey(size: Int) = {
    val rows = size | 1; val cols = size | 1
    val centerRow = rows / 2; val centerCol = cols / 2
    val maxLen = centerRow min centerCol
    val normalize = VectorClipProperty.value / maxLen

    val img = new BufferedImage(cols, rows, BufferedImage.TYPE_3BYTE_BGR)
    val raster = img.getRaster
    for (row <- 0 until rows; col <- 0 until cols) {
      val dx = (col - centerCol) * normalize
      val dy = (row - centerRow) * normalize
      val v = Vector(dy, dx)
      vectorToColorMap(v, rgb)
      //if (v.normL2 <= VectorClipProperty.value) {
        raster.setSample(col, row, 0, red)
        raster.setSample(col, row, 1, green)
        raster.setSample(col, row, 2, blue)
      //} else {
      //  val rgb = background.getRGB
      //  raster.setSample(col, row, 0, rgb >> 16 & 0xFF)
      //  raster.setSample(col, row, 1, rgb >> 8  & 0xFF)
      //  raster.setSample(col, row, 2, rgb >> 0  & 0xFF)
      //}
    }

    // Draw grid
    val g = img.getGraphics
    g.setColor(java.awt.Color.BLACK)
    g.drawLine(centerRow, 0, centerRow, rows)
    g.drawLine(0, centerCol, cols, centerCol)
    g.dispose()

    new ImagePanel(img)
  }

  def toolbarComponents: Seq[ComponentGroup] = {
    val spinnerLabel = new Label("Clamp to:")
    val spinner = ToolFactory.spinner(VectorClipProperty, 0, 100, 1f)
    spinner.model.asInstanceOf[SpinnerNumberModel].setMaximum(null)
    val showKeyButton = ToolFactory.toggle(ShowKeyProperty)
    val keyGroup = ComponentGroup(showKeyButton)
    val controlGroup = ComponentGroup(spinnerLabel, Swing.HStrut(5), spinner)
    Seq(keyGroup, controlGroup)
  }

}

//class VectorToHSVColorMap2(vectorClipLength: Float, private val MaxIndex: Int = 100)
//        extends VecToColorMap {
//  import scala.math._
//  private val MapSize = 2 * MaxIndex + 1
//  private val map = Array.ofDim[Pixel](MapSize, MapSize)
//  private val Span = (2 * Pi / 3).toFloat
//
//  // Initialize the lookup table.
//  for (row <- -MaxIndex to MaxIndex; col <- -MaxIndex to MaxIndex) {
//    // Convert (row, col) to polar coordinates with angles in (-Pi, Pi)
//    var angle = atan2(row, col).toFloat
//    while (angle < 0)
//      angle += 2 * Pi.toFloat
//    val length = sqrt(row * row  + col * col).toFloat
//    val saturation = (length / MaxIndex) min 1.0f
//
//    // This doesn't match the paper, but the transformation is not specified.
//    // Good enough for now.
//    val (red, green, blue) = hsv2rgb(angle, saturation, 1)
//    update(row, col, new Pixel(red, green, blue))
//  }
//
//  /** Read map at ("row", "col") using indices on (-MaxIndex, MaxIndex). */
//  private def apply(row: Int, col: Int): Pixel = {
//    val r = (row min MaxIndex) max -MaxIndex
//    val c = (col min MaxIndex) max -MaxIndex
//    map(r + MaxIndex)(c + MaxIndex)
//  }
//
//  /** Write ("row", "col") with "color", indices on (-MaxIndex, MaxIndex). */
//  private def update(row: Int, col: Int, color: Pixel) {
//    map(row + MaxIndex)(col + MaxIndex) = color
//  }
//
//  /** Convert HSV encoding to RGB. See the Wikipedia article on HSL and HSV for
//    * a description of the encoding and the algorithm.
//    */
//  private def hsv2rgb(h: Float, s: Float, v: Float): (Float, Float, Float) = {
//    require(h >= 0 && h <= 2 * Pi)
//    require(s >= 0 && s <= 1)
//    require(v >= 0 && v <= 1)
//    val hDegrees = (h / (2 * Pi)) * 360
//    val hi = math.floor(hDegrees / 60.0).toInt % 6
//    val f =  ((hDegrees / 60.0) - math.floor(hDegrees / 60.0)).toFloat
//    val p = v * (1.0f - s)
//    val q = v * (1.0f - (f*s))
//    val t = v * (1.0f - ((1.0f - f) * s))
//    hi match {
//      case 0 => (v, t, p)
//      case 1 => (q, v, p)
//      case 2 => (p, v, t)
//      case 3 => (p, q, v)
//      case 4 => (t, p, v)
//      case 5 => (v, p, q)
//    }
//  }
//
//  /** Map "vector" to a Pixel. */
//  def map(vector: Vector): Pixel = {
//    val row = (vector(0) * MaxIndex / vectorClipLength).toInt
//    val col = (vector(1) * MaxIndex / vectorClipLength).toInt
//    val pixel = apply(row, col)
//    require(pixel != null)
//    pixel
//  }
//
//  def apply(v: Vector) = {
//    val px = map(v)
//    new Color(px.redInt, px.greenInt, px.blueInt)
//  }
//}
