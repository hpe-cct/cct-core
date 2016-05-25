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

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.ui.components._
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState}
import scala.swing._
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent
import javax.swing.JPanel
import java.awt.Color
import scala.swing.event.ValueChanged
import cogdebugger.ui.fieldvisualizations.ComponentGroup
import cogdebugger.PropertyValueChanged


/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/21/13
 * Time: 2:43 PM
 */

/**
  * A panel for graphically displaying a Scalar field. Each layer in the field
  * is displayed as a separate grayscale image with dimensions columns x rows,
  * with each element in a layer represented by a single pixel.
  * <p>
  * The mapping from a value to a grayscale shade depends on the minimum and
  * maximum values in the whole field (and not just the layer). By default,
  * black represents a minimum and white a maximum, with a linear gradient
  * representing values in-between. Toggling the 'Invert' property to true will
  * reverse this gradient such that white represetns a minimum and black a
  * maximum.
  * <p>
  * The minimum and maximum used for shading purposes are by default
  * 'cumulative,' that is, they are the smallest and largest values seen in the
  * field across all updates since the last reset. This behavior can be changed
  * by toggling the 'Floating Max' property to true, which causes shading to be
  * based on the minimum and maxmimum values present in the field only at the
  * latest update, ignoring all previous updates.
  * <p>
  * All put together, it looks something like this for a three-dimensional
  * scalar field with 6 layers:
  * <pre>
  *      +-+              +-+
  *      |*| = -3.62      | | = 3.62
  *      +-+              +-+
  *
  *
  *      +---------+   +---------+   +---------+
  *      |         |   |         |   |         |
  *      | layer 0 |   | layer 1 |   | layer 2 |
  *      |         |   |         |   |         |
  *      +---------+   +---------+   +---------+
  *      +---------+   +---------+   +---------+
  *      |         |   |         |   |         |
  *      | layer 3 |   | layer 4 |   | layer 5 |
  *      |         |   |         |   |         |
  *      +---------+   +---------+   +---------+
  * </pre>
  *
  * @param fieldType The shape of the fields being visualized.
  */
class ScalarMemoryView (fieldType: FieldType)
    extends BorderPanel
    with EventDrivenViewer
    with ZoomProperty
    with RestorableState
    with ToolbarItems {

  def this(target: ProbedField) = this(target.fieldType)

  // An alias for this used in some pattern matches, since Scala won't accept
  // `this` in backticks
  private val Outer = this

  private def fieldShape = fieldType.fieldShape // convenient alias

  val dimensions = fieldType.dimensions
  val (layers, rows, columns) = fieldShape.dimensions match {
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case 3 => (fieldShape(0), fieldShape(1), fieldShape(2))
  }

  /** Determines the value of min and max when shading. When set to false (the
    * default setting) min and max consider all values in the field since this
    * view was reset. When set to true, min and max consider only the data in
    * the latest update. */
  val FloatingMaxProperty = new BooleanProperty("Floating Max", false)

  /** Determines the color mapping from values in the field to grayscale
    * shades. When set to false (the default), black represents a minimum and
    * white a maximum. When true, white represents a minimum and black a
    * maximum. */
  val InvertProperty = new BooleanProperty("Invert", false)

  val imgPanels = Array.tabulate(layers) { layer => buildImage(layer) }
  val wrapPanel = new WrapPanel(); wrapPanel.contents ++= imgPanels
  var absMin = Float.MaxValue // Smallest value seen since last reset
  var absMax = Float.MinValue // Largest value seen since last reset
  var stepMin = absMin // Smallest value in the latest update
  var stepMax = absMax // Largest value in the latest update

  properties += FloatingMaxProperty
  properties += InvertProperty
  properties += ZoomProperty

  FloatingMaxProperty.action =
    Action("Floating Max") {
      //viewerTools.updateLabels()
      MinMax.updateMinMax(min, max)
      if (lastData != null)
        // TODO Move this potentially expensive rendering off the EDT
        lastData match {
          case sfr: ScalarFieldReader => render(sfr)
          case arr: Seq[Array[Array[Float]]] @unchecked => render(arr)
        }
    }

  // We have to set the text to something other than null for Swing to turn on
  // tooltips. It doesn't matter what the String contains; our panel
  // implemntation overrides getToolTipText to build a String on the fly.
  imgPanels.foreach { panel => panel.tooltip = "Enable" }

  def min = if (FloatingMaxProperty.value) stepMin else absMin
  def max = if (FloatingMaxProperty.value) stepMax else absMax

  add(wrapPanel, BorderPanel.Position.Center)

  final def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 0) => update(src, data.asInstanceOf[ScalarFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  /** A reference to the data this viewer was last updated with. Sometimes its
    * necessary to redraw the visualization between updates (e.g. in response
    * to 'invert'). */
  var lastData: AnyRef = null
  //var lastData: ScalarFieldReader = null

  /** Updates the visualization based on the contents of `data`. */
  final def update(src: AnyRef, data: ScalarFieldReader) {
    updateMinAndMax(data)
    render(data)
    lastData = data
  }

  /** Default zoom step size. */
  val zoomIncrement = 1f

  listenTo(InvertProperty, ZoomProperty)
  reactions += {
    case PropertyValueChanged(InvertProperty, _, _) =>
      if (lastData != null)
      // TODO Move this potentially expensive rendering off the EDT
        lastData match {
          case sfr: ScalarFieldReader => render(sfr)
          case arr: Seq[Array[Array[Float]]] @unchecked => render(arr)
        }

    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      imgPanels.foreach(_.zoomLevel = newValue)
      wrapPanel.revalidate()
      wrapPanel.repaint()
  }

  /** Update absMin, absMax, stepMin, and stepMax according to the contents of
    * `data`.*/
  protected def updateMinAndMax(data: ScalarFieldReader) {
    stepMin = Float.MaxValue
    stepMax = Float.MinValue
    for (f <- data) {
      if (!f.isNaN) {
        if (f > absMax) { absMax = f; stepMax = f }
        else if (f > stepMax) stepMax = f
        if (f < absMin) { absMin = f; stepMin = f }
        else if (f < stepMin) stepMin = f
      }
    }
    MinMax.updateMinMax(min, max)
  }

  protected def render(data: ScalarFieldReader) {
    val iterator: Iterator[Float] = data.iterator
    dimensions match {
      case 0 =>
        imgPanels(0).update(_.setSample(0, 0, 0, scaleValue(iterator.next())))
      case 1 =>
        imgPanels(0).update(raster =>
          for (c <- 0 until fieldShape(0))
            raster.setSample(c, 0, 0, scaleValue(iterator.next()))
        )
      case 2 =>
        imgPanels(0).update(raster =>
          for (r <- 0 until fieldShape(0); c <- 0 until fieldShape(1))
            raster.setSample(c, r, 0, scaleValue(iterator.next()))
        )
      case 3 =>
        for (l <- 0 until fieldShape(0)) {
          imgPanels(l).update(raster =>
            for (r <- 0 until fieldShape(1); c <- 0 until fieldShape(2))
              raster.setSample(c, r, 0, scaleValue(iterator.next()))
          )
        }
      case x => throw new RuntimeException("Unsupported field dimension: "+x)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // A collection of overloaded methods to let this viewer be re-used by     //
  // other viewers that break fields apart into scalar fields. Initially     //
  // added for use in ComponentsPanel, which treats each plane of a vector   //
  // field as a scalar field.                                                //

  final def update(data: Seq[Array[Array[Float]]]) {
    updateMinAndMax(data)
    render(data)
    lastData = data
  }
  protected def updateMinAndMax(data: Seq[Array[Array[Float]]]) {
    stepMin = Float.MaxValue
    stepMax = Float.MinValue
    for (field <- data; row <- field; f <- row) {
      if (!f.isNaN) {
        if (f > absMax) { absMax = f; stepMax = f }
        else if (f > stepMax) stepMax = f
        if (f < absMin) { absMin = f; stepMin = f }
        else if (f < stepMin) stepMin = f
      }
    }
    //viewerTools.updateLabels()
    MinMax.updateMinMax(min, max)
  }
  protected def render(data: Seq[Array[Array[Float]]]) {
    for ((field, idx) <- data.zipWithIndex) {
      imgPanels(idx).update(raster =>
        for (r <- 0 until field.length; c <- 0 until field(0).length)
          raster.setSample(c, r, 0, scaleValue(field(r)(c)))
      )
    }
  }

  /////////////////////////////////////////////////////////////////////////////

  /** Maps a float value to a (grayscale) shade based on the current values of
    * min and max. This respects the Invert image button which transposes
    * black and white. */
  protected def scaleValue(f: Float): Float = {
    if (!InvertProperty.value)
      // no inversion
      (f - min) / (max - min) * 255f
    else
      // invert grayscale
      255.0f - ((f - min) / (max - min) * 255f)
  }

  /** Build an image into which to render a specific layer of a scalar field.
    * Supplying the correct layer index is important for tooltips to work
    * correctly. */
  protected def buildImage(layer: Int) = {
    val dim = dimensions match {
      case 0 => new Dimension(1, 1)
      case 1 => new Dimension(fieldShape(0), 1)
      case 2 => new Dimension(fieldShape(1), fieldShape(0))
      case 3 => new Dimension(fieldShape(2), fieldShape(1))
      case x => throw new RuntimeException("Unsupported field dimension: "+x)
    }
    new DoubleBufferedImagePanel(dim, BufferedImage.TYPE_BYTE_GRAY) {
      override lazy val peer = new JPanel with SuperMixin {
        override def getToolTipText(event: MouseEvent) = {
          // It may be impossible to get the tooltip for certain points in the
          // field if zoomLevel is < 1
          val row = math.floor(event.getY / zoomLevel).toInt
          val col = math.floor(event.getX / zoomLevel).toInt
          popupText(layer, row, col)
        }
      }
    }
  }

  protected def popupText(layer: Int, row: Int, column: Int) = {
    if (lastData != null &&
            layer >= 0 && layer < layers &&
            row >= 0 && row < rows &&
            column >= 0 && column < columns) {
      lastData match {
        case sfr: ScalarFieldReader =>
          sfr.fieldShape.dimensions match {
            case 0 =>
              val value = sfr.read()
              val hex = "%08x" format java.lang.Float.floatToRawIntBits(value)
              s"(): ${value}   [0x" + hex + "]"
            case 1 =>
              val value = sfr.read(column)
              val hex = "%08x" format java.lang.Float.floatToRawIntBits(value)
              s"($column): ${value}   [0x" + hex + "]"
            case 2 =>
              val value = sfr.read(row, column)
              val hex = "%08x" format java.lang.Float.floatToRawIntBits(value)
              s"($row, $column): ${value}   [0x" + hex + "]"
            case 3 =>
              val value = sfr.read(layer, row, column)
              val hex = "%08x" format java.lang.Float.floatToRawIntBits(value)
              s"($layer, $row, $column): ${value}  [0x" + hex + "]"
            case x =>
              "Unsupported dimension: " + x
          }
        case arr: Seq[Array[Array[Float]]] @unchecked =>
          s"($row, $column): ${arr(layer)(row)(column)}"
      }
    } else
      null // No tooltip to display
  }

  object MinMax extends Publisher {
    def updateMinMax(newMin: Float, newMax: Float) {
      publish(new ValueChanged(Outer))
    }
  }

  /** Index of the ComponentGroup that contains the user controls, as returned
    * by the toolbarComponents method. */
  val controlGroupIdx = 0

  /** Index of the ComponentGroup that contrains the color key, as returned by
    * the toolbarComponents method. */
  val legendGroupIdx  = 1

  /** Builds and returns a new collection of components that supplement this
    * viewer (either user controls or additional displays) suitable for
    * installation in a toolbar. Components are logically grouped into
    * ComponentGroups based on their function (e.g. all the components
    * responsible for showing the color key are collected into a single
    * ComponentGroup).
    */
  def toolbarComponents: Seq[ComponentGroup] = {

    val floatMaxButton = ToolFactory.toggle(FloatingMaxProperty)
    val invertButton = ToolFactory.toggle(InvertProperty)
    val controlsGroup = ComponentGroup(floatMaxButton, invertButton)

    val (minColor, maxColor) =
      if (InvertProperty.value)
        (java.awt.Color.WHITE, java.awt.Color.BLACK)
      else
        (java.awt.Color.BLACK, java.awt.Color.WHITE)

    val minIcon = new SquareIcon(minColor)
    val minKey = new Label(" = ", minIcon, Alignment.Left)
    minKey.tooltip = "The color used to indicate a minimum value."
    minKey.listenTo(InvertProperty)
    minKey.reactions += {
      case PropertyValueChanged(InvertProperty, _, invert: Boolean) =>
        if (invert) minIcon.color = Color.WHITE
        else minIcon.color        = Color.BLACK
        minKey.repaint()
    }

    val maxIcon = new SquareIcon(maxColor)
    val maxKey = new Label(" = ", maxIcon, Alignment.Left)
    maxKey.tooltip = "The color used to indicate a maximum value."
    maxKey.listenTo(InvertProperty)
    maxKey.reactions += {
      case PropertyValueChanged(InvertProperty, _, invert: Boolean) =>
        if (invert) maxIcon.color = Color.BLACK
        else maxIcon.color        = Color.WHITE
        maxKey.repaint()
    }

    val minLabel = new Label("?", Swing.EmptyIcon, Alignment.Leading)
    val maxLabel = new Label("?", Swing.EmptyIcon, Alignment.Leading)
    minLabel.preferredSize = new Dimension(60, minLabel.preferredSize.height)
    maxLabel.preferredSize = new Dimension(60, maxLabel.preferredSize.height)

    val formatString = "%6.3f"
    minLabel.listenTo(MinMax)
    minLabel.reactions += {
      case ValueChanged(Outer) => minLabel.text = formatString.format(min)
    }
    maxLabel.listenTo(MinMax)
    maxLabel.reactions += {
      case ValueChanged(Outer) => maxLabel.text = formatString.format(max)
    }

    val legendGroup = ComponentGroup(minKey, minLabel, Swing.HStrut(1), maxKey, maxLabel)

    Seq(controlsGroup, legendGroup)

  }

  def save: scala.xml.Elem =
    <ScalarView>
      { propertiesTag }
    </ScalarView>

  def restore(savedState: scala.xml.Node) {
    (savedState \ "ScalarView" \ "properties").headOption.foreach(xmlToProperties)
  }

}
