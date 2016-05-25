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

package cogdebugger.ui.fieldvisualizations.matrix

import libcog._
import cogdebugger._
import cogdebugger.ui.components._
import cogdebugger.ui.fieldvisualizations._
import scala.swing._
import scala.swing.event.ValueChanged
import scala.xml.{Node, Elem}
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent
import javax.swing.JPanel

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/26/13
 * Time: 2:38 PM
 */

/**
 * A panel for graphically displaying a Matrix field. Each Matrix
 * is displayed as a grayscale image of size rows x columns, with one entry in
 * the image for each element in the matrix.
 * <p>
 * The mapping of matrix element value to grayscale value depends on the
 * current state of the "Floating Max" toggle button in this viewer's toolbar.
 * When toggled off (the default state), the smallest value ever recorded by
 * the viewer (absMin) is rendered as black, the largest value ever recored
 * (absMax) is white, and intermediate data values display as gray
 * proportionate to where they lie between absMin and absMax.
 * If the toggle is on, the ends of the scale are determined by the values
 * of the current step only (stepMin and stepMax).
 * <p>
 * The display also shows the grayscale encoding to help interpret the view,
 * which looks roughly like this for a sequence of three output elements,
 * each with two inputs:
 * <pre>
 *
 *      +-+              +-+
 *      |*| = -3.62      | | = 3.62
 *      +-+              +-+
 *
 *
 *      +---------+   +---------+   +---------+
 *      |         |   |         |   |         |
 *      |  image  |   |  image  |   |  image  |
 *      |         |   |         |   |         |
 *      +---------+   +---------+   +---------+
 *      +---------+   +---------+   +---------+
 *      |         |   |         |   |         |
 *      |  image  |   |  image  |   |  image  |
 *      |         |   |         |   |         |
 *      +---------+   +---------+   +---------+
 * </pre>
 *
 * @param target The MatrixField to be displayed
 * @param fieldShape The shape of the matrixField
 * @param tensorShape The shape of tensors within the field
 */ 
class MatrixMemoryView(target: AnyRef, fieldShape: Shape, tensorShape: Shape)
    extends BorderPanel
    with EventDrivenViewer
    with ZoomProperty
    with RestorableState
    with ToolbarItems
{

  require(tensorShape.dimensions == 2, "That isn't a matrix field!")

  /** The (Cog4) fields this viewer is visualizing. */
  def targets = Set(target)

  val FloatingMaxProperty = new BooleanProperty("Floating Max", false)
  val InvertProperty = new BooleanProperty("Invert", false)

  /** Default zoom increment. */
  val zoomIncrement = 1f

  properties ++= Seq(FloatingMaxProperty, InvertProperty, ZoomProperty)

  protected var absMin = Float.MaxValue
  protected var absMax = Float.MinValue
  protected var stepMin = absMin
  protected var stepMax = absMax
  protected def min = if (FloatingMaxProperty.value) stepMin else absMin
  protected def max = if (FloatingMaxProperty.value) stepMax else absMax

  val (layers, rows, columns) = fieldShape.dimensions match {
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case 3 => (fieldShape(0), fieldShape(1), fieldShape(2))
    case x => throw new UnsupportedDimensionException(x)
  }

  val (matrixRows, matrixCols) = (tensorShape(0), tensorShape(1))
  val tmpMatrix = Matrix(matrixRows, matrixCols, (_,_) => 0f)

  // Each layer goes into its own panel to help pwith the layout
  val layerPanels = Array.tabulate(layers)(_ => new MatrixLayerPanel(rows, columns))
  val matrixPanels = Array.tabulate(layers * rows * columns)(_ => {
    new DoubleBufferedImagePanel(new Dimension(matrixCols, matrixRows), BufferedImage.TYPE_BYTE_GRAY) {
      var fieldLayer = -1
      var fieldRow = -1
      var fieldCol = -1
      override lazy val peer = new JPanel with SuperMixin {
        override def getToolTipText(event: MouseEvent) = {
          val row = math.floor(event.getY / zoomLevel).toInt
          val col = math.floor(event.getX / zoomLevel).toInt
          MatrixMemoryView.this.getToolTipText(fieldLayer, fieldRow, fieldCol, row, col)
        }
      }
    }
  })

  for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns) {
    val panel = matrixPanels(l * (rows * columns) + r * columns + c)
    panel.fieldLayer = l
    panel.fieldRow = r
    panel.fieldCol = c
    layerPanels(l) add panel
  }

  // We have to set the text to something other than null for Swing to turn on
  // tooltips. It doesn't matter what the String contains; our panel
  // implemntation overrides getToolTipText to build a String on the fly.
  matrixPanels.foreach(panel => panel.tooltip = "Enable")

  val centerPanel = new WrapPanel()
  centerPanel.contents ++= layerPanels.zipWithIndex.map {
    // Put a header indicating which layer we're showing
    case (panel: Panel, i: Int) =>
      new BorderPanel() {
        border = Swing.EmptyBorder
        add(new Label("Layer "+i), BorderPanel.Position.North)
        add(panel, BorderPanel.Position.Center)
      }
  }

  listenTo(FloatingMaxProperty, InvertProperty, ZoomProperty)
  reactions += {
    case PropertyValueChanged(FloatingMaxProperty, _, _) =>
      if (lastData != null) {
        updateImages(lastData)
        publish(new ValueChanged(this)) // Let legend know we've switched modes
      }
    case PropertyValueChanged(InvertProperty, _, _) =>
      if (lastData != null) updateImages(lastData)
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      matrixPanels.foreach(_.zoomLevel = newValue)
      centerPanel.revalidate()
  }

  add(centerPanel, BorderPanel.Position.Center)

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 2) => update(src, data.asInstanceOf[MatrixFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  protected var lastData: MatrixFieldReader = null

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: MatrixFieldReader) {
    updateMinAndMax(data)
    updateImages(data)
    lastData = data
  }

  private def updateMinAndMax(data: MatrixFieldReader) {
    stepMin = Float.MaxValue
    stepMax = Float.MinValue
    forEachMatrixIn(data) { matrix =>
      for (r <- 0 until matrixRows; c <- 0 until matrixCols) {
        val f = matrix(r, c)
        if (!f.isNaN) {
          if (f > absMax) { absMax = f; stepMax = f }
          else if (f > stepMax) stepMax = f
          if (f < absMin) { absMin = f; stepMin = f }
          else if (f < stepMin) stepMin = f
        }
      }
    }
    publish(new ValueChanged(this))
  }

  private def updateImages(data: MatrixFieldReader) {
    var imgIdx = 0
    forEachMatrixIn(data) { matrix =>
      matrixPanels(imgIdx).update(raster =>
        for (r <- 0 until matrixRows; c <- 0 until matrixCols)
          raster.setSample(c, r, 0, scaleValue(matrix(r, c)))
      )
      imgIdx += 1
    }
  }

  /** Maps a float value to a (grayscale) shade based on the current values of
    * min and max. This respects the Invert image button which transposes
    * black and white. */
  protected def scaleValue(f: Float): Float = {
    val shade = (f - min) / (max - min) * 255f
    if (InvertProperty.value)
      255f - shade // invert grayscale
    else
      shade        // no inversion
  }

  protected def forEachMatrixIn(field: MatrixFieldReader)(f: (Matrix) => Unit) {
    fieldShape.dimensions match {
      case 0 => field.read(tmpMatrix); f(tmpMatrix)
      case 1 => for (c <- 0 until columns) { field.read(c, tmpMatrix); f(tmpMatrix) }
      case 2 => for (r <- 0 until rows; c <- 0 until columns) { field.read(r, c, tmpMatrix); f(tmpMatrix) }
      case 3 => for (l <- 0 until layers; r <- 0 until rows; c <- 0 until columns) { field.read(l, r, c, tmpMatrix); f(tmpMatrix) }
      case x => throw new UnsupportedDimensionException(x)
    }
  }

  private val Outer = this

  def toolbarComponents: Seq[ComponentGroup] = {

    val floatMaxButton = ToolFactory.toggle(FloatingMaxProperty)
    val invertButton = ToolFactory.toggle(InvertProperty)

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

    val minLabel = new Label("?")
    val maxLabel = new Label("?")
    // Fix the size of the labels to prevent toolbar elements from shifting
    // as the length of the displayed stings change.
    minLabel.preferredSize = new Dimension(60, minLabel.preferredSize.height)
    maxLabel.preferredSize = new Dimension(60, maxLabel.preferredSize.height)

    val formatString = "%6.3f"
    minLabel.listenTo(Outer)
    minLabel.reactions += {
      case ValueChanged(Outer) => minLabel.text = formatString.format(min)
    }
    maxLabel.listenTo(Outer)
    maxLabel.reactions += {
      case ValueChanged(Outer) => maxLabel.text = formatString.format(max)
    }

    Seq(
      ComponentGroup(floatMaxButton, invertButton),
      ComponentGroup(minKey, minLabel, Swing.HStrut(5), maxKey, maxLabel)
    )
  }

  class MatrixLayerPanel(rows: Int, cols: Int) extends GridBagPanel {
    private var nextIdx = 0
    val gap = 2 // Spacing between matrices displayed in this panel
    def add(comp: Component) {
      val constraints = new Constraints()
      constraints.insets = new Insets(gap, gap, gap, gap)
      constraints.grid = (nextIdx % cols, nextIdx / cols)
      this.add(comp, constraints)
      nextIdx += 1
    }
  }

  protected def getToolTipText(fieldLayer: Int, fieldRow: Int, fieldCol: Int, row: Int, col: Int) = {
    if (lastData != null) {
      lastData.fieldShape.dimensions match {
        case 0 =>
          lastData.read(tmpMatrix)
          s"()($row, $col): ${tmpMatrix(row, col)}}"
        case 1 =>
          lastData.read(fieldCol, tmpMatrix)
          s"($fieldCol)($row, $col): ${tmpMatrix(row, col)}}"
        case 2 =>
          lastData.read(fieldRow, fieldCol, tmpMatrix)
          s"($fieldRow, $fieldCol)($row, $col): ${tmpMatrix(row, col)}}"
        case 3 =>
          lastData.read(fieldLayer, fieldRow, fieldCol, tmpMatrix)
          s"($fieldLayer, $fieldRow, $fieldCol)($row, $col): ${tmpMatrix(row, col)}}"
        case x => "Unsupported dimension: "+x
      }
    } else null
  }

  def save: Elem =
    <Matrices>
      { propertiesTag }
    </Matrices>

  def restore(node: Node) {
    (node \ "Matrices" \ "properties").headOption.foreach(xmlToProperties)
  }

}


