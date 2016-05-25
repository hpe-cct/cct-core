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
import scala.swing._
import cogdebugger.{PropertyValueChanged, ToolFactory, RestorableState, BooleanProperty}
import cogdebugger.ui.components.{SquareIcon, DoubleBufferedImagePanel, WrapPanel}
import cogdebugger.ui.fieldvisualizations._
import java.awt.image.{WritableRaster, BufferedImage}
import java.awt.event.MouseEvent
import scala.xml.{Node, Elem}

/** An alternative view for matrix fields that decomposes the field into
  * collections of like components rather than displaying individual matrices,
  * analogous to the VectorComponents view defined for vector fields. This view
  * is most useful for large fields of small matrices.
  *
  * In the same way that a vector is a collection of components (x, y, z, ...)
  * a matrix is a collection of components - it just happens to be a tensor of
  * a higher dimension. Just as we could collect all the x's from all the
  * vectors in a vector field and display them together, we can collect the
  * first elements from all the matrices and display them together, and we
  * could do the same for the second element, and the third, and so on.
  *
  * Consider this 2x3 field of 2x2 matrices, rendered the "normal" or intuitive
  * way (like [[MatrixMemoryView]] does). The outer indices tell you the
  * location of a particular tensor (matrix) within the field, the inner
  * indices tell you the location of the element within the tensor:
  * {{{
  *  0, 0            0, 1            0, 2
  *  +------+------+ +------+------+ +------+------+
  *  | 0, 0 | 0, 1 | | 0, 0 | 0, 1 | | 0, 0 | 0, 1 |
  *  +------+------+ +------+------+ +------+------+
  *  | 1, 0 | 1, 1 | | 1, 0 | 1, 1 | | 1, 0 | 1, 1 |
  *  +------+------+ +------+------+ +------+------+
  *
  *  0, 0            0, 1            0, 2
  *  +------+------+ +------+------+ +------+------+
  *  | 0, 0 | 0, 1 | | 0, 0 | 0, 1 | | 0, 0 | 0, 1 |
  *  +------+------+ +------+------+ +------+------+
  *  | 1, 0 | 1, 1 | | 1, 0 | 1, 1 | | 1, 0 | 1, 1 |
  *  +------+------+ +------+------+ +------+------+
  * }}}
  *
  * Each individual matrix has a component (0, 0), (0, 1), (1, 0), and so
  * forth. Displayed as above, we have many small matrices. We can instead
  * gather all the (0, 0) tensor elements and display them together, then do
  * the same for the (0, 1) tensor elements and so on. The outer indices
  * now refer to the element index within tensors, and the inner index which
  * of the tensors in the field a particular element came from.
  *
  * {{{
  *  0, 0 in tenors         0, 1 in tensors
  *  +------+------+------+ +------+------+------+
  *  | 0, 0 | 0, 1 | 0, 0 | | 0, 0 | 0, 1 | 0, 0 |
  *  +------+------+------+ +------+------+------+
  *  | 1, 0 | 1, 1 | 1, 0 | | 1, 0 | 1, 1 | 1, 0 |
  *  +------+------+------+ +------+------+------+
  *
  *  0, 0 in tensors        0, 1 in tensors
  *  +------+------+------+ +------+------+------+
  *  | 0, 0 | 0, 1 | 0, 0 | | 0, 0 | 0, 1 | 0, 0 |
  *  +------+------+------+ +------+------+------+
  *  | 1, 0 | 1, 1 | 1, 0 | | 1, 0 | 1, 1 | 1, 0 |
  *  +------+------+------+ +------+------+------+
  * }}}
  *
  * This swapping of indices compacts the visualization for this particular
  * matrix field a bit - rather than many small images, we have a few larger
  * ones.
  *
  * Created by gonztobi on 4/23/2014.
  */
class MatrixComponentsView(fieldType: FieldType)
    extends WrapPanel
    with EventDrivenViewer
    with ZoomProperty
    with RestorableState
    with ToolbarItems {

  require(fieldType.tensorOrder == 2,
    "MatrixComponentsView only works with matrix fields.")
  require(fieldType.dimensions <= 2,
    "MatrixComponentsView only works with fields of dimension 2 or less.")

  def fieldShape  = fieldType.fieldShape
  def tensorShape = fieldType.tensorShape

  /** Controls color key. Normal mode (invert off) shows minimum values as
    * black and maximums as white. */
  val InvertProperty      = new BooleanProperty("Invert", false)
  val BipolarProperty     = new BooleanProperty("Bipolar", false)
  val FloatingMaxProperty = new BooleanProperty("Floating max", false)
  properties ++=
    Seq(InvertProperty, BipolarProperty, FloatingMaxProperty, ZoomProperty)

  /** Controls size of zoom steps */
  var zoomIncrement = 1.1f
  zoomType = ZoomableProperty.ZoomType.Multiplicative

  /** field dimensions */
  private val (fRows, fCols) = fieldShape.dimensions match {
    case 0 => (1, 1)
    case 1 => (1, fieldShape(0))
    case 2 => (fieldShape(0), fieldShape(1))
  }

  /** matrix (tensor) dimensions */
  private val (mRows, mCols) = (tensorShape(0), tensorShape(1))

  /** Memory allocated for reading tensors out of field data updates */
  private val tmpMatrix = Matrix(mRows, mCols, (_, _) => 0f)

  /** Size of the images in child MatrixComponentPanels - with one pixel
    * per tensor in the target field, image dimensions will match field
    * dimensions. */
  private val imgDim = new Dimension(fCols, fRows)
  private val componentPanels = Array.tabulate(mRows, mCols) { (r, c) =>
    new MatrixComponentPanel(r, c, imgDim)
  }

  /** Convenience declaration for mass panel operations. */
  private val flattenedPanels = {
    // componentPanels.flatten doesn't work; returns Array[Nothing]
    var list = List.empty[MatrixComponentPanel]
    for (r <- mRows - 1 to 0 by -1; c <- mCols - 1 to 0 by -1)
      list = componentPanels(r)(c) :: list
    list
  }

  /** Format string used for MatrixComponentPanel legends. */
  private val fmtString = "%6.3f"

  /** Update the legend for the given panel with the latest min/max values. */
  def updateMinMax(panel: MatrixComponentPanel) {
    val (r, c) = (panel.row, panel.col)
    val (min, max) =
      if (FloatingMaxProperty.value)
        (componentMins(r)(c), componentMaxs(r)(c))
      else
        (absComponentMins(r)(c), absComponentMaxs(r)(c))
    panel.minLabel.text = fmtString.format(min)
    panel.maxLabel.text = fmtString.format(max)
  }

  // Hook up events and their reactions
  listenTo(InvertProperty, BipolarProperty, FloatingMaxProperty, ZoomProperty)
  reactions += {
    case PropertyValueChanged(InvertProperty, oldValue, newValue: Boolean) =>
      if (oldValue != newValue && lastData != null) {
        flattenedPanels.foreach(_.invertColorKey(newValue))
        render(lastData)
      }
    case PropertyValueChanged(BipolarProperty, oldValue, newValue) =>
      if (oldValue != newValue && lastData != null) render(lastData)
    case PropertyValueChanged(FloatingMaxProperty, oldValue, newValue) =>
      if (oldValue != newValue && lastData != null) render(lastData)
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      if (newValue > 0)
        for (panel <- flattenedPanels)
          panel.image.zoomLevel = newValue
  }

  // Add child panels to the view
  componentPanels.foreach(_.foreach(contents += _))

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
    data match {
      case mfr: MatrixFieldReader => update(mfr, simTime)
      case x => throw new RuntimeException("View updated with unexpected " +
              "data type: "+x.getClass.getSimpleName)
    }
  }

  // Proper shading requires us to track the minimum and maximum values for
  // each component. Shading may be based on historical/"cumulative" min/max
  // or only on the latest values in the field.
  private val componentMins    = Array.fill(mRows, mCols)(Float.MaxValue)
  private val componentMaxs    = Array.fill(mRows, mCols)(Float.MinValue)
  private val absComponentMins = Array.fill(mRows, mCols)(Float.MaxValue)
  private val absComponentMaxs = Array.fill(mRows, mCols)(Float.MinValue)

  // Keep a reference to the latest update so that we can re-render the data
  // in response to changing floating max/bipolar/invert properties even if no
  // updates are coming in from the debugger
  private var lastData: MatrixFieldReader = null

  /** Resets the viewer its initial state. In particular, clears the tracked
    * min/max data so that the Floating Max option continues to work. */
  override def reset() {
    super.reset()
    for (r <- 0 until mRows; c <- 0 until mCols) {
      componentMins(r)(c)    = Float.MaxValue
      componentMaxs(r)(c)    = Float.MinValue
      absComponentMins(r)(c) = Float.MaxValue
      absComponentMaxs(r)(c) = Float.MinValue
      componentPanels(r)(c).minLabel.text = "?"
      componentPanels(r)(c).maxLabel.text = "?"
    }
  }

  def update(data: MatrixFieldReader, simTime: Long) {
    require(data.fieldType == this.fieldType)
    updateMinAndMax(data)
    render(data)
    lastData = data
  }
  
  private def updateMinAndMax(data: MatrixFieldReader) {
    
    def updateWith(datum: Matrix) {
      for (r <- 0 until mRows; c <- 0 until mCols) {
        val elem = datum(r, c)
        if (!elem.isNaN) {
          if (elem < absComponentMins(r)(c)) {
            absComponentMins(r)(c) = elem
            componentMins(r)(c)    = elem
          } else if (elem < componentMins(r)(c))
            componentMins(r)(c) = elem
          if (elem > absComponentMaxs(r)(c)) {
            absComponentMaxs(r)(c) = elem
            componentMaxs(r)(c) = elem
          } else if (elem > componentMaxs(r)(c))
            componentMaxs(r)(c) = elem
        }
      }
    }
    
    fieldType.dimensions match {
      case 0 =>
        data.read(tmpMatrix)
        updateWith(tmpMatrix)
      case 1 =>
        for (c <- 0 until fCols) {
          data.read(c, tmpMatrix)
          updateWith(tmpMatrix)
        }
      case 2 =>
        for (r <- 0 until fRows; c <- 0 until fCols) {
          data.read(r, c, tmpMatrix)
          updateWith(tmpMatrix)
        }
    }
    
  }
  
  private def render(data: MatrixFieldReader) {
    
    // Capture property values for the duration of the render op - we may do
    // rendering to a back buffer on a backgroudn thread while the user is
    // changing these options on the EDT
    val shadeUsingLatestDataOnly = FloatingMaxProperty.value
    val bipolarMode              = BipolarProperty.value
    val invertGrayscale          = InvertProperty.value

    def minForComponent(r: Int, c: Int): Float =
      if (shadeUsingLatestDataOnly) componentMins(r)(c)
      else absComponentMins(r)(c)
    def maxForComponent(r: Int, c: Int): Float =
      if (shadeUsingLatestDataOnly) componentMaxs(r)(c)
      else absComponentMaxs(r)(c)

     /* Returns the grayscale shade (a value on the range [0, 255] for the
      * given value. Respects the `bipolarMode` and `invertGrayscale`
      * settings.
      * @param r element/component row
      * @param c element/component column
      * @param value value of the element
      */
    def shadeValue(r: Int, c: Int, value: Float) = {
      var min = minForComponent(r, c)
      var max = maxForComponent(r, c)
      if (bipolarMode) {
        if (Math.abs(min) > math.abs(max)) max = -min
        else min = -max
      }
      val shade =
        if ((max - min) == 0) 0f
        else (value - min) / (max - min) * 255f
      if (invertGrayscale) 255 - shade
      else shade
    }

    // Update min/max labels in child MatrixComponentPanels
    for (r <- 0 until mRows; c <- 0 until mCols) {
      val min = minForComponent(r, c)
      val max = maxForComponent(r, c)
      val panel = componentPanels(r)(c)
      if (bipolarMode) {
        if (min.abs > max.abs) {
          panel.minLabel.text = fmtString.format(min)
          panel.maxLabel.text = fmtString.format(-min)
        } else {
          panel.minLabel.text = fmtString.format(-max)
          panel.maxLabel.text = fmtString.format(max)
        }
      } else {
        panel.minLabel.text = fmtString.format(min)
        panel.maxLabel.text = fmtString.format(max)
      }
    }
    
    // Each element in each matrix will be going to different componentPanel,
    // so we can't make use of DoubleBufferedImagePanel's usual `update`
    // method (which requires updating the whole image at once). Well, unless
    // we want to read out the entire matrix field at once.
    val rasters = componentPanels.map(_.map(_.back.getRaster))

    fieldShape.dimensions match {
      case 0 =>
        data.read(tmpMatrix)
        for (mRow <- 0 until mRows; mCol <- 0 until mCols) {
          val element = tmpMatrix(mRow, mCol)
          rasters(mRow)(mCol).setSample(0, 0, 0, shadeValue(mRow, mCol, element))
        }

      case 1 =>
        for (fCol <- 0 until fCols) {
          data.read(fCol, tmpMatrix)
          for (mRow <- 0 until mRows; mCol <- 0 until mCols) {
            val element = tmpMatrix(mRow, mCol)
            rasters(mRow)(mCol).setSample(fCol, 0, 0, shadeValue(mRow, mCol, element))
          }
        }

      case 2 =>
        for (fRow <- 0 until fRows; fCol <- 0 until fCols) {
          data.read(fRow, fCol, tmpMatrix)
          for (mRow <- 0 until mRows; mCol <- 0 until mCols) {
            val element = tmpMatrix(mRow, mCol)
            rasters(mRow)(mCol).setSample(fCol, fRow, 0, shadeValue(mRow, mCol, element))
          }
        }
    }

    // Updating each panel with a no-op will cause them to flip the front/back
    // buffers and draw the updated rasters to screen.
    componentPanels.foreach( innerArray =>
      innerArray.foreach( panel =>
        panel.update(_ => Unit)
      )
    )
  }

  def save: Elem = {
    <MatrixComponents>
      { propertiesTag }
    </MatrixComponents>
  }

  def restore(elem: Node) {
    (elem \ "MatrixComponents" \ "properties").headOption.foreach(xmlToProperties)
  }

  def toolbarComponents: Seq[ComponentGroup] =
    Seq(
      ComponentGroup(
        ToolFactory.toggle(FloatingMaxProperty),
        ToolFactory.toggle(BipolarProperty),
        ToolFactory.toggle(InvertProperty)
      )
    )

  /** Matrix memory for building tooltips. We need this second matrix since
    * tooltip building can happen on the EDI in parallel with a viewer update
    * on a background thread. */
  private val tmpMatrix2 = new Matrix(mRows, mCols)

  /** A subpanel used by [[MatrixComponentsView]] that collects together a
    * title, image, and legend for a single matrix component.
    */
  private class MatrixComponentPanel(
        val row: Int,
        val col: Int,
        imgDim: Dimension)
      extends BorderPanel {

    val image =
      new DoubleBufferedImagePanel(imgDim, BufferedImage.TYPE_BYTE_GRAY)
              with PerElementTooltips {
        override def popupText(mouseEvent: MouseEvent): Option[String] = {
          val fRow = math.floor(mouseEvent.getY / zoomLevel).toInt
          val fCol = math.floor(mouseEvent.getX / zoomLevel).toInt
          if (fRow >= 0 && fRow < fRows &&
              fCol >= 0 && fCol < fCols &&
              lastData != null) {
            fieldType.fieldShape.dimensions match {
              case 0 => lastData.read(tmpMatrix2)
              case 1 => lastData.read(fCol, tmpMatrix2)
              case 2 => lastData.read(fRow, fCol, tmpMatrix2)
            }
            val elem = tmpMatrix2(row, col)
            Some(s"($fRow, $fCol)($row, $col): $elem")
          } else None
        }
      }

    add(image, BorderPanel.Position.Center)

    private val label = new Label(s"Component ($row, $col)")
    private val minKeyIcon = new SquareIcon(java.awt.Color.BLACK)
    private val maxKeyIcon = new SquareIcon(java.awt.Color.WHITE)
    private val minKeyLabel = new Label(" = ", minKeyIcon, Alignment.Right)
    private val maxKeyLabel = new Label(" = ", maxKeyIcon, Alignment.Right)
    val minLabel = new Label("?")
    val maxLabel = new Label("?")

    private val legend = new FlowPanel()
    legend.contents ++= Seq(minKeyLabel, minLabel, maxKeyLabel, maxLabel)

    private val gbPanel = new GridBagPanel
    gbPanel.layout(image) = new gbPanel.Constraints()

    layout(label)  = BorderPanel.Position.North
    layout(gbPanel)  = BorderPanel.Position.Center
    layout(legend) = BorderPanel.Position.South

    def update(renderOp: (WritableRaster) => Unit) {
      image.update(renderOp)
    }

    def back = image.back
    def getRaster = image.back.getRaster

    def invertColorKey(invert: Boolean) {
      if (invert) {
        minKeyIcon.color = java.awt.Color.WHITE
        maxKeyIcon.color = java.awt.Color.BLACK
      } else {
        minKeyIcon.color = java.awt.Color.BLACK
        maxKeyIcon.color = java.awt.Color.WHITE
      }
      minKeyLabel.repaint()
      maxKeyLabel.repaint()
    }

  }

}
