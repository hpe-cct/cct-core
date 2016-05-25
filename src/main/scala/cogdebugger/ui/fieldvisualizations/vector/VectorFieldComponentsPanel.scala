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

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.PropertyValueChanged
import cogdebugger.ui.components.ToolBar
import cogdebugger.ui.fieldvisualizations.{ZoomProperty, UnsupportedDimensionException, EventDrivenViewer}
import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView
import scala.swing._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 12/3/12
 * Time: 11:34 AM
 */

/** A VectorField display that collects together the same-indexed components of
  * the vectors and shows them like we do ScalarFields.
  *
  * E.g., for a vector field of vectors with three compnents, x, y, and z,
  * all the x's will be grouped together and displayed like a single
  * ScalarField, as will the y's and z's.
  *
  * Note that this sort of visualization can be made with a bit of field
  * slicing in the Cog model itself.
  */
class VectorFieldComponentsPanel(fieldType: FieldType)
    extends BorderPanel
    with EventDrivenViewer
    with ZoomProperty
{

  def this(target: ProbedField) = this(target.fieldType)

  private def fieldShape = fieldType.fieldShape
  private def tensorShape = fieldType.tensorShape

  // We need to cache the last seen data in case we have to redraw the view for
  // some reason (zoom changes, floating max toggled on/off, etc.)
  protected var _data: AbstractFieldMemory = null
  protected def data: VectorFieldReader = _data.asInstanceOf[VectorFieldReader]

  val vectorLength = tensorShape(0)
  val (layers, rows, columns) = fieldShape.dimensions match { //(fShape.layers, fShape.rows, fShape.columns)
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case x => throw new RuntimeException("Only 0, 1, and 2D VectorFields are supported.")
  }

  val scalarPanels = Array.tabulate(vectorLength) { i => new ScalarMemoryView(fieldType) }
  private var idx = 0
  val componentDisplays = scalarPanels.map(panel => {
    // Give each panel a title and legend
    val label = new Label("Component "+idx); idx += 1
    val legend = new BoxPanel(Orientation.Horizontal)
    legend.contents ++= Swing.HGlue +: panel.toolbarComponents(panel.legendGroupIdx).components :+ Swing.HGlue

    new BorderPanel() {
      // The invisible border adds a bit of white space between panels to help
      // visually separate them.
      border = Swing.EmptyBorder(2, 2, 2, 2)
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  })

  val viewPanel = new FlowPanel(FlowPanel.Alignment.Leading)(componentDisplays: _*)
  add(viewPanel, BorderPanel.Position.Center)
  add(new ComponentsViewToolbar, BorderPanel.Position.North)

  val zoomIncrement: Float = 1f
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      scalarPanels foreach(_.zoomLevel = newValue)
      // When zooming in, grow into any available space, or, if there's no room
      // left in which to grow, add scrollbars to panels.
      // When zooming out, get rid of scrollbars as soon as panels are small
      // enough to fit (this is a little buggy at the moment).
      if (viewPanel.preferredSize.width < this.size.width &&
              viewPanel.preferredSize.height < this.size.height)
        revalidate()
  }
  properties += ZoomProperty

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) =>
        _data = data
        update(src, data.asInstanceOf[VectorFieldReader])
      case _ =>
        throw new RuntimeException("Viewer got unexpected data")
    }
  }

  def update(src: AnyRef, data: VectorFieldReader) {
    // SliceTensor doesn't exist in Cog 4 (not yet anyways, so we have to do
    // this manually.
    val layers = extractLayers(data)
    for (componentIdx <- 0 until vectorLength) {
      scalarPanels(componentIdx).update(Seq(layers(componentIdx)))
    }

  }

  /** Workaround for the fact that sliceTensor doesn't yet exist in Cog 4. */
  protected val tmpVector = new cogx.Vector(vectorLength)
  protected def extractLayers(vectorField: VectorFieldReader) = {
    fieldShape.dimensions match {

      case 0 =>
        vectorField.read(tmpVector)
        Array.tabulate(vectorLength, 1, 1) { (l, r, c) => tmpVector(l) }.toSeq

      case 1 =>
        val (rows, cols) = (1, fieldShape(0))
        val theLayers = Array.ofDim[Float](vectorLength, rows, cols)
        for (r <- 0 until rows; c <- 0 until cols) {
          vectorField.read(c, tmpVector)
          for ((scalarFieldProxy, i) <- theLayers.zipWithIndex) {
            scalarFieldProxy(r)(c) = tmpVector(i)
          }
        }
        theLayers.toSeq

      case 2 =>
        val (rows, cols) = (fieldShape(0), fieldShape(1))
        val theLayers = Array.ofDim[Float](vectorLength, rows, cols)
        for (r <- 0 until rows; c <- 0 until cols) {
          vectorField.read(r, c, tmpVector)
          for ((scalarFieldProxy, i) <- theLayers.zipWithIndex) {
            scalarFieldProxy(r)(c) = tmpVector(i)
          }
        }
        theLayers.toSeq

      case x => throw new UnsupportedDimensionException(x)
    }
  }

  class ComponentsViewToolbar extends ToolBar("Components View") {
    floatable = false

    val floatingMaxButton = new ToggleButton {
      tooltip = VectorFieldComponentsPanel.resetAbsMaxPixelTooltip
      action = Action("Floating Max") {
        for (panel <- scalarPanels)
          panel.FloatingMaxProperty.value = this.selected
        update(null, data) // Force redraw of the data
      }
    }

    //contents += Button("+")(zoomIn())
    //contents += Button("-")(zoomOut())
    contents += floatingMaxButton

  }

}

object VectorFieldComponentsPanel {
  val resetAbsMaxPixelTooltip =
    "Controls how points in the field are mapped to luminance values. When " +
    "enabled, luminance is based on maximum value in the field at the " +
    "current sim tick. When disabled, luminance is based on maximum value " +
    "in the field seen across entire sim history."
}