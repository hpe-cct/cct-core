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

package cogdebugger.ui.fieldvisualizations.symmetrictensor

import libcog._
import scala.swing._
import cogdebugger.ui.fieldvisualizations.{Viewer, ZoomProperty}
import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView
import libcog.fields.SymmetricTensor
import cogdebugger.ui.components.{PanPane, WrapPanel}
import cogdebugger.{RestorableState, PropertyValueChanged}
import scala.xml.{Node, Elem}

/** A panel which displays a symmetric tensor field as 3 panels, side by side:
  * 
  * stickness
  * 
  * ballness
  * 
  * orientation
  *
  * @param fieldType Shape of the complex field being displayed
  *
  * @author Greg Snider
  */
class TensorComponentsSubpanel(fieldType: FieldType)
        extends BorderPanel
        with Viewer
        with ZoomProperty
        with RestorableState
{

  def fieldShape = fieldType.fieldShape
  def tensorShape = fieldType.tensorShape

  require(fieldShape.dimensions == 2, "Only 2D tensor fields supported now")
  require(tensorShape.dimensions == 1)
  require(tensorShape(0) == SymmetricTensor.Components)
  val rows = fieldShape(0)
  val columns = fieldShape(1)

  /** Panel displaying stickness. */
  private val sticknessPanel = new ScalarMemoryView(fieldType)
  /** Panel displaying ballness. */
  private val ballnessPanel = new ScalarMemoryView(fieldType)
  /** Panel displaying orientation. */
  private val orientationPanel = new ScalarMemoryView(fieldType)

  /** Default zoom increment */
  var zoomIncrement = 1f

  properties += ZoomProperty

  // Initialize
  setFloatingMax(sticknessPanel)
  setFloatingMax(ballnessPanel)
  setFloatingMax(orientationPanel)

  private val prettyPanel = new WrapPanel(
    wrapPanel(sticknessPanel, "stickness"),
    wrapPanel(ballnessPanel, "ballness"),
    wrapPanel(orientationPanel, "orientation")
  )
  add(prettyPanel, BorderPanel.Position.Center)

  /** Update the display with a new symmetric tensor field.
    *
    * @param src Requester of the update (?).
    * @param data The new tensor field data
    * @param time Current simulation time.
    */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    val vectorData = data.asInstanceOf[VectorFieldMemory]
    val sticknessData = Array.ofDim[Float](rows, columns)
    val ballnessData = Array.ofDim[Float](rows, columns)
    val orientationData = Array.ofDim[Float](rows, columns)
    val vector = new Vector(SymmetricTensor.Components)
    for (row <- 0 until rows; col <- 0 until columns) {
      vectorData.read(row, col, vector)
      val tensor = new SymmetricTensor(vector)
      sticknessData(row)(col) = tensor.stickness
      ballnessData(row)(col) = tensor.ballness
      orientationData(row)(col) = tensor.orientation
    }
    sticknessPanel.update(Seq(sticknessData))
    ballnessPanel.update(Seq(ballnessData))
    orientationPanel.update(Seq(orientationData))
  }

  // Rig up zoom functionality
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      sticknessPanel.zoomLevel = newValue
      ballnessPanel.zoomLevel = newValue
      orientationPanel.zoomLevel = newValue
  }

  def save: Elem =
    <TensorComponentsSubpanel>
      { propertiesTag }
    </TensorComponentsSubpanel>

  def restore(tag: Node) {
    (tag \ "TensorComponentsSubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  /** Force floating max to be true. */
  private def setFloatingMax(panel: ScalarMemoryView) {
    panel.FloatingMaxProperty.value = true
  }

  /** Wraps a panel with another panel, removing the wrapped panel's toolbar
    * while adding a title and legend.
    *
    * @param panel The panel being wrapped.
    * @param title The title displayed for the wrapped panel.
    */
  private def wrapPanel(panel: ScalarMemoryView, title: String): BorderPanel = {
    val label = new Label(title)
    val legend = new BoxPanel(Orientation.Horizontal)
    legend.contents ++= panel.toolbarComponents(panel.legendGroupIdx).components
    new BorderPanel() {
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  }
}