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
import cogdebugger.ui.fieldvisualizations.{Viewer, ZoomProperty}
import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView
import cogdebugger.ui.components.{PanPane, WrapPanel}
import cogdebugger.{RestorableState, PropertyValueChanged}
import scala.xml.{Node, Elem}

/** A panel which displays a complex field as a modulus panel and an
  * argument panel, side by side.
  *
  * @param fieldType Shape of the complex field being displayed
  *
  * @author Greg Snider
  */
class MagnitudePhaseSubpanel(fieldType: FieldType)
        extends BorderPanel
        with Viewer
        with ZoomProperty
        with RestorableState
{

  def fieldShape = fieldType.fieldShape

  require(fieldShape.dimensions == 2, "Only 2D complex fields supported now")
  val rows = fieldShape(0)
  val columns = fieldShape(1)

  /** Panel displaying magnitude part of complex field. */
  private val magnitudePanel = new ScalarMemoryView(fieldType)
  /** Panel displaying phase part of complex field. */
  private val phasePanel = new ScalarMemoryView(fieldType)
  /** Zooming? */
  var zoomIncrement = 1f

  // Initialize
  setFloatingMax(magnitudePanel)
  setFloatingMax(phasePanel)
  properties += ZoomProperty

  private val prettyPanel = new WrapPanel(
    wrapPanel(magnitudePanel, "magnitude"),
    wrapPanel(phasePanel, "phase")
  )
  add(prettyPanel, BorderPanel.Position.Center)

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
    for (row <- 0 until rows; col <- 0 until columns) {
      val complex: Complex = complexData.read(row, col)
      magnitudeData(row)(col) = complex.magnitude
      phaseData(row)(col) = complex.phase
    }
    magnitudePanel.update(Seq(magnitudeData))
    phasePanel.update(Seq(phaseData))
  }

  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      magnitudePanel.zoomLevel = newValue
      phasePanel.zoomLevel = newValue
  }

  /** Force floating max to be true. */
  private def setFloatingMax(panel: ScalarMemoryView) {
    panel.FloatingMaxProperty.value = true
  }

  def save: Elem =
    <MagnitudePhaseSubpanel>
      { propertiesTag }
    </MagnitudePhaseSubpanel>

  def restore(tag: Node) {
    (tag \ "MagnitudePhaseSubpanel" \ "properties").headOption.foreach(xmlToProperties)
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
      border = Swing.EmptyBorder(2, 0, 2, 0)
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  }
}