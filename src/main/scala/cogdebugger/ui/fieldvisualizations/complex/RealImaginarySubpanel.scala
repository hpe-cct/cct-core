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
import cogdebugger.{BooleanProperty, RestorableState, PropertyValueChanged}
import scala.xml.{Node, Elem}

/** A panel which displays a complex field as a real panel and an
  * imaginary panel, side by side.
  *
  * @param fieldType Shape of the complex field being displayed
  *
  * @author Greg Snider
  */
class RealImaginarySubpanel(fieldType: FieldType)
        extends BorderPanel
        with Viewer
        with ZoomProperty
        with RestorableState
{

  def fieldShape = fieldType.fieldShape

  require(fieldShape.dimensions == 2, "Only 2D complex fields supported now")
  val rows = fieldShape(0)
  val columns = fieldShape(1)

  /** Panel displaying real part of complex field. */
  private val realPanel = new ScalarMemoryView(fieldType)
  /** Panel displaying imaginary part of complex field. */
  private val imaginaryPanel = new ScalarMemoryView(fieldType)
  /** Zooming? */
  var zoomIncrement = 1f

  val FloatingMaxProperty = new BooleanProperty("Floating Max", true)

  // Initialize
  setFloatingMax(realPanel)
  setFloatingMax(imaginaryPanel)
  properties += ZoomProperty
  properties += FloatingMaxProperty

  private val prettyPanel = new WrapPanel(
    wrapPanel(realPanel, "real"),
    wrapPanel(imaginaryPanel, "imaginary")
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
    val realData = Array.ofDim[Float](rows, columns)
    val imaginaryData = Array.ofDim[Float](rows, columns)
    for (row <- 0 until rows; col <- 0 until columns) {
      val complex: Complex = complexData.read(row, col)
      realData(row)(col) = complex.real
      imaginaryData(row)(col) = complex.imaginary
    }
    realPanel.update(Seq(realData))
    imaginaryPanel.update(Seq(imaginaryData))
  }

  // Rig up zoom functionality
  listenTo(ZoomProperty)
  reactions += {
    case PropertyValueChanged(FloatingMaxProperty, oldValue, newValue: Boolean) =>
      realPanel.FloatingMaxProperty.value = newValue
      imaginaryPanel.FloatingMaxProperty.value = newValue
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      realPanel.zoomLevel = newValue
      imaginaryPanel.zoomLevel = newValue
  }

  def save: Elem =
    <RealImaginarySubpanel>
      { propertiesTag }
    </RealImaginarySubpanel>

  def restore(tag: Node) {
    (tag \ "RealImaginarySubpanel" \ "properties").headOption.foreach(xmlToProperties)
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
      border = Swing.EmptyBorder(2, 0, 2, 0)
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  }
}