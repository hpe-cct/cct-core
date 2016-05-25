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

import cogx.runtime.debugger.ProbedField
import libcog._
import scala.swing._
import cogdebugger.ui.fieldvisualizations.{ToolbarItems, Subpanel, Zoomable, EventDrivenViewer}
import cogdebugger.ui.components.{MouseDragZoom, PanPane, ToolBar}
import cogdebugger.ui.fieldvisualizations.symmetrictensor.{StickGeometrySubpanel, SticknessOrientationSubpanel}
import cogdebugger.{ToolFactory, OneOfNProperty, RestorableState}
import scala.xml.{Node, Elem}
import scala.language.postfixOps

/** A panel for displaying complex fields that allows you to change the format
  * of the display at runtime.
  *
  * @author Greg Snider
  */
class ComplexFieldSuperPanel(fieldType: FieldType)
        extends BorderPanel
        with EventDrivenViewer
        with Zoomable
        with RestorableState
{

  def this(target: ProbedField) = this(target.fieldType)

  private def fieldShape = fieldType.fieldShape
  private def tensorShape = fieldType.tensorShape

  /** String for displaying real / imaginary components. */
  val RealImaginary = "real / imaginary"
  /** String for displaying magnitude / phase components. */
  val MagnitudePhase = "magnitude / phase"
  /** String for displaying each complex number in polar coordinates. */
  val PolarCoordinates = "polar coordinates"
  /** String for displaying complex numbers as pseudo vectors. */
  val AsVectors = "pseudo-vectors (real = row, imaginary = column)"
  /** String for displaying as stick tensor components. */
  val SticknessOrientation = "stick tensors: stickness / orientation"
  /** String for displaying stick tensor geometry. */
  val StickGeometry = "stick tensors: geometry"

  /** Display options. */
  val displayOptions = Seq(RealImaginary, MagnitudePhase, PolarCoordinates,
    AsVectors, SticknessOrientation, StickGeometry)
  /** Real / imaginary subpanel. */
  private lazy val realImaginaryPanel =
    new RealImaginarySubpanel(fieldType) with Subpanel with MouseDragZoom
  /** Magnitude / phase subpanel. */
  private lazy val magnitudePhasePanel =
    new MagnitudePhaseSubpanel(fieldType) with Subpanel with MouseDragZoom
  private lazy val polarCoordinatesPanel =
    new PolarCoordinatesSubpanel(fieldShape, asPseudoVectors = false) with Subpanel with MouseDragZoom
  /** As vectors subpanel. */
  private lazy val asVectorsPanel =
    new PolarCoordinatesSubpanel(fieldShape, asPseudoVectors = true) with Subpanel with MouseDragZoom
  /** Stickness / orientation subpanel. */
  private lazy val sticknessOrientationPanel =
    new SticknessOrientationSubpanel(fieldType) with Subpanel with MouseDragZoom
  /** Stick geometry subpanel. */
  private lazy val stickGeometryPanel =
    new StickGeometrySubpanel(fieldShape) with Subpanel with MouseDragZoom

  /** The panels that have been instantiated so far. Each of these will be
    * asked to produce an XML encoding their state if the debugger is closed
    * while this SuperPanel is still open. */
  private val instantiated = collection.mutable.Set.empty[Subpanel]
  /** The subpanels that have already been restored from an XML config file. */
  private val restored = collection.mutable.Set.empty[Subpanel]

  // We want to remember the currently selected viewer across debugger runs.
  // Note that this property needs to be defined before we instantiate the
  // toolbar, since it uses this property to build its combobox.
  val SelectedViewerProperty =
    new OneOfNProperty("SelectedView", displayOptions.head, displayOptions.toList)
  SelectedViewerProperty.action = Action("Change Display") {
    changeDisplay(SelectedViewerProperty.selection)
  }
  properties += SelectedViewerProperty

  /** Current active subpanel. */
  private var currentPanel: Subpanel  = realImaginaryPanel
  /** Last field data received, needed if panel needs to be regenerated. */
  private var lastData: AbstractFieldMemory = null
  /** Producer of `lastData`. */
  private var lastDataSource: AnyRef = null
  /** Toolbar */
  private val toolbar = new ComplexFieldToolbar

  private val scroller = new PanPane(currentPanel.asInstanceOf[Panel])
  scroller.border = Swing.EmptyBorder

  // Add components to super panel.
  add(scroller, BorderPanel.Position.Center)
  add(toolbar, BorderPanel.Position.North)
  refreshToolbar() // Make sure current view's toolbar items are installed
  instantiated += currentPanel

  def zoomIn() {
    currentPanel.zoomIn()
    currentPanel.update(lastDataSource, lastData, 0L)
    revalidate()
    repaint()
  }
  def zoomOut() {
    currentPanel.zoomOut()
    currentPanel.update(lastDataSource, lastData, 0L)
    revalidate()
    repaint()
  }

  /** Change display option, called by toolbar. */
  private def changeDisplay(option: String) {
    currentPanel = option match {
      case RealImaginary        => realImaginaryPanel
      case MagnitudePhase       => magnitudePhasePanel
      case PolarCoordinates     => polarCoordinatesPanel
      case AsVectors            => asVectorsPanel
      case SticknessOrientation => sticknessOrientationPanel
      case StickGeometry        => stickGeometryPanel
    }
    instantiated += currentPanel
    restoreSubpanel(currentPanel)
    refreshToolbar()
    scroller.contents = currentPanel.asInstanceOf[Panel]
    revalidate()
    if (lastData != null)
      currentPanel.update(lastDataSource, lastData, 0L)
    repaint()
  }

  /** Update the display with new field `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    lastData = data
    lastDataSource = src
    currentPanel.update(src, data, time)
  }

  private var _savedState: Option[Node] = None
  private def savedState = _savedState
  private def savedState_=(state: Option[Node]) {
    if (state != None) restored.clear()
    _savedState = state
  }

  def save: Elem =
    <ComplexFieldSuperPanel>
      <Subpanels>
        { instantiated collect { case restorable: RestorableState => restorable.save } }
      </Subpanels>
    </ComplexFieldSuperPanel>

  def restore(tag: Node) {
    savedState = (tag \ "ComplexFieldSuperPanel").headOption
    restoreSubpanel(currentPanel)
  }

  def restoreSubpanel(viewer: Subpanel) {
    savedState foreach { state =>
      if (!restored.contains(viewer))
        viewer match {
          case restorable: RestorableState =>
            restorable.restore(state \ "Subpanels" head)
          case _ =>
        }
      restored += viewer
    }
  }

  def refreshToolbar() {
    toolbar.contents.clear()
    toolbar.contents += toolbar.zoomInButton
    toolbar.contents += toolbar.zoomOutButton
    currentPanel match {
      case hasItems: ToolbarItems =>
        val flattened = hasItems.toolbarComponents.foldLeft(Seq.empty[Component]) {
          (listSoFar, nextCompGroup) =>
            (listSoFar :+ Swing.HStrut(10)) ++ nextCompGroup.components.toSeq
        }
        toolbar.contents ++= flattened
      case _ =>
    }
    if (displayOptions.size > 1)
      toolbar.contents += Swing.HStrut(10)
    toolbar.contents += Swing.HGlue
    if (displayOptions.size > 1)
      toolbar.contents += toolbar.displayBox
    //toolbar.revalidate()
  }

  /** Toolbar for super panel. */
  class ComplexFieldToolbar extends ToolBar("ComplexField") {
    floatable = false
    val zoomInButton = Button("+")(zoomIn())
    val zoomOutButton = Button("-")(zoomOut())
    val displayBox = ToolFactory.comboBox(SelectedViewerProperty)
    contents += zoomInButton
    contents += zoomOutButton
    contents += Swing.HGlue
    contents += displayBox
  }
}

object ComplexFieldSuperPanel {
  def apply(target: ProbedField) = new ComplexFieldSuperPanel(target.fieldType)
}