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
import scala.swing._
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.components.{MouseDragZoom, PanPane, ToolBar}
import cogdebugger.ui.fieldvisualizations.symmetrictensor.{TensorComponentsSubpanel, SymmetricGeometrySubpanel}
import cogdebugger.{ToolFactory, RestorableState, OneOfNProperty}
import scala.xml.{Elem, Node}
import scala.language.postfixOps
import cogx.runtime.debugger.ProbedField

/** A panel for displaying vector fields that allows you to change the format
  * of the display at runtime.
  *
  * @author Greg Snider
  */
class VectorFieldSuperPanel(fieldType: FieldType)
        extends BorderPanel
        with EventDrivenViewer
        with Zoomable
        with RestorableState
{

  def this(target: ProbedField) = this(target.fieldType)

  private def fieldShape = fieldType.fieldShape
  private def tensorShape = fieldType.tensorShape

  require(tensorShape.dimensions == 1, "non-VectorField received")
  val vectorLength = tensorShape(0)

  /** String for displaying vector components. */
  val VectorComponents = "vector components"
  /** String for displaying vectors geometrically. */
  val Vector2D = "2D vectors"
  /** String for displaying each 2D vector as a color pixel. */
  val ColorFlow2D = "color flow"
  /** String for displaying symmetric tensors. */
  val SymmetricTensors = "symmetric tensors"
  /** String for displaying complex numbers as pseudo vectors. */
  val TensorComponents = "stickness, ballness, orientation"
  /** String for treating vector field as RGB planes. */
  val ColorImages = "Color Images"

  /** Display options. */
  val displayOptions = (vectorLength match {
    case 1 => Seq(VectorComponents)
    case 2 if fieldShape.dimensions == 2 => Seq(VectorComponents, Vector2D, ColorFlow2D)
    case 2 => Seq(VectorComponents, Vector2D)
    case 3 if fieldShape.dimensions == 2 => Seq(VectorComponents, SymmetricTensors, TensorComponents)
    case 3 => Seq(VectorComponents, SymmetricTensors)
    case x => Seq(VectorComponents)
  }) ++
    (if (tensorShape(0) % 3 == 0) Seq(ColorImages) else Seq.empty[String])

  /** Components subpanel. */
  private lazy val componentsPanel =
    new VectorComponentsSubpanel(fieldType) with Subpanel with MouseDragZoom
  /** Geometric view of 2D vectors. */
  private lazy val vector2DPanel =
    new VectorGeometricSubpanel(fieldShape, tensorShape) with Subpanel with MouseDragZoom
  /** Symmetric tensors subpanel */
  private lazy val symmetricTensorsPanel =
    new SymmetricGeometrySubpanel(fieldShape, tensorShape) with Subpanel with MouseDragZoom
  /** Color flow subpanel. */
  private lazy val colorFlowPanel =
     new ColorFlowSubpanel(fieldType) with Subpanel with MouseDragZoom
  /** Stickness / orientation subpanel. */
  private lazy val tensorComponentsPanel =
    new TensorComponentsSubpanel(fieldType) with Subpanel with MouseDragZoom
  private lazy val colorImagesPanel =
    new VectorsToColorFields(fieldType) with Subpanel with MouseDragZoom

  /** The panels that have been instantiated so far. Each of these will be
    * asked to produce an XML encoding their state if the debugger is closed
    * while this SuperPanel is still open. */
  private val instantiated = collection.mutable.Set.empty[Subpanel]
  /** The subpanels that have already been restored from an XML config file. */
  private val restored = collection.mutable.Set.empty[Subpanel]

  // We want to remember the currently selected viewer across debugger runs.
  // This property needs to be defined before we instantiate the toolbar, since
  // it will be used by toolbar to create a combobox.
  val SelectedViewerProperty =
    new OneOfNProperty("SelectedView", displayOptions.head, displayOptions.toList)
  SelectedViewerProperty.action = Action("Change Display") {
    changeDisplay(SelectedViewerProperty.selection)
  }
  properties += SelectedViewerProperty

  /** Current active subpanel. */
  private var currentPanel: Subpanel = componentsPanel
  /** Last field data received, needed if panel needs to be regenerated. */
  private var lastData: AbstractFieldMemory = null
  /** Producer of `lastData`. */
  private var lastDataSource: AnyRef = null
  /** Toolbar */
  private val toolbar = new VectorFieldToolbar
  /** Zooming field. */
  var zDelta = 1f

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
      case VectorComponents => componentsPanel
      case Vector2D         => vector2DPanel
      case SymmetricTensors => symmetricTensorsPanel
      case TensorComponents => tensorComponentsPanel
      case ColorFlow2D      => colorFlowPanel
      case ColorImages      => colorImagesPanel
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
    <VectorFieldSuperPanel>
      <Subpanels>
        { instantiated collect { case restorable: RestorableState => restorable.save } }
      </Subpanels>
    </VectorFieldSuperPanel>

  def restore(tag: Node) {
    savedState = (tag \ "VectorFieldSuperPanel").headOption
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
  class VectorFieldToolbar extends ToolBar("") {
    floatable = false
    val zoomInButton  = Button("+")(zoomIn())
    val zoomOutButton = Button("-")(zoomOut())
    val displayBox = ToolFactory.comboBox(SelectedViewerProperty)
    contents += zoomInButton
    contents += zoomOutButton
    contents += Swing.HGlue
    contents += displayBox
  }
}

object VectorFieldSuperPanel {
  def apply(target: ProbedField) = new VectorFieldSuperPanel(target.fieldType)
}
