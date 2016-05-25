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

package cogdebugger.ui.fieldvisualizations.complexvector

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.{RestorableState, Memoize}
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.components.MouseDragZoom
import scala.swing.BorderPanel
import scala.xml.Node

/** Host panel for complex vector field visualizations. Provides a toolbar with
  * common controls and a combo box for selecting between the different complex
  * vector field visualizations.
  *
  * If you've implemented a new visualization for complex vector fields and
  * want to plug it into the GUI, provide a name for it to the `viewerNames`
  * list and a case for it in the `viewerNameToInstance` memoized factory.
  *
  * Created by gonztobi on 3/6/14.
  */
class ComplexVectorSuperPanel protected (target: ProbedField,
                                         options: List[String],
                                         factory: (String) => Viewer)
        extends ViewerSuperPanel(target, options, factory) {
  require(target.fieldType.elementType == Complex32 &&
          target.fieldType.tensorShape.dimensions == 1,
    "ComplexVectorSuperPanel only works with complex vector fields!")
}

object ComplexVectorSuperPanel {

  val ComponentsPanelName = "Complex Components"

  val ColorPanelName = "Color Image"
  val viewerNames = List(ComponentsPanelName)

  def apply(target: ProbedField) = {
    val memoizer = Memoize[String, EventDrivenViewer] {
      case ComponentsPanelName => buildComponentsView(target)
    }
    new ComplexVectorSuperPanel(target, viewerNames, memoizer)
  }

  private def buildComponentsView(target: ProbedField) = {
    val viewer = new ComplexVectorComponentsView(target.fieldType) with MouseDragZoom
    val borderPanel = new BorderPanel
        with EventDrivenViewer
        with Zoomable
        with RestorableState
        with ToolbarItems {
      def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
        viewer.update(src, data, simTime)
      }
      def zoomLevel = viewer.zoomLevel
      def zoomIn() { viewer.zoomIn() }
      def zoomOut() { viewer.zoomOut() }
      def save = viewer.save
      def restore(tag: Node) { viewer.restore(tag) }
      def toolbarComponents = viewer.toolbarComponents
    }
    borderPanel.layout(viewer) = BorderPanel.Position.Center
    borderPanel
  }

}