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

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.ui.fieldvisualizations.{Viewer, ViewerSuperPanel, EventDrivenViewer}
import cogdebugger.Memoize
import cogdebugger.ui.components.MouseDragZoom


/**
 * Host panel for Matrix field visualizations. Provides a toolbar with common
 * controls and a combo box for selecting between the different matrix field
 * visualizations.
 *
 * If you've implemented a new visualization for matrix fields and want to plug
 * it into the GUI, provide a name for it to the `viewerNames` list and a case
 * for it in the `viewerNameToInstance` memoized factory.
 *
 * Created by gonztobi on 2/24/14.
 */
class MatrixFieldSuperPanel protected (target: ProbedField,
                                      options: List[String],
                                      factory: (String) => Viewer)
        extends ViewerSuperPanel(target, options, factory) {
  require(target.fieldType.tensorShape.dimensions == 2,
    "MatrixFieldSuperPanel only works with Matrix Fields!")
}

object MatrixFieldSuperPanel {

  val MatrixPanelName = "Matrices"
  val MatrixComponentsName = "Matrix Components"

  def apply(target: ProbedField) = {
    val fieldShape  = target.fieldType.fieldShape
    val tensorShape = target.fieldType.tensorShape
    val viewerNames =
      if (fieldShape.dimensions < 3)
        List(MatrixComponentsName, MatrixPanelName)
      else
        List(MatrixPanelName)
    val memoizer = Memoize[String, EventDrivenViewer] {
      case MatrixPanelName => new MatrixMemoryView(target, fieldShape, tensorShape) with MouseDragZoom
      case MatrixComponentsName => new MatrixComponentsView(target.fieldType) with MouseDragZoom
    }
    new MatrixFieldSuperPanel(target, viewerNames, memoizer)
  }
}