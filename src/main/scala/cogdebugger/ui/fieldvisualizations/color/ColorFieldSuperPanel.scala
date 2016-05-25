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

package cogdebugger.ui.fieldvisualizations.color

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.ui.fieldvisualizations.{Viewer, ViewerSuperPanel, EventDrivenViewer}
import cogdebugger.Memoize
import cogdebugger.ui.components.MouseDragZoom

/**
 * Host panel for Color field visualizations. Provides a toolbar with common
 * controls and a combo box for selecting between the different color field
 * visualizations.
 *
 * If you've implemented a new visualization for color fields and want to plug
 * it into the GUI, provide a name for it to the `viewerNames` list and a case
 * for it in the `viewerNameToInstance` memoized factory.
 *
 * Created by gonztobi on 2/24/14.
 */
class ColorFieldSuperPanel protected (target: ProbedField,
                                      options: List[String],
                                      factory: (String) => Viewer)
    extends ViewerSuperPanel(target, options, factory) {
  require(target.fieldType.tensorShape.dimensions == 1,
    "ColorFieldSuperPanel only works with Color Fields!")
}

object ColorFieldSuperPanel {

    val ColorPanelName = "Color Image"
    val viewerNames = List(ColorPanelName)

  def apply(target: ProbedField) = {
    val fieldShape  = target.fieldType.fieldShape
    val memoizer = Memoize[String, EventDrivenViewer] {
      case ColorPanelName => new ColorFieldMemoryView(target, fieldShape) with MouseDragZoom
    }
    new ColorFieldSuperPanel(target, viewerNames, memoizer)
  }
}