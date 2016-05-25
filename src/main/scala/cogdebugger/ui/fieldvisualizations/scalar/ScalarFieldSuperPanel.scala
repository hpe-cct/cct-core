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

package cogdebugger.ui.fieldvisualizations.scalar

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.ui.fieldvisualizations.{UnsupportedDimensionException, Viewer, ViewerSuperPanel, EventDrivenViewer}
import cogdebugger.Memoize
import cogdebugger.ui.components.MouseDragZoom

/**
 * Host panel for Scalar field visualizations. Provides a toolbar with common
 * controls and a combo box for selecting between the different scalar field
 * visualizations.
 *
 * If you've implemented a new visualization for scalar fields and want to plug
 * it into the GUI, provide a name for it to the `viewerNames` list and a case
 * for it in the `viewerNameToInstance` memoized factory.
 *
 * Created by gonztobi on 2/24/14.
 */
class ScalarFieldSuperPanel protected (target: ProbedField,
                                       options: List[String],
                                       factory: (String) => Viewer)
        extends ViewerSuperPanel(target, options, factory) {
  require(target.fieldType.tensorShape == Shape(),
    "ScalarFieldSuperPanel only works with Scalar Fields!")
}

object ScalarFieldSuperPanel {

  val RealFieldName   = "Scalars"
  val GrayscaleName   = "Grayscale Image"
  val TimeseriesName  = "Timeseries"
  val ScatterplotName = "Scatter Plot"

  val viewerNames = List(RealFieldName,
                         GrayscaleName,
                         TimeseriesName,
                         ScatterplotName)
  val viewerNamesExceptTimeseries = List(RealFieldName,
                                         GrayscaleName,
                                         ScatterplotName)

  /** The timeseries plot takes a very long time to initialize of the target
    * field has lots of elements - it will pretty much hang the GUI if you
    * open it on a "big" field. To prevent this, the view is disabled if the
    * field contains more elements than this threshold (which has been set more
    * or less arbitrarily at 100 for the time being). */
  private val timeseriesSizeThreshold = 100

  def apply(target: ProbedField) = {
    val fieldShape  = target.fieldType.fieldShape
    val tensorShape = target.fieldType.tensorShape

    val compatibleViews = fieldShape.dimensions match {
      case 0 | 1 | 2 if fieldShape.points < timeseriesSizeThreshold =>
        List(RealFieldName, GrayscaleName, TimeseriesName, ScatterplotName)
      case 0 | 1 | 2 =>
        List(RealFieldName, GrayscaleName, ScatterplotName)
      case 3 =>
        List(RealFieldName)
      case x => throw new UnsupportedDimensionException(x)
    }

    val memoizer = Memoize[String, EventDrivenViewer] {
      case RealFieldName   => new ScalarMemoryView(target) with MouseDragZoom
      case GrayscaleName   => new GrayscaleImagePanel(target, fieldShape) with MouseDragZoom
      case TimeseriesName  => new StackedTimeSeriesPanel(target)
      case ScatterplotName => new ScatterPlot(target)
    }
    new ScalarFieldSuperPanel(target, compatibleViews, memoizer)
  }
}