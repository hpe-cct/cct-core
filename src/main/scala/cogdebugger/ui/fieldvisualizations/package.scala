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

package cogdebugger.ui

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 11/15/13
 * Time: 11:27 AM
 */
package object fieldvisualizations {

  // Built-in scalar views
  type ScalarMemoryView    = scalar.ScalarMemoryView
  type GrayscaleImagePanel = scalar.GrayscaleImagePanel
  type ScatterPlot         = scalar.ScatterPlot
  type Timeseries          = scalar.StackedTimeSeriesPanel
  type Timeseries2         = scalar.Timeseries2

  // Built-in vector views
  type Geometric2DVectorView  = vector.Geometric2DVectorView
  type ColorFlow              = vector.ColorFlow
  type ColorFlowVectorPanelGL = vector.ColorFlowVectorPanelGL

  // Built-in matrix views
  type MatrixMemoryView = matrix.MatrixMemoryView

  // Built-in color views
  type ColorFieldMemoryView = color.ColorFieldMemoryView

  // "Super" panels, that allow switching between different visualizations for
  // a given field type
  type ColorFieldSuperPanel    = color.ColorFieldSuperPanel
  val  ColorFieldSuperPanel    = color.ColorFieldSuperPanel
  type ComplexFieldSuperPanel  = complex.ComplexFieldSuperPanel
  val  ComplexFieldSuperPanel  = complex.ComplexFieldSuperPanel
  type ComplexVectorSuperPanel = complexvector.ComplexVectorSuperPanel
  val  ComplexVectorSuperPanel = complexvector.ComplexVectorSuperPanel
  type MatrixFieldSuperPanel   = matrix.MatrixFieldSuperPanel
  val  MatrixFieldSuperPanel   = matrix.MatrixFieldSuperPanel
  type ScalarFieldSuperPanel   = scalar.ScalarFieldSuperPanel
  val  ScalarFieldSuperPanel   = scalar.ScalarFieldSuperPanel
  type VectorFieldSuperPanel   = vector.VectorFieldSuperPanel
  val  VectorFieldSuperPanel   = vector.VectorFieldSuperPanel

}
