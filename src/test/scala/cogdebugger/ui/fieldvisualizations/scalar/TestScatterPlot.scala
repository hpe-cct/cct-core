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

//package cogdebugger.ui.fieldvisualizations.scalar
//
//import scala.swing._
//import cogx.platform.cpumemory.ScalarFieldMemory
//
///** A stand-alone test for the ScatterPlot visualion built for ScalarFields.
//  *
//  * Created by gonztobi on 5/9/2014.
//  */
//object TestScatterPlot extends SimpleSwingApplication {
//
//  val Rows = 9
//  val Cols = 16
//
//  val scalarField = ScalarFieldMemory(Rows, Cols, (r, c) => r + r * c)
//
//  lazy val top = new MainFrame() {
//    title = "Tetst "+classOf[ScatterPlot].getSimpleName
//    val view = new ScatterPlot(scalarField.fieldType)
//    val injector = new ScalarFieldMemoryInjector(scalarField, view)
//    val bp = new BorderPanel()
//    bp.layout(injector) = BorderPanel.Position.North
//    bp.layout(view)     = BorderPanel.Position.Center
//    view.update(scalarField)
//    contents = bp
//  }
//
//}
