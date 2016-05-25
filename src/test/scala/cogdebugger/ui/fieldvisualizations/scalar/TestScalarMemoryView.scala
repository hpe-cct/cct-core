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
//import cogdebugger.ui.components.ToolBar
//
///** Stand-alone testing of a field viewer.
//  *
//  * @author Greg Snider
//  */
//object TestScalarMemoryView extends SimpleSwingApplication {
//  // Create a simple color field
//  val Rows = 100
//  val Columns = 100
//  val scalarField = ScalarFieldMemory(Rows, Columns,
//    (r, c) => r + c)
//  //val scalarField = ScalarFieldMemory(3, Rows, Columns,
//  //  (l, r, c) => l * 100 + r + c)
//
//  lazy val top = new MainFrame {
//    title = "Test ScalarMemoryView"
//    contents = new BorderPanel {
//      val view = new ScalarMemoryView(scalarField.fieldType)
//      view.update(scalarField, scalarField, 0L)
//
//      val viewTools = new ToolBar("Tools")
//      viewTools.contents ++= view.toolbarComponents.foldLeft(List.empty[Component]) {
//        (list, group) =>
//          (list :+ Swing.HStrut(10)) ++ group.components.toSeq
//      }
//
//      val tools = new BoxPanel(Orientation.Vertical)
//      tools.contents += new ScalarFieldMemoryInjector(scalarField, view)
//      tools.contents += viewTools
//
//      layout(tools) = BorderPanel.Position.North
//      layout(view) = BorderPanel.Position.Center
//    }
//    minimumSize = new Dimension(250, 100)
//  }
//}
