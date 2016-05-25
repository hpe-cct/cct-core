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

//package cogdebugger.ui.fieldvisualizations.symmetrictensor
//
//import scala.swing._
//import cogx.Complex
//import cogx.platform.cpumemory.ComplexFieldMemory
//import cogdebugger.ui.components.ToolBar
//import cogdebugger.ui.fieldvisualizations.complex.ComplexFieldMemoryInjector
//
///** A standadlone test for the StickGeometrySubpanel visualization.
//  *
//  * Created by gonztobi on 5/20/2014.
//  */
//object TestStickGeometrySubpanel extends SimpleSwingApplication {
//
//  val Rows = 72;  val CenterRow = Rows / 2
//  val Cols = 128; val CenterCol = Cols / 2
//  val fieldData = ComplexFieldMemory(Rows, Cols,
//    (r, c) => Complex(r - CenterRow, c - CenterCol)
//  )
//
//  lazy val top = new MainFrame {
//    title = "Test "+classOf[StickGeometrySubpanel].getSimpleName
//
//    val view = new StickGeometrySubpanel(fieldData.fieldType.fieldShape)
//
//    val injector = new ComplexFieldMemoryInjector(fieldData, view)
//
//    val viewTools = new ToolBar("Viewer Tools")
//    viewTools.floatable = false
//    viewTools.contents += Button("+")(view.zoomIn())
//    viewTools.contents += Button("-")(view.zoomOut())
//    viewTools.contents += Swing.HGlue
//    viewTools.contents ++= view.toolbarComponents
//            .map(group => group.components)
//            .reduce((g1, g2) => (g1 :+ Swing.HStrut(15)) ++ g2)
//
//    // It's important that 'update' is called after creating the toolbar. The
//    // statistics computed in update are delivered to the toolbar components
//    // via Swing's event mechanism; updating prior to creating the toolbar UI
//    // means those components will never see the update.
//    view.update(None, fieldData, 0L)
//
//    val tools = new BoxPanel(Orientation.Vertical)
//    tools.contents += injector
//    tools.contents += viewTools
//
//    val bp = new BorderPanel
//    bp.layout(tools) = BorderPanel.Position.North
//    bp.layout(view) = BorderPanel.Position.Center
//
//    contents = bp
//  }
//}
