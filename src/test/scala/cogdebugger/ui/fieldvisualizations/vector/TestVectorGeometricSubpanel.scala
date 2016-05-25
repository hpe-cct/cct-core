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

//package cogdebugger.ui.fieldvisualizations.vector
//
//import scala.swing._
//import cogx.Vector
//import cogx.platform.cpumemory.VectorFieldMemory
//import cogdebugger.ui.components.ToolBar
//
///** A standalone test for the VectorGeometricSubpanel class.
//  *
//  * Created by gonztobi on 5/13/2014.
//  */
//object TestVectorGeometricSubpanel extends SimpleSwingApplication {
//  val Layers = 2
//  val Rows = 9
//  val Cols = 16
//  val vectorField =
//    // VectorFieldMemory(Rows, Cols, (r, c) => Vector(r, c))
//    VectorFieldMemory(Layers, Rows, Cols, (l, r, c) => Vector((Layers - l) * 4, r, c))
//    //VectorFieldMemory(Layers, Rows, Cols, (l, r, c) => Vector((Layers - l) * 4, r, c, r * c))
//
//  lazy val top = new MainFrame {
//    title = "Test "+classOf[VectorGeometricSubpanel]
//
//    val view = new VectorGeometricSubpanel(vectorField.fieldShape, vectorField.tensorShape)
//    val injector = new VectorFieldMemoryInjector(vectorField, view)
//    injector.addSeparator()
//    injector.contents += Button("+")(view.zoomIn())
//    injector.contents += Button("-")(view.zoomOut())
//
//    // These tools should NOT include the 'Index Chooser', since we have 2D
//    // vectors in a 2D field.
//    val viewerTools = {
//      val bar = new ToolBar("Viewer tools")
//      val comps =
//        view.toolbarComponents
//                .map(group => group.components)
//                .reduce((g1, g2) => (g1 :+ Swing.HStrut(10)) ++ g2)
//      bar.contents ++= comps
//      bar
//    }
//
//    val tools = new BoxPanel(Orientation.Vertical)
//    tools.contents += injector
//    tools.contents += viewerTools
//
//    val bp = new BorderPanel
//    bp.layout(tools) = BorderPanel.Position.North
//    bp.layout(view) = BorderPanel.Position.Center
//
//    view.update(None, vectorField, 0L)
//    contents = bp
//  }
//}
