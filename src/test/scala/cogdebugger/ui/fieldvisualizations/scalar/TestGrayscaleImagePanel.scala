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
///** Simple stand-alone test-case for grayscale images.
//  * Created by gonztobi on 5/8/2014.
//  */
//object TestGrayscaleImagePanel extends SimpleSwingApplication {
//
//  val Rows = 90
//  val Cols = 160
//
//  val scalarField = ScalarFieldMemory(Rows, Cols,
//    (r, c) => {
//      // An image of concentric circles centered at (0, 0).
//      val d = math.sqrt(r * r + c * c)
//      val shade = (math.sin(d / 4) + 1) / 2
//      shade.toFloat
//    }
//  )
//
//  lazy val top = new MainFrame {
//    title = "Test "+classOf[GrayscaleImagePanel].getSimpleName
//    contents = new BorderPanel {
//      val view = new GrayscaleImagePanel(scalarField, scalarField.fieldShape)
//      view.update(scalarField, scalarField, 0L)
//      val injector = new ScalarFieldMemoryInjector(scalarField, view)
//      layout(injector) = BorderPanel.Position.North
//      layout(view) = BorderPanel.Position.Center
//    }
//  }
//
//}
