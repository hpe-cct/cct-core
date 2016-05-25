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

//package cogdebugger.ui.fieldvisualizations.color
//
//import cogx._
//import scala.swing._
//import cogx.platform.types.Pixel
//import cogx.platform.cpumemory.ColorFieldMemory
//
///** Stand-alone testing of a field viewer.
//  *
//  *
//  * @author Greg Snider
//  */
//object TestColorFieldMemoryView extends SimpleSwingApplication {
//  // Create a simple color field
//  val Rows = 100
//  val Columns = 100
//  val colorField = ColorFieldMemory(Rows, Columns,
//    (r, c) => new Pixel(r.toFloat / Rows, c.toFloat / Columns, 0f))
//
//  lazy val top = new MainFrame {
//    title = "Test ColorFieldMemoryView"
//    contents = new BoxPanel(Orientation.Horizontal) {
//      contents += new ColorFieldMemoryView(colorField, colorField.fieldShape) {
//        update(colorField, colorField, 0L)
//      }
////      contents += new Geometric2DVectorView(field0D)
//    }
//    minimumSize = new Dimension(250, 100)
//  }
//}
