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
//import libcog._
//import scala.swing._
//import cogx.platform.cpumemory.VectorFieldMemory
//import libcog.fields.SymmetricTensor
//
///** Stand-alone testing of a field viewer.
//  *
//  * @author Greg Snider
//  */
//object TestVectorFieldSuperPanel extends SimpleSwingApplication {
//  // Create a simple vector field
//  val Rows = 25
//  val Columns = 25
//  val vectorField = VectorFieldMemory(Rows, Columns,
//    (r, c) => Vector(r - Rows / 2, c - Columns / 2)
//  )
//
//  val tensorField = VectorFieldMemory(Rows, Columns, (row, col) => {
//    val stickness: Float = row + col + 1
//    val ballness: Float = 2 * row
//    var orientation = (col + 0.00001f) * math.Pi.toFloat / Columns
//    new SymmetricTensor(stickness, ballness, orientation)
//  })
//
//  lazy val top = new MainFrame {
//    title = "Test VectorFieldSuperPanel"
//
//    // Select field to view
//    //val field = vectorField
//    val field = tensorField
//
//    contents = new BoxPanel(Orientation.Horizontal) {
//      contents += new VectorFieldSuperPanel(field.fieldType) {
//        update(null, field, 0L)
//      }
//    }
//    minimumSize = new Dimension(250, 100)
//  }
//}