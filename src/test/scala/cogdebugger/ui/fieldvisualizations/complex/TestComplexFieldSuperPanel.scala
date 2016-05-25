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

//package cogdebugger.ui.fieldvisualizations.complex
//
//import libcog._
//import scala.swing._
//import cogx.platform.cpumemory.ComplexFieldMemory
//
///** Stand-alone testing of a field viewer.
//  *
//  * @author Greg Snider
//  */
//object TestComplexFieldSuperPanel extends SimpleSwingApplication {
//  // Create a simple complex field
//  val Rows = 8
//  val Columns = 8
//  val complexFieldPolar = ComplexFieldMemory(Rows, Columns,
//    (r, c) => Complex.polar(r, c * 2 * math.Pi.toFloat / Columns)
//  )
//  val complexFieldCartesian = ComplexFieldMemory(Rows, Columns,
//    (r, c) => Complex(r - Rows / 2, c - Columns / 2)
//  )
//  val stickTensorField = ComplexFieldMemory(Rows, Columns, (row, col) => {
//    val magnitude = row
//    val orientation = col * (math.Pi.toFloat / Columns)
//    // We double the orientation to encode it in a complex number.
//    Complex.polar(magnitude, orientation * 2)
//  })
//
//  lazy val top = new MainFrame {
//    title = "Test ComplexFieldSuperPanel"
//
//    // Select field to view
//    //val complexField = complexFieldCartesian
//    //val complexField = complexFieldPolar
//    val complexField = stickTensorField
//
//    contents = new BoxPanel(Orientation.Horizontal) {
//      contents += new ComplexFieldSuperPanel(complexField.fieldType) {
//        update(null, complexField, 0L)
//      }
//    }
//    minimumSize = new Dimension(250, 100)
//  }
//}
