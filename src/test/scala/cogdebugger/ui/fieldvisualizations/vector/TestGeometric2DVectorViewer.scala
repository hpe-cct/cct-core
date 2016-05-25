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
//import cogx._
//import scala.swing._
//import cogx.platform.cpumemory.{VectorFieldMemory, ComplexFieldMemory, ScalarFieldMemory}
//import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView
//
///** Stand-alone testing of a field viewer.
//  *
//  * @author Greg Snider
//  */
//object TestComplexFieldSuperPanel extends SimpleSwingApplication {
//  // Create a simple complex field
//  val Rows = 20
//  val Columns = 20
//  val vectorField = VectorFieldMemory(Rows, Columns,
//    (r, c) => new Vector(r, c)
//  )
//
//  lazy val top = new MainFrame {
//    title = "Test Geometric2DVectorView"
//    contents = new BoxPanel(Orientation.Horizontal) {
//      contents += new Geometric2DVectorView(null, vectorField.fieldShape,
//        vectorField.tensorShape)
//      {
//        update(null, vectorField, 0L)
//      }
//      //      contents += new Geometric2DVectorView(field0D)
//    }
//    minimumSize = new Dimension(250, 100)
//  }
//}
