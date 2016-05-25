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
//import cogx.platform.cpumemory.VectorFieldMemory
//import scala.swing._
//
///**
// * A simple visual test case for the OpenGL accelerated ColorFlow vector
// * visualization.
// *
// * Initially, the display is updated with a "key" field (vectors radiate out
// * from center of field, increasing in length towards the edges) to verify
// * that the color coding is what we expect. There are also a couple of buttons
// * to update the visualization with random data - notable is the "Churn" button,
// * which will first generate a large number of fields in memory, and then
// * display them, one after the other and as fast as possible, to test how
// * quickly the view can animate.
// *
// * Created by gonztobi on 2/24/14.
// */
//object TestColorFlowGL extends SimpleSwingApplication {
//  val (rows, cols) = (480, 640)
//  val (midRow, midCol) = (rows / 2, cols / 2)
//  val MaxLength = midRow min midCol
//  val Normalize = 1f / MaxLength
//  val field = VectorFieldMemory(rows, cols, (row, col) => {
//    val r = (row - midRow) * Normalize
//    val c = (col - midCol) * Normalize
//    new Vector(r, c)
//  })
//
////  val field = ColorFlowVectorPanelGL.keyField
////  val (rows, cols) = (field.rows, field.columns)
//
//  val viewer = new ColorFlowGL(null, field.fieldShape, field.tensorShape)
//  viewer.update(null, field, 0L)
//
//  lazy val bunchOfFields = {
//    println("Pre-generating a bunch of random fields...")
//    for (i <- 0 until 100)
//      yield VectorFieldMemory(rows, cols, (_, _) => Vector.random(2) * 2 - 1)
//  }
//
//  lazy val top = new MainFrame() {
//    title = "Test ColorFlowGL"
//
//    val model = new javax.swing.SpinnerNumberModel(1, 0, 25, 1)
//    val jSpinner = new javax.swing.JSpinner(model)
//    jSpinner.addChangeListener(new javax.swing.event.ChangeListener {
//      def stateChanged(e: javax.swing.event.ChangeEvent) {
//        viewer.maxVectorLength = model.getValue.asInstanceOf[Int]
//      }
//    })
//    val spinner = Component.wrap(jSpinner)
//
//    val resetButton = Button("Reset") { viewer.update(null, field, 0L) }
//
//    /** Update the viewer with a random vector field. */
//    val updateButton = Button("Random") {
//      val mem = VectorFieldMemory(rows, cols, (_, _) => Vector.random(2) * 2 - 1)
//      viewer.update(null, mem, 0L)
//    }
//
//    /** Used to test the speed at which the visualization can redraw. This is
//      * done by pre-generating a bunch of random fields, and then drawing them
//      * one after the other as quickly as possible.*/
//    val churnButton = Button("Churn") {
//      bunchOfFields // Initialize the lazy val
//      println("Execute!")
//      val t0 = java.lang.System.currentTimeMillis()
//      for (field <- bunchOfFields) viewer.update(null, field, 0L)
//      val t1 = java.lang.System.currentTimeMillis()
//      println("Rendered "+bunchOfFields.length+" fields in "+(t1 - t0)+" ms " +
//              "("+bunchOfFields.length * 1000f / (t1 - t0)+" fps)")
//    }
//
//    val buttonPanel = new BoxPanel(Orientation.Horizontal)
//    buttonPanel.contents ++= Seq(resetButton,
//                                 updateButton,
//                                 churnButton,
//                                 Swing.HStrut(10),
//                                 new Label("Saturation Threshold:"),
//                                 Swing.HStrut(5),
//                                 spinner,
//                                 Swing.HGlue)
//
//    val panel = new BorderPanel()
//    panel.layout(viewer) = BorderPanel.Position.Center
//    panel.layout(buttonPanel) = BorderPanel.Position.South
//
//    contents = panel
//  }
//}