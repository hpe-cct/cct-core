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
///** Simple visual test case for the ConfidenceView. Includes buttons to zoom
//  * the display and generate random data with which to uppdate the view.
//  *
//  * Created by gonztobi on 3/12/14.
//  */
//object TestConfidenceView {
//
//  val classes = Seq(
//    "Plane",
//    "Train",
//    "Automobile",
//    "Bicycle"
//  )
//
//  private val confs = Array.ofDim[Float](classes.length)
//  def randomConfidences = {
//    var remaining = 1f
//    for (i <- 0 until classes.length - 1) {
//      val conf = util.Random.nextFloat() * remaining
//      confs(i) = conf
//      remaining -= conf
//    }
//    confs(classes.length - 1) = remaining
//    Vector(confs: _*)
//  }
//
//  def main(args: Array[String]) {
//
//    // Have 1 pie of "confidence" to distribute amongst classes; each class
//    // gets half of what's left.
//    var remaining = 1f
//    val confs = Array.ofDim[Float](classes.length)
//    for (i <- 0 until classes.length - 1) {
//      val conf = remaining / 2
//      confs(i) = conf
//      remaining -= conf
//    }
//    confs(classes.length - 1) = remaining
//    val confidences = Vector(confs: _*)
//
//    val mem = VectorFieldMemory(confidences)
//
//    Swing.onEDT {
//      val view = new ConfidenceView(classes, mem.fieldType)
//      view.update(mem)
//
//      val shuffleButton = Button("Random Confidences") {
//        mem.write(randomConfidences)
//        view.update(mem)
//      }
//
//      val toolbar = new BoxPanel(Orientation.Horizontal)
//      toolbar.contents += Button("+")(view.zoomIn())
//      toolbar.contents += Button("-")(view.zoomOut())
//      toolbar.contents += shuffleButton
//
//      val bp = new BorderPanel
//      bp.layout(toolbar) = BorderPanel.Position.North
//      bp.layout(view) = BorderPanel.Position.Center
//
//      val frame = new MainFrame()
//      frame.title = "Testing: Confidence View"
//      frame.contents = bp
//      frame.visible = true
//    }
//  }
//
//}
