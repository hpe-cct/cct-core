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
//import cogx.platform.cpumemory.ScalarFieldMemory
//import cogdebugger.ui.components.ToolBar
//import cogdebugger.ui.fieldvisualizations.EventDrivenViewer
//import scala.swing._
//import java.awt.FlowLayout
//
///**
//  * A GUI element that allows a user to modify or generate a two dimensional
//  * ScalarFieldMemory and inject it into a viewer. Useful for testing a viewer's
//  * ability to deal with changing data.
//  *
//  * A ScalarFieldMemoryInjector maintains an internal ScalarFieldMemory that
//  * can be mutated by the user. Users key in values for row, column, and value,
//  * and then press a button to write that value to the memory and update the
//  * view. The injector can also generate a random field (without mutating its
//  * internal ScalarFieldMemory member) and update the view with that.
//  *
//  * TODO Add support for 0, 1, and 3 dimensional fields.
//  *
//  * Created by gonztobi on 2/20/14.
//  */
//class ScalarFieldMemoryInjector(initialData: ScalarFieldMemory,
//                                target: EventDrivenViewer,
//                                orientation: Orientation.Value = Orientation.Horizontal)
//        extends ToolBar("Injector") {
//
//  val (rows, cols) = (initialData.rows, initialData.columns)
//
//  val data = ScalarFieldMemory(rows, cols, (r, c) => initialData.read(r, c))
//  var step = 0
//
//  peer.setLayout(new FlowLayout(orientation.id))
//
//  val rowBox = new TextArea("0",   1, 3)
//  val colBox = new TextArea("0",   1, 3)
//  val valBox = new TextArea("0.0", 1, 5)
//
//  contents ++= Seq(new Label("Row:"),   rowBox,
//                   new Label("Col:"),   colBox,
//                   new Label("Value:"), valBox)
//
//  contents += Button("Inject") {
//    try {
//      val r = rowBox.text.toInt
//      val c = colBox.text.toInt
//      val v = valBox.text.toFloat
//      setAndInject(0, r, c, v)
//    } catch {
//      // TODO Prevent users from keying in anything but (valid) numbers
//      case e: Exception => Console.err.println(e.getMessage)
//    }
//  }
//
//  contents += Swing.HStrut(10)
//  contents += Button("Randomize") { injectRandom() }
//  contents += Swing.HStrut(10)
//  contents += Button("Reset") { reset() }
//  contents += Swing.HGlue
//
//  /** Updates the target view with a ScalarField filled with random values on
//    * the range [0, 1]. This method does not mutate the internal
//    * ScalarFieldMemory maintained by this Injector. */
//  def injectRandom() {
//    val rand = new scala.util.Random()
//    val randomData = ScalarFieldMemory.apply(rows, cols, (r, c) => rand.nextFloat())
//    inject(randomData)
//  }
//
//  /** Update the target view with the given ScalarFieldMemory. */
//  def inject(data: ScalarFieldMemory) {
//    target.update(None, data, step)
//    step += 1
//  }
//
//  /** Update the target view with the current state of the ScalarFieldMemory
//    * maintained by this Injector. */
//  def inject() { this.inject(data) }
//
//  /** Mutate a single element of the internal ScalarFieldMemory maintained by
//    * this Injector and then update the target view with it. This method
//    * requires three indices into the memory, but the extra indices are ignored
//    * if the memory is of less than three dimensions (e.g. the `layer` argument
//    * is ignored if the memory is only 2D). */
//  def setAndInject(layer: Int, row: Int, col: Int, value: Float) {
//    data.fieldShape.dimensions match {
//      case 0 => data.write(value)
//      case 1 => data.write(col, value)
//      case 2 => data.write(row, col, value)
//      case 3 => data.write(layer, row, col, value)
//    }
//    inject()
//  }
//
//  /** Reset the target view with the initial data supplied to this Injector. */
//  def reset() {
//    step = 0
//    target.reset()
//    target.update(None, initialData, step)
//  }
//
//}
