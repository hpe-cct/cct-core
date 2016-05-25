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
//import scala.swing._
//import libcog._
//import cogx.platform.cpumemory.ComplexFieldMemory
//import cogdebugger.ui.fieldvisualizations.Viewer
//import cogdebugger.ui.components.ToolBar
//import java.awt.FlowLayout
//
///** A GUI element that allows a user to modify or generate a ComplexFieldMemory
//  * (of up to three dimensions) and inject it into a viewer. Useful for testing
//  * a viewer's ability to deal wwith changing data.
//  *
//  * A ComplexFieldMemoryInjector maintains an internal ComplexFieldMemory that
//  * can be mutated by the user. Users key in values for layer, row, column,
//  * vector component index, and value, and then press a button to write that
//  * value to the memory and update the view. The injector can also generate a
//  * random field (without mutating its internal ComplexFieldMemory member) and
//  * update the view with that.
//  *
//  * Created by gonztobi on 5/27/2014.
//  */
//class ComplexFieldMemoryInjector(initialData: ComplexFieldMemory,
//                                 target: Viewer,
//                                 orientation: Orientation.Value = Orientation.Horizontal)
//        extends ToolBar("Vector Injector", orientation) {
//
//  def fieldShape = initialData.fieldShape
//  def tensorShape = initialData.tensorShape
//  def fieldDimension = fieldShape.dimensions
//  def vectorLength = tensorShape.lastDimension
//
//  val (layers, rows, cols) = fieldDimension match {
//    case 0 => (1, 1, 1)
//    case 1 => (1, 1, fieldShape(0))
//    case 2 => (1, fieldShape(0), fieldShape(1))
//    case 3 => (fieldShape(0), fieldShape(1), fieldShape(2))
//  }
//
//  private var step = 0
//
//  // All elements are initialized to zero
//  private val data = fieldDimension match {
//    case 0 => ComplexFieldMemory(Complex(0f, 0f))
//    case 1 => ComplexFieldMemory(cols, (c) => Complex(0f, 0f))
//    case 2 => ComplexFieldMemory(rows, cols, (r, c) => Complex(0f, 0f))
//    case 3 => ComplexFieldMemory(layers, rows, cols, (l, r, c) => Complex(0f, 0f))
//  }
//
//  private val RealItem = "Real"
//  private val ImaginaryItem = "Imaginary"
//
//  // Selectors for dimensions the field doesn't have won't be made visible
//  private val layerBox = new TextArea("0",   1, 3) // Selects field layer
//  private val rowBox   = new TextArea("0",   1, 3) // Selects field row
//  private val colBox   = new TextArea("0",   1, 3) // Selects field col
//  private val compBox  = new ComboBox(Seq(RealItem, ImaginaryItem))
//  private val valBox   = new TextArea("0.0", 1, 5) // Value for vector component
//
//  peer.setLayout(new FlowLayout(orientation.id))
//
//  contents ++= {
//    var comps = Seq(new Label("Component:"), compBox, new Label("Value:"), valBox)
//    if (fieldDimension > 0) comps = Seq(new Label("Col:"), colBox) ++ comps
//    if (fieldDimension > 1) comps = Seq(new Label("Row:"), rowBox) ++ comps
//    if (fieldDimension > 2) comps = Seq(new Label("Layer:"), layerBox) ++ comps
//    comps
//  }
//
//  contents += Button("Inject") {
//    try {
//      val l = layerBox.text.toInt
//      val r = rowBox.text.toInt
//      val c = colBox.text.toInt
//      val i = compBox.selection.item match {
//        case RealItem => 0
//        case ImaginaryItem => 1
//      }
//      val v = valBox.text.toFloat
//      setAndInject(l, r, c, i, v)
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
//  /** Update the target view with the given VectorFieldMemory. */
//  def inject(data: ComplexFieldMemory) {
//    target.update(None, data, step)
//    step += 1
//  }
//
//  /** Update the target view with the current state of the ComplexFieldMemory
//    * maintained by this Injector. */
//  def inject() { this.inject(data) }
//
//  /** Updates the target view with a ComplexFieldMemory filled with random
//    * values (each component on the range [-1, 1]). This method does not mutate
//    * the internal ComplexFieldMemory maintained by this Injector. */
//  def injectRandom() {
//    val randomData = fieldDimension match {
//      case 0 => ComplexFieldMemory(randomComplex)
//      case 1 => ComplexFieldMemory(cols, (c) => randomComplex)
//      case 2 => ComplexFieldMemory(rows, cols, (r, c) => randomComplex)
//      case 3 => ComplexFieldMemory(layers, rows, cols, (l, r, c) => randomComplex)
//    }
//    inject(randomData)
//  }
//
//  /** Mutate either the real or imaginary part of a single Complex element in
//    * the ComplexFieldMemory maintained by this Injector and then update the
//    * target view with it. This method requires three indices into the field,
//    * but the extra indices are ignored if the field is of less than three
//    * dimensions (e.g. the `layer` argument is ignored if the memory is only
//    * 2D).
//    */
//  def setAndInject(layer: Int, row: Int, col: Int, component: Int, value: Float) {
//    val tmp = data.fieldShape.dimensions match {
//      case 0 => data.read()
//      case 1 => data.read(col)
//      case 2 => data.read(row, col)
//      case 3 => data.read(layer, row, col)
//    }
//    val c =
//      if (component == 0)
//        Complex(value, tmp.imaginary)
//      else
//        Complex(tmp.real, value)
//    data.fieldShape.dimensions match {
//      case 0 => data.write(c)
//      case 1 => data.write(col, c)
//      case 2 => data.write(row, col, c)
//      case 3 => data.write(layer, row, col, c)
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
//  private val rand = new scala.util.Random()
//
//  /** Produces a Complex with random real and imaginary components, each on the
//    * interval [-1, 1]. */
//  private def randomComplex =
//    Complex(rand.nextFloat() * 2 - 1, rand.nextFloat() * 2 - 1)
//
//}
