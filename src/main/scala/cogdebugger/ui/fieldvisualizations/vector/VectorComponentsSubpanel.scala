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

package cogdebugger.ui.fieldvisualizations.vector

import libcog._
import cogdebugger.ui.components.WrapPanel
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.fieldvisualizations.scalar.ScalarMemoryView
import scala.swing._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState, PropertyValueChanged}
import scala.xml.{Node, Elem}
import java.nio.FloatBuffer

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 12/3/12
 * Time: 11:34 AM
 */


/** A display for VectorFields that breaks the vectors apart into their
  * individual components and displays each set of components as a grayscale
  * image - essentially, slicing each component out of the field as displaying
  * it with a [[ScalarMemoryView]].
  *
  * Just as a vector is a linear collection of scalars, a VectorField can be
  * thought of as a linear collection of ScalarFields, one for each component.
  * This visualization treats the target VectorField just so, and essentially
  * serves to translate the vector field into a multi-layer (3D) ScalarField
  * and view it as such.
  *
  * Each individual vector component is "sliced" out of the field to create
  * a scalar field with the same dimensions as the original vector field. That
  * scalar field is visually represented as a grayscale image, with a single
  * pixel per field element. The default color mapping represents the minimum
  * value for the vector component as black, the maximum as white, and
  * intermediate values falling on a linear gradient between those two
  * extremes. Thus, a vector field of n dimensions will be displayed as n
  * grayscale images. Each image annotated with a title indicating the vector
  * component it represents as well as a color key showing which colors
  * correspond to the minimum and maximum values for that component, as well
  * as the value of the minimum and maximums themselves.
  *
  * As a rought example, a vector field with 50 rows and 100 columns containing
  * two-dimensional vectors would look something like this:
  * {{{
  *      Component 0                 Component 1
  *   +---------------+           +---------------+
  *   |               |           |               |
  *   | 100x50 image  |           | 100x50 image  |
  *   |               |           |               |
  *   +---------------+           +---------------+
  *   [X] = <min> [ ] = <max>     [X] = <min> [ ] = <max>
  * }}}
  *
  * @param fieldType A FieldType describing the shape of the field and its
  *                  vectors.
  */
class VectorComponentsSubpanel(fieldType: FieldType)
        extends BorderPanel
        with Viewer
        with ZoomProperty
        with RestorableState
        with ToolbarItems
{

  private def fieldShape = fieldType.fieldShape
  private def tensorShape = fieldType.tensorShape

  // We need to cache the last seen data in case we have to redraw the view for
  // some reason (zoom changes, floating max toggled on/off, etc.)
  protected var _data: AbstractFieldMemory = null
  protected def data: VectorFieldReader = _data.asInstanceOf[VectorFieldReader]

  /** Default zoom increment. */
  var zoomIncrement = 1f

  val FloatingMaxProperty = new BooleanProperty("Floating Max", false)
  properties += ZoomProperty
  properties += FloatingMaxProperty

  val vectorLength = tensorShape(0)
  val (layers, rows, columns) = fieldShape.dimensions match {
    case 0 => (1, 1, 1)
    case 1 => (1, 1, fieldShape(0))
    case 2 => (1, fieldShape(0), fieldShape(1))
    case x => throw new RuntimeException("Only 0, 1, and 2D VectorFields are supported.")
  }

  val scalarPanels = Array.tabulate(vectorLength) { i => new ScalarMemoryView(fieldType) }
  private var idx = 0
  val componentDisplays = scalarPanels.map(panel => {
    // Attach title and legend to each subpanel
    val label = new Label("Component "+idx); idx += 1
    val legend = new BoxPanel(Orientation.Horizontal)
    legend.contents ++= panel.toolbarComponents(panel.legendGroupIdx).components
    new BorderPanel() {
      add(label,  BorderPanel.Position.North)
      add(panel,  BorderPanel.Position.Center)
      add(legend, BorderPanel.Position.South)
    }
  })

  listenTo(FloatingMaxProperty, ZoomProperty)
  reactions += {
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      scalarPanels.foreach(_.zoomLevel = newValue)

    case PropertyValueChanged(FloatingMaxProperty, oldValue, newValue: Boolean) =>
      for (panel <- scalarPanels)
        panel.FloatingMaxProperty.value = newValue
      if (data != null)
        update(null, data) // Force redraw of the data
  }

  val viewPanel = new WrapPanel(componentDisplays: _*)
  add(viewPanel, BorderPanel.Position.Center)

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) =>
        _data = data
        update(src, data.asInstanceOf[VectorFieldReader])
      case _ =>
        throw new RuntimeException("Viewer got unexpected data")
    }
  }

  def update(src: AnyRef, data: VectorFieldReader) {
//    for (i <- (0 until vectorLength)) {
    for (i <- (0 until vectorLength).par) {
      val sfr = new VectorComponentSliceAsScalarFieldReader(data.asInstanceOf[VectorFieldMemory], i)
      scalarPanels(i).update(null, sfr)
    }

  }

  def save: Elem =
    <VectorComponentsSubPanel>
      { propertiesTag }
    </VectorComponentsSubPanel>

  def restore(tag: Node) {
    (tag \ "VectorComponentsSubPanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  def toolbarComponents: Seq[ComponentGroup] = {
    val floatingMaxButton = ToolFactory.toggle(FloatingMaxProperty)
    Seq(ComponentGroup(floatingMaxButton))
  }

}

object VectorComponentsSubpanel {
  val resetAbsMaxPixelTooltip =
    "Controls how points in the field are mapped to luminance values. When " +
            "enabled, luminance is based on maximum value in the field at " +
            "the current sim tick. When disabled, luminance is based on " +
            "maximum value in the field seen across entire sim history."
}

/** A wrapper for VectorFieldMemory that presents a single component slice of
  * it as a ScalarFieldReader. Handy for re-using viewers that already know how
  * to present scalar fields but don't have explicit support for vector fields.
  *
  * This is perhaps dodging the Cog API too much. The last checkin that didn't
  * use this system was revision 7386.
  *
  * @param vectorField The target VectorField out of which a single vector
  *                    component is to be sliced out as a ScalarField.
  * @param componentIdx The index of the vector component in `vectorField` that
  *                     is to be presented as a ScalarField.
  */
 class VectorComponentSliceAsScalarFieldReader(vectorField: VectorFieldMemory,
                                              componentIdx: Int)
    extends ScalarFieldReader {

  val layers: Int  = vectorField.layers
  val rows: Int    = vectorField.rows
  val columns: Int = vectorField.columns

  /** Shape of the tensors in the field. */
  val tensorShape: Shape = Shape()
  /** Shape of the field. */
  val fieldShape: Shape = vectorField.fieldShape
  /** Type of the field. */
  val fieldType: FieldType =
    new FieldType(fieldShape, tensorShape, vectorField.fieldType.elementType)

  private def paddedColumns = vectorField.paddedColumns
  private def dimensions    = fieldShape.dimensions

  private val page = {
    val startPage = componentIdx * vectorField.pageSize
    vectorField.directBuffer.duplicate()
            .position(startPage)
            .limit(startPage + vectorField.pageSize)
            .asInstanceOf[FloatBuffer].slice()
  }

  /** Compute the L-infinity norm on the difference of `this` and `that`.
    *
    * @param that AbstractFieldMemory to compare to `this`
    * @return L-infinity error
    */
  def compareLinf(that: FieldReader): Float = {
    require(fieldType.equals(that.fieldType))
    require(that.isInstanceOf[ScalarFieldReader])
    iterator.zip(that.asInstanceOf[ScalarFieldReader].iterator)
            .map(v => math.abs(v._1 - v._2))
            .max
  }

  /** An iterator over all values in the field, scanning in row-major order. */
  def iterator: Iterator[Float] = new Iterator[Float] {
    private var index = 0
    private var column = 0
    private val actualColumns = fieldType.fieldShape.lastDimension
    private val columnPadding = paddedColumns - actualColumns
    private val lastIndex = layers * rows * paddedColumns - columnPadding
    private val buffer = page.duplicate
    buffer.rewind

    def hasNext = (index == 0) || (index < lastIndex)
    def next(): Float = {
      val value = buffer.get(index)
      column += 1
      if (column == actualColumns) {
        column = 0
        index += columnPadding + 1
      } else
        index += 1
      value
    }
  }

  /** Read the single value in a 0D scalar field. */
  def read(): Float = {
    require(dimensions == 0)
    page.get(0)
  }

  /** Read the value at (`col`) in a 1D scalar field. */
  def read(col: Int): Float = {
    require(dimensions == 1)
    page.get(col)
  }

  /** Read the value at (`row`, `col`) in a 2D scalar field. */
  def read(row: Int, col: Int): Float = {
    require(dimensions == 2)
    page.get(row * paddedColumns + col)
  }

  /** Read the value at (`layer`, `row`, `col`) in a 3D scalar field. */
  def read(layer: Int, row: Int, col: Int): Float = {
    require(dimensions == 3)
    page.get(layer * rows * paddedColumns + row * paddedColumns + col)
  }

  /** Read the entire 0D or 1D field into the provided Array[Float]. */
  def get(dst: Array[Float]): Unit = {
    require(dimensions == 0 || dimensions == 1)
    page.rewind
    require(dst.size == columns,
      s"Mismatched column array in VectorComponentsSubpanel.get(), expecting $columns, saw ${dst.size}.")
    page.get(dst)
  }

  /** Read a portion of the values of the 0D or 1D scalar field into an
    * Array[Float], starting at the source buffer's `srcIndex` position.
    */
  def get(srcIndex: Int, dst: Array[Float]) {
    require(dimensions == 0 || dimensions == 1)
    page.position(srcIndex)
    page.get(dst)
  }

  /** Read `length` values of the 0D or 1D scalar field into the `dst`
    * Array[Float], starting at the source buffer's `srcIndex` position,
    * and placing the values in the `dst` Array starting at position
    * `dstIndex`.
    */
  def get(srcIndex: Int, dst: Array[Float], dstIndex: Int, length: Int) {
    require(dimensions == 0 || dimensions == 1)
    page.position(srcIndex)
    page.get(dst, dstIndex, length)
  }

  /** Read the entire 2D field into the provided Array[Array[Float]] */
  def get(dst: Array[Array[Float]]): Unit = {
    require(dimensions == 2)
    for (r <- 0 until dst.size) {
      page.position(r*paddedColumns)
      require(dst(r).size == columns, s"Mismatched column array in VectorComponentsSubpanel.get(), expecting $columns, saw ${dst(r).size}.")
      page.get(dst(r))
    }
  }

  /** Read the entire 3D field into the provided Array[Array[Array[Float]]] */
  def get(dst: Array[Array[Array[Float]]]): Unit = {
    require(dimensions == 3)
    for (l <- 0 until dst.size) {
      for (r <- 0 until dst(l).size) {
        page.position(l*r*paddedColumns)
        require(dst(l)(r).size == columns, s"Mismatched column array in VectorComponentsSubpanel.get(), expecting $columns, saw ${dst(l)(r).size}.")
        page.get(dst(l)(r))
      }
    }
  }
}