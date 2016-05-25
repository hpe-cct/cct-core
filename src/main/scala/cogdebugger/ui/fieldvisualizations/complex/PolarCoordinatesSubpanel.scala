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

package cogdebugger.ui.fieldvisualizations.complex

import libcog._
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState}
import scala.swing._
import scala.swing.event.ValueChanged
import scala.xml.{Node, Elem}
import java.awt.geom.{Line2D, Ellipse2D}

/** Subpanel which displays each number in a complex field in polar coordinates
  * (as an arrow).
  *
  * There are two modes here.
  *
  * **************************************************************
  * MODE 1: Standard complex number representation in an (x, y) plane.
  *
  * This assumes a conventional (x, y) right-handed coordinate system for
  * displaying each individual complex number. This means that:
  *
  *    1. Complex(1, 0)  will point right
  *
  *    2. Complex(0, 1)  with point up
  *
  *    3. Complex(-1, 0) will point left
  *
  *    4. Complex(0, -1) will point down
  *
  * **************************************************************
  * MODE 2: As pseudo vectors in the (row, column) plane.
  *
  * It is sometimes useful to use complex numbers as tiny vectors in a 2D
  * field. You must be careful in doing that, though, because Cog uses a
  * left-handed (row, column) coordinate system. In this mode, the real
  * part of the number as treated as a row coordinate, and the imaginary part
  * is treated as the columns coordinate. This means that:
  *
  *    1. Complex(1, 0)  will point down
  *
  *    2. Complex(0, 1)  with point right
  *
  *    3. Complex(-1, 0) will point up
  *
  *    4. Complex(0, -1) will point left
  *
  * @param fieldShape The shape of the complex field to be displayed.
  * @param asPseudoVectors If true, display in MODE 2 as pseudo vectors in a
  *        (row, column) plane; if false, display is MODE 1 in polar coordinates
  *        assuming a conventional x, y plane.
  *
  * @author Greg Snider
  */
class PolarCoordinatesSubpanel(fieldShape: Shape, asPseudoVectors: Boolean)
        extends TileBasedFieldPanel[ComplexFieldReader](fieldShape)
        with Viewer
        with RestorableState
        with ToolbarItems
{

  // Cumulative (over time) max magnitude in field.
  private var absMaxAmplitude  = Float.MinValue
  // Current max magnitude in field.
  private var stepMaxAmplitude = Float.MinValue
  // Max magnitude as desired by user.
  private def maxAmplitude =
    if (FloatingMaxProperty.value) stepMaxAmplitude else absMaxAmplitude

  private val FloatingMaxProperty = new BooleanProperty("Floating Max", false)

  // Initialize
  properties += ZoomProperty
  properties += FloatingMaxProperty

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    //data.elementType match {
    //case c: ComplexFloatElement => update(src, data.asInstanceOf[ComplexFieldReader])
    data match {
      case cfm: ComplexFieldMemory => update(src, data.asInstanceOf[ComplexFieldReader])
      case x => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  /** Compute any needed statistics on your data (for display or to help draw
    * later). Called after new data has been received (in updateData) and
    * before drawing begins for that new data. */
  def updateStatistics(data: ComplexFieldReader) {
    // println("Updating statistics on: "+Thread.currentThread().toString)
    stepMaxAmplitude = Float.MinValue
    forEachComplexIn(data) { complex =>
      val mag = complex.magnitude
      if (mag > absMaxAmplitude) {
        absMaxAmplitude = mag
        stepMaxAmplitude = mag
      }
      else if (mag > stepMaxAmplitude) {
        stepMaxAmplitude = mag
      }
    }
    publish(new ValueChanged(this))
  }

  /** Draw the single field element at the given indices.
    *
    * Called after the pen has been moved to the top-left corner of the cell to
    * draw in. Clients should take care to draw within the bounds of the cell,
    * which is a cellSize by cellSize square.
    *
    * It would be nice if we could give an element as an argument instead of a
    * bunch of indices, but the Field class doesn't currently define any means
    * to actually extract elements from the field; that's left to subclasses.
    * Further complicating things, the panel classes don't know in advance how
    * many dimensions the data field has, so we have to use a variable length
    * indices parameter. If your field class has a read(indices: Int*) method,
    * you can get the current element, regardless of field dimensions, like so:
    * {{{
    *   val element = read(indices: _*)
    * }}}
    */
  def drawElement(data: ComplexFieldReader, g: Graphics2D, indices: Int*) {
    val oldColor = g.getColor
    g.setColor(java.awt.Color.LIGHT_GRAY)
    g.draw(circle)
    g.setColor(oldColor)

    val complex = dim match {
      case 0 => data.read()
      case 1 => data.read(indices(0))
      case 2 => data.read(indices(0), indices(1))
      case 3 => data.read(indices(0), indices(1), indices(2))
      case n => throw new UnsupportedDimensionException(n)
    }
    val amplitude = complex.magnitude / maxAmplitude
    val angle = complex.phase

    val (deltaX, deltaY) =
      if (!asPseudoVectors) {
        // (x, y) plane
        val deltaX = amplitude * math.cos(angle) * (cellSize / 2)
        val deltaY = -amplitude * math.sin(angle) * (cellSize / 2)
        (deltaX, deltaY)
      } else {
        // (row, column) plane
        val deltaX =  amplitude * math.sin(angle) * (cellSize / 2)
        val deltaY =  amplitude * math.cos(angle) * (cellSize / 2)
        (deltaX, deltaY)
      }
    line.setLine(c, c, c + deltaX, c + deltaY)
    g.draw(line)
  }

  // Shapes'n'things used for drawing.
  private val circle =
    new Ellipse2D.Double(cellSize / 2 - 1, cellSize / 2 - 1, 2, 2)
  private val line  = new Line2D.Double() // Reuse to avoid creation overhead
  private def c = cellSize / 2

  def save: Elem =
    <PolarCoordinatesSubpanel>
      { propertiesTag }
    </PolarCoordinatesSubpanel>

  def restore(tag: Node) {
    (tag \ "PolarCoordinatesSubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  protected def forEachComplexIn(data: ComplexFieldReader)(op: Complex => Unit) {
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until cols)
      op(read(data, l, r, c))
  }

  private def read(data: ComplexFieldReader, l: Int, r: Int, c: Int): Complex = dim match {
    case 0 => data.read()
    case 1 => data.read(c)
    case 2 => data.read(r, c)
    case 3 => data.read(l, r, c)
    case n => throw new UnsupportedDimensionException(n)
  }

  private val Outer = this

  /** Toolbar for controlling view of complex field. */
  def toolbarComponents: Seq[ComponentGroup] = {
    val floatingMaxButton = ToolFactory.toggle(FloatingMaxProperty)

    val maxMagLabel = new Label()
    maxMagLabel.listenTo(Outer)
    maxMagLabel.reactions += {
      case ValueChanged(Outer) => maxMagLabel.text = "%6.3f".format(maxAmplitude)
    }

    Seq(ComponentGroup(floatingMaxButton),
        ComponentGroup(new Label("Max magnitude:"), Swing.HStrut(5), maxMagLabel))
  }
}
