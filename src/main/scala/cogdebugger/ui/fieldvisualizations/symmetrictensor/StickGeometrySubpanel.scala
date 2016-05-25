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

package cogdebugger.ui.fieldvisualizations.symmetrictensor

import libcog._
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState}
import scala.swing._
import scala.swing.event.ValueChanged
import scala.xml.{Node, Elem}
import java.awt.geom.Line2D

/** A panel which displays a stick tensor field as geometrical objects.
  *
  * Each tensor is rendered as a line contained in a single cell in a grid of
  * cells. The midpoint of a line is fixed to the center of its cell, and its
  * orientation and length are determined by the phase and magnitude of the
  * tensor it represents. The visual length of the tensors are normalized such
  * that the longest line is contained entirely in its cell.
  *
  * This visualization supports the notion of a "floating maximum." The scaling
  * of the lines representing tensors can be based either upon the largest
  * magnitude tensor seen by this visualization across all updates since the
  * last reset or upon only the values present in the latest field data handed
  * to the viewer. The floating max property is set to 'false' by default; all
  * lines drawn have their lengths scaled according to the maximum magnitude
  * seen by the visualization across all updates. A user control for changing
  * this setting is supplied by the `toolbarComponents` method.
  *
  * @param fieldShape Shape of the stick tensor field being displayed
  *
  * @author Greg Snider
  */
class StickGeometrySubpanel(fieldShape: Shape)
        extends TileBasedFieldPanel[ComplexFieldReader](fieldShape)
        with Viewer
        with RestorableState
        with ToolbarItems
{

  // Cumulative (over time) max magnitude in field.
  private var absMaxAmplitude  = Float.MinValue
  // Current max magnitude in field.
  private var stepMaxAmplitude = Float.MinValue
  // Determines whether maxAmplitude reports step maximum or cumulative max.
  val FloatingMaxProperty = new BooleanProperty("Floating Max", false)
  // Max magnitude as desired by user.
  private def maxAmplitude =
    if (FloatingMaxProperty.value) stepMaxAmplitude else absMaxAmplitude

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
    * later). Called after new data has been received (in updateData) but
    * before drawing begins for that new data. */
  def updateStatistics(data: ComplexFieldReader) {
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
    //val oldColor = g.getColor
    //g.setColor(java.awt.Color.LIGHT_GRAY)
    //g.draw(circle)
    //g.setColor(oldColor)

    val complex = dim match {
      case 0 => data.read()
      case 1 => data.read(indices(0))
      case 2 => data.read(indices(0), indices(1))
      case 3 => data.read(indices(0), indices(1), indices(2))
      case n => throw new UnsupportedDimensionException(n)
    }
    val amplitude = complex.magnitude / maxAmplitude
    // The orientation is equal to half the phase, and must be in (0, Pi]
    var angle = complex.phase / 2
    if (angle <= 0)
      angle += math.Pi.toFloat

    // (x, y) plane
    val deltaX =  amplitude * math.cos(angle) * (cellSize / 2)
    val deltaY = -amplitude * math.sin(angle) * (cellSize / 2)

//    line.setLine(c, c, c + deltaX, c + deltaY)
    line.setLine(c - deltaX, c - deltaY, c + deltaX, c + deltaY)
    g.draw(line)
  }

  private val line  = new Line2D.Double() // Reuse to avoid creation overhead
  private def c = cellSize / 2

  def save: Elem =
    <StickGeometrySubpanel>
      { propertiesTag }
    </StickGeometrySubpanel>

  def restore(tag: Node) {
    (tag \ "StickGeometrySubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  protected def forEachComplexIn(data: ComplexFieldReader)(op: Complex => Unit) {
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until cols)
      op(read(data, l, r, c))
  }

  /** Read a single Complex out of `data`. This method requires three indices
    * as arguments, but if the field shape of `data` is less than the three
    * dimensions the extra indices are ignored. */
  private def read(data: ComplexFieldReader, l: Int, r: Int, c: Int): Complex = dim match {
    case 0 => data.read()
    case 1 => data.read(c)
    case 2 => data.read(r, c)
    case 3 => data.read(l, r, c)
    case n => throw new UnsupportedDimensionException(n)
  }

  /** An alias for `this` used by toolbar UI components to set up their
    * reactions. */
  private val Outer = this

  /** Builds a list of controls and other components that supplement this
    * viewer and are suitable for installation in a toolbar.
    *
    * Controls/components are grouped into ComponentGroups based on their
    * function (e.g. components that make up the legend are grouped into one
    * ComponentGroup, while controls that affect the rendering are in another).
    *
    * The toggle for the Floating Max property is the sole member of the first
    * ComponentGroup. The legend is in the second group.
    */
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
