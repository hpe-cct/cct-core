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
import libcog.fields.SymmetricTensor
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState}
import scala.swing._
import scala.xml.{Node, Elem}
import scala.swing.event.ValueChanged
import java.awt.geom.{Line2D, Ellipse2D}



/** A panel which displays a symmetric tensor field as geometrical objects.
  *
  * @param fieldShape Shape of the symmetric tensor field being displayed.
  *
  * @author Greg Snider
  */
class SymmetricGeometrySubpanel(fieldShape: Shape, tensorShape: Shape)
        extends TileBasedFieldPanel[VectorFieldReader](fieldShape)
        with Viewer
        with RestorableState
        with ToolbarItems
{
  require(tensorShape.dimensions == 1)
  require(tensorShape(0) == SymmetricTensor.Components)

  val FloatingMaxProperty = new BooleanProperty("Floating Max", false)
  properties += ZoomProperty
  properties += FloatingMaxProperty

  // Cumulative (over time) max magnitude in field.
  private var absMaxMagnitude  = Float.MinValue
  // Current max magnitude in field.
  private var stepMaxMagnitude = Float.MinValue
  // Max magnitude as desired by user.
  private def maxMagnitude =
    if (FloatingMaxProperty.value) stepMaxMagnitude
    else absMaxMagnitude

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    super.update(src, data.asInstanceOf[VectorFieldMemory])
  }

  /** Compute any needed statistics on your data (for display or to help draw
    * later). Called after new data has been received (in updateData) and
    * before drawing begins for that new data. */
  def updateStatistics(data: VectorFieldReader) {
    stepMaxMagnitude = Float.MinValue
    forEachTensorIn(data) {
      vector =>
        val tensor = new SymmetricTensor(vector)
        val stickness = tensor.stickness
        val ballness = tensor.ballness
        val magnitude = stickness + ballness
        if (magnitude > absMaxMagnitude) {
          absMaxMagnitude = magnitude
          stepMaxMagnitude = magnitude
        } else if (magnitude > stepMaxMagnitude) {
          stepMaxMagnitude = magnitude
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
  def drawElement(data: VectorFieldReader, g: Graphics2D, indices: Int*) {
    val vector = new Vector(SymmetricTensor.Components)
    dim match {
      case 0 => data.read(vector)
      case 1 => data.read(indices(0), vector)
      case 2 => data.read(indices(0), indices(1), vector)
      case 3 => data.read(indices(0), indices(1), indices(2), vector)
      case n => throw new UnsupportedDimensionException(n)
    }
    val tensor = new SymmetricTensor(vector)
    val ballness = tensor.ballness
    val stickness = tensor.stickness
    var angle = tensor.orientation
    val magnitude = (stickness + ballness) / maxMagnitude

    // Compute line segment in (x, y) plane
    val deltaX = magnitude * math.cos(angle) * (cellSize / 2)
    val deltaY = -magnitude * math.sin(angle) * (cellSize / 2)
    line.setLine(center - deltaX, center - deltaY, center + deltaX, center + deltaY)

    // Compute circle in (x, y) plane
    val diameter = (ballness / maxMagnitude) * cellSize
    val circleX = center - diameter / 2
    val circleY = center - diameter / 2
    val circleWidth = diameter
    val circleHeight = diameter
    circle.setFrame(circleX, circleY, circleWidth, circleHeight)

    val oldColor = g.getColor
    g.setColor(java.awt.Color.LIGHT_GRAY)
    g.draw(circle)
    g.setColor(oldColor)

    g.draw(line)
  }

  // Shapes'n'things used for drawing.
  private val circle = new Ellipse2D.Double(cellSize / 2 - 1, cellSize / 2 - 1,
    2, 2)
  private val line  = new Line2D.Double() // Reuse to avoid creation overhead
  private def center = cellSize / 2

  protected def forEachTensorIn(data: VectorFieldReader)(op: Vector => Unit) {
    val vector = new Vector(SymmetricTensor.Components)
    for (l <- 0 until layers; r <- 0 until rows; c <- 0 until cols)
      op(read(data, l, r, c, vector))
  }

  private def read(data: VectorFieldReader, l: Int, r: Int, c: Int, vector: Vector): Vector = {
    dim match {
      case 0 => data.read(vector)
      case 1 => data.read(c, vector)
      case 2 => data.read(r, c, vector)
      case 3 => data.read(l, r, c, vector)
      case n => throw new UnsupportedDimensionException(n)
    }
    vector
  }

  def save: Elem =
    <SymmetricGeometrySubpanel>
      { propertiesTag }
    </SymmetricGeometrySubpanel>

  def restore(tag: Node) {
    (tag \ "SymmetricGeometrySubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  private val Outer = this

  /** Toolbar for controlling view of complex field. */
  def toolbarComponents: Seq[ComponentGroup] = {
    val floatingMaxButton = ToolFactory.toggle(FloatingMaxProperty)
    val maxMagLabel = new Label()
    maxMagLabel.listenTo(Outer)
    reactions += {
      case ValueChanged(Outer) =>
        maxMagLabel.text = "%6.3f".format(maxMagnitude)
    }
    Seq(ComponentGroup(floatingMaxButton),
        ComponentGroup(new Label("Max magnitude:"), Swing.HStrut(5), maxMagLabel))
  }
}
