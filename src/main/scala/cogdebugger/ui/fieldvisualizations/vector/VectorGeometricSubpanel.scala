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
import cogdebugger.RestorableState
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.components.PopupButton
import scala.swing._
import scala.swing.event.ValueChanged
import scala.xml.{Node, Elem}
import java.awt.geom.{Ellipse2D, Line2D}

/**
  * Created with IntelliJ IDEA.
  * User: gonzatob
  * Date: 9/6/12
  * Time: 3:46 PM
  *
  * A visualization for VectorFields that treats vector components as Cartesian
  * components in two-dimensional space. Two components of each vector are
  * treated as x and y and then displayed as directed lines on a grid.
  *
  * For fields of three dimensions, each layer is sliced out into a separate
  * grid. Each cell in the grid(s) renders a directed line representing a
  * single vector. Lines originates from the center of their respective cells,
  * and the visual length of all lines are normalized such that the longest
  * line/vector in the field is fully contained in its cell.
  *
  * Since this visual representation can only account for two components of
  * vectors at a time, the `toolbarComponents` method will return a user
  * control for picking which components to treat as 'x' and 'y' if the field
  * contains vectors of three or more dimensions. This user control is not
  * provided if the field contains vectors of exactly two dimensions - in that
  * case, the first component is taken to be -y and the second component as x.
  * This is the mapping between Cog's (row, column) indexing an a conventional
  * Cartesian space with y increasing in the upward direction and x increasing
  * to the right.
  */
class VectorGeometricSubpanel(fieldShape: Shape, tensorShape: Shape)
        extends TileBasedFieldPanel[VectorFieldReader](fieldShape)
        with Viewer
        with RestorableState
        with ToolbarItems
{

  require(tensorShape(0) > 1, "Need at least 2D vectors.")

  /* If the length of the vector equals the dimensionality of the space, then
   * Cog defines that vector to be a tensor in that space. Tensors have a
   * definite geometric meaning, and the order of their components have a
   * prescribed order.
   *
   * Cog uses layer, row, column indexing. In the 2D case we just have rows and
   * columns. Note that (row, col) maps to (-y, x).
   *
   * If the vector dimension does not match the space dimension, then we have
   * an abstract vector rather than a tensor, and the geometric mapping is
   * arbitrary. */
  private var (xIndex, yIndex) = (1, 0)
  private var (flipX, flipY) = (false, true)

  private var maxAmplitude = 0f
  private var minAmplitude = Float.MaxValue

  properties += ZoomProperty

  /** A pre-rendering step. Finds the minimum and maximum vector amplitudes
    * in the field, which are needed for visually scaling the vector display.
    * Also fires a ValueChanged event to signal interested parties (namely
    * the legend UI component) that there are new values for min and max.
    */
  def updateStatistics(data: VectorFieldReader) {
    maxAmplitude = 0f
    minAmplitude = Float.MaxValue

    VectorFieldViewer.updateHelper(data, vector => {
      val amplitude = math.sqrt(vector dot vector).toFloat
      if (amplitude > maxAmplitude) maxAmplitude = amplitude
      if (amplitude < minAmplitude) minAmplitude = amplitude
    })

    publish(new ValueChanged(this))

    // Avoid divide by zero errors
    if (maxAmplitude == 0f) maxAmplitude = 1e-4f

    // Leave a little spacing between points in the field.
    maxAmplitude *= 1.1f
  }

  private val line  = new Line2D.Double() // Reuse to avoid creation overhead
  private def c = cellSize / 2

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) => update(src, data.asInstanceOf[VectorFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
    repaint()
  }

  val tmpVector = Vector(tensorShape(0), _ => 0f)
  def drawElement(data: VectorFieldReader, g: Graphics2D, indices: Int*) {
    val oldColor = g.getColor
    g.setColor(java.awt.Color.LIGHT_GRAY)
    g.draw(circle)
    g.setColor(oldColor)

    val vector = {
      data.fieldShape.dimensions match { //data.read(indices: _*)
        case 0 => data.read(tmpVector)
        case 1 => data.read(indices(0), tmpVector)
        case 2 => data.read(indices(0), indices(1), tmpVector)
        case 3 => data.read(indices(0), indices(1), indices(2), tmpVector)
        case d => throw new RuntimeException("Unsupported dimension: "+d)
      }
      tmpVector
    }
    val amplitude = math.sqrt(vector dot vector) / maxAmplitude

    val x = if (flipX) vector(xIndex) * -1 else vector(xIndex)
    val y = if (flipY) vector(yIndex) * -1 else vector(yIndex)

    val angle = math.atan2(y, x)
    val deltaX =  amplitude * math.cos(angle) * (cellSize / 2)
    val deltaY = -amplitude * math.sin(angle) * (cellSize / 2)
    line.setLine(c, c, c + deltaX, c + deltaY)
    g.draw(line)
  }

  private val circle = new Ellipse2D.Double(cellSize / 2 - 1, cellSize / 2 - 1,
    2, 2)

  def save: Elem =
    <VectorGeometricSubpanel>
      { propertiesTag }
    </VectorGeometricSubpanel>

  def restore(tag: Node) {
    (tag \ "VectorGeometricSubpanel" \ "properties").headOption.foreach(xmlToProperties)
  }

  /** An alias used by anonymous inner classes to listen to 'this'. */
  private val Outer = this

  /** Produces a sequence of user controls and supplemental views, grouped into
    * ComponentGroups by function.
    *
    * For this view, the first ComponentGroup contains a legend showing the
    * minimum/maximum values in the field and the colors that map to them. If
    * it isn't the case that both the field and the vectors within are of two
    * dimensions, the sequence returned will have a second ComponentGroup with
    * a control for selecting which vector component maps to x and y for the
    * purposes of visualization.
    */
  def toolbarComponents: Seq[ComponentGroup] = {
    val indexChooser = buildIndexChooser()
    val magnitudePanel = new BoxPanel(Orientation.Horizontal) {
      //border = new javax.swing.border.EtchedBorder()
      val maxTextLabel = new Label("  magnitude (")
      val maxValueLabel = new Label(" 0.000")
      val commaLabel = new Label(", ")
      val minValueLabel = new Label(" 0.000")
      val closeLabel = new Label(") ")
      minValueLabel.listenTo(Outer)
      minValueLabel.reactions += {
        case ValueChanged(Outer) => minValueLabel.text = "%6.3f".format(minAmplitude)
      }
      maxValueLabel.listenTo(Outer)
      maxValueLabel.reactions += {
        case ValueChanged(Outer) => maxValueLabel.text = "%6.3f".format(maxAmplitude)
      }
      contents += maxTextLabel
      contents += minValueLabel
      contents += commaLabel
      contents += maxValueLabel
      contents += closeLabel
    }

    indexChooser match {
      case Some(chooser) => Seq(ComponentGroup(magnitudePanel), ComponentGroup(chooser))
      case None =>          Seq(ComponentGroup(magnitudePanel))
    }
  }

  /** Builds a small menu that lets users pick which components of vectors to
    * treat as x and y for rendering purposes. Also allows for flipping vectors
    * over the x or y axes when drawing. The addition of this menu makes the
    * visualization potentially useful even if the VectorField contains vectors
    * of more than two dimensions.
    */
  private def buildIndexChooser() = {
    val vectorDim = tensorShape(0)
    //if (vectorDim != fieldShape.dimensions) {
    if (!(vectorDim == 2 && vectorDim == fieldShape.dimensions)) {
      val button = new PopupButton("Options")

      val xGroup = new ButtonGroup()
      button.contents += new Menu("x index") {
        for (i <- 0 until vectorDim) {
          val item = new RadioMenuItem("") {
            if (i == xIndex) selected = true
            action = Action(i.toString) { xIndex = i; repaint() }
          }
          xGroup.buttons += item
          contents += item
        }
      }

      button.contents += new CheckMenuItem("Flip x") {
        selected = flipX
        action = Action("Flip x") { flipX = selected; repaint() }
      }

      button.menu.peer.addSeparator()

      val yGroup = new ButtonGroup()
      button.contents += new Menu("y index") {
        for (i <- 0 until vectorDim) {
          val item = new RadioMenuItem("") {
            if (i == yIndex) selected = true
            action = Action(i.toString) { yIndex = i; repaint() }
          }
          yGroup.buttons += item
          contents += item
        }
      }

      button.contents += new CheckMenuItem("Flip y") {
        selected = flipY
        action = Action("Flip y") { flipY = selected; repaint() }
      }

      Some(button)
    } else
      None
  }

}
