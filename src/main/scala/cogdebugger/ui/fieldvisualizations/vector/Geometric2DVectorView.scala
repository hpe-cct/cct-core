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
import cogdebugger.ui.fieldvisualizations.{ComponentGroup, ToolbarItems, EventDrivenViewer, TileBasedFieldPanel}
import cogdebugger.ui.components.PopupButton
import scala.swing._
import scala.swing.event.ValueChanged
import java.awt.geom.Line2D

/**
 * Created with IntelliJ IDEA.
 * User: gonzatob
 * Date: 9/6/12
 * Time: 3:46 PM
 *
 * Viewer for fields of two-dimensional geometric vectors. Vectors are
 * displayed as directed lines on a grid, whith the grid having identical
 * dimensions to that of the field.
 *
 * Cog uses a row, column coordinate system. We can think of rows and columns
 * corresponding to y and x in a Cartesian coordinate system, but bear in mind
 * then that row indices increase in the ''downward'' direction, and thus our
 * y coordinate increases in the downward direction as well. Also note the
 * order in which we present row, column coordinates: the vertical or y
 * component comes first. For these reasons, the canonical 2D geometric vector
 * in Cog maps to a (-y, x) ordered pair in a traditional x-increases-to-the-
 * right and y-increased-up Cartesian space.
 *
 * So, a Cog vector v = Vector(-5, 7) looks like this in Cartesian coordinates:
 * {{{
 *
 *                       |             v
 *                       |          .  .
 *                       |       .     .
 *                       |    .        .
 *                       | .           .
 *   - - - - - - - - - - 0 - - - - - - - - - -
 *                       |
 *                       |
 *                       |
 *                       |
 * }}}
 */
class Geometric2DVectorView(target: AnyRef, fShape: Shape, tShape: Shape)
    extends TileBasedFieldPanel[VectorFieldReader](fShape)
    with EventDrivenViewer
    with ToolbarItems {

  require(tShape(0) > 1, "Need at least 2D vectors.")

  def targets = Set(target)

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
  //val indexChooser = buildIndexChooser()

  //val viewerTools = new Geometric2DVectorViewerToolbar()

  private var maxAmplitude = 0f
  private var minAmplitude = Float.MaxValue

  //layout(viewerTools) = BorderPanel.Position.North

  def updateStatistics(data: VectorFieldReader) {
    maxAmplitude = 0f
    minAmplitude = Float.MaxValue

    VectorFieldViewer.updateHelper(data, vector => {
      val amplitude = math.sqrt(vector dot vector).toFloat
      if (amplitude > maxAmplitude) maxAmplitude = amplitude
      if (amplitude < minAmplitude) minAmplitude = amplitude
    })

    //viewerTools.updateMagnitude(minAmplitude, maxAmplitude)
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
    //FieldMemoryPool.release(data)
  }

  val tmpVector = Vector(tShape(0), _ => 0f)
  def drawElement(data: VectorFieldReader, g: Graphics2D, indices: Int*) {
    val vector = {
      data.fieldShape.dimensions match { //data.read(indices: _*)
        case 0 => data.read(tmpVector)
        case 1 => data.read(indices(0), tmpVector)
        case 2 => data.read(indices(0), indices(1), tmpVector)
        case 3 => data.read(indices(0), indices(1), indices(2), tmpVector)
        case e => throw new RuntimeException("Unsupported dimension: "+e)
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

  /** Builds the toolbar controls and displays for this visualization and
    * returns them as a sequence of ComponentGroups. In this case, the first
    * group of components contains the "magnitude panel" (showing the lengths
    * of the shortest and longest vectors in the field) and the second group
    * contains controls that change how the view is rendered. */
  def toolbarComponents: Seq[ComponentGroup] = {
    //floatable = false

    val indexChooser = buildIndexChooser()
    val magnitudePanel = new BoxPanel(Orientation.Horizontal) {
      //border = new javax.swing.border.EtchedBorder()
      val maxTextLabel = new Label("  magnitude (")
      val maxValueLabel = new Label(" 0.000")
      val commaLabel = new Label(", ")
      val minValueLabel = new Label(" 0.000")
      val closeLabel = new Label(") ")
      contents += maxTextLabel
      contents += minValueLabel
      contents += commaLabel
      contents += maxValueLabel
      contents += closeLabel
    }

    magnitudePanel.minValueLabel.listenTo(Geometric2DVectorView.this)
    magnitudePanel.minValueLabel.reactions += {
      case ValueChanged(src) => magnitudePanel.minValueLabel.text = "%6.3f".format(minAmplitude)
    }
    magnitudePanel.maxValueLabel.listenTo(Geometric2DVectorView.this)
    magnitudePanel.maxValueLabel.reactions += {
      case ValueChanged(src) => magnitudePanel.maxValueLabel.text = "%6.3f".format(maxAmplitude)
    }

    if (indexChooser.isDefined)
      Seq(ComponentGroup(magnitudePanel), ComponentGroup(indexChooser.get))
    else
      Seq(ComponentGroup(magnitudePanel))

  }

  /** Builds a GUI menu for configuring which components of the vectorfield are
    * used to draw the visualization. Specifically, this menu lets the user
    * set which vector components to treat as x and y for rendering purposes.
    * It also allows for flipping a particular vector component prior to
    * rendering. */
  private def buildIndexChooser() = {
    val vectorDim = tShape(0)
    if (vectorDim != fShape.dimensions) {
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

object VectorFieldViewer {

  // I'd like to put a function like this in the TileBasedFieldPanel, but
  // the read method isn't defined on the Field supertype.
  def updateHelper(data: VectorFieldReader, f: (Vector) => Unit) {
    val dimensions = data.fieldShape.dimensions
    val vectorLength = data.tensorShape(0) // Vectors required to have dimensionality 1
    val indices = new Array[Int](dimensions)
    val limits = Array.tabulate(dimensions) { data.fieldShape(_) }

    val tmpVector = Vector(vectorLength, _ => 0f)

    dimensions match {

      case 0 =>
        data.read(tmpVector)
        f(tmpVector)

      case 1 =>
        for (c <- 0 until data.fieldShape(0)) {
          data.read(c, tmpVector)
          f(tmpVector)
        }

      case 2 =>
        for (r <- 0 until data.fieldShape(0); c <- 0 until data.fieldShape(1)) {
          data.read(r, c, tmpVector)
          f(tmpVector)
        }

      case 3 =>
        for (l <- 0 until data.fieldShape(0); r <- 0 until data.fieldShape(1); c <- 0 until data.fieldShape(2)) {
          data.read(l, r, c, tmpVector)
          f(tmpVector)
        }

      case x => throw new RuntimeException("Unsupported dimension: "+x)

    }

  }

}

//import cog.distributed.CogApp
