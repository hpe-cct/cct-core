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

package cogdebugger.coggui3ports

import java.awt.{Graphics, Rectangle}
import java.awt.event.{MouseMotionListener, MouseAdapter, MouseEvent}
import javax.swing.{JComponent, JFrame, SwingUtilities}
import java.util.Hashtable

import scala.language.implicitConversions

import com.mxgraph.analysis.mxGraphAnalysis
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.mxCell
import com.mxgraph.swing.handler.mxCellMarker
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.swing.util.mxICellOverlay
import com.mxgraph.util.mxRectangle
import com.mxgraph.view.{mxGraph, mxCellState}

/**
 * A Panel which displays a Graph (vertices and edges). The graph uses
 * rectangles filled with one or two lines of text for the vertices, and
 * lines terminated with an arrowhead for the edges. The graph is
 * automatically placed using a vertical hierarchical placement algorithm; this
 * means that very large graphs may take a long time to display since the
 * placement algorithmic is roughly quadratic.
 * <p>
 * The panel has "as-needed" scrollbars to accommodate large graphs, but
 * there is no mechanism for resizing the graph itself. The placement algorithm
 * attempts to make as compact of a layout as it can, so this should not be
 * too big of a problem. Java2D might be used later to add zooming if needed.
 * <p>
 * A vertex or edge within the viewed graph may be "selected" or "deselected" by
 * clicking on it with the mouse; the selected state toggles with each click.
 * Graphically the selected state is shown as a heavy, black line (edge) or
 * heavy, black bounding box (vertex). Deselecting a vertex or edge returns
 * it to a thinner, lighter appearance.
 * <p>
 * When a vertex or edge has its selected state changed by the user's mouse,
 * a vertexEvent or edgeEvent is generated respectively. These create calls
 * into user specified routines, "vertexEvent" or "edgeEvent", to report the
 * state change. When either routine is called, the user object associated
 * with the vertex or edge is passed as the first parameter, and the new
 * selected state is passed as the second parameter. See the Graph class
 * below for a description of how user objects are associated with vertices
 * and edges.
 * <p>
 * Note that creating a GraphPanel freezes the state of the Graph being
 * displayed, so that edges or vertices may no longer be added to it. It would
 * be fairly easy to allow for dynamic modification in the future.
 *
 * @param _graph The graph model to be displayed.
 * @param vertexLeftClick Routine called whenever a vertex is clicked on
 *        with a mouse. The first parameter is the vertex itself (an mxCell).
 *        The second parameter is an object which was bound to the vertex when
 *        that vertex was created.
 * @param edgeLeftClick Routine called whenever an edge is clicked on
 *        with a mouse. The first parameter is an object which was bound to
 *        the edge when that edge was created.
  * @author Greg Snider
 */
class GraphPanel(private var _graph: Graph,
                 vertexLeftClick: (mxCell, Object) => Unit,
                 vertexRightClick: (Object) => Unit,
                 edgeLeftClick: (Object) => Unit,
                 edgeRightClick: (Object) => Unit)
        extends mxGraphComponent(_graph) {
  
  setToolTips(true)

  /* Disables creation of new edges, but also kills highlighting */
//  setConnectable(false)

  // Freeze the graph and lay it out into a compact, hierarchical display.
  //graph.freeze
  _graph.freeze()

  // Since the graph layout might be slow, best to let the user call
  // layoutGraph with his/her preferred parameters just once.
//  layoutGraph()

  private var delays = computeDelays()

  /**
   * When enabled, delay annotations drawn on cells will persist even after
   * the mouse is no longer hovering over the cell. Otherwise, they vanish the
   * moment the mouse leaves the cell's bounds.
   */
  var stickyDelays = false

  override def setGraph(graph: mxGraph) {
    super.setGraph(graph)
    delays = computeDelays()
    graph.asInstanceOf[Graph].freeze()
  }

  implicit def pimpMxGraph(graph: mxGraph): Graph = graph.asInstanceOf[Graph]

  // Collect mouse clicks on vertices and edges and relay events.
  getGraphControl.addMouseListener(new MouseAdapter {
    override def mouseReleased(e: MouseEvent) {
      val cell: mxCell = getCellAt(e.getX, e.getY).asInstanceOf[mxCell]
      if (cell != null) {
        if (cell.isVertex) {
          val userVertex = graph.userVertex(cell)
          if (SwingUtilities.isLeftMouseButton(e))
            vertexLeftClick(cell, userVertex)
          else
            vertexRightClick(userVertex)
        } else {
          graph.userEdge(cell) match {
            case Some(edge) =>
              if (SwingUtilities.isLeftMouseButton(e)) edgeLeftClick(edge)
              else edgeRightClick(edge)
            case None =>
          }
        }
        graph.repaint()
      }
    }
  })

  //////////////////////////////////////////////////////////////////////////////
  // Shortest Path Delay ///////////////////////////////////////////////////////

  val delayAnnotationTrigger = new MouseMotionListener {
    var lastVertex: AnyRef = null
    def mouseMoved(e: MouseEvent) {
      val cell = getCellAt(e.getX, e.getY)
      if (cell != lastVertex) {
        lastVertex = cell
        showDelays(cell)
      }
    }
    def mouseDragged(e: MouseEvent) {}
  }
  getGraphControl.addMouseMotionListener(delayAnnotationTrigger)

  /**
   * Computes the shortest path distances from each cell in the graph to every
   * other cell and returns the result as a map from src -> (sink -> distance).
   * Unreachable cells will have a distance of None, the source cell has a
   * distance of 0.
    *
    * @return
   */
  def computeDelays() = {
    val maxDijkstrasDepth = 100
    val directed = true
    val model = graph.getModel
    
    // This could be made a lot more efficient if we used our own implementation
    // of Dijkstra's algorithm, but we only need to do it once, since we don't
    // allow changes to the graph.
    
    val shortestPathsFrom = collection.mutable.Map[AnyRef, Map[AnyRef, Option[Int]]]()
    val vertices = graph.cells.filter(model.isVertex)
    
    for (src <- vertices) {
      val shortestPathTo = collection.mutable.Map[AnyRef, Option[Int]]()
      for (sink <- vertices) {
        shortestPathTo(sink) = {
          if (src eq sink) {
            Some(0)
          } else {
            // We used to use a special shortest-path finder that ignored
            // "swimlane" type cells. That's no longer necessary with the
            // current graph visualization (we no longer create or draw such
            // cells), but we can achieve more or less the same thing using
            // mxGraph's built-in shortest path function by feeding it a cost
            // function that heavily penalizes swimlane cells. E.g.:
            //val costFunc = new mxICostFunction {
            //  override def getCost(state: mxCellState): Double =
            //    if (graph.isSwimlane(state.getCell)) 9999999 else 1
            //}
            val path = mxGraphAnalysis.getInstance().getShortestPath(graph, sink, src, null, maxDijkstrasDepth, directed)
            val edges = path.filter(model.isEdge)
            if (edges.length > 0) Some(edges.length) else None
          }
        }
      }
      shortestPathsFrom(src) = shortestPathTo.toMap
    }
    shortestPathsFrom
  }

  /**
   * Draws an annotation on each vertex showing the shortest path distance from
   * cell to the vertex.
    *
    * @param cell
   */
  def showDelays(cell: AnyRef) {
    if (graph.getModel.isVertex(cell)) {
      clearCellOverlays()
      for (vertex <- graph.cells.filter(graph.getModel.isVertex).filterNot(graph.isSwimlane)) {
        delays(cell)(vertex) match {
          case Some(distance) => annotate(vertex, "Delay: "+distance.toString)
          case None => // no-op
        }
      }
    } else {
      if (!stickyDelays) clearCellOverlays()
    }
  }

  /**
   * Draws a String on top of the given vertex in its bottom-right corner.
    *
    * @param vertex
   * @param str
   */
  def annotate(vertex: AnyRef, str: String) {
    clearCellOverlays(vertex)
    val state = graph.getView.getState(vertex)
    if (state != null) {
      addCellOverlay(vertex, new JComponent with mxICellOverlay {
        setFont(GraphPanel.this.getFont)
        private val fm = getFontMetrics(getFont)
        private val w = fm.stringWidth(str)
        private val h = fm.getHeight
        val pad = 2
        val xOffset = state.getWidth.toInt - w - pad + 5
        val yOffset = state.getHeight.toInt - h + 5

        override def paint(g: java.awt.Graphics) {
          val g2 = g.asInstanceOf[java.awt.Graphics2D]
          g2.setColor(java.awt.Color.DARK_GRAY)
          g2.fillRect(0, 0, w + 2 * pad, h)
          g2.setColor(java.awt.Color.WHITE)
          g2.drawString(str, pad, h - fm.getDescent)
        }

        def getBounds(state: mxCellState): mxRectangle = {
          new mxRectangle(state.getX + xOffset, state.getY + yOffset, w + 2 * pad, h)
        }
      })
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Vertex & Edge Highlighting ////////////////////////////////////////////////

  // This mxCellMarker enables mouseover highlighting of edges in JGraph 1.9.1.3
  getConnectionHandler.setMarker(new mxCellMarker(this) {
    var lastStyle: java.util.Map[String, AnyRef] = null

    override def process(e: MouseEvent) = {
      var state: mxCellState = null
      if (isEnabled) {
        state = getState(e)
        val isValid = if (state != null) isValidState(state) else false
        if (!isValid) {
          state = null
        }
        highlight(state)
      }
      state
    }

    def highlightVertex(state: mxCellState) {
      require(graph.getModel.isVertex(state.getCell))
      // TODO Highlight edges leaving vertex
    }

    def highlightEdge(state: mxCellState) {
      require(graph.getModel.isEdge(state.getCell))
      // TODO highlight src and sink vertices
    }

    // TODO Replace this function with specific cases for edges and vertices
    def highlight(state: mxCellState) {
      if (validState != state) {
        var dirty: Rectangle = null
        if (validState != null) {
          validState.setStyle(lastStyle)
          dirty = validState.getBoundingBox.getRectangle
          dirty.grow(4, 4)
        }
        if (state != null) {
          lastStyle = state.getStyle
          state.setStyle(new Hashtable(state.getStyle))
          state.getStyle.put("strokeColor", "#00ff00")
          //state.getStyle.put("fontColor", "#00ff00")
          state.getStyle.put("strokeWidth", "3")
          val tmp = state.getBoundingBox.getRectangle
          if (dirty != null) {
            dirty.add(tmp)
          } else {
            dirty = tmp
          }
          dirty.grow(4, 4)
        }
        validState = state
        dirty.translate(-getHorizontalScrollBar.getValue, -getVerticalScrollBar.getValue)
        graphComponent.repaint(dirty)
      }
    }

    override def reset() { highlight(null) }

    override def paint(g: Graphics) { /* Do nothing */ }
  })

  //////////////////////////////////////////////////////////////////////////////
  // Graph Layout //////////////////////////////////////////////////////////////

  /** Layout the graph in a compact, hierarchical structure. */
  def layoutGraph(layoutFromSinks: Boolean = true, orientation: Int = Graph.InitialGraphOrientation) {
    val layout = new mxHierarchicalLayout(graph, orientation)
    val model = graph.getModel

    layout.setMoveParent(true)
    layout.setResizeParent(true)
    layout.setParentBorder(15)
//    layout.setLayoutFromSinks(layoutFromSinks) // Not available in 1.10.0.4

    /* Not sure that this helps, since we build a new layout every time. */
//    layout.setDeterministic(true) // Not available in 1.10.0.4

    //layout.setFineTuning(false)
    //layout.setInterRankCellSpacing(40)
    //layout.setIntraCellSpacing(40)

    /* We need to figure out the size and shape of any child cells before we
     * can lay out the current cell. JGraph won't do this for us (yet
     * anyways). */
    def helper(node: mxCell) {
      for (i <- 0 until model.getChildCount(node)) {
        val cell = model.getChildAt(node, i).asInstanceOf[mxCell]
        if (graph.isSwimlane(cell))
          helper(cell)
      }
      layout.execute(node)
    }

    helper(graph.getDefaultParent.asInstanceOf[mxCell])
  }
}

/**
 * Test code for the Graph and GraphPanel classes. Also supplies a simple
 * example of how to use the classes.
 *
 * @author Greg Snider
 */
object TestGraphPanel {
  def leftClickVertex(vertex: mxCell, userVertex: Object) =
    println("left click on vertex " + userVertex)

  def rightClickVertex(userVertex: Object) =
    println("right click on vertex " + userVertex)

  def leftClickEdge(userEdge: Object) =
    println("left click on edge " + userEdge)

  def rightClickEdge(userEdge: Object) =
    println("right click on edge " + userEdge)

  def main(args: Array[String]) {
    // Create user vertices and edges that will be represented by the graph.
    val userV1 = new Object {
      override def toString = "userV1"
    }
    val userV2 = new Object {
      override def toString = "userV2"
    }
    val userV3 = new Object {
      override def toString = "userV3"
    }
    val userV4 = new Object {
      override def toString = "userV4"
    }
    val userEdge12 = new Object {
      override def toString = "userE12"
    }
    val userEdge13 = new Object {
      override def toString = "userE13"
    }
    val userEdge23 = new Object {
      override def toString = "userE23"
    }
    val userEdge24 = new Object {
      override def toString = "userE24"
    }
    val userEdge43 = new Object {
      override def toString = "userE43"
    }
    val userEdge11 = new Object {
      override def toString = "userE11"
    }

    // Create the graph model.
    val graph = new Graph {
      val v1 = addVertex(userV1.toString, userV1)
      val v2 = addVertex(userV2.toString, userV2)
      val v3 = addVertex(userV3.toString, userV3)
      val v4 = addVertex(userV4.toString, userV4)
      connect(v1, v2, userEdge12)
      connect(v1, v3, userEdge13)
      connect(v2, v3, userEdge23)
      connect(v2, v4, userEdge24)
      connect(v4, v3, userEdge43)
      connect(v1, v1, userEdge11)
    }

    val graphPanel = new GraphPanel(graph, leftClickVertex, rightClickVertex,
      leftClickEdge, rightClickEdge)
    graphPanel.layoutGraph()
    new JFrame {
      getContentPane.add(graphPanel)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      setSize(400, 320)
      setVisible(true)
    }
  }
}
