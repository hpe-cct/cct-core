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

package cogdebugger.ui.structure

import cogx.runtime.debugger.ProbedField
import libcog.ComputeGraph
import cogdebugger.ModuleHierarchyTree
import cogdebugger.ui.components.{PopupButton, ToolBar}
import cogdebugger.coggui3ports.{mxDummyEdge, GraphPanel, Graph}
import com.mxgraph.model.mxCell
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.view.mxGraph
import scala.swing._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/13/13
 * Time: 11:16 AM
 */

/** Presents a graphical representation of the structure of a Cog ComputeGraph,
  * i.e. a display of its fields and their inputs and outputs.
  *
  * Each (probed) field in the compute graph is represented as a single vertex
  * in a directed graph. Each field is then connected to its inputs and outputs
  * via directed edges. Forward edges in the graph are rendered in one color,
  * while recurrence or "backward" edges are rendered in another.
  *
  * Clicking a field vertex or edge raises an
  * [[cogdebugger.ui.structure.InteractiveGraphEvent]] that other debugger
  * components (such as [[cogdebugger.ui.Desktop]]) may react to.
  *
  * The graph viewer operates in one of two modes: the default simple mode
  * simply shows everything at once - each vertex represents a (probed) field,
  * every input/ouput it has is represented by an incoming/outgoing edge. The
  * second mode, referred to as the Drill-Down view, tries to compact the
  * visual presentation by taking advantage of any module structure defined in
  * the input Compute Graph.
  *
  * The module hierarchy of a cmpute graph is implied from the organization of
  * its fields and other classes. Essentially, each class instance that
  * contains fields is considered a module, including the top-level compute
  * graph instance itself (for many of the short example apps, this top-level
  * module is the only module). The name of a module is not based on the class
  * name, but rather the val/var name to which it is first bound. Consider the
  * following pseudo-code:
  * {{{
  * class ModuleExample extends ComputeGraph {
  *   class ModuleA() {
  *     val aField = ScalarField(...)
  *   }
  *   val modA = new ModuleA()
  *   probeAll
  * }
  * }}}
  * The above code defines a ScalarField `aField` within a ModuleA instance
  * `modA`, which is itself within a ComputeGraph instance. Keep in mind that
  * modules and fieldsa are named after the val/var to which they are bound.
  * The long name of a field includes its path in the app's module structure.
  * The full name of the singular field in the above compute graph is
  * 'modA.aField'.
  *
  * In the Drill-Down view, only a single module is "in focus" at a time. The
  * children of that module (fields and other modules) are displayed as well
  * as inputs and outputs to the module. Clicking a field raises an event as
  * in the simple graph viewer, but clicking a module makes that module the
  * new focus and redraws the display. A toolbar at the top of the display
  * allows for focusing the parent of the current module or jumping back to
  * the top level.
  *
  * @param computeGraph The compute graph being debugged.
  */
class GraphViewer(computeGraph: ComputeGraph)
    extends BorderPanel {

  val toolbar = new GraphViewerToolBar
  add(toolbar, BorderPanel.Position.North)

  // We build both both graph viewers (simple and drill-down) and put them in
  // separate panels, but only show one panel at a time (dependent on the value
  // of the modulesButton in the toolbar's options menu definition).

  // TODO Defer graph instantiation and layout to another thread (large graphs hang the GUI)

  val graph = GraphViewer.buildGraph(computeGraph)
  graph.graphLayout.execute(graph.getDefaultParent)
  val graphPanel = new GraphPanel(graph, leftClick, rightClick, edgeLeftClick, edgeRightClick)
  val wrappedGraphPanel = Component.wrap(graphPanel)
  add(wrappedGraphPanel, BorderPanel.Position.Center)

  lazy val drilldown = {
    println("Building drilldown viewer")
    val hierarchy = new ModuleHierarchyTree(computeGraph.probedCircuit)
    val drilldown = new DrillDownGraphViewer(hierarchy)
    // forward events
    deafTo(this) // Don't listen to ourselves; or we'll loop forever when republishing.
    listenTo(drilldown)
    reactions += {
      case v: VertexLeftClick => GraphViewer.this.publish(v)
      case v: VertexRightClick => GraphViewer.this.publish(v)
    }
    drilldown
  }

  // In our graphing library, mxCells can be either edges or vertices. However,
  // our viewer implementations only bind objects (always ProbedFields for now)
  // to vertices.
  def leftClick(cell: mxCell, value: AnyRef) { doAction(value, leftClick = true) }
  def rightClick(value: AnyRef) { doAction(value, leftClick = false) }
  def edgeLeftClick(value: AnyRef) { doAction(value, leftClick = true) }
  def edgeRightClick(value: AnyRef) { doAction(value, leftClick = false) }

  /** Take action in response to the user clicking something in our displayed
    * graph.
    * @param value The object associated with the clicked graph element (a
    *              vertex or edge)
    * @param leftClick True if the user left-clicked the graph, false
    *                  otherwise
    */
  protected def doAction(value: AnyRef, leftClick: Boolean = true) {
    (value, leftClick) match {
      case (pf: ProbedField, true) => publish(VertexLeftClick(pf))
      case (pf: ProbedField, false) => publish(VertexRightClick(pf))
      case _ =>
    }
  }

  /** Re-execute the layout algorithm on the currently displayed graph view.
    * This method has no effect if the graph's current layout engine is not an
    * com.mxgraph.layout.hierarchical.mxHierarchicalLayout.
    */
  protected def relayout(orientation: Int) {
    graph.graphLayout match {
      case layout: mxHierarchicalLayout =>
        layout.setOrientation(orientation)
        layout.execute(graph.getDefaultParent)
      case _ =>
    }
  }

  /** Implements the toolbar for [[cogdebugger.ui.structure.GraphViewer]].
    * Includes buttons for chossing between a horizontal or vertical layout,
    * and for switching between the simple graph view or the Drill-Down view.
    */
  class GraphViewerToolBar extends ToolBar {
    floatable = false

    val orientationMenu = new Menu("Orientation")
    val southToNorthItem = new RadioMenuItem("") {
      action = Action("South -> North")(relayout(javax.swing.SwingConstants.NORTH))
    }
    val westToEastItem = new RadioMenuItem("") {
      action = Action("West -> East")(relayout(javax.swing.SwingConstants.EAST))
    }
    val northToSouthItem = new RadioMenuItem("") {
      action = Action("North -> South")(relayout(javax.swing.SwingConstants.SOUTH))
    }
    val eastToWestItem = new RadioMenuItem("") {
      action = Action("East -> West")(relayout(javax.swing.SwingConstants.WEST))
    }
    val orientationGroup = new ButtonGroup(southToNorthItem, westToEastItem, northToSouthItem, eastToWestItem)
    orientationGroup.select(Graph.InitialGraphOrientation match {
      case javax.swing.SwingConstants.NORTH => southToNorthItem
      case javax.swing.SwingConstants.EAST => westToEastItem
      case javax.swing.SwingConstants.SOUTH => northToSouthItem
      case javax.swing.SwingConstants.WEST => eastToWestItem
    })
    orientationMenu.contents ++= orientationGroup.buttons

    val modulesButton = new CheckMenuItem("") {
      action = Action("Show Modules") {
        if (selected)
          //GraphViewer.this.layout(wrappedModuleGraphPanel) = BorderPanel.Position.Center
          GraphViewer.this.layout(drilldown) = BorderPanel.Position.Center
        else
          GraphViewer.this.layout(wrappedGraphPanel) = BorderPanel.Position.Center
        GraphViewer.this.revalidate()
        GraphViewer.this.repaint()
      }
    }

    val optionsMenu = new PopupButton("Graph Options")
    optionsMenu.contents += modulesButton
    optionsMenu.contents += orientationMenu
    contents += optionsMenu

    contents += Button("Re-layout") {
      if (modulesButton.selected) {
//        moduleGraphLayout.execute(moduleGraph.getDefaultParent)
      } else {
        graph.graphLayout.execute(graph.getDefaultParent)
      }
    }

    contents += Swing.HGlue
    contents += Button("?") {
      Dialog.showMessage(parent = this, message = helpMessage, title="Help")
    }

    val helpMessage =
      "Left-click on a field to launch the default viewer for that\n" +
      "field type on the Probe Desktop.\n" +
      "\n" +
      "Right-click on a field to choose between all available viewers\n" +
      "for that field type." +
      "\n" +
      "Clicking a field for a which a viewer is already open brings that\n" +
      "viewer to the foreground."
  }

}

object GraphViewer {

  /** Constructs a graph model from the given `computeGraph` that can be
    * visually displayed using the JGraph library. This is a very direct
    * translation from Cog to JGraph: each probed field in the compute graph is
    * represented by a single vertex; each input or output to each field is
    * represented by an edge from/to the corresponding field vertex. This
    * method does NOT take into account the implicit module structure of the
    * compute graph.
    */
  def buildGraph(computeGraph: ComputeGraph): Graph = {
    val circuit = computeGraph.probedCircuit
    val graph = new Graph
    val model = graph.getModel

    val objToVertexMap = collection.mutable.Map[AnyRef, mxCell]()
    implicit def obj2Vertex(obj: AnyRef) = objToVertexMap(obj)

    try {
      model.beginUpdate()

      // Make all vertices
      circuit.traversePreorder(field => {
        val name = field.name.mkString(".")
        objToVertexMap(field) = graph.addVertex(name, field)
      })

      // Make all edges
      circuit.traversePreorder(field => {
        for (input <- field.dependsOn) graph.connect(input, field, mxDummyEdge)
        for (feedback <- field.feedbackFrom) graph.connectControl(feedback, field, mxDummyEdge)
      })

    } finally {
      model.endUpdate()
    }
    graph
  }

  /** Checks if there is an edge from src to dst in the given graph. */
  private def edgeExistsBetween(graph: mxGraph, src: mxCell, dst: mxCell): Boolean = {
    val model = graph.getModel
    for (i <- 0 until model.getEdgeCount(src)) {
      val edge = model.getEdgeAt(src, i)
      val target = model.getTerminal(edge, true)
      if (target eq dst) return true
    }
    false
  }

}

// These events could (and probably should) be decoupled from the graph. The
// vertex or edge click isn't really what's important - what matters is that
// the user is interested in some field or another. Any object that knows
// when the user wants to see a field should be able to raise the event,
// regardless of whether or not it's a graph (e.g., the ButtonsPanel, which
// has a button for every field, knows the user wants to see a particular
// field when they click its button - the panel ought to be able to raise an
// event even though the button isn't a vertex, per se).
/** An event signalling that a user has interacted with a graph view in some
  * way. */
sealed trait InteractiveGraphEvent extends event.Event

/** An event indiciating the user has left-clicked a vertex.
  * @param vertexContent The object bound to the vertex
  */
case class VertexLeftClick(vertexContent: AnyRef) extends InteractiveGraphEvent

/** An event indiciating the user has right-clicked a vertex.
  * @param vertexContent The object bound to the vertex
  */
case class VertexRightClick(vertexContent: AnyRef) extends InteractiveGraphEvent

/** An event indiciating the user has left-clicked an edge.
  * @param edgeContent The object bound to the edge
  */
case class EdgeLeftClick(edgeContent: AnyRef) extends InteractiveGraphEvent

/** An event indiciating the user has right-clicked an edge.
  * @param edgeContent The object bound to the edge
  */
case class EdgeRightClick(edgeContent: AnyRef) extends InteractiveGraphEvent