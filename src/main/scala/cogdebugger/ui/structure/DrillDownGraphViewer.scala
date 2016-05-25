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

import cogdebugger.{ModuleHierarchyTree, ModuleNode, FieldNode}
import cogdebugger.coggui3ports.{mxDummyEdge, GraphPanel, Graph}
import cogdebugger.ui.components.{ToolBar, LoadingAnimationPanel}
import cogx.runtime.debugger.ProbedField
import com.mxgraph.model.mxCell
import com.mxgraph.view.mxGraph
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import scala.swing._

/** A graph viewer that is module-aware. Rather than displaying the entire
  * graph at once (which may be quite large), it only draws the contents of a
  * single module of interest at a time.
  *
  * The user navigates the module hierarchy through the use of a navigation
  * toolbar as well as by clicking on the vertices on the graph that correspond
  * to modules The vertices corresponding to fields are colored blue; those
  * corresponding to modules are a shade of gray.
  *
  * @param hierarchy A ModuleHierarchyTree describing the module structure of
  *                  the ComputeGraph being debugged.
  */
class DrillDownGraphViewer(hierarchy: ModuleHierarchyTree)
    extends BorderPanel {
  import DrillDownPanel._

  private def root = hierarchy.moduleHierarchyRoot

  var graph = new Graph

  // The panel that actually does the drawing, a Java Swing component.
  val mxGraphPanel = new GraphPanel(graph, click, rightClick, click, rightClick)

  // Wrap the panel for integration in Scala Swing
  val graphComponent = Component.wrap(mxGraphPanel)

  private val navigation = new NavigationPanel

  // The "focused" module whose children, inputs, and outputs are drawn
  var currentlyViewedModule = root

  /** Map from objects to their corresponding vertex in the displayed graph. */
  val vertexMap = collection.mutable.Map[AnyRef, mxCell]()
  implicit def obj2vertex(obj: AnyRef) = vertexMap(obj)

  navigateTo(currentlyViewedModule) // Initialize

  add(navigation, BorderPanel.Position.North)
  add(graphComponent, BorderPanel.Position.Center)

  def click(vertex: mxCell, value: AnyRef) { click(value) }
  def click(value: AnyRef) {
    value match {
      case m: ModuleNode => navigateTo(m)
      case f: FieldNode => publish(VertexLeftClick(f.wrappedField))
      case p: ProbedField => publish(VertexLeftClick(p))
      case _ =>
    }
  }
  def rightClick(value: AnyRef) {
    value match {
      case m: ModuleNode => navigateTo(m)
      case f: FieldNode => publish(VertexRightClick(f.wrappedField))
      case p: ProbedField => publish(VertexRightClick(p))
      case _ =>
    }
  }

  /** "Drill-down" into a module defined in the compute graph, displaying its
    * child modules and fields, as well as their connections. Child modules
    * are drawn in a collapsed state, i.e., you can't see their children; but
    * edges going to or from those children are shown as entering/leaving the
    * child module (they're "promoted" to some module with a visible vertex).
    *
    * The child fields of the focused module are drawn as blue boxes, while
    * child modules are drawn as gray boxes. Source or sink fields that
    * exist outside the focused module are drawn as light grey boxes, labeled
    * with the full path name of the field.
    *
    * @param module The newly focused module whose children, inputs, and
    *               outputs are to be drawn
    */
  def navigateTo(module: ModuleNode) {
    // Could take a while; we want to move work to a separate thread and show
    // a loading animation in the meantime
    add(new LoadingAnimationPanel("Working..."), BorderPanel.Position.Center)
    revalidate()
    repaint()
    new Worker(module).execute()
  }

  class Worker(module: ModuleNode) extends javax.swing.SwingWorker[Graph, Unit] {
    def doInBackground(): Graph = {
      vertexMap.clear()
      currentlyViewedModule = module
      graph = new Graph
      graph.setAutoOrigin(true)
      graph.setBorder(10)

      graph.getModel.beginUpdate()
      try {

        // Build vertices for all the immediate children (fields and
        // sub-modules) of the currently focused module
        module.children.foreach {
          case m @ ModuleNode(name, parent) =>
            vertexMap(m) = graph.addVertex(name, m, childModuleStyle)
            Log.d("added vertex for module "+name)
          case f @ FieldNode(probedField, parent) =>
            vertexMap(probedField) = graph.addVertex(probedField.simpleName, f)
            Log.d("added vertex for field: "+probedField.name.mkString("."))
        }

        // Start building edges. In an edge would go to/from a field that's not
        // an immediate child of the currently viewed module, we'll have to
        // build a vertex for it.
        hierarchy.postOrderTraverse {
          case f @ FieldNode(field, parent) =>
            field.dependsOn.foreach(src =>
              buildConnections(getNodeForField(src), f, graph.defaultEdgeStyle))
            field.feedbackFrom.foreach(src =>
              buildConnections(getNodeForField(src), f, graph.controlEdgeStyle))
          case _ => // Don't need to handle module vertices explicitly
        }

      } finally {
        graph.getModel.endUpdate()
      }

      val layout = new mxHierarchicalLayout(graph)
      layout.setMoveParent(true)
      layout.setResizeParent(true)
      layout.setParentBorder(10)
      layout.execute(graph.getDefaultParent)

      graph

    }

    override def done(): Unit = {
      graph = get()
      mxGraphPanel.setGraph(graph)
      navigation.currentModuleNameLabel.text = module.fullName
      add(graphComponent, BorderPanel.Position.Center)
      revalidate()
      repaint()
    }
  }

  /** Build edges between the given source and sink nodes if it's appropriate to
    * do so based on the currently focused module. Edges that connect children
    * (fields or modules) of the current focus needs to be drawn, as so those
    * that enter or leave the module. Everything else is contained fully inside
    * some other module and shouldn't be displayed.
    *
    * @param srcNode The source node from which the directed edge will
    *                originate, if it's to be drawn at all
    * @param dstNode The destination node on which the directed edge will
    *                terminate, if it's to be drawn at all
    * @param edgeStyle The style whith which the edge should bd drawn.
    */
  private def buildConnections(srcNode: FieldNode, dstNode: FieldNode, edgeStyle: String) {
    val (srcField, dstField) = (srcNode.wrappedField, dstNode.wrappedField)
    (srcNode.hasAncestor(currentlyViewedModule), dstNode.hasAncestor(currentlyViewedModule)) match {
      case (true, true) =>
        if ((dstNode.parent eq currentlyViewedModule) || (srcNode.parent eq currentlyViewedModule)) {
          // Either the source or sink vetex is a direct child of the
          // focused vertex; we must show the edge
          val (srcVertex, sinkVertex) = (getVertexFor(srcField), getVertexFor(dstField))
          if (!edgeExistsBetween(graph, srcVertex, sinkVertex))
            graph.connect(srcVertex, sinkVertex, mxDummyEdge, edgeStyle)
        } else if (srcNode.hasAncestor(dstNode.parent) || dstNode.hasAncestor(srcNode.parent)) {
          // Edge is contained entirely in one of the child modules
          // and needn't be shown.
        } else {
          // Edge spans between two of the currently focused module's
          // child modules and needs to be drawn.
          val (srcVertex, sinkVertex) = (getVertexFor(srcField), getVertexFor(dstField))
          if (!edgeExistsBetween(graph, srcVertex, sinkVertex))
            graph.connect(srcVertex, sinkVertex, mxDummyEdge, edgeStyle)
        }
      case (true, false) | (false, true) =>
        val (srcVertex, sinkVertex) = (getVertexFor(srcField), getVertexFor(dstField))
        if (!edgeExistsBetween(graph, srcVertex, sinkVertex))
          graph.connect(srcVertex, sinkVertex, mxDummyEdge, edgeStyle)
      case _ =>
      // Both source and sink terminals are somewhere outside the
      // module of interest. No edge to show in this case.
    }
  }

  /** Returns the ModuleNode corresponding to the given field's parent.
    * @param field Field whose parent module is to be fetched
    */
  private def getModuleForField(field: ProbedField) = {
    val path = field.name.slice(0, field.name.length - 1)
    var module = root
    for (step <- path) module = module.childModules(step)
    module
  }

  /** Returns the FieldNode corresponding to the given field.
    * @param field Field whose corresponding FieldNode is to be fetched
    */
  private def getNodeForField(field: ProbedField) =
    getModuleForField(field).fields.find(node => node.wrappedField.simpleName == field.simpleName).get

  /** Gets the vertex for the given field; vertex type varies according to the
    * currently viewed module.
    *
    * If the field is an immediate child of the currently viewed module, the
    * vertex corresponds to the field directly. If the field is in some other
    * module, the vertex corresponds to that module. If there is no vertex
    * associated with the target field, one is created and added to the current
    * graph.
    *
    * @param field Field for which to fetch corresponding vertex.
    */
  private def getVertexFor(field: ProbedField): mxCell = vertexMap.get(field) match {
    case Some(vertex) => vertex
    case None =>

      Log.d("Looking for a vertex for "+field.name.mkString("."))

      var parentModule = getModuleForField(field)

      // Climb the tree until we find something that already has a vertex, or
      // the tree root.
      while ((parentModule ne root) && (!vertexMap.contains(parentModule)))
        parentModule = parentModule.parent

      if (parentModule ne root) {
        // Any vertex we find at this point must correspond to a child module
        // of the currently viewed module, since it's already in vertexMap.
        Log.d(s"  Pre-existing vertex found: ($parentModule) "+vertexMap(parentModule))
        vertexMap(parentModule)
      } else {
        // If we made it all the way to the top, the currently viewed module
        // isn't an ancestor of the field; the fielld must belong to some other
        // higher level or sibling module.
        val parentModule = getModuleForField(field)
        graph.addVertex(field.name.mkString("."), parentModule, dependModuleStyle)
      }
  }

  /** Checks for the existence of a directed edge originating at `src` and
    * terminating on `dst` in the given graph `graph`. */
  private def edgeExistsBetween(graph: mxGraph, src: mxCell, dst: mxCell): Boolean = {
    val model = graph.getModel
    for (i <- 0 until model.getEdgeCount(src)) {
      val edge = model.getEdgeAt(src, i)
      val target = model.getTerminal(edge, true)
      if (target eq dst) return true
    }
    false
  }

  /** A toolbar with navigation buttons (go home, step in, step out) for
    * controlling the DrillDownGraphPanel. */
  private class NavigationPanel extends ToolBar("Graph Navigation") {
    floatable = false

    val currentModuleNameLabel = new Label(root.name)

    contents += Button("Home") { navigateTo(root) }
//    contents += Button("Step Out") { navigateTo(...) }
    contents += Swing.HStrut(10)
    contents += currentModuleNameLabel
  }

}

object DrillDownPanel {

  object Log {
    val verbosity = 1
    def e(msg: String) { if (verbosity > 0) Console.err.println(msg) }
    def w(msg: String) { if (verbosity > 1) Console.err.println(msg) }
    def i(msg: String) { if (verbosity > 2) Console.println(msg) }
    def d(msg: String) { if (verbosity > 3) Console.println(msg) }
  }

  val childModuleStyle = "defaultVertex;" +
    "align=center;" +
    //"verticalAlign=center;" +
    "fillColor=#AAAAAA"
  val dependModuleStyle = "defaultVertex;" +
    "align=center;" +
    //"verticalAlign=center;" +
    "fillColor=#DDDDDD;" +
    "opacity=50"
  val feedbackModuleStyle = "defaultVertex;" +
    "align=center;" +
    "verticalAlign=center;" +
    "fillColor=#FFDDDD"
}