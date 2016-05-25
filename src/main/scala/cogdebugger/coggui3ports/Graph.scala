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

import com.mxgraph.view.mxGraph
import com.mxgraph.layout.mxIGraphLayout
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.model.{mxGraphModel, mxCell}
import com.mxgraph.view.mxGraph.mxICellVisitor
import collection.mutable._

/**
 * A model of a Graph which can be visually displayed using the GraphPanel
 * class. This is built on the JGraphX library.
 * <p>
 * A Graph consists of a set of vertices connected with a set of directed edges.
 * Each vertex (or edge) is bound to an Object (of any class) when it is
 * created. This creates a one-to-one correspondence between an arbitrary
 * programmatic representation of a Graph and the specific model provided by
 * JGraphX.
 * <p>
 * Vertices are created using one of the "addVertex" methods, and are
 * interconnected using the "connect" method. See the TestGraphPanel class
 * for an example of building a Graph model.
 *
 * @author Greg Snider
 */
class Graph extends mxGraph {

  setBorder(10)
  autoOrigin = true // This option doesn't exist in scxml jgraph
  autoSizeCells = true
  setAllowDanglingEdges(false)
  setConnectableEdges(false)

  /////////////////////////////////////////////////////////////////////////////
  // Vertex and Edge Styles ///////////////////////////////////////////////////

  /**JGraphX specification of an unselected edge. */
  val dataEdgeStyle = ";" +
          "shape=connector;" + "startArrow=classic;" +
          "verticalAlign=middle;" + "align=center;" +
          "strokeColor=#6482B9;" + "rounded=true;" +
          "fontColor=#446299;" + "strokeWidth=1.5"

  val controlEdgeStyle = ";" +
          "shape=connector;" + "startArrow=classic;" +
          "verticalAlign=middle;" + "align=center;" +
          "strokeColor=#C00404;" + "rounded=true;" + // purple strokeColor=#804080 too subtle
          "fontColor=#446299;" + "strokeWidth=1"

  val defaultEdgeStyle = dataEdgeStyle

  /**JGraphX specification of a selected edge. */
  val selectedEdgeStyle = ";" +
          "shape=connector;" + "startArrow=classic;" +
          "verticalAlign=middle;" + "align=center;" +
          "strokeColor=black;" + "rounded=true;" +
          "fontColor=black;" + "strokeWidth=2"

  /**JGraphX specification of an unselected vertex. */
  val vertexStyle = "defaultVertex;"

  /** JGraphX specification of a selected vertex. */
  val selectedVertexStyle = "defaultVertex;strokeColor=black;strokeWidth=2"

  val moduleStyle = "defaultVertex;" +
          "shape=swimlane;" +
          "fillColor=#00AAAA;" +
          "opacity=80"

  val altModuleStyle = "defaultVertex;" +
          "align=left;" +
          "verticalAlign=top;" +
          "fillColor=#00AAAA"

  // End Styles ///////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  private val defaultVertexWidth = 80
  private val defaultVertexHeight = 30
  private val userVertices = Map[mxCell, Object]()
  private val userEdges = Map[mxCell, Object]()
  private var frozen = false
  
  class MyLayout(graph: Graph, orientation: Int)
          extends mxHierarchicalLayout(graph, orientation) {

    // These flags enable automatic resizing of group/module vertices
    setParentBorder(15)
    setMoveParent(true)
    setResizeParent(true)
  }

  var graphLayout: mxIGraphLayout =
    new MyLayout(this, Graph.InitialGraphOrientation)

  def addVertex(name: String, userVertex: Object, parent: mxCell, style: String) = {
    require(userVertex != null)
    require(!frozen)
    val cell = insertVertex(parent, null, name, 0, 0, defaultVertexWidth,
      defaultVertexHeight, style).asInstanceOf[mxCell]
    userVertices(cell) = userVertex
    cell
  }

  def addVertex(name: String, userVertex: Object, parent: mxCell): mxCell = {
    addVertex(name, userVertex, parent, vertexStyle)
  }
  
  def addVertex(name: String, userVertex: Object, style: String): mxCell = {
    addVertex(name, userVertex, getDefaultParent.asInstanceOf[mxCell], style)
  }

  /** Create a vertex bound to "userVertex", labeled with "name" on display. */
  def addVertex(name: String, userVertex: Object): mxCell = {
    addVertex(name, userVertex, getDefaultParent.asInstanceOf[mxCell])
  }

  /**
   * Create a vertex bound to "userVertex", labeled with "name1" on top of
   * "name2" on display.
   */
  def addVertex(name1: String, name2: String, userVertex: Object): mxCell =
    addVertex(name1 + "\n" + name2, userVertex)

  /** Implements a tooltip that shows the actual source and target of an
    * edge */
  override def getToolTipForCell(cell: AnyRef): String = {
    if (model.isEdge(cell)) {
      convertValueToString(model.getTerminal(cell, false)) + " -> " +
        convertValueToString(model.getTerminal(cell, true))
    } else {
      null
    }
  }

  /**
   * Connect "from" vertex to "to" vertex, and bind edge to "userEdge" with
   * style given by "edgeStyle".
   * <p>
   * BEWARE !!!
   * This function actually connects the edge backwards! We changed the edge
   * style so that the arrow is drawn at the end of the edge opposite where it
   * would normally be. The arrows will then appear to go the right way, and we
   * usually get a better layout, but it can cause weird behavior elsewhere in
   * JGraph (e.g., be careful with getModel.getTerminal, or any JGraph method
   * with an 'isSource' parameter, since under the hood, the source and
   * terminal are reversed).
   * */
  def connect(from: mxCell, to: mxCell, userEdge: Object, edgeStyle: String) = {
    require(userEdge != null)
    require(!frozen)
    val cell = insertEdge(getDefaultParent, null, null,
      to, from, edgeStyle).asInstanceOf[mxCell]
    userEdges(cell) = userEdge
    cell
  }

  /** Connect "from" vertex to "to" vertex, and bind edge to "userEdge" with
    * default style. */
  def connect(from: mxCell, to: mxCell, userEdge: Object): mxCell = {
    connect(from, to, userEdge, defaultEdgeStyle)
  }

  /** Connect "from" vertex to "to" vertex, and bind edge to "userEdge" with a
    * data edge style */
  def connectData(from: mxCell, to: mxCell, userEdge: Object): mxCell = {
    connect(from, to, userEdge, dataEdgeStyle)
  }

  /** Connect "from" vertex to "to" vertex, and bind edge to "userEdge" with a
    * control edge style */
  def connectControl(from: mxCell, to: mxCell, userEdge: Object): mxCell = {
    connect(from, to, userEdge, controlEdgeStyle)
  }

  /** Freeze state of graph, disallowing new vertices and edges. */
  def freeze(): Unit = {
    frozen = true
    setCellsEditable(false)
    setCellsMovable(false)
    setCellsSelectable(false)
  }

  /** Retrieve a list of all JGraphX "cells" (edges and vertices) in graph. */
  def cells: List[mxCell] = {
    var cellList = List[mxCell]()
    userVertices.keys.foreach(c => cellList ::= c)
    userEdges.keys.foreach(c => cellList ::= c)
    cellList
  }

  /** Get the user edge bound to the JGraphX "cell". */
  def userEdge(cell: mxCell) = userEdges.get(cell)

  /** Get the user vertex bound to the JGraphX "cell". */
  def userVertex(cell: mxCell) = userVertices(cell)

}

object Graph {
  /** The Input -> Output direction for the initial model graph layout. */
  val InitialGraphOrientation = javax.swing.SwingConstants.NORTH
}
