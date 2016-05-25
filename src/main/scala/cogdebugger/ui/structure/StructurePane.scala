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

import cogx.parameters.Cog
import libcog.ComputeGraph
import cogdebugger.RestorableState
import scala.swing._
import scala.xml.{Node, Elem}
import scala.Some

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 8/22/13
 * Time: 4:00 PM
 */

class StructurePane(graph: ComputeGraph)
    extends TabbedPane
    with RestorableState {

  val network =
    if (graph.probedCircuit.flatten.length <= StructurePane.MaxLayoutProbedFields) {
      Some(new GraphViewer(graph))
    } else {
      None
    }

  val buttons = new ButtonsPanel(graph)

  private val networkPage = {
    val tabTitle = "Graph"
    val tabContent = network match {
      case Some(graphView) => graphView
      case None            =>
        val label = new Label("Too many probed fields for the Graph View " +
          "(threshold is "+StructurePane.MaxLayoutProbedFields+" fields).")
        // Wrap label in a BorderPanel to center the text
        val bp = new BorderPanel
        bp.layout(label) = BorderPanel.Position.Center
        bp
    }
    new TabbedPane.Page(tabTitle, tabContent)
  }

  private val buttonsPage = {
    val buttonsScroller = new ScrollPane(buttons)
    buttonsScroller.horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    new TabbedPane.Page("Fields", buttonsScroller)
  }

  pages ++= Seq(networkPage, buttonsPage)


  /** Encode the state of this object into an XML node. */
  def save: Elem = <StructurePane></StructurePane>

  /** Restore this object to the state described in the given XML node. */
  def restore(savedState: Node) {

  }
}

object StructurePane {

  /** Graph layout takes a long time for large models and can hang the UI.
    * What's more, the graph view becomes unintelligible when there are lots
    * of edges and fields to display. For these reasons, the GraphViewer
    * shouldn't be instantiated if the model has more ProbedFields than this
    * threshold. */
  val MaxLayoutProbedFields = Cog.maxLayoutProbedFields

}