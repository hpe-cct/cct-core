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

package cogdebugger

import cogx._


/** A wrapper for for any debugger state that is tied to a particular compute
  * graph. If this object is lost/destroyed, any of the GUI state here should
  * be cleaned up.
  *
  * Created by gonztobi on 1/22/2015.
  */
class DebuggerState(computegraph: ComputeGraph) {

  /** The ComputeGraph that this DebuggerState is associated with */
  val cg = computegraph

  /** The hierarchy of modules implicit in the compute graph definition */
  val mh = new ModuleHierarchyTree(cg.probedCircuit)

  /** The probe manager associated with `cg` */
  val pm = new ProbeManager(cg)

  //val openProbes = ...
  //val activeNetworkViewerTabIdx = ...

  /** A frame containing the GUI components, if they've been built */
  var ui: Option[UI] = None

  /** The name of the loaded application/computegraph, if it was provided */
  var appName: Option[String] = None

}

import scala.swing.{Panel, BorderPanel, SplitPane, BoxPanel, Orientation}
import scala.xml.{Node, Elem}
import cogdebugger.ui.structure.StructurePane
import cogdebugger.ui.{MessagesPanel, Desktop}
import java.awt.{Dimension, Point}

/** A graphical user interface that supports saving/restoring at least some of
  * its state to disk between runs (things like windows position and size). */
trait UI extends Panel with RestorableState {
  def teardown(): Unit
}

/** A GUI designed to look and feel like what existed for Cog 3. Consists of
  * three major UI portions: the main toolbar, the graph viewer, and the probe
  * desktop. The main toolbar goes across the top of the window, the graph
  * graph viewer exists as a sort of sidebar in the main area, the and probe
  * desktop takes up the reaming space (and the builk of the entire UI). This
  * style eschews the menu bar, status bar,a nd message windows. */
case class Cog3StyleDebuggerUI(
    mainToolbar: BoxPanel,
    structurePane: StructurePane,
    probeDesktop: Desktop,
    msgPanel: MessagesPanel
  ) extends BorderPanel with UI {

  protected val vSplit = new SplitPane(Orientation.Vertical)
  vSplit.oneTouchExpandable = true
  vSplit.resizeWeight = 0.2
  vSplit.leftComponent = structurePane
  vSplit.rightComponent = probeDesktop

  // The message box was removed to present a UI more consistent with Cog 3
  //// TODO intercept standard out/err and copy to messages panel?
  //val hSplit = new SplitPane(Orientation.Horizontal)
  //hSplit.oneTouchExpandable = true
  //hSplit.resizeWeight = 1.0
  //hSplit.topComponent = vSplit
  //hSplit.bottomComponent = messages

  layout(mainToolbar) = BorderPanel.Position.North

  // No message box, consistent with Cog 3
  //mainPanel.layout(hSplit) = BorderPanel.Position.Center

  // No status bar, consistent with Cog 3
  //val status    = new StatusPanel
  //mainPanel.layout(status) = BorderPanel.Position.South

  layout(vSplit) = BorderPanel.Position.Center

  def save: Elem =
    <UI>
      <divider>{ vSplit.dividerLocation }</divider>
      <MainToolBar></MainToolBar>
      { structurePane.save }
      { probeDesktop.save }
    </UI>

  def restore(savedState: Node) {
    (savedState \ "UI" \ "divider").headOption.foreach(divider => {
      RestorableState.restoreInt(divider)(divLoc => vSplit.dividerLocation = divLoc)
    })
    (savedState \ "UI").headOption.foreach(node => {
      structurePane.restore(node)
      probeDesktop.restore(node)
    })
  }

  def teardown() {

    // Is there actually anything to do here? I was thinking it would be
    // helpful to the garbage collector to disconnect all the components from
    // their

  }

}
