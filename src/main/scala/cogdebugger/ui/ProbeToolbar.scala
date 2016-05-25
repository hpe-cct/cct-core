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

package cogdebugger.ui

import cogdebugger.ProbeManager
import scala.swing._

/** Implements a toolbar with a combo box for controlling the rate at which
  * probes refresh. Spawns a new thread to call the manager's readProbes method
  * at the set rate. */
class ProbeToolbar(manager: ProbeManager) extends BoxPanel(Orientation.Horizontal) {
  import ProbeToolbar._

  val rateBox = new ComboBox(rates)
  rateBox.selection.item = 20
  rateBox.maximumSize = rateBox.preferredSize

  listenTo(rateBox.selection)
  reactions += {
    case event.SelectionChanged(`rateBox`) =>
      manager.probeDriver.updatesPerSec = rateBox.selection.item
  }

  contents += new Label("Frames/sec:")
  contents += Swing.HStrut(10)
  contents += rateBox
  //contents += Swing.HGlue // Take up the extra space

}

object ProbeToolbar {
  private val rates = Seq[Double](60, 30, 20, 10, 5, 2, 1, 0.5, 0.1)
}