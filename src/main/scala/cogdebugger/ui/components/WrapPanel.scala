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

package cogdebugger.ui.components

import scala.swing.FlowPanel

import org.jdesktop.swingx.WrapLayout

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 11/29/12
 * Time: 12:22 PM
 */

/** A (hopefully) more intelligent variant of the scala.swing.FlowPanel, in
  * that it should wrap its child components properly when placed inside of
  * ScrollPanes or SplitPanes.
  */
class WrapPanel(alignment: scala.swing.FlowPanel.Alignment.Value)(contents0: scala.swing.Component*)
  extends FlowPanel(alignment)(contents0: _*) {
  override lazy val peer =
    new javax.swing.JPanel(new WrapLayout(alignment.id)) with SuperMixin
  def this(contents0: scala.swing.Component*) = this(FlowPanel.Alignment.Center)(contents0: _*)
  def this() = this(FlowPanel.Alignment.Center)()
}
