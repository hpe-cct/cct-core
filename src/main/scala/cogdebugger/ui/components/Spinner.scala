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

import scala.swing.Component
import scala.swing.event.ValueChanged
import javax.swing.{SpinnerNumberModel, JSpinner, SpinnerModel}

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 10/2/13
 * Time: 4:12 PM
 */
class Spinner(initModel: SpinnerModel) extends Component {
  override lazy val peer = new JSpinner(initModel) with SuperMixin
  def this() = this(new SpinnerNumberModel())

  def value = peer.getValue
  def value_=(newValue: Any) { peer.setValue(newValue) }
  def editor = peer.getEditor
  def editor_=(newEditor: Component) { peer.setEditor(newEditor.peer) }
  def commit() = peer.commitEdit()
  def model = peer.getModel
  def model_=(newModel: SpinnerModel) { peer.setModel(newModel) }
  def nextValue = peer.getNextValue
  def prevValue = peer.getPreviousValue

  peer.addChangeListener(scala.swing.Swing.ChangeListener {
    e => publish(new ValueChanged(this))
  })
}
