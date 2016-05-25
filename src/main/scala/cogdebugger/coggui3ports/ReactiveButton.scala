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

import scala.swing.Button
import scala.swing.event._

/**
  * A ReactiveButton is a Button that allows you to directly attach a function
  * literal that is called whenever the button is clicked. The button is
  * labeled "label", and "function" is invoked on each button click.
  *
  * @param label String printed on button.
  * @param function Function called when button is pressed.
  *
  * @author Greg Snider
  */
class ReactiveButton(label: String, function: => Unit) extends Button {
  text = label
  reactions += { case ButtonClicked(_) => function }
}
