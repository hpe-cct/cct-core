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

package cogx.runtime.checkpoint

import cogx.compiler.parser.syntaxtree.{UnpipelinedColorActuator, Field}

/** An unpipelined color actuator class set up as its own class, not an anonymous subclass of UnpipelinedColorActuator.
  *
  * A simpler approach that only requires a factory object is shown in SimpleTestActuator.
  *
  * @author Dick Carter
  */
class UnpipelinedTestColorActuator(source: Field, update: (Iterator[Byte]) => Unit, resetHook: () => Unit,
                            parameters: () => String) extends UnpipelinedColorActuator(source, update, resetHook) {
  override def restoreParameters = parameters()

  // Note: this.getClass.getName will return "UnpipelinedTestColorActuator" as desired, so we need not override
  // the restoringClass method.
}

/** Factory for an unpipelined actuator class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object UnpipelinedTestColorActuator {
  /** The factory method for this actuator. */
  private def apply(source: Field, initState: Int): UnpipelinedTestColorActuator = {

    /** The smallest value supplied by the next iterator returned by this actuator. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this actuator to its current state. */
    def parameters = (state - 1).toString

    def reset() {
      state = initState
    }

    def updateAndCheck(it: Iterator[Byte]): Unit = {
      var expected = state
      while (it.hasNext) {
        val actual = it.next & 0xff
        require(expected == actual, s"Data mismatch: expected $expected, saw $actual.")
        expected += 1
      }
      state += 1
    }

    new UnpipelinedTestColorActuator(source, updateAndCheck _, reset _, parameters _)
  }

  /** The factory method used to create an unpipelined color actuator. */
  def apply(source: Field): UnpipelinedTestColorActuator = apply(source, 0)

  /** The factory method used to create an unpipelined color actuator from its stored parameter string. */
  def restore(source: Field, parameterString: String) = {
    require(source.fieldType.tensorShape.points == 3,
      "Expecting ColorField as input to ColorActuator, found " + source.fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(source, initState)
  }
}
