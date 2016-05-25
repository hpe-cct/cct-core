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

import cogx.compiler.parser.syntaxtree.{Field, UnpipelinedActuator}

/** Factory for an unpipelined actuator class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object SimpleUnpipelinedTestActuator {
  /** The factory method for this actuator. */
  private def apply(source: Field, initState: Int): UnpipelinedActuator = {

    /** The smallest value supplied by the next iterator returned by this actuator. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this actuator to its current state. */
    def parameters = (state - 1).toString

    def reset() {
      state = initState
    }

    def updateAndCheck(it: Iterator[Float]): Unit = {
      var expected = state
      while (it.hasNext) {
        val actual = it.next
        require(expected == actual, s"Data mismatch: expected $expected, saw $actual.")
        expected += 1
      }
      state += 1
    }

    new UnpipelinedActuator(source, updateAndCheck _, reset _) {
      override def restoreParameters = parameters
      // The default restoringClass would identify this as an anonymous subclass of UnpipelinedActuator.
      // We override this here to point to the SimpleUnpipelinedTestActuator factory object (so the restore method will be found)
      override def restoringClass = SimpleUnpipelinedTestActuator
    }
  }

  /** The factory method used to create an unpipelined actuator. */
  def apply(source: Field): UnpipelinedActuator = apply(source, 0)

  /** The factory method used to create an unpipelined actuator from its stored parameter string. */
  def restore(source: Field, parameterString: String) = {
    require(source.fieldType.tensorShape.dimensions == 0,
      "Expecting ScalarField Actuator, found " + source.fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(source, initState)
  }
}
