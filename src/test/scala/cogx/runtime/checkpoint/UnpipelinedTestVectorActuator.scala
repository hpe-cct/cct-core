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

import cogx.compiler.parser.syntaxtree.{UnpipelinedVectorActuator, Field, UnpipelinedActuator}

/** An unpipelined vector actuator class set up as its own class, not an anonymous subclass of UnpipelinedVectorActuator.
  *
  * A simpler approach that only requires a factory object is shown in SimpleTestActuator.
  *
  * @author Dick Carter
  */
class UnpipelinedTestVectorActuator(source: Field, update: (Iterator[Float]) => Unit, resetHook: () => Unit,
                            parameters: () => String) extends UnpipelinedVectorActuator(source, update, resetHook) {
  override def restoreParameters = parameters()

  // Note: this.getClass.getName will return "UnpipelinedTestVectorActuator" as desired, so we need not override
  // the restoringClass method.
}

/** Factory for an unpipelined actuator class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object UnpipelinedTestVectorActuator {
  /** The factory method for this actuator. */
  private def apply(source: Field, initState: Int): UnpipelinedTestVectorActuator = {

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

    new UnpipelinedTestVectorActuator(source, updateAndCheck _, reset _, parameters _)
  }

  /** The factory method used to create an unpipelined vector actuator. */
  def apply(source: Field): UnpipelinedTestVectorActuator = apply(source, 0)

  /** The factory method used to create an unpipelined vector actuator from its stored parameter string. */
  def restore(source: Field, parameterString: String) = {
    require(source.fieldType.tensorShape.dimensions == 1,
      "Expecting VectorField as source field to VectorActuator, found " + source.fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(source, initState)
  }
}
