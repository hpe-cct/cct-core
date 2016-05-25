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

/** This class demonstrates one approach to restorable Actuators, particularly those that write to a data array.
  * The typical usage pattern to discover the actuator whose source field was named "myActuatedData" and start using
  * its data array is:
  *
  * val cg = ComputeGraph.readFromFile("myModel.h5")
  *
  * val actuator = cg.getUnpipelinedActuator("myActuatedData")
  *
  * val actuatorData : Array[Float] = actuator.data
  *
  * The value of actuator.state() may be helpful to correlate the data from the actuator with other state of the
  * user's application.  Upon reset, the actuator's data will have been written once, and the state would typically
  * show 0 (actually, this number is the number of steps executed before saving the model).  Upon each subsequent
  * step, the state number with increment.
  *
  * @author Dick Carter
  */
class UnpipelinedTestActuatorWithArray(source: Field, val data: Array[Float], update: (Iterator[Float]) => Unit, resetHook: () => Unit,
                            parameters: () => String) extends UnpipelinedActuator(source, update, resetHook) {
  override def restoreParameters = parameters()

  def state = parameters().toInt

  // Note: this.getClass.getName will return "UnpipelinedTestActuatorWithArray" as desired, so we need not override
  // the restoringClass method.
}

/** Factory for an unpipelined actuator class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object UnpipelinedTestActuatorWithArray {
  /** The factory method for this actuator. */
  private def apply(source: Field, initState: Int): UnpipelinedTestActuatorWithArray = {

    /** The smallest value supplied by the next iterator returned by this actuator. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this actuator to its current state. */
    def parameters = (state - 1).toString

    def reset() {
      state = initState
    }

    val data = new Array[Float](source.fieldType.fieldShape.points * source.fieldType.tensorShape.points)

    def updateArrayData(data: Iterator[Float], storage:Array[Float]): Unit = {
      var iData = 0
      while (data.hasNext) {
        storage(iData) = data.next()
        iData += 1
      }
      state += 1
      assert(iData==storage.length, "Dimensions for read-out did not match!")
    }

    new UnpipelinedTestActuatorWithArray(source, data, updateArrayData(_ , data), reset _, parameters _)
  }

  /** The factory method used to create an UnpipelinedTestActuatorWithArray. */
  def apply(source: Field): UnpipelinedTestActuatorWithArray = apply(source, 0)

  /** The factory method used to create an UnpipelinedTestActuatorWithArray from its stored parameter string. */
  def restore(source: Field, parameterString: String) = {
    require(source.fieldType.tensorShape.dimensions == 0,
      "Expecting ScalarField Actuator, found " + source.fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(source, initState)
  }
}
