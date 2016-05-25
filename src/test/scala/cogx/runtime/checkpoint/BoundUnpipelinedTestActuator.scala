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

import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.syntaxtree.{Field, UnpipelinedActuator}

import scala.collection.mutable

/** This class demonstrates one approach to restorable Actuators.  Another approach, embodied by the class
  * UnpipelinedTestActuatorWithArray, is now preferred over this class.  This class is a singleton that can
  * be told before a computegraph restore operation is performed what unpipelined actuators are expected
  * (by name).  The user would first invoke addBinding() to associate an actuator name with both a reset() and update()
  * callback.  Then the user would restore the compute graph, and when the restorer performs the
  * BoundUnpipelinedTestActuator.restore(), the actuator will be created with the provided callbacks. Singletons
  * with state are generally to be avoided, hence the recommendation to seek other approaches.
  *
  * @author Dick Carter
  */
object BoundUnpipelinedTestActuator {

  private val updateCallbacks = new mutable.HashMap[String, (Iterator[Float]) => Unit]()
  private val resetCallbacks = new mutable.HashMap[String, () => Unit]()

  def addBinding(sourceFieldName: String,
                 update: (Iterator[Float]) => Unit,
                  reset: () => Unit): Unit = {
    updateCallbacks.put(sourceFieldName, update)
    resetCallbacks.put(sourceFieldName, reset)
  }
  /** The factory method for this actuator. */
  private def apply(source: Field,
                    update: (Iterator[Float]) => Unit,
                    reset: () => Unit,
                    initState: Int): UnpipelinedActuator = {

    /** The smallest value supplied by the next iterator returned by this actuator. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this actuator to its current state. */
    def parameters = (state - 1).toString

    def watchfulReset() {
      reset()
      state = initState
    }

    def watchfulUpdate(it: Iterator[Float]): Unit = {
      update(it)
      state += 1
    }

    new UnpipelinedActuator(source, watchfulUpdate _, watchfulReset _) {
        override def restoreParameters = parameters
        // The default restoringClass would identify this as an anonymous subclass of UnpipelinedActuator.
        // We override this here to point to the SimpleUnpipelinedTestActuator factory object (so the restore method will be found)
        override def restoringClass = BoundUnpipelinedTestActuator
    }

  }

  /** The factory method used to create an unpipelined actuator. */
  def apply(source: Field, update: (Iterator[Float]) => Unit, reset: () => Unit): UnpipelinedActuator =
    apply(source, update, reset, 0)

  /** The factory method used to create an unpipelined actuator from its stored parameter string. */
  def restore(source: Field, parameterString: String) = {
    require(source.fieldType.tensorShape.dimensions == 0,
      "Expecting ScalarField Actuator, found " + source.fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt

    require(updateCallbacks.contains(source.name),
      "Actuator field -> update function binding missing for " + source.name)
    val update = updateCallbacks(source.name)
    updateCallbacks.remove(source.name)

    require(resetCallbacks.contains(source.name),
      "Actuator field -> reset function binding missing for " + source.name)
    val reset = resetCallbacks(source.name)
    resetCallbacks.remove(source.name)

    apply(source, update, reset, initState)
  }
}
