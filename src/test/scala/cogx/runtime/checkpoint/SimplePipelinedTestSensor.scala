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

import cogx.compiler.parser.syntaxtree.Sensor
import cogx.platform.types.FieldType

/** Similar to the PipelinedTestSensor, but simpler because it needs only the factory object.
  *
  * @author Dick Carter
  */
object SimplePipelinedTestSensor {
  /** The factory method for this sensor. */
  def apply(rows: Int, cols: Int, initState: Int = 0) = {

    /** The smallest value supplied by the next iterator returned by this sensor. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this sensor to its current state. */
    def parameters = (state - 2).toString

    def reset() { state = initState}

    def nextValue() = Some(new Iterator[Float] {
      var nextValue = state
      state += 1
      def next() = {
        val retVal = nextValue
        nextValue += 1
        retVal
      }
      def hasNext = true
    })

    new Sensor(rows, cols, nextValue _, reset _) {
      override def restoreParameters = parameters
      // The default restoringClass object instance would identify this as an anonymous subclass of a (pipelined) Sensor.
      // We override this here to point to the SimplePipelinedTestSensor factory object (so the restore method will be found)
      override def restoringClass = SimplePipelinedTestSensor
    }
  }

  /** The factory method used to create a sensor from its stored parameter string. */
  def restore(fieldType: FieldType, parameterString: String) = {
    require(fieldType.dimensions == 2 && fieldType.tensorShape.dimensions == 0,
      "Expecting 2D ScalarField Sensor, found " + fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(fieldType.rows, fieldType.columns, initState)
  }
}
