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

import cogx.compiler.parser.syntaxtree.UnpipelinedColorSensor
import cogx.platform.types.FieldType

/** Similar to the UnpipelinedTestSensor, but simpler because it needs only the factory object.
  *
  * @author Dick Carter
  */
object SimpleUnpipelinedTestColorSensor {
  /** The factory method for this sensor. */
  def apply(rows: Int, cols: Int, initState: Int = 0) = {

    /** The smallest value supplied by the next iterator returned by this sensor. */
    var state = initState

    // Note the we subtract 1 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this sensor to its current state. */
    def parameters = (state - 1).toString

    def reset() { state = initState}

    def nextValue() = new Iterator[Byte] {
      var nextValue = state
      state += 1
      var remainingValues = 3 * rows * cols
      def next() = {
        val retVal = nextValue
        nextValue += 1
        remainingValues -= 1
        (retVal & 0xff).toByte
      }
      def hasNext = remainingValues > 0
    }

    new UnpipelinedColorSensor(rows, cols, nextValue _, reset _) {
      override def restoreParameters = parameters
      // The default restoringClass would identify this as an anonymous subclass of UnpipelinedSensor.
      // We override this here to point to the SimpleUnpipelinedTestSensor factory object (so the restore method will be found)
      override def restoringClass = SimpleUnpipelinedTestColorSensor
    }
  }

  /** The factory method used to create an unpipelined color sensor from its stored parameter string. */
  def restore(fieldType: FieldType, parameterString: String) = {
    require(fieldType.dimensions == 2 && fieldType.tensorShape.points == 3,
      "Expecting 2D ColorField Sensor, found " + fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(fieldType.rows, fieldType.columns, initState)
  }
}
