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
import cogx.compiler.parser.syntaxtree.ColorSensor
import cogx.platform.types.FieldType

/** A pipelined color sensor class set up as its own class, not an anonymous subclass of Sensor.
  *
  * A simpler approach that only requires a factory object is shown in SimpleTestSensor.
  *
  * @author Dick Carter
  */
class PipelinedTestColorSensor(rows: Int, cols: Int,
                          nextValue: () => Option[Iterator[Byte]],
                          reset: () => Unit,
                          parameters: () => String) extends ColorSensor(Shape(rows, cols), nextValue, reset) {
  override def restoreParameters = parameters()

  // Note: this.getClass.getName will return "PipelinedTestColorSensor" as desired, so we need not override
  // the restoringClass method.
}

/** Factory for an pipelined color sensor class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object PipelinedTestColorSensor {
  /** The factory method for this sensor. */
  def apply(rows: Int, cols: Int, initState: Int = 0) = {

    /** The smallest value supplied by the next iterator returned by this sensor. */
    var state = initState

    // Note the we subtract 2 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this sensor to its current state. */
    def parameters = (state - 2).toString

    def reset() { state = initState}

    def nextValue() = Some(new Iterator[Byte] {
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
    })

    new PipelinedTestColorSensor(rows, cols, nextValue, reset, parameters _)
  }

  /** The factory method used to create a pipelined color sensor from its stored parameter string. */
  def restore(fieldType: FieldType, parameterString: String) = {
    require(fieldType.dimensions == 2 && fieldType.tensorShape.points == 3,
      "Expecting 2D ColorField Sensor, found " + fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(fieldType.rows, fieldType.columns, initState)
  }
}
