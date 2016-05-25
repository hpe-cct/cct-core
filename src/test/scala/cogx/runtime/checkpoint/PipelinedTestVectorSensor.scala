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

import cogx.cogmath.algebra.real.Vector
import cogx.cogmath.geometry.Shape
import cogx.compiler.parser.syntaxtree.VectorSensor
import cogx.platform.types.FieldType

/** A pipelined vector sensor class set up as its own class, not an anonymous subclass of VectorSensor.
  *
  * A simpler approach that only requires a factory object is shown in SimpleTestSensor.
  *
  * @author Dick Carter
  */
class PipelinedTestVectorSensor(fieldShape: Shape, tensorShape: Shape,
                          nextValue: () => Option[Iterator[Vector]],
                          reset: () => Unit,
                          parameters: () => String) extends VectorSensor(fieldShape: Shape, tensorShape: Shape, nextValue, reset) {
  override def restoreParameters = parameters()

  // Note: this.getClass.getName will return "PipelinedTestVectorSensor" as desired, so we need not override
  // the restoringClass method.
}

/** Factory for an pipelined vector sensor class- creates instances from user code or from a stored parameter string.
  *
  * @author Dick Carter
  */
object PipelinedTestVectorSensor {
  /** The factory method for this sensor. */
  def apply(fieldShape: Shape, tensorShape: Shape, initState: Int = 0) = {
    val fieldPoints = fieldShape.points
    val tensorPoints = tensorShape.points

    /** The smallest value supplied by the next iterator returned by this sensor. */
    var state = initState

    // Note the we subtract 2 from `state`, since we want to repeat the last iterator that supplied the current state.*/
    /** The parameters that would restore this sensor to its current state. */
    def parameters = (state - 2).toString

    def reset() { state = initState}

    def nextValue() = Some(new Iterator[Vector] {
      var nextValue = state
      state += 1
      def next() = {
        val retVal = Vector(tensorPoints, (i) => nextValue + i * fieldPoints)
        nextValue += 1
        retVal
      }
      def hasNext = true
    })

    new PipelinedTestVectorSensor(fieldShape, tensorShape, nextValue, reset, parameters _)
  }

  /** The factory method used to create a pipelined vector sensor from its stored parameter string. */
  def restore(fieldType: FieldType, parameterString: String) = {
    require(fieldType.tensorShape.dimensions == 1, "Expecting VectorField Sensor, found " + fieldType)
    val parameters = parameterString.split(" ")
    require(parameters.length == 1, "Expecting 1 parameter, found " + parameters.length)
    val initState = parameters(0).toInt
    apply(fieldType.fieldShape, fieldType.tensorShape, initState)
  }
}
