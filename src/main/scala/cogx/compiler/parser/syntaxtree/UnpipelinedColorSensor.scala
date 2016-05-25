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

package cogx.compiler.parser.syntaxtree

import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Uint8Pixel
import cogx.compiler.parser.op.UnpipelinedColorSensorOp
import cogx.compiler.parser.semantics.SemanticError
import cogx.cogmath.geometry.Shape

/** Inputs to a Cog computation are called sensors.  This implements the unpipelined version of ColorSensors.
  *
  * Sensors can be either pipelined or unpipelined.  Pipelined sensors use the CPU to produce
  * an input to the GPU while the GPU is working on the previous input.  Thus, there's
  * effectively a pipeline stage between the CPU and the GPU and both do their work in parallel.
  * Unpipelined sensors have no such pipeline stage, so the CPU must provide its input first
  * before the GPU processes that input further, i.e. the CPU and GPU do their work in series.
  *
  * When an unpipelined sensor's nextValue method is called, it must always return an iterator
  * over the next input's data.  However, a pipelined sensor has the option of returning None,
  * if no new input is available.  In that case the pipeline register that the sensor is feeding
  * is not clocked and the same data is presented to the GPU.  This can be used to decouple a slow
  * sensor from a fast-running simulation, making the sensor appear effectively 'asynchronous'.
  *
  * Both sensor types can accept a resetHook method, which can be used for example to go back to
  * frame-0 of a movie that's played out from a file, or to start over from the first image of a
  * training set.  If a sensor supplies no nextValue iterator upon reset, an all-0 field will be supplied.
  *
  * Finally, sensors can be throttled back to a specified simulation rate by the '`desiredFramesPerSecond`
  * parameter.  This ensures that a movie is played out at the appropriate speed, for example.
  *
  * Both types of sensors supply their next input by `nextValue` function which (optionally) returns an
  * iterator of the values of the new input in row-major order.
  *
  * NOTE: if the user wishes to optimize input using, for example, multiple threads or double-buffering,
  * that must be done in the implementation of the function `nextValue`.
  *
  * @param fieldShape The shape of the scalar field produced by this input.
  * @param op The SensorOp with important parameters, namely:
  *
  *  nextValue:      Iterator that generates the values of the next
  *                  field in row-major order; Note that unpipelined sensors must
  *                  provide an iterator always (unlike pipelined sensors with can
  *                  return None).
  *
  * resetHook:       Optional Unit function to be executed upon reset.
  *
  * desiredFramesPerSecond: Optional parameter that specifies a desired
  *        frame rate for a synchronous sensor, throttling the sensor if it's
  *        too fast, gracefully degrading if it's not. If not specified, the
  *        sensor will run as fast as possible.
  *
  * @author Dick Carter
  */

class UnpipelinedColorSensor private[cogx] (fieldShape: Shape, op: UnpipelinedColorSensorOp)
  extends ColorField(op, Array[Field](), new FieldType(fieldShape, Shape(3), Uint8Pixel))
  with SemanticError
  with RestoreHooks
{
  /** Create a sensor of the given Shape. */
  def this(fieldShape: Shape, nextValue: () => Iterator[Byte],
           resetHook: () => Unit,
           desiredFramesPerSecond: Double) =
    this(fieldShape, UnpipelinedColorSensorOp(nextValue, resetHook, desiredFramesPerSecond))

  /** Create a sensor of the given Shape with no frames-per-second pacing. */
  def this(fieldShape: Shape, nextValue: () => Iterator[Byte],
           resetHook: () => Unit) =
    this(fieldShape, UnpipelinedColorSensorOp(nextValue, resetHook, 0.0))

  /** Create a sensor of the given Shape with no reset hook and no frames-per-second pacing. */
  def this(fieldShape: Shape, nextValue: () => Iterator[Byte]) =
    this(fieldShape, UnpipelinedColorSensorOp(nextValue, () => {}, 0.0))

  /** Create a 2D sensor. */
  def this(rows: Int, columns: Int, nextValue: () => Iterator[Byte], resetHook: () => Unit = () => {},
           desiredFramesPerSecond: Double = 0.0) =
    this(Shape(rows, columns), nextValue, resetHook, desiredFramesPerSecond)

  /** Feedback to inputs is not allowed. */
  override def <==(that: Field) {
    feedbackError("feedback to color sensors is not allowed")
  }
}

