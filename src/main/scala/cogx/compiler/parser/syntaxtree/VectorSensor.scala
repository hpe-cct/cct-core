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
import cogx.platform.types.ElementTypes.Float32
import cogx.compiler.parser.op._
import cogx.compiler.parser.semantics.SemanticError
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.Vector

/** Inputs to a Cog computation are called sensors.  This implements the pipelined version.
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
  * @param nextValue Optional iterator that generates the values of the next
  *                  field in row-major order; if None, there is no next field,
  *                  (useful for polling asynchronous inputs).
  * @param resetHook Unit function to be executed upon reset
  * @param desiredFramesPerSecond Optional parameter that specifies a desired
  *        frame rate for a synchronous sensor, throttling the sensor if it's
  *        too fast, gracefully degrading if it's not. If not specified, the
  *        sensor will run as fast as possible.
  *
  * @author Dick Carter
  */

class VectorSensor(fieldShape: Shape, tensorShape: Shape,
             val nextValue: () => Option[Iterator[Vector]],
             val resetHook: () => Unit,
             val desiredFramesPerSecond: Double)
  extends VectorField(VectorSensor.shapeToZeroInitOp(fieldShape, tensorShape), Array[Field](),
    new FieldType(fieldShape, tensorShape, Float32))
  with SemanticError
  with RestoreHooks
{
  val op = PipelinedVectorSensorOp(nextValue, resetHook, desiredFramesPerSecond)

  // What's going on here with the RestoreHooks is a bit tricky- the user has subclassed Sensor and provided
  // overridden restoreParameter() and possibly restoringClass() methods.  But the actual SensorKernel doing the
  // saving is attached to the `sensor` field below. Thus, we link the hooks of the `sensor` to the pipeline stage
  // "Sensor" field.
  val sensor = new VectorField(op, Array[Field](), new FieldType(fieldShape, tensorShape, Float32)) with RestoreHooks {
    override def restoreParameters: String = VectorSensor.this.restoreParameters
    override def restoringClass = VectorSensor.this.restoringClass
  }
  this <== sensor

  /** Create a vector sensor of the given Shape with no frames-per-second pacing. */
  def this(fieldShape: Shape, tensorShape: Shape,
            nextValue: () => Option[Iterator[Vector]],
            resetHook: () => Unit) =
    this(fieldShape, tensorShape, nextValue, resetHook, 0.0)

  /** Create a vector sensor of the given field and tensor Shapes with no reset hook. */
  def this(fieldShape: Shape, tensorShape: Shape,
            nextValue: () => Option[Iterator[Vector]],
            desiredFramesPerSecond: Double) =
    this(fieldShape, tensorShape, nextValue, () => {}, desiredFramesPerSecond)

  /** Create a vector sensor of the given field and tensor Shapes with no reset hook and no frames-per-second pacing. */
  def this(fieldShape: Shape, tensorShape: Shape,
            nextValue: () => Option[Iterator[Vector]]) =
    this(fieldShape, tensorShape, nextValue, () => {}, 0.0)
}

object VectorSensor {
  /** Create a dummy ConstantOp of the right dimension for the field. */
  def shapeToZeroInitOp(fieldShape: Shape, tensorShape: Shape) = {
    val v = new Vector(tensorShape.points)
    fieldShape.dimensions match {
      case 0 => ConstantVector0DOp(() => v)
      case 1 => ConstantVector1DOp((_) => v)
      case 2 => ConstantVector2DOp((_, _) => v)
      case 3 => ConstantVector3DOp((_, _, _) => v)
      case x => throw new RuntimeException("illegal number of dimensions")
    }
  }
}


