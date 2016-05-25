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

package cogx.compiler.codegenerator.opencl.cpukernels

import cogx.platform.checkpoint.{ObjectSaver, Saveable}
import cogx.platform.types.KernelTypes.SensorKernelType
import cogx.platform.types.{VirtualFieldRegister, AbstractKernel, FieldType}
import cogx.platform.opencl.{OpenCLCpuSingleOutputKernel, OpenCLFieldRegister}
import cogx.compiler.parser.op.VectorSensorOp
import cogx.platform.cpumemory.VectorFieldMemory
import cogx.cogmath.algebra.real.Vector

/** Kernel that copies user-supplied data to a buffer for use by consumers
  * on the GPU.
  *
  * @param fieldType The type of the scalar field produced by this input.
  * @param op The sensor opcode, with its nextInput, resetHook, desiredRate
  *           and pipeline parameters.
  * @param sensorClassname The actual subclass classname of the sensor, useful for save/restore.
  * @param restoreParameters A callback function for the user to add any sensor state needed for a sensor restore.
  *
  * @author Dick Carter
  */
private[cogx]
class VectorSensorKernel(fieldType: FieldType, op: VectorSensorOp, sensorClassname: String, restoreParameters: () => String)
  extends OpenCLCpuSingleOutputKernel(op, Array(), fieldType)
  with Saveable
{
  /** Optional iterator that generates the values of the next field in row-major order;
    * if None, there is no next field, (useful for polling asynchronous inputs).
    */
  val nextInput = op.nextInput
  /** Unit function to be executed upon reset */
  val resetHook = op.resetHook
  /** Optional parameter that specifies a desired frame rate for a synchronous sensor,
    * throttling the sensor if it's too fast, gracefully degrading if it's not. If not
    * specified, the sensor will run as fast as possible.
    */
  val desiredRate = op.desiredRate
  /** Does this sensor have an embedded pipeline stage that permits CPU/GPU parallelism. */
  val pipelined = op.pipelined
  /** The type of the kernel, either DeviceKernel, or one of a number of CPU kernel types. */
  override val kernelType = SensorKernelType

  /** The version of the kernel. */
  def majorVersion = 1

  /** The version of the kernel. */
  def minorVersion = 0

  final val vectorLen = fieldType.tensorShape(0)
  final val fieldDims = fieldType.fieldShape.dimensions

  private val synchronizer: RateSynchronizer =
    if (desiredRate != 0.0)
      new RateSynchronizer(desiredRate)
    else
      null

  private var newDataProvided = false

  override def reset() {
    resetHook()
    if (pipelined)
      compute(Array[OpenCLFieldRegister](), outputRegister(0), fromReset = true)
  }

  /** Grab the next input field from the user and write to the GPU. */
  def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister): Unit = {
    compute(in, out, fromReset = false)
  }

  /** Grab the next input field from the user and write to the GPU. */
  private def compute(in: Array[OpenCLFieldRegister], out: OpenCLFieldRegister, fromReset: Boolean) {
    if (synchronizer != null) {
      synchronizer.startDataCollection()
    }
    val iterator = nextInput()
    iterator match {
      case Some(iter) =>
        // Fill the CPU part of the buffer
        val cpuMemory = out.master.cpuMemory.asInstanceOf[VectorFieldMemory]
        cpuMemory.write(iter)
        // Copy the CPU part out to the GPU.
        out.master.write
        newDataProvided = true
      case None =>
        if (fromReset || !pipelined) {
          // This kernel must provide the initial reset state (the RecurrentFieldKernel usually does this, but
          // has been told not to when it shares a field register with this type of kernel in the pipelined mode).
          // If this kernel is in unpipelined mode, then we shouldn't expect to see None here (i.e. it's an internal
          // compiler error).  We do something sensible (produce 0's) rather than throwing an exception.

          // A simple iterator that keeps sourcing 0's for a default init.
          val it = new Iterator[Vector] {
            val v = new Vector(vectorLen)
            def next() = v
            def hasNext() = true
          }
          // Fill the CPU part of the buffer with 0's
          val cpuMemory = out.master.cpuMemory.asInstanceOf[VectorFieldMemory]
          cpuMemory.write(it)
          // Copy the CPU part out to the GPU.
          out.master.write
          newDataProvided = true
        }
        else {
          // No data this cycle, do nothing.
          newDataProvided = false
        }
    }
    if (synchronizer != null)
      synchronizer.endDataCollection()
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits. */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel =
    new VectorSensorKernel(fieldType, op, sensorClassname, restoreParameters)

  /** Save this instance using the facilities of the ObjectSaver */
  override def save(saver: ObjectSaver): Unit = {
    super.save(saver)
    saver.writeInt("majorVersion", majorVersion)
    saver.writeInt("minorVersion", minorVersion)
    saver.writeString("sensorClassname", sensorClassname)
    saver.writeString("restoreParameters", restoreParameters())
  }
}