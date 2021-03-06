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

package cogx.platform.opencl

import cogx.platform.types.KernelCodeTypes.CogOpenCL
import com.jogamp.opencl._
import cogx.platform.types.{VirtualFieldRegister, FieldType, AbstractKernel, Opcode}
import cogx.utilities.Stats

/** Base for OpenCL kernels (both device and CPU) that provides for the
  * binding of buffers to the inputs and outputs of those kernels, and for
  * instantiation of those kernels so they can executed.
  *
  * It is assumed that these kernel are to be executed according to a
  * dependency DAG. The protocol for executing them involves two post-order
  * traversals of the DAG (post-order since that means no kernel is visited
  * before all of the kernels feeding it have been visited):
  *
  * 1. Traverse the DAG post-order, calling `startComputation` on each kernel.
  * This will cause the kernel to get the output events from the kernels
  * feeding it (since it can't start until those events have triggered) and
  * enqueue a request on the command queue to execute the kernel when all of
  * the input events have occurred. The enqueueing generates an output event
  * which other kernels dependent on this can use to gate their execution.
  *
  * 2. Wait for all output events at the top of the DAG to complete.
  *
  * 3. Traverse the DAG again post-order, calling `resetTrigger`. This releases
  * all events that were generated during the computation, and checks them
  * all for errors. This sets things up for computation on the next cycle.
  *
  * @param opcode The operation performed by this kernel.
  * @param in The virtual field registers driving the inputs of this kernel.
  * @param fieldTypes The types of the results generated by this kernel.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class OpenCLAbstractKernel(opcode: Opcode,
                                    in: Array[VirtualFieldRegister],
                                    fieldTypes: Array[FieldType])
        extends AbstractKernel(opcode, in, fieldTypes)
{
  /** The language of the kernel.  This should be in a platform-independent
    * subclass of AbstractKernel named 'DeviceKernel' and there should be no
    * platform-dependent CPU kernels (e.g. OpenCLCpuKernel), but such is life.
    */
  val kernelCodeType = CogOpenCL

  /** Alternate constructor for the common case of a single-output kernel */
//  def this(opcode: Opcode, in: Array[VirtualFieldRegister], fieldType: FieldType) =
//    this(opcode, in, Array(fieldType))

  /** Input registers read by this kernel during execution. */
  protected lazy val inputRegisters =
    inputs.map(_.register.asInstanceOf[OpenCLFieldRegister]).toArray
  /** Output registers written by this kernel during execution. */
  protected lazy val outputRegisters =
    outputs.map(_.register.asInstanceOf[OpenCLFieldRegister]).toArray
  /** The runtime CLKernel used by the device. */
  protected var clKernel: CLKernel = null
  /** The OpenCLDevice for this kernel, providing the gateway to the platform, context and commandqueue. */
  private var device: OpenCLDevice = null
  /** The hardware device. */
  protected def clDevice: CLDevice = device.clDevice
  /** Command queue for this kernel. */
  protected def commandQueue: OpenCLParallelCommandQueue = device.commandQueue
  /** Platform for this kernel. */
  private def platform: OpenCLPlatform = device.platform
  /** Context for this kernel. */
  protected def clContext = platform.clContext

  def waitForEvents(events: Seq[CLEvent]) = device.waitForEvents(events)

  // Add the output VirtualFieldRegisters
  fieldTypes.foreach(new VirtualFieldRegister(_, this))

//  println("### OpenCLAbstractKernel created: " + this + " output(0) = " + outputs(0))
  /** Create a short string for debugging. */
  override def toString: String =
    "OpenCLAbstractKernel(" + opcode.toString + ")"  + " => " + fieldTypesString +
            (if (name == "") name else "'" + name + "'")
  
  /** The output FieldTypes as a string */
  def fieldTypesString = "(" + fieldTypes.mkString(" , ") + ")"
  
  /** The input FieldTypes as a string */
  def inputFieldTypesString = "(" + in.map(_.fieldType).mkString(" , ") + ")"

  /** An event list containing a single event that will be "triggered" (taking
    * on the state CL_COMPLETE or CL_ERROR) when the kernel finishes execution.
    * The event can be accessed as done.getEvent(0). It can also be waited on
    * by calling done.waitForEvents. Note that the event in `done` can change
    * each cycle, so it should be accessed by done.getEvent(0) whenever you
    * need it for launching a kernel.
    */
  protected var outputTrigger: CLEventList = null

  /** Get the event list containing a single event that will be triggered
    * when kernel execution is complete.
    *
    * This event can be waited on by calling CLEventList.waitForEvents.
    * Note that the events in the event list can be, and usually are,
    * different on every cycle, so this must be looked up each time before
    * launching a kernel.
    *
    * @return An OpenCL event that will be triggered (CL_COMPLETE) when the
    *         kernel successfully finishes execution; if execution fails, this
    *         will take on the state CL_ERROR.
    */
  def done: CLEventList = outputTrigger

  /** Accumulator of microsecond kernel execution times, if profiled */
  lazy val stats = new Stats(" us")

  /** Called by OpenCLDevice. Instantiates the kernel for execution on a device
    * (if it's a GPU kernel) or on the CPU.
    */
  def instantiate(_device: OpenCLDevice, kernel: CLKernel = null)
  {
    require(clKernel == null)
    require(device == null)
    clKernel = kernel
    device = _device
  }

  /** Called by OpenCLDevice. Release all OpenCL resources for this kernel.
   *
    * This only removes pointers to OpenCL resources so the garbage collector
    * can remove them later. The actual releasing of the resources is done
    * by OpenCLPlatform.
    */
  private[opencl] def release() {
    if (clKernel != null)
      clKernel.release()
    clKernel = null
    device = null
    if (outputTrigger != null) {
      if (outputTrigger.size == 1) {
        val event = outputTrigger.getEvent(0)
        if (event != null)
          if (!event.isReleased)
            event.release()
      }
    }
  }

  /** Reset / initialize the kernel state.  */
  def reset(): Unit

  // Note: in a prior approach, all kernels would clock their output registers
  // during phase0Clock().  Now, this responsibility has shifted to the
  // RecurrentFieldKernels, so nothing is done here by default.

  /** Clock the output registers on the kernel. */
  private[cogx] def phase0Clock() {
  }

  /** Asynchronously start execution of a kernel when all inputs are "done"
    * (events generated by them have triggered).
    */
  private[cogx] def phase1Clock(): Unit

  /** Check the result of the computation, making sure it completed correctly,
    * and initialize resources for next computation cycle. Must be called after
    * the done event is complete.
    */
  private[cogx] def phase2Clock(): Unit

  /** Get the input register for input `inputIndex`. */
  final def inputRegister(inputIndex: Int): OpenCLFieldRegister =
    inputRegisters(inputIndex)

  /** Get the output register for output `outputIndex`. */
  final def outputRegister(outputIndex: Int): OpenCLFieldRegister =
    outputRegisters(outputIndex)

  /** Print out a verbose description of the kernel for debugging. */
  def print() {
    println("OpenCLAbstractKernel " + id + ":")
    println("    class: " + getClass.getSimpleName)
    println("    opcode: " + opcode)
    println("    fieldType: " + fieldTypes.toString)
    println("    inputCount: " + inputs.length)
    println("    outputCount: " + outputs.length)
    for (i <- 0 until in.length) {
      println("    input " + i + ": kernel " + inputs(i).source.id)
    }
    println("---------------- end kernel " + id)
  }
}