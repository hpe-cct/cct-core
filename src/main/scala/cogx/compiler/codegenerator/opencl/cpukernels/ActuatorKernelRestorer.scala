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

import java.lang.reflect.InvocationTargetException

import cogx.cogmath.hypercircuit.Hypercircuit
import cogx.compiler.parser.op._
import cogx.compiler.parser.syntaxtree._
import cogx.platform.checkpoint.ObjectRestorer
import cogx.platform.types.{VirtualFieldRegister, AbstractKernel}
import cogx.runtime.ComputeGraphRestorerState

/** A factor object of sensor kernels that are restored from file-saved parameters.
  *
  * @author Dick Carter
  */
object ActuatorKernelRestorer {
  /** Create a SensorKernel instance through use of the provided ObjectRestorer
    *
    * @param restorer The restorer through which to read the new object state.
    * @return The created Sensor/VectorSensor/ColorSensor kernel based on the read information.
    */
  def restore(restorer: ObjectRestorer, sourceRegister: VirtualFieldRegister): AbstractKernel = {
    val restorerState = restorer.asInstanceOf[ComputeGraphRestorerState]
    val restoredMajorVersion = restorer.readInt("majorVersion")
    val restoredMinorVersion = restorer.readInt("minorVersion")
    val actuatorClassname = restorer.readString("actuatorClassname")
    val restoreParameters = restorer.readString("restoreParameters")

    // We use reflection to invoke the factory method to create the sensor *field* (not kernel).
    // Then we grab the sensorOp and future restore parameters hooks from the field, after which it's
    // not used (since we're building the KernelCircuit here, not the SyntaxTree).
    val actuatorClass = Class.forName(actuatorClassname)
    val restoreMethod = actuatorClass.getMethod("restore", fieldClass, "".getClass)

    // Now comes the really tricky part.  We are about to create a Field (part of a
    // new Hypercircuit[Operation]), but we don't want to destroy our ability to add the UnpipelinedActuatorKernel
    // to the restored KernelCiruit we are building.
    Hypercircuit.setCurrent(restorerState.restoredSyntaxTree)

    // Create a Field of the right fieldtype to serve as a stand-in while we create the Actuator Operation
    val dummyInput = Field(sourceRegister.fieldType)
    dummyInput.setVirtualFieldRegister(sourceRegister)
    dummyInput.probe(sourceRegister.name)

    // Notes on restoring pipelined actuators:  the following invocation will create yet another field.
    // We can throw both these fields away- we just want to grab the unpipelinedActuatorOp with its update hook.
    // The sourceRegister is the one driven by the recurrence kernel for the actuator-embedded pipeline stage.

    val actuatorOperation =
      try {
        restoreMethod.invoke(null, dummyInput.asInstanceOf[Field], restoreParameters) match {
          case unpiped: UnpipelinedActuator => unpiped
          case unpipedColor: UnpipelinedColorActuator => unpipedColor
          case unpipedVector: UnpipelinedVectorActuator => unpipedVector
          case piped: Actuator =>
            piped.unpipedActuator
          case vectorPiped: VectorActuator =>
            vectorPiped.unpipedVectorActuator
          case colorPiped: ColorActuator =>
            colorPiped.unpipedColorActuator
          case x => throw new RuntimeException("Expecting {Pipelined,Unpipelined}{Sensor,ColorSensor}, found: " + x)
        }
      } catch {
        case e: InvocationTargetException =>
          println(s"Could not invoke method $restoreMethod of class $actuatorClass (or method threw exception).")
          throw e
      }
    // Now that we're back making AbstractKernels, restore the KernelCircuit
    Hypercircuit.setCurrent(restorerState.restoredCircuit)
    // Now use what we've painstakingly recreated: the SensorOp!
    actuatorOperation.opcode match {
      case unpipelinedActuatorOp: UnpipelinedActuatorOp =>
        val kernel = new UnpipelinedActuatorKernel(sourceRegister, unpipelinedActuatorOp, actuatorClassname,
          actuatorOperation.asInstanceOf[RestoreHooks].restoreParameters _)
        checkVersion(kernel, kernel.majorVersion, kernel.minorVersion, restoredMajorVersion, restoredMinorVersion)
        kernel
      case unpipelinedVectorActuatorOp: UnpipelinedVectorActuatorOp =>
        val kernel = new UnpipelinedVectorActuatorKernel(sourceRegister, unpipelinedVectorActuatorOp, actuatorClassname,
          actuatorOperation.asInstanceOf[RestoreHooks].restoreParameters _)
        checkVersion(kernel, kernel.majorVersion, kernel.minorVersion, restoredMajorVersion, restoredMinorVersion)
        kernel
      case unpipelinedColorActuatorOp: UnpipelinedColorActuatorOp =>
        val kernel = new UnpipelinedColorActuatorKernel(sourceRegister, unpipelinedColorActuatorOp, actuatorClassname,
          actuatorOperation.asInstanceOf[RestoreHooks].restoreParameters _)
        checkVersion(kernel, kernel.majorVersion, kernel.minorVersion, restoredMajorVersion, restoredMinorVersion)
        kernel
      case x =>
        throw new RuntimeException("Internal compiler error: unexpected sensor opcode: " + x)
    }
  }
  /** Check the majorVersion/minorVersion numbers of the implementation against the assumed version from the restored file. */
  private def checkVersion(kernel: AbstractKernel, majorVersion: Int, minorVersion: Int, restoredMajorVersion: Int, restoredMinorVersion: Int): Unit = {
    require(majorVersion > restoredMajorVersion ||
            majorVersion == restoredMajorVersion && minorVersion >= restoredMinorVersion,
            "Incompatible (majorVersion, minorVersion) for kernel " + kernel.toString() +
            "Library implementation is at (" + majorVersion + "," + minorVersion + "), saved model is at (" +
                                               restoredMajorVersion + "," + restoredMinorVersion + ")")
  }

  /** Returns getClass for a Field instance without creating one. */
  private def fieldClass = {
    val fieldObjectClassname = Field.getClass.getName
    val fieldClassname = if (fieldObjectClassname.endsWith("$")) fieldObjectClassname.dropRight(1) else fieldObjectClassname
    Class.forName(fieldClassname)
  }
}
