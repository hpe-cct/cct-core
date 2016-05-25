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
import cogx.platform.types.{AbstractKernel, FieldType}
import cogx.runtime.ComputeGraphRestorerState

/** A factor object of sensor kernels that are restored from file-saved parameters.
  *
  * @author Dick Carter
  */
object SensorKernelRestorer {
  /** Create a SensorKernel instance through use of the provided ObjectRestorer
    *
    * @param restorer The restorer through which to read the new object state.
    * @return The created Sensor/VectorSensor/ColorSensor kernel based on the read information.
    */
  def restore(restorer: ObjectRestorer, resultType: FieldType): AbstractKernel = {
    val restorerState = restorer.asInstanceOf[ComputeGraphRestorerState]
    val restoredMajorVersion = restorer.readInt("majorVersion")
    val restoredMinorVersion = restorer.readInt("minorVersion")
    val sensorClassname = restorer.readString("sensorClassname")
    val restoreParameters = restorer.readString("restoreParameters")

    // We use reflection to invoke the factory method to create the sensor *field* (not kernel).
    // Then we grab the sensorOp and future restore parameters hooks from the field, after which it's
    // not used (since we're building the KernelCircuit here, not the SyntaxTree).
    val sensorClass = Class.forName(sensorClassname)
    val restoreMethod = sensorClass.getMethod("restore", resultType.getClass, "".getClass)

    // Now comes the really tricky part.  We are about to create a Field (part of a
    // new Hypercircuit[Operation]), but we don't want to destroy our ability to add the SensorKernel
    // to the restored KernelCiruit we are building.
    Hypercircuit.setCurrent(null)
    val sensorField =
      try {
        restoreMethod.invoke(null, resultType, restoreParameters) match {
          case unpiped: UnpipelinedSensor => unpiped
          case unpipedColor: UnpipelinedColorSensor => unpipedColor
          case unpipedVector: UnpipelinedVectorSensor => unpipedVector
          case piped: Sensor =>
            val recurrence = piped.recurrence match {
              case Some(f) => f
              case None => throw new RuntimeException("Expecting pipelined Sensor to have a recurrence.")
            }
            require(recurrence.opcode.isInstanceOf[PipelinedSensorOp],
              "Expecting PipelineSensorOp as input to pipelined sensor output register, found: " + recurrence.opcode)
            recurrence
          case vectorPiped: VectorSensor =>
            val recurrence = vectorPiped.recurrence match {
              case Some(f) => f
              case None => throw new RuntimeException("Expecting pipelined VectorSensor to have a recurrence.")
            }
            require(recurrence.opcode.isInstanceOf[PipelinedVectorSensorOp],
              "Expecting PipelineVectorSensorOp as input to pipelined vector sensor output register, found: " + recurrence.opcode)
            recurrence
          case colorPiped: ColorSensor =>
            val recurrence = colorPiped.recurrence match {
              case Some(f) => f
              case None => throw new RuntimeException("Expecting pipelined ColorSensor to have a recurrence.")
            }
            require(recurrence.opcode.isInstanceOf[PipelinedColorSensorOp],
              "Expecting PipelineColorSensorOp as input to pipelined color sensor output register, found: " + recurrence.opcode)
            recurrence
          case x => throw new RuntimeException("Expecting {Pipelined,Unpipelined}{Sensor,ColorSensor}, found: " + x)
        }
      } catch {
        case e: InvocationTargetException =>
          println(s"Could not invoke method $restoreMethod of class $sensorClass.")
          throw e
      }
    // Now that we're back making AbstractKernels, restore the KernelCircuit
    Hypercircuit.setCurrent(restorerState.restoredCircuit)
    // Now use what we've painstakingly recreated: the SensorOp!
    sensorField.opcode match {
      case sensorOp: SensorOp =>
        val kernel = new SensorKernel(resultType, sensorOp, sensorClassname, sensorField.asInstanceOf[RestoreHooks].restoreParameters _)
        checkVersion(kernel, kernel.majorVersion, kernel.minorVersion, restoredMajorVersion, restoredMinorVersion)
        kernel
      case vectorsensorOp: VectorSensorOp =>
        val kernel = new VectorSensorKernel(resultType, vectorsensorOp, sensorClassname, sensorField.asInstanceOf[RestoreHooks].restoreParameters _)
        checkVersion(kernel, kernel.majorVersion, kernel.minorVersion, restoredMajorVersion, restoredMinorVersion)
        kernel
      case colorsensorOp: ColorSensorOp =>
        val kernel = new ColorSensorKernel(resultType, colorsensorOp, sensorClassname, sensorField.asInstanceOf[RestoreHooks].restoreParameters _)
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
}
