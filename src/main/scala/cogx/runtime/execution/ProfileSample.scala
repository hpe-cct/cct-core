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
package cogx.runtime.execution

import cogx.platform.checkpoint.{ObjectRestorer, ObjectSaver, RestoreFactory, Saveable}

/** Simple class to hold results of a single KernelCircuit-profiling experiment.
  *
  * @param kernelCircuitHash hashcode for all the kernel codes in the timed circuit.
  * @param creationTimeMsec when the profiling experiment was run, in milliseconds since the Epoch.
  * @param warmupSteps The number of steps before clearing the runtime statistics.
  * @param runSteps The number of steps to execute while accumulating runtime statistics.
  * @param avgStepTimeUsec The average execution time of the profiled KernelCircuit per step.
  * @param fromCache Was this ProfileSample read from a file, or eas it created by a recent profiling experiment?
  *
  * @author Dick Carter
  */
class ProfileSample private[ProfileSample] (val kernelCircuitHash: String, val creationTimeMsec: Long,
                                            val warmupSteps: Int, val runSteps: Int, val avgStepTimeUsec: Double, val fromCache: Boolean)
  extends Saveable {

  /** Save this FieldType instance using the facilities of the ObjectSaver */
  def save(saver: ObjectSaver) {
    saver.writeString("kernelCircuitHash", kernelCircuitHash)
    saver.writeLong("creationTimeMsec", creationTimeMsec)
    saver.writeInt("warmupSteps", warmupSteps)
    saver.writeInt("runSteps", runSteps)
    saver.writeDouble("avgStepTimeUsec", avgStepTimeUsec)
    // 'fromCache' flag never saved, as it's always restored as 'true'.
  }

  // If you add/remove constructor parameters, you should alter the toString() implementation. */
  /** A string description of the instance in the "case class" style. */
  override def toString = this.getClass.getName +
    (kernelCircuitHash, creationTimeMsec, warmupSteps, runSteps, avgStepTimeUsec)
}

object ProfileSample extends RestoreFactory {
  def apply(kernelCircuitHash: String, creationTimeMsec: Long, warmupSteps: Int, runSteps: Int, avgStepTimeUsec: Double, fromCache: Boolean) =
    new ProfileSample(kernelCircuitHash, creationTimeMsec, warmupSteps, runSteps, avgStepTimeUsec, fromCache)

  /** Create a ProfileSample instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created ProfileSample based on the read information.
    */
  def restore(restorer: ObjectRestorer): ProfileSample = {
    val kernelCircuitHash = restorer.readString("kernelCircuitHash")
    val creationTimeMsec = restorer.readLong("creationTimeMsec")
    val warmupSteps = restorer.readInt("warmupSteps")
    val runSteps = restorer.readInt("runSteps")
    val avgStepTimeUsec = restorer.readDouble("avgStepTimeUsec")
    apply(kernelCircuitHash, creationTimeMsec, warmupSteps, runSteps, avgStepTimeUsec, true)
  }
}
