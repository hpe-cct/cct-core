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

import cogx.platform.checkpoint.{Saveable, RestoreFactory, ObjectSaver, ObjectRestorer}

/** Represents the work group parameters for launching a kernel on a GPU.
  * The "dimensions" must be in {1, 2, 3}, the other parameters are defined
  * by the OpenCL specification. Note that Cog uses a row-major coordinate
  * system as is customary in C-family languages, but OpenCL reverses this
  * for some reason. This means that in a 3D work group, layers is dimension
  * 2, rows is dimension 1, and columns is dimension 0.
  *
  * @param dimensions Number of dimensions in the work group
  *
  * @author Greg Snider
  */
private[cogx]
class WorkGroupParameters(val dimensions: Int) extends Saveable {
  require(dimensions >= 0 && dimensions <= 3)
  var globalLayers = 0L
  var globalRows = 0L
  var globalColumns = 0L
  var localLayers = 0L
  var localRows = 0L
  var localColumns = 0L

  /** The number of work items in a work group */
  def localWorkSize = (localLayers * localRows * localColumns).toInt

  /** The number of work groups */
  def workGroups = ((globalLayers/localLayers) *
                   (globalRows/localRows) *
                   (globalColumns/localColumns)).toInt

  /** Print out work group parameters for debugging. */
  def print() {
    println("WorkGroupParameters(dimensions = " + dimensions + ")")
    if (dimensions == 3)
      println("  globalLayers = " + globalLayers)
    if (dimensions >= 2)
      println("  globalRows = " + globalRows)
    println("  globalColumns = " + globalColumns)

    if (dimensions == 3)
      println("  localLayers = " + localLayers)
    if (dimensions >= 2)
      println("  localRows = " + localRows)
    println("  localColumns = " + localColumns)
  }

  /** Test "this" and "other" for deep equality, allowing "==" to work.
    *
    * @return Tr
    */
  final override def equals(other: Any): Boolean = {
    other match {
      case that: WorkGroupParameters =>
        if (that eq this)
          true
        else if ((that canEqual this) &&
                (that.dimensions == this.dimensions) &&
                (that.globalLayers == this.globalLayers) &&
                (that.globalRows == this.globalRows) &&
                (that.globalColumns == this.globalColumns) &&
                (that.localLayers == this.localLayers) &&
                (that.localRows == this.localRows) &&
                (that.localColumns == this.localColumns)) {
           true
        }
        else
          false
      case _ => false
    }
  }

  /** Helper for equals. Default canEqual requires that the two objects are the
    * same subclass of AbstractKernel. Subclasses may override this if a more
    * restrictive equality is required.
    */
  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[WorkGroupParameters]
  }

  /** Required because of overriding equals. */
  override val hashCode: Int = {
    val code: Long = dimensions + 4 * (
            globalLayers + 3 * (
            globalRows + 5 * (
            globalColumns + 7 * (
            localLayers + 11 * (
            localRows + 13 * localColumns)))))
    (code % ((1L << 31) - 1)).toInt
  }

  /** Save this WorkGroupParameter instance using the facilities of the ObjectSaver */
  def save(saver: ObjectSaver) {
    saver.writeInt("dimensions", dimensions)
    saver.writeLong("globalLayers", globalLayers)
    saver.writeLong("globalRows", globalRows)
    saver.writeLong("globalColumns", globalColumns)
    saver.writeLong("localLayers", localLayers)
    saver.writeLong("localRows", localRows)
    saver.writeLong("localColumns", localColumns)
  }
}

/** Factory object for creating WorkGroupParameters instances from their stored representations. */
object WorkGroupParameters extends RestoreFactory {
  /** Create a WorkGroupParameters instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created WorkGroupParameters based on the read information.
    */
  def restore(restorer: ObjectRestorer): WorkGroupParameters = {
    val dimensions = restorer.readInt("dimensions")
    val retVal = new WorkGroupParameters(dimensions)
    retVal.globalLayers = restorer.readLong("globalLayers")
    retVal.globalRows = restorer.readLong("globalRows")
    retVal.globalColumns = restorer.readLong("globalColumns")
    retVal.localLayers = restorer.readLong("localLayers")
    retVal.localRows = restorer.readLong("localRows")
    retVal.localColumns = restorer.readLong("localColumns")
    retVal
  }
}
