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

import java.nio.file.Files

import cogx.parameters.Cog
import cogx.platform.types.{CheckpointerType, Hdf5CheckpointerType, JavaCheckpointerType}
import cogx.runtime.checkpoint.hdf5.{Hdf5ObjectRestorer, Hdf5ObjectSaver}
import cogx.runtime.checkpoint.javaserialization.{JavaObjectRestorer, JavaObjectSaver}
import ncsa.hdf.hdf5lib.exceptions.HDF5FileInterfaceException

import scala.collection.mutable

/** A cache of profiled KernelCircuit execution times maintained for a given device type on the system.
  *
  * This cacheing approach is thread-safe for multiple ComputeGraphs compiled by the same App (so
  * under the same JVM).  However, multiple Apps running simultaneously may create a corrupt cache
  * file if the cache file is being updated with new ProfileSamples.  Java nio File locking may
  * solve this issue.
  *
  * @param deviceName The name of the cache file based on the device type.
  *
  * @author Dick Carter
  */
class ProfilerCache(deviceName: String) {
  private val checkpointerType: CheckpointerType =
    if (Hdf5CheckpointerType.isAvailable)
      Hdf5CheckpointerType
    else
      JavaCheckpointerType
  private val majorVersion = 1
  private val minorVersion = 0
  private val cacheFile = Profiler.profilerDir match {
    case Some(dir) =>
      Some(new java.io.File(dir.getPath, checkpointerType.addSuffixIfAbsent(deviceName)))
    case None => None
  }
  private val cache = mutable.HashMap[String, ProfileSample]()
  cacheFile match {
    case Some(file) =>
      if (file.exists() && !Cog.deleteProfilerCache) {
        /** A restorer object for the chosen serialization approach. */
        // If the file has been trashed, the following call returns
        // an ncsa.hdf.hdf5lib.exceptions.HDF5FileInterfaceException
        try {
          val restorer = checkpointerType match {
            case Hdf5CheckpointerType => new Hdf5ObjectRestorer(file.getPath)
            case JavaCheckpointerType => new JavaObjectRestorer(file.getPath)
          }
          try {
            val major = restorer.readInt("majorVersion")
            val minor = restorer.readInt("minorVersion")
            require(majorVersion == 1 && minorVersion == 0,
              s"Incompatible profiler versioning: file is $major.$minor, reader is $majorVersion.$minorVersion .")
            restorer.readRestorableArray("profileSamples", ProfileSample).foreach( x => {
              val sample = x.asInstanceOf[ProfileSample]
              cache(sample.kernelCircuitHash) = sample
            })
          } finally
            restorer.close()
        }
        catch {
          case e: HDF5FileInterfaceException =>
            println("Warning: found and now deleting corrupt profile file: " + file.toPath)
            Files.deleteIfExists(file.toPath)
        }
      }
    case None =>
  }

  /** Check for prior profiling data for a circuit in the cache based on the circuit's hash. */
  def get(circuitHashCode: String): Option[ProfileSample] = {
    cache.get(circuitHashCode)
  }

  /** Record profiling data for a circuit in the cache (circuit hash is part of the ProfileSample). */
  def put(sample: ProfileSample): Unit = {
    // Add circuit to online database.
    cache(sample.kernelCircuitHash) = sample
    // Write circuit to file (this should probably not be done on each `put`, but rather after a `sync`
    cacheFile match {
      case Some(file) =>
        /** A saver object for the chosen serialization approach. */
        val saver = checkpointerType match {
          case Hdf5CheckpointerType => new Hdf5ObjectSaver(file.getPath)
          case JavaCheckpointerType => new JavaObjectSaver(file.getPath)
        }
        try {
          saver.writeInt("majorVersion", majorVersion)
          saver.writeInt("minorVersion", minorVersion)
          saver.writeObjectArray("profileSamples", cache.values.toArray)
        } finally
          saver.close()
      case None =>
    }
  }
}

