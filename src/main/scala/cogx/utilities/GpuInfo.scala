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

package cogx.utilities

import java.io.File
import collection.mutable.HashMap
import io.Source
import java.util.zip.{ZipEntry, ZipInputStream}

/** Amazingly, the global memory bandwidth is not part of the GPU parameters
  * one can query through the OpenCL interface.  We maintain a database
  * of values in a directory of files (for now).  Better might be to
  * run a characterization test for any GPUs seen, and store that
  * in a system- or user-specific place.
  *
  * User: Dick Carter
  * Date: 5/3/12
  */

@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private object GpuInfo {

  // Reviewing how NVidia calculates its GPU bandwidths (from memory
  // clock frequency and memory bus width), I have concluded that the
  // bandwidth specs use GB/sec to mean 1e9 bytes/sec, not 2^30 bytes/sec.
  private val UnknownGpuBWGBsec = 100.0
  private val gpuInfoDir = new File("./releaseResources/gpus")
  private val src = getClass.getProtectionDomain.getCodeSource

  // When needed, create the associative map between the gpu name
  // and its global memory bandwidth
  private lazy val globalMemBWMap = new HashMap[String, Double]() {
    val globalMemBWRE = "globalMemoryBandwidth_GB/sec=(.*)".r

    // First, load map entries from the hosting jar file, if it exists.
    if (src != null) {
      val zip = new ZipInputStream(src.getLocation.openStream)
      var ze = null: ZipEntry

      ze = zip.getNextEntry
      while (ze != null) {
        val name = ze.getName

        if (name.startsWith("resources/gpus/") && !ze.isDirectory) {
          val stream = getClass.getClassLoader.getResourceAsStream(name)

          if (stream != null) {
            for (line <- Source.fromInputStream(stream).getLines()) {
              line match {
                case globalMemBWRE(rest) =>
                  put(name.split("/").reverse(0), rest.toString.toDouble)
                case _ =>
              }
            }
          }
        }

        ze = zip.getNextEntry
      }
    }

    // Map entries sitting outside the jar override entries inside the jar.
    if (gpuInfoDir.exists()) {
      gpuInfoDir.listFiles().foreach( file => {
        if (file.isFile) {
          for (line <- Source.fromFile(file).getLines())
            line match {
              case globalMemBWRE(rest) =>
                put(file.getName, rest.toString.toDouble)
              case _ =>
            }
        }
      })
    }
  }
  
  def globalMemBWGBsec(gpuName: String): Double = {
    for ((gpu, bandwidth) <- globalMemBWMap) {
      if (gpuName.toLowerCase.contains(gpu.toLowerCase))
        return bandwidth
    }
    println("GPU " + gpuName + " is not in " + gpuInfoDir + " database, " +
            UnknownGpuBWGBsec + " GB/sec global mem BW assumed.")
    globalMemBWMap.put(gpuName, UnknownGpuBWGBsec)  // Silence further lookups
    return UnknownGpuBWGBsec
  }

  // For Placer cost function evaluation speed, the time/byte is most useful
  def globalMemByteXferTimeSec(gpuName: String): Double = 1.0/(globalMemBWGBsec(gpuName)*1e9)
}
