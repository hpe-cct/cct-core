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

import java.io.File

/** Contains weirdnesses of the NVidea OpenCL platform.a
  *
  * @author Greg Snider
  */
private[cogx]
object OpenCLNvidia {
   lazy val binaryCacheDirectory: String = {
     val os = System.getProperty("os.name", "unknown").toLowerCase
     val sep = File.separator
     if (os.contains("windows") && System.getenv("APPDATA") != null)
       System.getenv("APPDATA") + sep + "NVIDIA" + sep + "ComputeCache"
     else if (os.contains("linux"))
       System.getProperty("user.home") + sep + ".nv"
     else
       null
   }

  /** Check if linux users have set LD_PRELOAD, as required to avoid the jvm crashes
    * caused by the nvidia-compiler installing its own signal handlers in a non-
    * chained mode.
    */
  def checkEnvironmentVariablesForLD_PRELOAD() {
    /** libjsig.so file must either exist or be path relative (at which point we
      * assume that the LD_LIBRARY_PATH is set up to find it).
      */
    def hasValidLibJsig(LD_PRELOAD_string: String): Boolean = {
      val filenames = LD_PRELOAD_string.split(" ")
      for (filename <- filenames) {
        if (filename.contains("libjsig.so") && (filename(0) != '/' || new File(filename).exists))
          return true
      }
      false
    }

    val os = if (System.getProperty("os.name").toLowerCase.indexOf("win") >= 0) "windows" else "linux"
    if (os == "linux") {
      val preloadedLibs = System.getenv("LD_PRELOAD")
      if (preloadedLibs == null || !hasValidLibJsig(preloadedLibs))
        println("Warning: set LD_PRELOAD=/usr/lib/jvm/java/jre/lib/amd64/libjsig.so (or equiv) in your app launch environment to avoid crashes.")
    }
  }
}