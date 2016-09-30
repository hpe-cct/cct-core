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

/** The user's Cog directory for gui state, profiler stats, etc.
  *
  * @author Tobin Gonzalez
  */
object CogDir {

  val CogSubDirectory = ".cogexmachina"

  private lazy val cogdir = {
    val userDirStr = java.lang.System.getProperty("user.home")
    val userDir = new java.io.File(userDirStr)
    val cogDir = new java.io.File(userDir, CogSubDirectory)
    if (cogDir.isDirectory() || cogDir.mkdir()) {
      Some(cogDir)
    } else {
      Console.err.println(s"Unable to find or create user '$CogSubDirectory' " +
        "directory.  Unable to save or restore state of the GUI, Profiler, etc.")
      None
    }
  }
  /** The subdirectory that Cog uses to save any state between invocations. */
  def apply(): Option[File] = cogdir
}