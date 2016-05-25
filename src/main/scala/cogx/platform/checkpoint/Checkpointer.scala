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

package cogx.platform.checkpoint

import cogx.platform.types.{JavaCheckpointerType, Hdf5CheckpointerType, CheckpointerType}
import cogx.runtime.checkpoint.hdf5.{Hdf5ObjectRestorer, Hdf5ObjectSaver}
import cogx.runtime.checkpoint.javaserialization.{JavaObjectRestorer, JavaObjectSaver}

/** A factory method for both 'ObjectSavers' and 'ObjectRestorers'
  *
  * Note that the ComputeGraph object reimplements this in order to mix in some global
  * state needed for the somewhat tricky KernelCircuit save/restore.
  *
  * @author Dick Carter.
  */

object Checkpointer {

  /** Returns an ObjectSaver instance ready to write the specified file. */
  def getSaver(filename: String, checkpointerType: CheckpointerType): ObjectSaver = {
    checkpointerType match {
      case Hdf5CheckpointerType => new Hdf5ObjectSaver(filename)
      case JavaCheckpointerType => new JavaObjectSaver(filename)
    }
  }

  /** Returns an ObjectSaver instance ready to write the specified file. */
  def getRestorer(filename: String, checkpointerType: CheckpointerType): ObjectRestorer = {
    checkpointerType match {
      case Hdf5CheckpointerType => new Hdf5ObjectRestorer(filename)
      case JavaCheckpointerType => new JavaObjectRestorer(filename)
    }
  }
}