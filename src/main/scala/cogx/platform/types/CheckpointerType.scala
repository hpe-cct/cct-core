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

package cogx.platform.types

import cogx.runtime.checkpoint.hdf5.Hdf5Common
import hdfloader.Hdf5NativesLoader


/** Classes that describe what checkpointing technologies are available
  *
  * @author Dick Carter
  */

private [cogx] sealed abstract class CheckpointerType(val fileSuffix: String) {
   /** Helper function to add the format-indicating suffix to the file name if necessary. */
  def addSuffixIfAbsent(filename: String) =
    if(filename.endsWith(fileSuffix)) filename else (filename + fileSuffix)
  /** Check that the checkpointer can be used- class jars or native libs might be missing. */
  def isAvailable: Boolean
}

/** The HDF5 checkpointer. */
private [cogx] case object Hdf5CheckpointerType extends CheckpointerType(".h5") {
  def isAvailable =
    try {
      new Hdf5Common() {
        val fileId = 0
      }.loadNativeLibrary()
      true
    }
    catch {
      case e: java.lang.UnsatisfiedLinkError => false
      case e: java.lang.NoClassDefFoundError => false
    }
}

/** The checkpointer that uses java serialization. */
private [cogx] case object JavaCheckpointerType extends CheckpointerType(".ser") {
  def isAvailable = true
}
