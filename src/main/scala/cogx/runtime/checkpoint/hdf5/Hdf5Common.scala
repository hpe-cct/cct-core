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

package cogx.runtime.checkpoint.hdf5

import ncsa.hdf.hdf5lib.H5._
import ncsa.hdf.hdf5lib.HDF5Constants._
import hdfloader.Hdf5NativesLoader

import scala.collection.mutable

/** Common functionality used by Hdf5 object savers and restorers
  *
  * @author Dick Carter
  */
trait Hdf5Common {
  /** The fileId of the opened (ObjectRestorer) or created (ObjectSaver) file */
  val fileId: Int

  // The creating of groups and datasets is done not using absolute object path names
  // (such as /ComputeGraph/fields/0), but rather using relative addressing to the currently
  // open group.  This stack of currently open groups is maintained and initialized here:

  /** The sequence of opened hdf5 groups, maintained as a stack. */
  lazy val groupIdStack = mutable.Stack[Int](openGroupAbsolutePath("/"))

  // Load the native libraries, silencing the INFO and DEBUG messages that the hdf5 loader emits
  // This tweeking with log levels could alternatively be placed in the hdf5-loader project, along with the
  // library dependency on ch.qos.logback.
  def loadNativeLibrary(): Unit = {
    val rootLogger = org.slf4j.LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[ch.qos.logback.classic.Logger]
    val currentLogLevel = rootLogger.getLevel()
    try {
      rootLogger.setLevel(ch.qos.logback.classic.Level.WARN);
      Hdf5NativesLoader.load()
    }
    finally
      rootLogger.setLevel(currentLogLevel)
  }

  /** Close object store, preventing further writes. */
  def close() {
    require(groupIdStack.length == 1, "Save error: mismatched group create/close.")
    H5Gclose(currentGroup)
    H5Fclose(fileId)
  }

  /** Open an hdf5 group given its absolute pathname. */
  def openGroupAbsolutePath(groupName: String) = {
    val errMsg = s"Cannot open group '$groupName'"
    val groupId = try {
      H5Gopen(fileId, groupName, H5P_DEFAULT)
    }
    catch {
      case e: Exception =>
        println(errMsg)
        throw e
    }
    require(groupId >= 0, errMsg)
    groupId
  }

  /** The most recently created or opened group. */
  def currentGroup = groupIdStack.top

  /** Create a group and make it the group used by any group-relative commands. */
  def createAndEnterGroup(groupName: String): Int = {
    val errMsg = s"Cannot create group $groupName"
    val groupId = try {
      H5Gcreate(currentGroup, groupName, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)
    }
    catch {
      case e: Exception =>
        println(errMsg)
        throw e
    }
    require(groupId >= 0, errMsg)
    groupIdStack.push(groupId)
    groupId
  }

  /** Open an existing group and make it the group used by any group-relative commands. */
  def openAndEnterGroup(name: String): Int = {
    val errMsg = s"HDf5Common: missing expected group '$name'"
    val groupId = try {
      H5Gopen(currentGroup, name, H5P_DEFAULT)
    }
    catch {
      case e: Exception =>
        println(errMsg)
        throw e
    }
    require(groupId >= 0, errMsg)
    groupIdStack.push(groupId)
    groupId
  }

  /** Close the last created/opened group, making the previous group the one used by any group-relative commands. */
  def leaveAndCloseGroup(expectedCurrentGroup: Int) {
    require(expectedCurrentGroup == currentGroup, "Save error: mismatched group create/close.")
    H5Gclose(expectedCurrentGroup)
    groupIdStack.pop()
  }

  /** How the individual array elements are named in the HDF5 object naming space */
  def arrayElementName(name: String, index: Int) = name + "[" + index + "]"

  /** How the size of an array is tagged in the HDF5 object naming space */
  def arraySizeName(name: String) = name + "Size"

}
