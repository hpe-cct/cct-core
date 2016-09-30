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

import cogx.platform.checkpoint.{RestoreFactory, ObjectRestorer}
import hdfloader.Hdf5NativesLoader
import ncsa.hdf.hdf5lib.H5._
import ncsa.hdf.hdf5lib.HDF5Constants._

/** Cog ComputeGraph restorer based on HDF5
  *
  * See Hdf5ObjectSaver for more on the approach used to save/restore ComputeGraphs.
  *
  * @author Dick Carter
  */
class Hdf5ObjectRestorer(filename: String) extends ObjectRestorer with Hdf5Common {
  //  /** Ensure that HDF5 native library is loaded */
  loadNativeLibrary()

  val fileOpenErrorMsg = s"Hdf5ObjectRestorer: Cannot open file $filename"
  // Open an existing HDF5 file read-only
  val fileId = try {
    H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)
  }
  catch {
    case e: Exception =>
      println(fileOpenErrorMsg)
      throw e
  }
  require(fileId >= 0, fileOpenErrorMsg)

  /** Get the number of elements in the 0D or 1D dataSpace */
  def numElements(dataSpaceId: Int) = {
    val arrayDims = H5Sget_simple_extent_ndims(dataSpaceId)
    val arraySize = arrayDims match {
      case 0 => 1
      case 1 => {
        val arraySizes = new Array[Long](arrayDims)
        H5Sget_simple_extent_dims(dataSpaceId, arraySizes, null)
        arraySizes(0).toInt
      }
      case x => throw new RuntimeException("Expecting 0D or 1D array, found " + arrayDims + "D array.")
    }
    arraySize
  }

  /** Read an Int from the object store. */
  def readInt(name: String): Int = {
    val readValues = readIntArray(name)
    require(readValues.size == 1, "Expecting simple integer, found array of length " + readValues.size )
    readValues(0)
  }

  def readOptionalInt(name: String): Option[Int] = {
    if (H5Lexists(currentGroup, name, H5P_DEFAULT))
      return Some(readInt(name))
    else
      return None
  }

  /** Read an Array[Int] from the object store. */
  def readIntArray(name: String): Array[Int] = {
    readPrimitiveArray(name, H5T_NATIVE_INT, "Int", Array.ofDim[Int]).asInstanceOf[Array[Int]]
  }

  /** Read a Long from the object store. */
  def readLong(name: String): Long = {
    val readValues = readLongArray(name)
    require(readValues.size == 1, "Expecting simple long int, found array of length " + readValues.size )
    readValues(0)
  }

  /** Read an Array[Long] from the object store. */
  def readLongArray(name: String): Array[Long] = {
    // Note that a native "long long" corresponds to a 64-bit int, i.e. Java's Long.
    readPrimitiveArray(name, H5T_NATIVE_LLONG, "Long", Array.ofDim[Long]).asInstanceOf[Array[Long]]
  }

  /** Read a Float from the object store. */
  def readFloat(name: String): Float = {
    val readValues = readFloatArray(name)
    require(readValues.size == 1, "Expecting simple float, found array of length " + readValues.size )
    readValues(0)
  }

  /** Read an Array[Float] from the object store. */
  def readFloatArray(name: String): Array[Float] = {
    readPrimitiveArray(name, H5T_NATIVE_FLOAT, "Float", Array.ofDim[Float]).asInstanceOf[Array[Float]]
  }

  /** Read a Double from the object store. */
  def readDouble(name: String): Double = {
    val readValues = readDoubleArray(name)
    require(readValues.size == 1, "Expecting simple double, found array of length " + readValues.size )
    readValues(0)
  }

  /** Read an Array[Double] from the object store. */
  def readDoubleArray(name: String): Array[Double] = {
    readPrimitiveArray(name, H5T_NATIVE_DOUBLE, "Double", Array.ofDim[Double]).asInstanceOf[Array[Double]]
  }

  /** Read an Array[Primitives] from the object store.
    *
    * @param name The key of this <key, value> pair to be read.
    * @param receivingTypeId A "type id" describing the data format in memory (i.e. in the Array[]).
    * @param typeName A name for the type (involved in the error reporting).
    * @param arrayMaker A function to create the returned array (passed the array length as an Int arg).
    */
  def readPrimitiveArray(name: String, receivingTypeId: Int, typeName: String,
                         arrayMaker: (Int) => Array[_]): Array[_] = {
    val dataSetId = H5Dopen(currentGroup, name, H5P_DEFAULT)
    require(dataSetId >= 0, s"Could not open dataset id for Array[$typeName].")
    val dataSpaceId = H5Dget_space(dataSetId)
    require(dataSpaceId >= 0, s"Invalid dataSpace id for Array[$typeName].")
    val arrayDims = H5Sget_simple_extent_ndims(dataSpaceId)
    require(arrayDims == 1, s"Expecting 1D Array[$typeName], found " + arrayDims + "D.")
    val readValues = arrayMaker(numElements(dataSpaceId))
    // read the array of integers
    H5Dread(dataSetId, receivingTypeId, H5S_ALL, H5S_ALL, H5P_DEFAULT, readValues)
    H5Dclose(dataSetId)
    H5Sclose(dataSpaceId)
    readValues
  }

  /** Read a String from the object store. */
  def readString(name: String): String = {
    val readValues = readStringArray(name)
    require(readValues.size == 1, "Expecting simple string, found array of length " + readValues.size )
    readValues(0)
  }

  def readOptionalString(name:String): Option[String] = {
    if (H5Lexists(currentGroup, name, H5P_DEFAULT))
      return Some(readString(name))
    else
      return None
  }

  /** Read an Array[String] from the object store. (Can't use readPrimitiveArray because of H5DreadVL() call).*/
  def readStringArray(name: String): Array[String] = {
    val dataSetId = H5Dopen(currentGroup, name, H5P_DEFAULT)
    require(dataSetId >= 0, "Could not open dataset id for string array")
    val typeId = H5Dget_type(dataSetId)
    require(typeId >= 0, "Invalid type id for string array")
    val dataSpaceId = H5Dget_space(dataSetId)
    require(dataSpaceId >= 0, "Invalid dataSpace id for float array")
    val readValues = new Array[String](numElements(dataSpaceId))
    // read the array of strings
    H5DreadVL(dataSetId, typeId, H5S_ALL, H5S_ALL, H5P_DEFAULT, readValues.asInstanceOf[Array[AnyRef]])
    H5Tclose(typeId)
    H5Dclose(dataSetId)
    H5Sclose(dataSpaceId)
    readValues
  }

  /** Read a non-primitive "Restorable" object from the object store. */
  def readRestorable(name: String, factory: RestoreFactory) = {
    val groupId = openAndEnterGroup(name)
    try
      factory.restore(this)
    finally
      leaveAndCloseGroup(groupId)
  }

  /** Read an array of non-primitive "Restorable" objects from the object store. */
  def readRestorableArray(name: String, factory: RestoreFactory) = {
    val groupId = openAndEnterGroup(name)
    val numElements = readInt(arraySizeName(name))

    try {
      val newObjectArray = Array.tabulate(numElements) {
        i =>
          try {
            readRestorable(arrayElementName(name, i), factory)
          }
          catch {
            case e: Exception =>
              println("restore of " + arrayElementName(name, i) + " fails with exception: ")
              throw e
          }
      }
      newObjectArray
    }
    finally
      leaveAndCloseGroup(groupId)
  }
}
