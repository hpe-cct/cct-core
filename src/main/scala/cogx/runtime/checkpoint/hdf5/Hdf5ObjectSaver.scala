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

import cogx.platform.checkpoint.{Saveable, ObjectSaver}
import hdfloader.Hdf5NativesLoader
import ncsa.hdf.hdf5lib.H5._
import ncsa.hdf.hdf5lib.HDF5Constants._

import scala.collection.mutable

/** Cog ComputeGraph saver based on HDF5
  *
  * First realize that HDF5 provides considerable flexibility in how ones
  * "web of objects" gets mapped to the HDF5 concepts of groups, datasets,
  * attributes, etc.  We have taken the approach of not using attributes and
  * mapping the objects to groups and datasets as follows:
  *
  * - Primitive values (Ints, Floats, Strings, etc.) are stored each in their
  *   own dataset, which is named by the writeXXX() call that wrote the primitive.
  * - Arrays of primitive values are similarly stored each in their own named
  *   dataset.  Only 1D Arrays are currently supported and the length of the
  *   array is stored and can be retrieved from the dataspace of the dataset.
  * - Single Objects are created as a group (which is like a directory in the
  *   filesystem analogy) with the name given in the writeObject() call.  The
  *   primitive data members of the object are stored in datasets that are within the
  *   object's group.
  * - Arrays of Objects are created with a two-level group hierarchy.  First a group
  *   is created that has the name provided by the writeObjectArray() call (e.g.
  *   "fields").  Then a special dataSet  called "fieldsSize" is created within this
  *   group to hold the integer length of the array.  Finally, each object of the
  *   array is stored in the group under a name that includes the array index (e.g.
  *   "fields[3]").  Note that the storing of the individual array objects will add
  *   a second level to the group hierarchy.
  *
  *   Note that this scheme permits a nesting of Objects and Arrays[Objects].
  *
  *   By example, assume you had the following class:
  *
  *   class Datapoint { val time: Int, val data: Float }
  *
  *   and instance:
  *
  *   val points = new Array[Datapoint](3)
  *
  *   Assume further that the Datpoint class is set up as a "Saveable" that wrote out
  *   its fields with names "time" and "data".
  *
  *   Then the call to writeObjectArray("points", points) would generate the
  *   following hdf5 datasets:
  *
  *   points/pointsSize
  *   points/points[0]/time
  *   points/points[0]/data
  *   points/points[1]/time
  *   points/points[1]/data
  *   points/points[2]/time
  *   points/points[2]/data
  *
  *   Note that under the hdf5 root group "/", the following groups were created:
  *
  *   points
  *   points/points[0]
  *   points/points[1]
  *   points/points[2]
  *
  * @author Dick Carter
  */
class Hdf5ObjectSaver(filename: String) extends ObjectSaver with Hdf5Common {
  //  /** Ensure that HDF5 native library is loaded */
  loadNativeLibrary()

  val fileOpenErrorMsg = s"Hdf5ObjectRestorer: Cannot create file $filename"
  // Open an existing HDF5 file read-only
  val fileId = try {
    H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)
  }
  catch {
    case e: Exception =>
      println(fileOpenErrorMsg)
      throw e
  }
  require(fileId >= 0, fileOpenErrorMsg)

  /** Write an Int to the object store. */
  def writeInt(name: String, i32: Int) { writeIntArray(name, Array(i32)) }

  /** Write an Array[Int] to the object store. */
  def writeIntArray(name: String, i32Array: Array[Int]) {
    writePrimitiveArray(name, i32Array, H5T_NATIVE_INT, H5T_STD_I32LE, "Int")
  }

  /** Write an Long to the object store. */
  def writeLong(name: String, i64: Long) { writeLongArray(name, Array(i64)) }

  /** Write an Array[Long] to the object store. */
  def writeLongArray(name: String, i64Array: Array[Long]) {
    writePrimitiveArray(name, i64Array, H5T_NATIVE_LLONG, H5T_STD_I64LE, "Long")
  }

  /** Write a Float to the object store. */
  def writeFloat(name: String, f: Float) { writeFloatArray(name, Array(f)) }

  /** Write an Array[Float] to the object store. */
  def writeFloatArray(name: String, fArray: Array[Float]) {
    writePrimitiveArray(name, fArray, H5T_NATIVE_FLOAT, H5T_IEEE_F32LE, "Float")
  }

  /** Write a Double to the object store. */
  def writeDouble(name: String, d: Double) { writeDoubleArray(name, Array(d)) }

  /** Write an Array[Double] to the object store. */
  def writeDoubleArray(name: String, dArray: Array[Double]) {
    writePrimitiveArray(name, dArray, H5T_NATIVE_DOUBLE, H5T_IEEE_F64LE, "Double")
  }

  /** Write a String to the object store. */
  def writeString(name: String, s: String) { writeStringArray(name, Array(s)) }

  /** Write an Array[String] to the object store. */
  def writeStringArray(name: String, sArray: Array[String]): Unit = {
    // create datatype for a variable length string
    val typeId = H5Tcopy(H5T_C_S1)
    require(typeId >= 0, "Could not create datatype id for in-memory simple string")
    H5Tset_size(typeId, H5T_VARIABLE)
    writePrimitiveArray(name, sArray, typeId, typeId, "String")
    // release created datatype
    H5Tclose(typeId)
  }

  /** Write an Array[<some primitive>] to the object store.
    *
    * @param name The key of this <key, value> pair to be stored.
    * @param anArray The array of values to write.
    * @param fromTypeId A "type id" describing the data format in memory (i.e. in the Array[]).
    * @param toTypeId A "type id" describing the data format as stored in the file.
    * @param typeName A name for the type (involved in the error reporting).
    */
   private def writePrimitiveArray(name: String, anArray: Array[_], fromTypeId: Int, toTypeId: Int, typeName: String) {
    val arraySizes = Array[Long](anArray.size)
    val arrayDimensions = arraySizes.size
    val dataSpaceId = H5Screate_simple(arrayDimensions, arraySizes, null)
    require(dataSpaceId >= 0,
      s"Could not create dataspace id for Array[$typeName] of size ${arraySizes(0)}")
    val dataSetId = H5Dcreate(currentGroup, name, toTypeId, dataSpaceId, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)
    require(dataSetId >= 0,
      s"Could not create dataset id for Array[$typeName] of size ${arraySizes(0)}")
    // write the array
    H5Dwrite(dataSetId, fromTypeId, H5S_ALL, H5S_ALL, H5P_DEFAULT, anArray)
    H5Dclose(dataSetId)
    H5Sclose(dataSpaceId)
  }

  /** Write a non-primitive "Saveable" object to the object store. */
  def writeObject(name: String, saveable: Saveable) {
    val groupId = createAndEnterGroup(name)
    saveable.save(this)
    leaveAndCloseGroup(groupId)
  }

  /** Write an Array of non-primitive "Saveable" object to the object store. */
  def writeObjectArray(name: String, saveables: Array[_ <:Saveable]) {
    val groupId = createAndEnterGroup(name)
    writeInt(arraySizeName(name), saveables.size)
    for (i <- 0 until saveables.size)
      writeObject(arrayElementName(name, i), saveables(i))
    leaveAndCloseGroup(groupId)
  }
}