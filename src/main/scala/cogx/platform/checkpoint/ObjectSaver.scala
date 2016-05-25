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

/**
  * Created by Dick Carter on 1/16/2015.
  *
  * Defines the functionality needed of an object saver, independent of
  * the specific technology (hdf5, java serialization, etc.)
  */
trait ObjectSaver {
  /** Write an Int to the object store, associating it with key `name`. */
  def writeInt(name: String, i32: Int)
  /** Write an Array[Int] to the object store, associating it with key `name`. */
  def writeIntArray(name: String, i32Array: Array[Int])
  /** Write a Long to the object store, associating it with key `name`. */
  def writeLong(name: String, i64: Long)
  /** Write a Array[Long] to the object store, associating it with key `name`. */
  def writeLongArray(name: String, i64Array: Array[Long])
  /** Write a Float to the object store, associating it with key `name`. */
  def writeFloat(name: String, f: Float)
  /** Write an Array[Float] to the object store, associating it with key `name`. */
  def writeFloatArray(name: String, fArray: Array[Float])
  /** Write a String to the object store, associating it with key `name`. */
  def writeString(name: String, s: String)
  /** Write an Array[String] to the object store, associating it with key `name`. */
  def writeStringArray(name: String, sArray: Array[String])
  /** Write a non-primitive "Saveable" object to the object store, associating it with key `name`. */
  def writeObject(name: String, saveable: Saveable)
  /** Write an Array of non-primitive "Saveable" object to the object store, associating it with key `name`. */
  def writeObjectArray(name: String, saveables: Array[_ <: Saveable])
  /** Close object store, preventing further writes. */
  def close()
}
