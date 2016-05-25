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
 * Defines the functionality needed of an object restorer, independent of
 * the specific technology (hdf5, java serialization, etc.)
 */
trait ObjectRestorer {
  /** Read an Int from the object store with the key `name`. */
  def readInt(name: String): Int
  /** Attempt to read an optional Int from the object store with key 'name',
    * returning None if the given name isn't present in the store. */
  def readOptionalInt(name: String): Option[Int]
  /** Read an Int array from the object store with the key `name`. */
  def readIntArray(name: String): Array[Int]
  /** Read a Long from the object store with the key `name`. */
  def readLong(name: String): Long
  /** Read a Long array from the object store with the key `name`. */
  def readLongArray(name: String): Array[Long]
  /** Read a Float from the object store with the key `name`. */
  def readFloat(name: String): Float
  /** Read a Float array from the object store with the key `name`. */
  def readFloatArray(name: String): Array[Float]
  /** Read a String from the object store with the key `name`. */
  def readString(name: String): String
  /** Attempt to read an optional String from the object store with key
    * 'name', returning None if the given name isn't present in the store. */
  def readOptionalString(name: String): Option[String]
  /** Read an Array[String] from the object store with the key `name`. */
  def readStringArray(name: String): Array[String]
  /** Read a non-primitive "Restoreable" object from the object store with the key `name`. */
  def readRestorable(name: String, factory: RestoreFactory): AnyRef
  /** Read an array of non-primitive "Restorable" objects from the object store with the key `name`. */
  def readRestorableArray(name: String, factory: RestoreFactory): Array[AnyRef]
  /** Close object store, preventing further reads. */
  def close()
}
