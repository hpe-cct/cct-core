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

package cogx.runtime.checkpoint.javaserialization

import java.io.{BufferedInputStream, FileInputStream, ObjectInputStream}

import cogx.platform.checkpoint.{RestoreFactory, ObjectRestorer}

/** Cog ComputeGraph restorer based on Java Serialization.
  *
  * @author Dick Carter
  */
class JavaObjectRestorer(filename: String) extends ObjectRestorer {
  import JavaObjectRestorer.Log

  /** An object that was read from the stream that we would like to put back
    * because we can't use it yet. Used to 'peek' ahead a single object into
    * the input stream, enabling this ObjectRestorer implementation some
    * limited ability to deal with optional keys in files.
    *
    * This is actually kind of tricky. We can't rewind an ObjectInputStream
    * directly, but we could manipulate the file underneath it. However,
    * rewinding the file can potentially break our ability to deserialize
    * correctly, because of the way that Java's object output/input streams
    * handle references. The first time a particular object is written to
    * the stream, the whole object is written out. Thereafter, all that's
    * output is a reference to the object. This reference takes the form of a
    * simple array index (e.g. if we're referring the 3rd unique object
    * written out by the stream, the index is 2). At the other end of the pipe,
    * ObjectInputStream maintains a simple object cache. When it encounters a
    * complete object in the input file, it recreates that object and sticks it
    * in an internal array. If it later sees a reference/index in the input
    * file, it* just grabs the appropriate object from that array.
    *
    * The problem with rewinding the file then, is that we can cause the
    * ObjectInputStream to encounter the full definition for a single unique
    * object multiple times, which will put multiple copies of that object
    * into the cache and throw off all the references encountered thereafter.
    * It is crucial that we never rewind the file, which means if read from
    * the stream and can't do anything right now with whatever we pull out, we
    * have to save it ourselves until it becomes useful later.
    *
    * In our use case, every value in the file is preceded by a String key, so
    * 'nextObject' here should really be 'nextKey,' since that's what we'll be
    * checking.
    */
  private var nextObject: Option[String] = None

  Log.debug("Constructing JavaObjectRestorer on file "+filename)
  val fis = new FileInputStream(filename)
  val ois = new ObjectInputStream(fis)

  /** To match the style of HDF5, objects are stored with a name tag.  This name precedes the
    * object in this Java serialization approach.  Verify this name tag is present.
    * @param name The expected name (a string) to be found in the object store
    */
  private def verifyName(name: String): Unit = {
    Log.debug("Verifying tag '"+name+"' at file position "+fis.getChannel.position())
    val nameAsRead = nextObject match {
      case Some(obj) =>
        Log.debug("  Using pre-read object")
        nextObject = None // Consume saved object
        obj.asInstanceOf[String]
      case None =>
        Log.debug("  Reading from stream")
        ois.readObject().asInstanceOf[String]
    }
    Log.debug("  Read: "+nameAsRead)
    require(name == nameAsRead, s"File format inconsistency: expected $name, found $nameAsRead")
  }

  /** Checks if the next object to be read is a String matching 'name,' and
    * save the read object into [[nextObject]] so that it is not lost. Or,
    * peek at the head of the stream without consuming it.
    *
    * This method is useful for checking for the presence of optional
    * fields/attributes in a file - a feature we allow in our alternate HDF5
    * serialization classes.
    *
    * @param name String to test for
    * @return True if 'name' is equal to the next object in the file, false
    *         otherwise.
    */
  private def checkName(name: String): Boolean = {
    Log.debug("Checking for tag '"+name+"; at file position "+fis.getChannel.position())
    val nameAsRead = nextObject match {
      case Some(obj) =>
        Log.debug("  Using pre-read object")
        obj.asInstanceOf[String]
      case None =>
        Log.debug("  Reading from stream")
        val s = ois.readObject().asInstanceOf[String]
        nextObject = Some(s)
        s
    }
    Log.debug("  Read: "+nameAsRead)
    return name == nameAsRead
  }

  /** How the size of an array is tagged in the HDF5 object naming space */
  private def arraySizeName(name: String) = name + "Size"

  /** Helper function to read any object preceded by a String name. */
  private def readNamedObject(name: String) = {
    verifyName(name)
    ois.readObject()
  }

  /** Read an Int from the object store. */
  def readInt(name: String): Int = {
    verifyName(name)
    ois.readInt()
  }

  /** Attempt to read an Int from the object store that may or may not actually
    * be present there. Returns None if this optional Int wasn't found. */
  def readOptionalInt(name: String): Option[Int] = {
    if (checkName(name)) {
      nextObject = None           // Consume the name tag...
      return Some(ois.readInt())  // ...and then read the Int that follows.
    } else return None
  }

  /** Read an Array[Int] from the object store. */
  def readIntArray(name: String): Array[Int] = {
    readNamedObject(name).asInstanceOf[Array[Int]]
  }

  /** Read a Long from the object store. */
  def readLong(name: String): Long = {
    verifyName(name)
    ois.readLong()
  }

  /** Read a Array[Long] from the object store. */
  def readLongArray(name: String): Array[Long] = {
    readNamedObject(name).asInstanceOf[Array[Long]]
  }

  /** Read a Float from the object store. */
  def readFloat(name: String): Float = {
    verifyName(name)
    ois.readFloat()
  }

  /** Read an Array[Float] from the object store. */
  def readFloatArray(name: String): Array[Float] = {
    readNamedObject(name).asInstanceOf[Array[Float]]
  }

  /** Read a Double from the object store. */
  def readDouble(name: String): Double = {
    verifyName(name)
    ois.readDouble()
  }

  /** Read an Array[Double] from the object store. */
  def readDoubleArray(name: String): Array[Double] = {
    readNamedObject(name).asInstanceOf[Array[Double]]
  }

  /** Read a String from the object store. */
  def readString(name: String): String = {
    readNamedObject(name).asInstanceOf[String]
  }

  /** Attempt to read a String from the object store that may or may not
    * actually be present there. Returns None if this optional String wasn't
    * found.
    */
  def readOptionalString(name: String): Option[String] = {
    if (checkName(name)) {
      nextObject = None                                   // Consume the name tag...
      return Some(ois.readObject().asInstanceOf[String])  // ...and return the String that follows.
    } else return None
  }

  /** Read an Array[String] from the object store. */
  def readStringArray(name: String): Array[String] = {
    readNamedObject(name).asInstanceOf[Array[String]]
  }

  /** Read a non-primitive "Restorable" object from the object store. */
  def readRestorable(name: String, factory: RestoreFactory) = {
    verifyName(name)
    factory.restore(this)
  }

  /** Read an array of non-primitive "Restorable" objects from the object store. */
  def readRestorableArray(name: String, factory: RestoreFactory) = {
    verifyName(name)
    val numElements = ois.readInt()
    val newObjectArray = Array.tabulate(numElements) {
      i => factory.restore(this)
    }
    newObjectArray
  }

  /** Close object store, preventing further writes. */
  def close(): Unit = {
    Log.debug("Closing JavaObjectRestorer")
    ois.close()
    fis.close()
  }

}

object JavaObjectRestorer {
  object Log {
    var logLevel = 3
    def debug(msg: String) { if (logLevel > 3) println(msg) }
    def info(msg: String) { if (logLevel > 2) println(msg) }
    def warn(msg: String) { if (logLevel > 1) println(msg) }
    def error(msg: String) { if (logLevel > 0) println(msg) }
  }
}