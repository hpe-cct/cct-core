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

import java.io._

/** Writes a Scala object to disk using JVM serialization.
  * This does not use Java Beans / XML, but instead uses
  * the basic serialization provided by the language. This is less portable,
  * but a little easier to implement.</p>
  *
  * @author Greg Snider
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private object ObjectReaderWriter {
  /**
   * Serialize an object and write it to a file.
   *
   * @param obj The object to write.
   * @param filename Where the net is to be written.
   */
  def write(obj: AnyRef, filename: String) {
    val file = new File(filename)
    val out = new ObjectOutputStream(
      new BufferedOutputStream(new FileOutputStream(file)))
    out.writeObject(obj)
    out.flush
    out.close
  }

  /**
   * Read an object from a file.
   *
   * @param filename The file containing the neural net.
   *
   * @return The object from the file, null if an error occurred.
   */
  def read(filename: String): AnyRef = {
    val file = new File(filename)
    val in = new ObjectInputStream(
      new BufferedInputStream(new FileInputStream(file)))
    val obj = in.readObject
    in.close
    obj
  }
  
  /** Test code for object reading and writing. */
  def main(args: Array[String]) {
    val foo = Array(1, 2, 3, 4)
    println("writing file")
    write(foo, "junk")
    val bar = read("junk").asInstanceOf[Array[Int]]
    bar.foreach(e => println(e))
  }
}
