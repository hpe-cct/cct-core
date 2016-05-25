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

import java.io.{FileInputStream, BufferedInputStream, DataInputStream, File}


/** Reads a raw data file consisting of a stream of floats and returns it
  * as an array of floats.
  *
  * @author Greg Snider
  */
@deprecated("This is defunct and should be removed in cog 5.0", "4.1.11") private [utilities] class DataFileReader(filename: String) {
  private val file = new File(filename)
  require(file.exists, "Cannot find file " + filename)
  require(file.canRead, "Cannot read file " + filename)
  private val bytes = file.length
  private val floats = (bytes / 4).toInt

  /** Read file as 1D array. */
  def read: Array[Float] = {
    val in  = new DataInputStream(new BufferedInputStream(
      new FileInputStream(file)))
    val data = new Array[Float](floats)
    for (i <- 0 until floats)
      data(i) = in.readFloat
    data
  }
}