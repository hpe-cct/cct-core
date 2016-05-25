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

package cogx.compiler.codegenerator.opencl.fragments

/** Generator of unique integers.
  *
  * To enable hits in a cache of compiled kernels, we attempt to have multiple threads compiling
  * the same Cog program generate the same set of kernel sources.  Giving each thread it's own
  * copy of _id enables that.
  *
  * @author Greg Snider
  */
private[cogx]
object UniqueID {
  //  The following three fields replace "private var id = 0".
  /** Number of declarations made since program startup. Each thread gets its instance own starting with 0. */
  private val _id =  new ThreadLocal[Int] {
    override def initialValue() = 0
  }
  private def id = _id.get()
  private def id_=(newId: Int) { _id.set(newId) }

  private val IDRegEx = "_temp[0-9]+_".r

  def findFirstIn(s: String): String = IDRegEx.findFirstIn(s) match {
    case Some(str) => str
    case None => null
  }

  /** Return a unique integer on each call. */
  def apply(): String = {
    id += 1
    "_temp" + id + "_"
  }
}
