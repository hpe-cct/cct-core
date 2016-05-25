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

package cogdebugger

/**
 * A collection of utility and miscellaneous functions used in the
 * implementation of the Cog Debugger that don't have a compelling reason to be
 *  placed in a particular package.
 *
 * User: gonztobi
 * Date: 8/21/13
 * Time: 2:40 PM
  *
  * @author Tobin Gonzalez
 */
object Util {
  /** List of illegal filename characters on Windows platforms */
  val illegalCharacters = Array(34, 60, 62, 124, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 58, 42, 63, 92, 47).sorted

  /** Remove characters from the given String that aren't legal in a Windows
    * filename (note that it's possible for the empty string to be returned if
    * everyz character is illegal). */
  def sanitizeString(str: String) =
    str.collect { case c: Char if !illegalCharacters.contains(c) => c }


  def time(op: => Unit) = {
    val start = System.currentTimeMillis()
    op
    System.currentTimeMillis() - start
  }

  /**
    * Wrapper for a [[java.util.IdentityHashMap]] that provides the usual Scala
    * collections APIs.
    * @tparam A key type
    * @tparam B value type
    */
  class IdentityHashMap[A, B] extends collection.mutable.AbstractMap[A, B] {
    protected val _map = new java.util.IdentityHashMap[A, B]()

    override def +=(kv: (A, B)): IdentityHashMap.this.type = {
      val (key, value) = kv
      _map.put(key, value)
      this
    }

    override def -=(key: A): IdentityHashMap.this.type = {
      _map.remove(key)
      this
    }

    override def get(key: A): Option[B] = Option(_map.get(key))

    override def iterator: Iterator[(A, B)] = {
      new Iterator[(A, B)] {
        private val _it = _map.entrySet().iterator()
        override def hasNext: Boolean = _it.hasNext
        override def next(): (A, B) = {
          val v = _it.next()
          (v.getKey, v.getValue)
        }
      }
    }
  }

  object Log {
    var verbosity = 4
    def e(msg: String) { if (verbosity > 0) Console.err.println(msg) }
    def w(msg: String) { if (verbosity > 1) Console.err.println(msg) }
    def i(msg: String) { if (verbosity > 2) Console.out.println(msg) }
    def d(msg: String) { if (verbosity > 3) Console.out.println(msg) }
  }
}
