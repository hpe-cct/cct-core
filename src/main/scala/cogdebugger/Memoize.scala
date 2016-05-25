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

/** A utility class for building memoized functions. Initially an empty
  * (mutable) Map, the first call to `apply` with a particular key uses the
  * `function` argument to produce a value and store it in the map before
  * returning it. Subsequent re-use of this key will return the stored value.
  *
  * Note that the underlying data structure is a
  * scala.collection.mutable.HashMap, which may not be thread-safe.
  *
  * Created by gonztobi on 2/25/14.
  */
class Memoize[-ArgType, +ReturnType](function: ArgType => ReturnType) extends (ArgType => ReturnType) {
  private[this] val map = collection.mutable.HashMap.empty[ArgType, ReturnType]
  def apply(key: ArgType): ReturnType = synchronized {
    map.getOrElseUpdate(key, function(key))
  }
  def isDefinedAt(key: ArgType) = map.isDefinedAt(key)
}

object Memoize {
  /** Memoize the given function. All this really saves you is a `new` keyword;
    * it just uses the basic constructor on [[cogdebugger.Memoize]]. */
  def apply[ArgType, ReturnType](function: ArgType => ReturnType): (ArgType => ReturnType) =
    new Memoize(function)
}
