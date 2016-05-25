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

/** Function which returns the class name of an object as a String, without all
  * of the package prefixes. For example, an instance of the class "foo.bar.Blah"
  * has the ClassName "Blah".
  *
  * @author Greg Snider
  */
private [utilities] object ClassName {
  /** Return the class name of "any" without package prefixes. */
  def apply(any: AnyRef): String = {
    var name = any.getClass.getName
    val firstChar = name.lastIndexOf(".") + 1
    if (firstChar > 0)
      name = name.substring(firstChar)
    name
  }
}