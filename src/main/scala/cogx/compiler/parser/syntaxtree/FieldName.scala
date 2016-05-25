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

package cogx.compiler.parser.syntaxtree

/** Trait that centralizes the policies for naming fields.
  *
  * Field names are Scala-like path names with '.' separated components. The
  * last component is called the simple name, while the components leading
  * up to the simple name comprise the path name prefix.
  *
  * Naming is sticky. Once a path name prefix has been declared, it cannot be
  * changed. Similarly, once a simple name has been declared, it cannot be
  * changed.
  *
  * @author Greg Snider
  */

trait FieldName {
  /** Separator character between path name components. */
  private val Separator = '.'
  /** Prefix of path name. */
  private var pathNamePrefix: Array[String] = Array()
  /** Last component of path name. */
  private var _simpleName: String = ""
  /** Flag for making simple name declarations "sticky". */
  private var simpleNameDeclared = false
  /** Flag for making path name prefix declarations "sticky". */
  private var pathNameDeclared = false

  /** Get the "full" name of a field, as a Scala-like path name. */
  def name: String = {
    val buffer = new StringBuilder
    for (component <- pathNamePrefix) {
      buffer append component
      buffer append Separator
    }
    buffer append _simpleName
    buffer.toString()
  }

  /** Set the simple name of a field to `name`; ignored if the simple name
    * has already been set.
    */
  private[cogx]
  def setSimpleName(name: String) {
    if (!simpleNameDeclared) {
      _simpleName = name
      simpleNameDeclared = true
    }
  }

  /** Set the path name of a field to `name`; if the path name prefix has
    * already been set, this is ignored, and if the simple name has already
    * been set, the simple name assignment is ignored.
    */
  private[cogx]
  def setPathName(name: String) {
    if (!pathNameDeclared) {
      // Sticky naming of path name
      val fullName = name.split(Separator)
      pathNamePrefix = fullName.dropRight(1)
      pathNameDeclared = true
      setSimpleName(fullName.last)
    }
  }
}