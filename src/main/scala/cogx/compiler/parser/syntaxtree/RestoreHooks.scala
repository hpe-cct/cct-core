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

/** Augmenting methods for those sensor and actuator classes wishing to be saved to a file and restored.
  *
  * @author Dick Carter
  */
trait RestoreHooks {
  /** A hook that should be overridden by users to supply the info needed to reconstruct the sensor or actuator. */
  def restoreParameters: String = "default"

  /** A hook that should be overridden by users if necessary to point to a class with a restore method. */
  def restoringClass: AnyRef = this

  /** The sensor or actuator class name as stored in a file- used to find the factory method to reconstruct. */
  private[cogx] def classnameAsSaved: String = {
    // The scala classname for a singleton object ends with a '$'.  The restore method invoke
    // fails with a null pointer exception when the class is looked up with that name, but
    // not when the $ is removed
    val rawName = restoringClass.getClass.getName
    val dollarSignTrimmedName =
      if (rawName.endsWith("$"))
        rawName.dropRight(1)
      else
        rawName
    dollarSignTrimmedName
  }
}
