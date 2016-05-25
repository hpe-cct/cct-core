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

import cogx.platform.opencl.WorkGroupParameters

/** Creates a multi-line string containing local index parameters using define
  * statements:
  * {{{
  *   _localLayers
  *   _localRows
  *   _localColumns
  *
  * @author Greg Snider
  * }}}
  */
private[cogx]
object LocalIndexParameters {

  /**
   * Create local parameter definitions as a multi-line string.
   *
   * @param workGroup Work group parameters.
   * @return String defining local work group sizes.
   */
  def apply(workGroup: WorkGroupParameters): String = {
    val parameters = new StringBuffer
    workGroup.dimensions match {
      case 3 =>
        parameters.append("#define _localLayers " + workGroup.localLayers + "\n")
        parameters.append("#define _localRows " + workGroup.localRows + "\n")
        parameters.append("#define _localColumns " + workGroup.localColumns + "\n")
      case 2 =>
        parameters.append("#define _localRows " + workGroup.localRows + "\n")
        parameters.append("#define _localColumns " + workGroup.localColumns + "\n")
      case 1 =>
        parameters.append("#define _localColumns " + workGroup.localColumns + "\n")
      case x => throw new RuntimeException("Illegal dimensions: " + x)
    }
    parameters.toString
  }

  /**
   * Clean up #defines made by apply.
   *
   * @param workGroup Work group parameters.
   * @return String defining local work group sizes.
   */
  def cleanup(workGroup: WorkGroupParameters): String = {
    val parameters = new StringBuffer
    workGroup.dimensions match {
      case 3 =>
        parameters.append("#undef _localLayers \n")
        parameters.append("#undef _localRows \n")
        parameters.append("#undef _localColumns \n")
      case 2 =>
        parameters.append("#undef _localRows \n")
        parameters.append("#undef _localColumns \n")
      case 1 =>
        parameters.append("#undef _localColumns \n")
      case x => throw new RuntimeException("Illegal dimensions: " + x)
    }
    parameters.toString
  }
}