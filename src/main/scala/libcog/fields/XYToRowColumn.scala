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

package libcog.fields

/** Converts local (x, y) coordinates to (row, column) coordinates.
  *
  * @author Greg Snider
  */
trait XYToRowColumn {

  /** Convert a local (x, y) coordinate to row
    *
    * @param x The local x coordinate.
    * @param y The local y coordinate.
    * @return The local row coordinate.
    */
  def xyToRow(x: Int, y: Int) = -y

  /** Convert a local (x, y) coordinate to column
    *
    * @param x The local x coordinate.
    * @param y The local y coordinate.
    * @return The local column coordinates.
    */
  def xyToColumn(x: Int, y: Int) = x
}

