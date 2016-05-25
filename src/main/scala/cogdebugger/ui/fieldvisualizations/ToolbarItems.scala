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

package cogdebugger.ui.fieldvisualizations

import scala.swing._

/** A trait for subpanels that define controls or labels that a
  * parent/container panel can place in a toolbar or similar.
  *
  * Created by gonztobi on 3/21/2014.
  */
trait ToolbarItems {

  /** A sequence of components related to this viewer that belong in a panel
    * other than the main display (usually a toolbar). These components may
    * be controls (buttons, combo boxes, spinners etc.) or small
    * labels/displays (e.g. min/max, color key). */
   def toolbarComponents: Seq[ComponentGroup]

}

/** A group of related toolbar items. The compoentns in this group should be
  * visually distanced from components in other groups with a small amount of
  * empty space. */
case class ComponentGroup(components: Component*)
