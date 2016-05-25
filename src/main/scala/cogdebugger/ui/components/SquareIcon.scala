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

package cogdebugger.ui.components

import javax.swing.Icon
import java.awt.{Graphics, Component}

/**
 * An icon that is just a small, colored square.
 * 
 * Created by gonztobi on 2/20/14.
 */
class SquareIcon(var color: java.awt.Color,
                 iconWidth: Int  = 12,
                 height: Int = 12)
        extends Icon {
  def getIconWidth: Int  = iconWidth
  def getIconHeight: Int = height
  def paintIcon(c: Component, g: Graphics, x: Int, y: Int) {
    g.setColor(color)
    g.fillRect(x, y, iconWidth, height)
  }
}
