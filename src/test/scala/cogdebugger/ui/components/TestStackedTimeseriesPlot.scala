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

import scala.swing._

/** Simple standalone test case for the StackedTimeseriesPlot. Creates three
  * different series (increasing, v-shaped, and stepped) and displays them
  * in a window.
  *
  * Created by gonztobi on 5/14/2014.
  */
object TestStackedTimeseriesPlot extends SimpleSwingApplication {

  val XData =
    Array(1f, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val YData = Array(
    Array(0f, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    Array(5f, 4, 3, 2, 1, 2, 3, 4, 5, 6),
    Array(9f, 9, 7, 7, 5, 5, 3, 3, 6, 6)
  )
  val YLabels = Array("increasing", "v-shaped", "stepped")

  override def top = new MainFrame {
    title = "Test "+classOf[StackedTimeseriesPlot].getSimpleName
    val panel = new StackedTimeseriesPlot(
      "Testing Timeseries",
      "Indices",
      YLabels,
      XData,
      YData
    )

    panel.updateData(Array(1f, 2, 3))
    // Post update have:
    // increasing -- 1, 2, 3, 4, 5, 6, 7, 8, 9, 1
    // v-shaped   -- 4, 3, 2, 1, 2, 3, 4, 5, 6, 2
    // stepped    -- 9, 7, 7, 5, 5, 3, 3, 6, 6, 3
    // domain     -- 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

    panel.updateData(3, Array(1f, 2, 3))
    // Post update have:
    // increasing -- 2, 3, 4, 5, 6, 7, 8, 9, 1,  1
    // v-shaped   -- 3, 2, 1, 2, 3, 4, 5, 6, 2,  2
    // stepped    -- 7, 7, 5, 5, 3, 3, 6, 6, 3,  3
    // domain     -- 2, 3, 4, 5, 6, 7, 8, 9, 10, 3
    //
    // Data is sorted before rendering, so it effectively becomes this:
    // increasing -- 2, 3, 1, 4, 5, 6, 7, 8, 9, 1
    // v-shaped   -- 3, 2, 2, 1, 2, 3, 4, 5, 6, 2
    // stepped    -- 7, 7, 3, 5, 5, 3, 3, 6, 6, 3
    // domain     -- 2, 3, 3, 4, 5, 6, 7, 8, 9, 10

    contents = panel
  }
}
