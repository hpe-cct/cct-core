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

package cogdebugger.ui.fieldvisualizations.scalar

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.ui.fieldvisualizations.{Zoomable, UnsupportedDimensionException, EventDrivenViewer}
import cogdebugger.ui.components.StackedTimeseriesPlot
import scala.swing._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 9/6/13
 * Time: 3:08 PM
 */

/** A visualization for SalarFields of two or fewer dimensions that renders the
  * field as a number of stacked timeseries plots, one per field element. This
  * visaulization's performance is quite sensitive to the target field's size;
  * it is not recommened for fields with a large number of elements, as it will
  * render quite slowly.
  *
  * This implementation keeps a fixed size history of the last 500 steps. The
  * oldest value is at the left end of the chart, the most recent at the right.
  * (This history is part of the reason this visualization can't handle large
  * fields; the field size is effecitvely multiplied by 500.))
  *
  * This visualization is based off the old Cog 3.x MultiXYPlot, which was
  * not particularly efficient. This visualization also takes a long time to
  * update, especially if its tracking many values from a large field. If the
  * target field is updating more rapidly than the visualization, there will be
  * gaps in the history. The most recently recorded value is extended into
  * these gaps (which has the tendency to cause the timeseries to render as a
  * stepped line).
  *
  * @param fieldType A FieldType describing the shape of the ScalarField being
  *                  visualized.
  */
class StackedTimeSeriesPanel(fieldType: FieldType)
    extends BorderPanel
    with EventDrivenViewer
    with Zoomable
{
  
  def this(target: ProbedField) = this(target.fieldType)
  
  private def fieldShape = fieldType.fieldShape

  if (fieldShape.points > 100)
    Console.out.println(
      s"[TimeseriesPlotPanel] Warning: Creating a Timeseries Plot on a " +
              s"field with ${fieldShape.points} elements. This may take a " +
              s"while, and will use a lot of memory.")

  val HistorySize = 500 // How many steps back the saved history goes

  private var lastUpdateTime = -1L

  // An array holding our axis labels (which are just numbers). Remember, we're
  // looking back in time, so these stretch from -HistorySize to 0.
  private val xData = Array.tabulate[Float](HistorySize) {
    i => i - HistorySize
  }

  // Y data tracks the value of each point in the field as it has evolved in
  // time for the last HistorySize steps. Thus, this is a 2-D array of
  // dimensions fieldShape.points x HistorySize.
  private val yData = Array.tabulate[Array[Float]](fieldShape.points) {
    i => new Array[Float](HistorySize)
  }

  private val yLabels = Array.tabulate[String](fieldShape.points)(i => " ")

  private val plot = new StackedTimeseriesPlot("", "time delta", yLabels, xData, yData)

  def zoomIncrement = plot.zDelta

  add(plot, BorderPanel.Position.Center)

  def zoomIn() { plot.zoomIn() }
  def zoomOut() { plot.zoomOut() }

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 0) => update(src, data.asInstanceOf[ScalarFieldReader], time)
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
  }

  def update(src: AnyRef, data: ScalarFieldReader, time: Long) {
    extractYData(data, time)
    plot.replaceYData(yData)
    lastUpdateTime = time
  }

  def extractYData(data: ScalarFieldReader, time: Long) {
    val shiftAmount = ((time - lastUpdateTime) min HistorySize).toInt
    if (shiftAmount > 0) {
      for (plot <- 0 until yData.length) {
        for (i <- 0 until HistorySize - shiftAmount) yData(plot)(i) = yData(plot)(i + shiftAmount)
        for (i <- HistorySize - shiftAmount until HistorySize) yData(plot)(i) = yData(plot)(HistorySize - 1)
      }
      fieldShape.dimensions match {
        case 0 => yData(0)(HistorySize - 1) = data.read()
        case 1 => for (c <- 0 until fieldShape(0)) yData(c)(HistorySize - 1) = data.read(c)

        case 2 =>
          for (r <- 0 until fieldShape(0); c <- 0 until fieldShape(1))
            yData(r * fieldShape(1) + c)(HistorySize - 1) = data.read(r, c)

        case x => throw new UnsupportedDimensionException(x)
      }
    } else if (shiftAmount < 0) {
      // System has been reset; clear all data.
      for (i <- 0 until yData.length; j <- 0 until yData(i).length)
        yData(i)(j) = 0
    }
  }
}
