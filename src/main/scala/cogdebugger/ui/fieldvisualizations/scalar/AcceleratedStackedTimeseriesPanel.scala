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
import scala.swing._
import cogdebugger.ui.fieldvisualizations.{UnsupportedDimensionException, EventDrivenViewer}
import org.jfree.chart.plot.{XYPlot, PlotOrientation, CombinedDomainXYPlot}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import org.jfree.data.xy.AbstractXYDataset
import org.jfree.chart.{JFreeChart, ChartPanel}

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 10/9/13
 * Time: 1:13 PM
 */

/** A timeseries plot that shows a fixed sized history, not unlike the old
  * TimeseriesPlotPanel (which was renamed to StackedTimeseriesPanel). This
  * plot, however, uses a different dataset implementation and is better
  * suited for fast changing, live data.
  *
  * Like the old implementation, each point in the target field is displayed in
  * its own subplot, with the subplots stacked on top of each other. For fields
  * with more than a handful of points, this can get pretty expensive to draw.
  *
  * This version of the viewer supports up to 2D fields.
  *
  * @param fieldType A FieldType desribing the shape of the field being
  *                  visualized.
  * @param historySize How many time steps back the plot shows.
  */
class AcceleratedStackedTimeseriesPanel(fieldType: FieldType, historySize: Int)
    extends Panel
    with EventDrivenViewer {

  /** Supplies a default history size of 500 time steps */
  def this(fieldType: FieldType) = this(fieldType, 500)

  /** Supplies a default history size of 500 time steps */
  def this(target: ProbedField) = this(target.fieldType)
  
  private def fieldShape = fieldType.fieldShape

  val plot = new CombinedDomainXYPlot(new NumberAxis("history"))
  plot.setGap(10.0)
  plot.setOrientation(PlotOrientation.VERTICAL)

  val xAxis = plot.getDomainAxis
  xAxis.setLowerMargin(0)
  xAxis.setUpperMargin(0)

  val dataset = new DynamicXYDataset(fieldShape.points, historySize)

  // Each subplot charts a single value in the field as it evolves.
  val subplots = for (i <- 0 until fieldShape.points) yield {
    val renderer = new StandardXYItemRenderer()
    renderer.setBaseSeriesVisible(false)
    val plot = new XYPlot(dataset, null, new NumberAxis(""), renderer)
    renderer.setSeriesVisible(i, true, false)
    plot
  }; subplots foreach plot.add


  val chart = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false)
  _contents += Component.wrap(new ChartPanel(chart))

  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
    data match {
      case sfr: ScalarFieldReader => update(sfr, simTime)
      case x => throw new Exception("Expeting ScalarFieldReader; got: "+x)
    }
  }

  protected var lastUpdateTick = -1L
  protected val tmpData = new Array[Float](fieldShape.points)
  def update(data: ScalarFieldReader, time: Long) {
    data.fieldShape.dimensions match {
      case 0 => tmpData(0) = data.read()
      case 1 =>
        val cols = data.fieldShape(0)
        for (c <- 0 until cols) tmpData(c) = data.read(c)
      case 2 =>
        val (rows, cols) = (data.fieldShape(0), data.fieldShape(1))
        for (r <- 0 until rows; c <- 0 until cols) tmpData(r * cols + c) = data.read(r, c)
      case x => throw new UnsupportedDimensionException(x)
    }

    val delta = time - lastUpdateTick
    if (delta < 0)
    // Sim must have reset
      dataset.clear()
    else //if (delta > 0)
      dataset.advanceTime(delta.toInt)
    dataset.appendData(tmpData)
    lastUpdateTick = time
  }

}

/** An XYDataset with a fixed size internal circular buffer amenable to fast
  * "drop oldest and append" operations. */
class DynamicXYDataset(nSeries: Int, nMoments: Int)
        extends AbstractXYDataset {

  /** The circular buffer holding chart data. */
  protected val data = Array.tabulate(nSeries)(i => new Array[Float](nMoments))
  protected var oldestIdx = 0

  def getItemCount(series: Int): Int = data(series).size
  def getX(series: Int, item: Int): Number = item - nMoments + 1//translateIdx(item) + 1 - nMoments
  def getY(series: Int, item: Int): Number = data(series)(translateIdx(item))
  def getSeriesCount: Int = data.size
  def getSeriesKey(series: Int): Comparable[_] = series

  def clear() {
    oldestIdx = 0
    for (series <- data) java.util.Arrays.fill(series, 0f)
    fireDatasetChanged()
  }

  /** Controls whether dataset changed events are fired on dataset updates. */
  var doNotify = true
  
  def advanceTime() { advanceTime(1) }  
  def advanceTime(ticks: Int) {
    val mostRecentValues = for (i <- 0 until nSeries) yield getY(i, nMoments -1).floatValue()
    for (i <- 0 until nSeries)
      for (j <- 0 until (ticks min nMoments))
        data(i)(translateIdx(j)) = mostRecentValues(i)
    oldestIdx = translateIdx(ticks)
    if (doNotify) fireDatasetChanged()
  }
  
  def appendData(newData: Array[Float]) {
    require(data.size == newData.size)
    for ((series, idx) <- data.zipWithIndex) series(oldestIdx) = newData(idx)
    oldestIdx = (oldestIdx + 1) % nMoments
    if (doNotify) fireDatasetChanged()
  }
  
  protected def translateIdx(idx: Int): Int = (oldestIdx + idx) % nMoments
//    (oldestIdx + idx) % nMoments match {
//      case x if x < 0 => x + nMoments
//      case x => x
//    }

}