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

import cogdebugger.ui.fieldvisualizations.Zoomable
import org.jfree.chart.axis.NumberAxis
import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import org.jfree.chart.plot.{XYPlot => JFreeXYPlot, PlotOrientation, CombinedDomainXYPlot}
import scala.swing.{Component, BorderPanel}
import org.jfree.chart.{ChartPanel, JFreeChart}

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 9/6/13
 * Time: 1:30 PM
 */

/** A data visualization that displays a number of timeseries plots stacked on
  * top of each other. Each chart has its own set of y/range values and range
  * axis, but all share the same x/domain values and axis. This class is
  * intended to provide the same functionality as Cog 3's MultiXYPlot class.
  *
  * All data is sorted according by the domain value prior to rendering. Thus,
  * the lines drawn on the timeseries plots can never double back.
  *
  * @param title        The chart's title, displayed at the top of the panel.
  * @param xLabel       Label for the domain axis
  * @param yLabels      An array of labels for the range axes, one per series.
  * @param initialXData An array of Floats containing the initial domain
  *                     values.
  * @param initialYData A two dimension array of floats containing the range
  *                     data for each series. The first index into the array
  *                     selects the data for an individual series, the
  *                     second/innermost index selects an element in that
  *                     series.
  */
class StackedTimeseriesPlot(title: String,
                            xLabel: String,
                            yLabels: Array[String],
                            initialXData: Array[Float],
                            initialYData: Array[Array[Float]])
    extends BorderPanel
    with Zoomable {

  def this(title: String, xLabel: String, yLabels: Array[String]) =
    this(title, xLabel, yLabels, new Array[Float](0), new Array[Array[Float]](0))

  require(initialYData.length == yLabels.length)
  require(initialYData.length == 0 || initialYData(0).length == initialXData.length)

  def ZoomFactor = zDelta // New axis size, relative to before zoom (should be < 1)
  val PanFactor = 0.5     // Fraction of current xAxis size to shift by

  val fullScaleXAxisLowerBound = if (initialXData.length > 0) initialXData(0) else 0
  val fullScaleXAxisUpperBound = if (initialXData.length > 0) initialXData(initialXData.length - 1) else 0

  val plot = new CombinedDomainXYPlot(new NumberAxis(xLabel))
  plot.setGap(10.0)
  plot.setOrientation(PlotOrientation.VERTICAL)

  val xAxis = plot.getDomainAxis
  xAxis.setLowerMargin(0)
  xAxis.setUpperMargin(0)
  //xAxis.setRange(...)

  val subPlots = for (i <- 0 until yLabels.length) yield {
    val label = yLabels(i)
    require(label != null)
    val data = createDataSet(label, initialXData, initialYData(i))
    new JFreeXYPlot(data, null, new NumberAxis(label), new StandardXYItemRenderer())
    //subPlot.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT)
    //subPlot.setRangeZeroBaselineVisible(true)
    //subPlot
  }
  for (subPlot <- subPlots) plot.add(subPlot)

  val chart = new JFreeChart(title, JFreeChart.DEFAULT_TITLE_FONT, plot, false)
  add(Component.wrap(new ChartPanel(chart)), BorderPanel.Position.Center)

  def xRangeSize = xAxis.getUpperBound - xAxis.getLowerBound
  def xRangeSize_=(size: Double) {
    val upperBound = xAxis.getUpperBound
    val lowerBound = (upperBound - size) max fullScaleXAxisLowerBound
    xAxis.setRange(lowerBound, upperBound)
  }

  /** Shifts the visible X-axis range by `shiftBy` units. */
  def shiftXAxisRange(shiftBy: Double) {
    var lowerBound = xAxis.getLowerBound + shiftBy
    var upperBound = xAxis.getUpperBound + shiftBy
    if (lowerBound < fullScaleXAxisLowerBound) {
      lowerBound = fullScaleXAxisLowerBound
      upperBound = lowerBound + xRangeSize
    } else if (upperBound > fullScaleXAxisUpperBound) {
      upperBound = fullScaleXAxisUpperBound
      lowerBound = upperBound - xRangeSize
    }
    xAxis.setRange(lowerBound, upperBound)
  }

  def panLeft()  { shiftXAxisRange(-xRangeSize * PanFactor) }
  def panRight() { shiftXAxisRange( xRangeSize * PanFactor) }

  override def zoomIn()  { changeZoomLevel(xRangeSize * ZoomFactor) }
  override def zoomOut() { changeZoomLevel(xRangeSize / ZoomFactor) }

  /** Updates the data set by shifting the y value of each (x1, y) pair in each
    * data series to the previous (x0, y) pair, and then dropping the `yData`
    * argument values into the now vacant y component of the last xy pairs.
    *
    * E.g., consider the 2-series data set here:
    * {{{
    *  series 0 y values -- | 1 | 2 | 3 | 4 | 5 |
    *  series 1 y values -- | 4 | 7 | 2 | 9 | 1 |
    *  domain values     -- | 2 | 4 | 5 | 7 | 9 |
    * }}}
    * Calling `updateData` with Array(3, 4) yields:
    * {{{
    *  series 0 y values -- | 2 | 3 | 4 | 5 | 3 |
    *  series 1 y values -- | 7 | 2 | 9 | 1 | 4 |
    *  domain values     -- | 2 | 4 | 5 | 7 | 9 |
    * }}}
    */
  def updateData(yData: Array[Float]) {

    // This sort of operation could be made *much* faster with a special purpose
    // data set that maintains a circular buffer, so as to not require all this
    // shifting.

    require(yData.length == subPlots.length)
    for ((subPlot, idx) <- subPlots.zipWithIndex) {
      val series = subPlot.getDataset.asInstanceOf[XYSeriesCollection].getSeries(0)
      series.setNotify(false)
      for (i <- 0 until series.getItemCount - 1)
        series.updateByIndex(i, series.getDataItem(i + 1).getY)
      series.updateByIndex(series.getItemCount - 1, yData(idx))
      series.setNotify(true)
    }
  }

  /** Updates the data set by dropping the first/left-most item from each data
    * series and then appending the values given as arguments to this method.
    * Keep in mind that the data will be sorted by domain value prior to
    * rendering.
    *
    * E.g., consider the 2-series data set here:
    * {{{
    *  series 0 y values -- | 1 | 2 | 3 | 4 | 5 |
    *  series 1 y values -- | 4 | 7 | 2 | 9 | 1 |
    *  domain values     -- | 2 | 4 | 5 | 7 | 9 |
    * }}}
    * Calling `updateData` with (3, Array(3, 4)) yields:
    * {{{
    *  series 0 y values -- | 3 | 4 | 5 | 3 | 3 |
    *  series 1 y values -- | 2 | 9 | 1 | 4 | 4 |
    *  domain values     -- | 4 | 5 | 7 | 9 | 3 |
    * }}}
    * But sorting will cause values to be rendered in this order:
    * * {{{
    *  series 0 y values -- | 3 | 3 | 4 | 5 | 3 |
    *  series 1 y values -- | 4 | 2 | 9 | 1 | 4 |
    *  domain values     -- | 3 | 4 | 5 | 7 | 9 |
    * }}}
    */
  def updateData(x: Float, yData: Array[Float]) {
    require(yData.length == subPlots.length)
    for ((subPlot, idx) <- subPlots.zipWithIndex) {
      val series = subPlot.getDataset.asInstanceOf[XYSeriesCollection].getSeries(0)
      series.remove(0)
      series.add(x, yData(idx))
    }
  }

  /** Overwrites existing Y data with the contents of the given array. Assumes
    * (series, col) indexing. This translates into a chart that looke like
    * this:
    * {{{
    *           +-----------+---------------+---------------+------
    * series 0  | T - times | T - times + 1 | T - times + 2 | ...
    *           +-----------+---------------+---------------+------
    * series 1  | T - times | T - times + 1 | T - times + 2 | ...
    *           +-----------+---------------+---------------+------
    * ...       | ...
    * }}}
    * */
  def replaceYData(yData: Array[Array[Float]]) {
    //require(yData(0).length == subPlots.length)
//    for ((subPlot, idx) <- subPlots.zipWithIndex) {
//      val series = subPlot.getDataset.asInstanceOf[XYSeriesCollection].getSeries(0)
//      for (i <- 0 until series.getItemCount)
//        series.updateByIndex(i, yData(i)(idx))
//    }
    for ((subPlot, idx) <- subPlots.zipWithIndex) {
      val series = subPlot.getDataset.asInstanceOf[XYSeriesCollection].getSeries(0)
      series.setNotify(false)
      for (i <- 0 until series.getItemCount) series.updateByIndex(i, yData(idx)(i))
      series.setNotify(true) // Fires series changed event
    }
  }

  private def createDataSet(title: String, xData: Array[Float], yData: Array[Float]) = {
    require(xData.length == yData.length)
    val series = new XYSeries(title)
    for ((x, y) <- xData zip yData) series.add(x, y)
    new XYSeriesCollection(series)
    //TODO Switch to new DynamicTimeSeriesCollection(..., ...)
  }

  def zDelta: Float = 0.8f
  def changeZoomLevel(delta: Double) { changeZoomLevel(delta.toFloat) }
  def changeZoomLevel(delta: Float) { xRangeSize = delta }
}
