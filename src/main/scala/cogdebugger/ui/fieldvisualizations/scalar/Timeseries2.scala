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

import libcog._
import cogdebugger.ui.fieldvisualizations.{UnsupportedDimensionException, EventDrivenViewer}
import scala.swing.Panel
import org.jfree.data.time.{RegularTimePeriod, Millisecond, DynamicTimeSeriesCollection}

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 10/8/13
 * Time: 4:34 PM
 */

/** An experimental, accelerated version of the old XYPlot (since renamed to
  * Timeseries and TimeseriesPlot). There are several big visual differences -
  * for one, all series share a single drawing area (rather than having one
  * graph per series).
  *
  * Needs some tools to control scaling and range of the axes. */
class Timeseries2(target: AnyRef, fShape: Shape)
    extends Panel
    with EventDrivenViewer {

  def targets: Set[AnyRef] = Set(target)

  val historyLength = 100 // How far back the history goes

  /* The DynamicTimeSeriesCollection class is a little broken... or at least
   * unfinished. This simplest constructor uses a timebase of milliseconds, but
   * calls an alternate constructor that doesn't know what to do with that
   * timebase and leaves some fields unitialized. If we don't subclass it and
   * manually set the timebase, like so, you'll get NPEs. */
  val dataset = new DynamicTimeSeriesCollection(fShape.points, historyLength) {
    pointsInTime = new Array[RegularTimePeriod](historyLength)
  }; dataset.setTimeBase(new Millisecond())

  val dummyData = new Array[Float](historyLength)
  for (i <- 0 until fShape.points)
    dataset.addSeries(dummyData, i, i)

  val chart = org.jfree.chart.ChartFactory.createTimeSeriesChart(
    null,       // title
    "history",  // x-axis label
    "value",    // y-axis label
    dataset,
    false,
    true,
    false
  )

//val chart = org.jfree.chart.ChartFactory.createXYLineChart(
//  "title",       // title
//  "history",  // x-axis label
//  "value",    // y-axis label
//  dataset,
//  PlotOrientation.VERTICAL,
//  false,
//  false,
//  false
//)

  //val plot = chart.getXYPlot
  //val axis = plot.getDomainAxis
  //axis.setAutoRange(true)
  //axis.setFixedAutoRange(60 * 1000) // 60 seconds

  peer.add(new org.jfree.chart.ChartPanel(chart))

  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
    data match {
      case sfr: ScalarFieldReader => update(sfr)
      case x => throw new Exception("Expecting a ScalarFieldReader, got: "+x)
    }
  }

  val tmpData = new Array[Float](fShape.points)

  def update(data: ScalarFieldReader) {
    dataset.advanceTime()
    data.fieldShape.dimensions match {
      case 0 => dataset.appendData(Array(data.read()))

      case 1 =>
        for (c <- 0 until fShape(0)) tmpData(c) = data.read(c)
        dataset.appendData(tmpData)

      case 2 =>
        for (r <- 0 until fShape(0); c <- 0 until fShape(1)) tmpData(r * fShape(1) + c) = data.read(r, c)
        dataset.appendData(tmpData)

      case x => throw new UnsupportedDimensionException(x)
    }
  }
}
