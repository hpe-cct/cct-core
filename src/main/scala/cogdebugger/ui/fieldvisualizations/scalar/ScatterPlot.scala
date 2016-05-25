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
import cogdebugger.ui.fieldvisualizations.{UnsupportedDimensionException, EventDrivenViewer}
import scala.swing.Panel
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYSeries, DefaultTableXYDataset}

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 10/8/13
 * Time: 3:14 PM
 */

/** A visualization for ScalarFields of two dimensions or less that presents
  * the field as a scatter plot of column index vs value. For 2D fields, rows
  * are differentiated by using a different set of colors/symbols for each row.
  *
  * An example of a 2x10 2D scalar field containing values in [0, 1] viewed as
  * a scatter plot would look something like this:
  *
  * {{{
  *     1.0 |                   x
  *         |             o
  *         |    o                 x     o
  *         |             x        o
  * value   |    x  x  o     x           x
  *         | o        x     o        o
  *         | x     o            o    x
  *         |
  *       0 +------------------------------
  *           0  1  2  3  4  5  6  7  8  9
  *                       column
  *         +-----------------------------+
  *         |   x = row 0   o = row 1     |
  *         +-----------------------------+
  * }}}
  *
  * @param fieldType A FieldType describing the shape of the field to be
  *                  visualized.
  */
class ScatterPlot(fieldType: FieldType)
    extends Panel
    with EventDrivenViewer {

  def this(target: ProbedField) = this(target.fieldType)

  private def fieldShape = fieldType.fieldShape

  val (rows, cols) = fieldShape.dimensions match {
    case 0 => (1, 1)
    case 1 => (1, fieldShape(0))
    case 2 => (fieldShape(0), fieldShape(1))
    case x => throw new UnsupportedDimensionException(x)
  }

  private val dataset = new DefaultTableXYDataset()
  for (row <- 0 until rows) {
    val series = new XYSeries(s"Row $row", false, false)
    dataset.addSeries(series)
  }

  val chart = org.jfree.chart.ChartFactory.createScatterPlot(
    null,       // chart title
    "column",   // x-axis label
    "value",    // y-acis label
    dataset,
    PlotOrientation.VERTICAL,
    rows > 1, false, false
  )

  peer.add(new org.jfree.chart.ChartPanel(chart))

  /** Updates the visualization based on the contents of `data`. */
  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long): Unit = {
    data match {
      case sfr: ScalarFieldReader => update(sfr)
      case x => throw new Exception("Expecting a ScalarFieldReader; got: "+x)
    }
  }

  def update(data: ScalarFieldReader) {
    data.fieldShape.dimensions match {

      case 0 =>
        val f = data.read()
        dataset.getSeries(0).addOrUpdate(0, f)

      case 1 =>
        val series = dataset.getSeries(0)
        series.setNotify(false)
        for (c <- 0 until cols) series.addOrUpdate(c, data.read(c))
        series.setNotify(true)
        series.fireSeriesChanged()

      case 2 =>
        for (r <- 0 until rows) {
          val series = dataset.getSeries(r)
          series.setNotify(false)
          for (c <- 0 until cols) series.addOrUpdate(c, data.read(r, c))
          series.setNotify(true)
          series.fireSeriesChanged()
        }

        // Is there a fireDatasetChanged method somewhere? We manage to avoid
        // refreshing/redrawing the scatter plot after each individual element
        // update by making use of setNotify and fireSeriesChanged, but we'd
        // really like to only fire an event after updating the entire dataset.

      case x => throw new UnsupportedDimensionException(x)
    }
  }

}
