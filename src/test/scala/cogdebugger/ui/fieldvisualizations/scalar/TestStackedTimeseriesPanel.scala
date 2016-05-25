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

//package cogdebugger.ui.fieldvisualizations.scalar
//
//import scala.swing._
//import cogx.platform.cpumemory.ScalarFieldMemory
//import cogx.platform.cpumemory.readerwriter.ScalarFieldReader
//
///** A standalone test case for the stacked timeseries visualization.
//  *
//  * Created by gonztobi on 5/12/2014.
//  */
//object TestStackedTimeseriesPanel extends SimpleSwingApplication {
//
//  val Rows = 2
//  val Cols = 3
//
//  val memory = ScalarFieldMemory(Rows, Cols, (r, c) => 0f )
//
//  override def top = new MainFrame {
//    title = "Test "+classOf[StackedTimeSeriesPanel].getSimpleName
//
//    val view = new StackedTimeSeriesPanel(memory.fieldType)
//    view.update(memory, memory.asInstanceOf[ScalarFieldReader], 0L)
//
//    val injector = new ScalarFieldMemoryInjector(memory, view)
//
//    /** A simple driver thread for continuously updating the visualization.
//      * Be warned that this just uses a spinlock when "paused" (i.e. the
//      * `running` flag is set to false). */
//    val driver = new Thread {
//      @volatile var killed = false
//      @volatile var running = false
//      override def run() {
//        while (!killed && !Thread.interrupted()) {
//          while (!running && !Thread.interrupted())
//            Thread.`yield`()
//          injector.injectRandom()
//        }
//      }
//    }
//    driver.start()
//
//    val startButton = new Button
//    val stopButton  = new Button
//    startButton.action = Action("Start") {
//      driver.running = true
//      startButton.enabled = false
//      stopButton.enabled = true
//    }
//    stopButton.action = Action("Stop") {
//      driver.running = false
//      startButton.enabled = true
//      stopButton.enabled = false
//    }
//    stopButton.enabled = false
//
//    injector.addSeparator()
//    injector.contents += new Label("Autoinjector:")
//    injector.contents += Swing.HStrut(5)
//    injector.contents += startButton
//    injector.contents += stopButton
//
//    val bp = new BorderPanel()
//    bp.layout(injector) = BorderPanel.Position.North
//    bp.layout(view)     = BorderPanel.Position.Center
//
//    contents = bp
//
//    override def closeOperation() {
//      driver.interrupt()
//      driver.killed = true
//      super.closeOperation()
//    }
//  }
//}
