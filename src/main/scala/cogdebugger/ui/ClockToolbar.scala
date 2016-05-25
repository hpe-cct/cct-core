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

package cogdebugger.ui

import libcog.ComputeGraph
import scala.swing._
import java.awt.Insets

/** Implements a toolbar with buttons for stepping a model as well as counters
  * for showing how many simulation ticks have passed and the model's current
  * rate of execution (in ticks per second). This class spawns a new thread
  * that will periodically query `computeGraph` for the current sim tick and
  * update the counters.
  * @param computeGraph Graph to be stepped and monitored by this toolbar
  * @param learningRate Learning rate of the leaky integrator that calculates
  *                     the graph's current rate of execution
  * @param udpateIntervalMSec Interval between counter updates in milliseconds
  */
class ClockToolbar(computeGraph: ComputeGraph,
                    learningRate: Float,
                    udpateIntervalMSec: Int)
    extends BoxPanel(Orientation.Horizontal) {

  protected[cogdebugger] val counterPanel = new BoxPanel(Orientation.Horizontal) {
    val counterLabel = new Label("Cycle:")
    val counter = new Label("?") {
      def update(): Unit = {
        computeGraph.time(time => this.text = time.toString)
      }
      // TODO Need a fixed size to prevent subsequent components from moving
      // about the toolbar as the text gets longer
    }
    contents ++= Seq(counterLabel, Swing.HStrut(10), counter)
  }

  protected[cogdebugger] val ratePanel = new BoxPanel(Orientation.Horizontal) {
    // The learnRate of 0.1 gives a more stable response, but is far too slow to
    // be useful for debugging. I've sped it up a bit. GSS.
    //val learnRate = 0.1
    //val learningRate = 0.5
    var lastTime = System.currentTimeMillis()
    var lastCount = 0L
    var lastRate = 0.0
    val rateLabel = new Label("Cycles/sec:")
    val rate = new Label("0") {
      def update() {
        computeGraph.time(time => {
          val newCount = time
          val newTime = System.currentTimeMillis()
          val deltaCount = newCount - lastCount
          val deltaTime = newTime - lastTime
          if (deltaTime > 0) {
            val rate = deltaCount / (deltaTime / 1000f)
            val weightedRate = rate * learningRate + lastRate * (1 - learningRate)
            Swing.onEDT {
              //text = String.format("%.2f", weightedRate) }
              text = weightedRate.toFloat.formatted("%.2f")
            }
            lastTime = newTime
            lastCount = newCount
            lastRate = weightedRate
          }
        })
      }
    }
    contents ++= Seq(rateLabel, Swing.HStrut(10), rate)
  }

  //val stepButton  = Button("Step")  { computeGraph.step }
  //val runButton   = Button("Run")   { computeGraph.run }
  val stopButton  = Button("Stop")  { computeGraph.stop }
  val resetButton = Button("Reset") {
    computeGraph.reset
    ratePanel.lastTime = System.currentTimeMillis()
    ratePanel.lastCount = 0L
    ratePanel.lastRate = 0.0
  }

  val cycleCountDriver =
    new Thread(new CycleCountDriver(udpateIntervalMSec)).start()

  //  contents ++= Seq(stepButton, runButton, stopButton, resetButton,
//    Swing.HStrut(10), counterPanel,
//    Swing.HStrut(10), ratePanel,
//    Swing.HGlue)


  /////////////////////////////////////////////////////////////////////////////
  // A few additional buttons and a button re-ordering for the August demo.  //

  val stepBox = new TextField("0", 5)
  stepBox.verifier = (s: String) => s.toCharArray.forall(_.isDigit)
  stepBox.horizontalAlignment = Alignment.Right
  stepBox.maximumSize = stepBox.preferredSize
  val runButton = Button("Run") {
    stepBox.text.toInt match {
      case 0 => computeGraph.run
      case x: Int => computeGraph.step(x)
      //case _ => throw new RuntimeException("Bad user input in stepcount box")
    }
  }
  // TODO These are synchronous 'step' calls and will hang the UI
  // Move to background worker thread
  val step1Button     = Button("Step 1")(computeGraph.step)
  val step10Button    = Button("10")(    computeGraph.step(10))
  val step100Button   = Button("100")(   computeGraph.step(100))
  val step1000Button  = Button("1000")(  computeGraph.step(1000))
  val step10000Button = Button("10000")( computeGraph.step(10000))

  contents ++= Seq(runButton, stepBox, stopButton, step1Button, step10Button,
    step100Button, step1000Button, step10000Button, resetButton,
    Swing.HStrut(20), counterPanel)

  contents collect { case b: Button =>
    b.opaque = false
    b.contentAreaFilled = false
    b.borderPainted = false
    b.margin = new Insets(2, 2, 2, 2)
  }

  /** A Runnable that periodically updates the simulation tick counter and
    * frequency panels.
    * @param updateIntervalMSec How many milliseconds to wait between updates
    *                           to counter and rate panels.
    */
  class CycleCountDriver(updateIntervalMSec: Int) extends Runnable {
    override def run() {
      val CycleCountUpdateIntervalMSec = udpateIntervalMSec
      while (true) {
        counterPanel.counter.update()
        ratePanel.rate.update()
        Thread.sleep(CycleCountUpdateIntervalMSec)
      }
    }
  }

}
