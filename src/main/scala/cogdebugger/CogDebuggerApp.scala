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

package cogdebugger

import libcog._
import cogdebugger.ui.CogDebugger
import scala.swing._
import org.interactivemesh.scala.swing.InternalFrame

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/11/13
 * Time: 2:39 PM
 */

/** A debugger that probes all fields in a compute graph.
  *
  * @param codeBody Code that implements the field computation for the
  *        compute graph.
  */
abstract class CogProbedDebuggerApp(codeBody: => Unit)
  extends CogDebuggerApp({
    val graph = new ComputeGraph() { codeBody }
    graph.probeAll
    graph
  })

/** Base class for Cog applications that make use of the Cog Browser for the
  * purpose of visualizing field contents. It works sort of like
  * SimpleSwingApplication, except that instead of defining some anonymous
  * subclass of a MainFrame, clients will define a ComputeGraph. */
abstract class CogDebuggerApp(computeGraph: ComputeGraph)
{
  /** Alternate constructor that builds the compute graph for the user. The
    * `probeAll` function does not work with this constructor.
    *
    * @param codeBody Code that implements the field computation for the
    *        compute graph.
    */
  def this(codeBody: => Unit) =
    this(new ComputeGraph() { codeBody })

  /** Alternate constructor that uses a ComputeGraph restored from a file. */
  def this(filename: String) = this(ComputeGraph.readFromFile(filename))

  def main(args: Array[String]) {

    // Establish default values for various debugger parameters; these may be
    // overridden by program arguments provided by users
    /** Learning rate of the leaky integrator that calculates an app's rate
      * of execution. */
    var learnRate = CogDebugger.Defaults.DefaultClockToolbarLearningRate
    /** Update interval (in milliseconds) between updates to the GUI's clock
      * displays. */
    var updateInterval = CogDebugger.Defaults.DefaultClockToolbarUpdateIntervalMSec

    // Parse arguments
    var currentArg = 0
    while (currentArg < args.length) {
      val arg = args(currentArg)
      arg match {
        case "--learnRate" =>
          val userLearnRate = args(currentArg + 1).toFloat
          if (userLearnRate > 0 && userLearnRate < 1)
            learnRate = userLearnRate
          currentArg += 1
        case "--updateInterval" =>
          val userUpdateInterval = args(currentArg + 1).toInt
          if (userUpdateInterval > 0)
            updateInterval = userUpdateInterval
          currentArg += 1
        case "-v" | "--verbose" =>
          CogDebuggerApp.verboseMode = true
        case "-h" | "--help" =>
          printUsage()
          System.exit(0)
        case _ =>
      }
      currentArg += 1
    }

    val graph = computeGraph // realize the computeGraph if it was declared lazy
    if (CogDebuggerApp.verboseMode) {
      println("[i] ComputeGraph realized")
      println("    Graph has "+graph.probedCircuit.flatten.size+" probed field(s).")
    }

    //graph.reset
    Swing.onEDT {

//      Thread.setDefaultUncaughtExceptionHandler(
//        new UncaughtExceptionHandler {
//          def uncaughtException(t: Thread, e: Throwable) {
//            Console.err.println("Uncaught exception on thread: "+t)
//            Console.err.println("Releasing OpenCL resources.")
//            graph.release
//          }
//        }
//      )
      val rawName = getClass.getSimpleName
      val appName = if (rawName endsWith "$")
        rawName.substring(0, rawName.length - 1)
      else
        rawName
      val cogVersion =
        " -- Cog X " + libcog.getClass.getPackage.getImplementationVersion

      val debugger = new CogDebugger(graph, appName + cogVersion, learnRate, updateInterval)
      if (CogDebuggerApp.verboseMode) println("[i] Debugger object initialized")

      debugger.cleanupOperation = Some(() => cleanupOperation())
      if (CogDebuggerApp.verboseMode) println("[i] User cleanup operation registered")

      for (viewer <- predefinedViewers) {
        val frame = new InternalFrame()
        frame.contents = viewer
        frame.iconifiable = true
        frame.resizable = true
        frame.maximizable = true
        frame.closable = true

        // Too early? In the old GUI, viewers didn't know their size until they
        // had been updated once (they needed some field data to inspect its
        // dimensions).
        frame.pack

        debugger.debuggerState map { state =>
          state.ui match {
            case Some(ui: Cog3StyleDebuggerUI) =>
              ui.probeDesktop.addFrame(frame)
              ui.probeDesktop.listenTo(frame.frame) // We want to know when this frame is closed.
            case _ =>
          }
        }

      }
      if (CogDebuggerApp.verboseMode) println("[i] Predefined viewers set up")

      val (hOffset, vOffset) = (50, 100)
      val screenSize = java.awt.Toolkit.getDefaultToolkit.getScreenSize

      // Default window position. Centers the window on the screen, and size it
      // so it takes up most of the available resolution
      debugger.location = new Point(hOffset, vOffset)
      debugger.size = new Dimension(screenSize.width - hOffset * 2, screenSize.height - vOffset * 2)

      // Attempt to restore pre-existing state. If this fails, we'll be left
      // in the default position set above.
      debugger.restore()
      if (CogDebuggerApp.verboseMode) println("[i] User config restore op compete")

      debugger.visible = true
      if (CogDebuggerApp.verboseMode) println("[i] Debugger set to visible")
    }
    //graph.print()
  }

  /** Any special application specific viewers should be listed in this set.
    * Eventually, we'll do better than just adding them to the desktop right
    * off the bat. */
  def predefinedViewers = Set[cogdebugger.ui.fieldvisualizations.EventDrivenViewer]()
  def cleanupOperation() {}

  /** Print command usage information to the console. */
  def printUsage(): Unit = {
    println("Cog Debugger App options:")
    println("--learnRate rate ......... Learning rate for the debugger's leaky")
    println("                           integrator that computes execution rate")
    println("--updateInterval count ... Wait count milliseconds between updates")
    println("                           to the debugger's clock displays")
    println("-v | --verbose ........... Enable CogDebugger debug messages")
    println("-h | --help .............. Print this help information and terminate")
    println("                           the program")
  }

}

object CogDebuggerApp {

  var verboseMode = false

  val cogdir = {
    val userDirStr = java.lang.System.getProperty("user.home")
    val userDir = new java.io.File(userDirStr)
    val cogDir = new java.io.File(userDir, ".cogexmachina")
    if (cogDir.exists() || cogDir.mkdir()) {
      Some(cogDir)
    } else {
      Console.err.println("Unable to find or create user '.cogexmachina' " +
                          "folder. Unable to save or restore GUI state.")
      None
    }
  }
}