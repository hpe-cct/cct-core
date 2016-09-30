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

import libcog._
import cogdebugger._
import cogdebugger.ui.structure.StructurePane
import cogdebugger.ui.MessagesPanel.{Message, ErrMsg}
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.swing._
import scala.xml.{XML, Elem, Node}
import javax.swing.filechooser.FileNameExtensionFilter
import scala.language.postfixOps

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/11/13
 * Time: 2:39 PM
 */

/** Implements the main window for the Cog Ex Machina Visual Debugging Tool.
  *
  * The debugger consists of two large views and a main toolbar:
  *
  * The first of the main views displays the structure of probed fields in a
  * compute graph. It can show data dependencies between fields by drawing the
  * actual graph, or more compactly just show the fields without their data
  * connections. Clicking a field in this first main view launches a probe
  * window on the second view.
  *
  * The second view serves as the home for probe windows. A probe window
  * visualizes the current state of a field in some manner (exactly how is
  * dependent on what type of field is being probed; some fields have several
  * visualizations that a user can switch between). Probe windows can be
  * rearranged, resized, and closed on this view.
  *
  * The main toolbar hosts controls for stepping, running continuously,
  * stopping, and resetting the ComputeGraph. It also hosts counters for
  * monitoring the current simulation time and rate of execution.
  *
  * ---
  *
  * Until we're past our demo in mid-August, the Cog 4 browser's toolbars
  * have been reworked to look more like they did in Cog 3, so that we don't
  * have to rewrite the whole programmer's manual. That mostly means no
  * menubar and different button placements on the toolbars.
  *
  * @param computeGraphAndAppName An (optional) ComputeGraph and window title
  *                               with which to immediately initialize the UI.
  *                               If None is provided, the user will be
  *                               prompted to select a ComputeGraph file using
  *                               a file browser.
  * @param clockToolbarLearningRate Learning rate for the leaky integrator that
  *                                 calculates the graph's rate of execution
  * @param clockToolbarUpdateInterval Interval between updates to the counters
  *                                   tracking simulation tick and tick rate
  */
class CogDebugger(computeGraphAndAppName: Option[(ComputeGraph, String)],
                  clockToolbarLearningRate: Float,
                  clockToolbarUpdateInterval: Int)
    extends MainFrame
    with RestorableState {
  import CogDebugger.Log

  /////////////////////////////////////////////////////////////////////////////
  // Alternate constructors ///////////////////////////////////////////////////

  def this(computeGraph: ComputeGraph,
           appName: String,
           clockToolbarLearningRate: Float,
           clockToolbarUpdateInterval: Int) =
    this(Some(computeGraph, appName), clockToolbarLearningRate, clockToolbarUpdateInterval)

  def this(computeGraph: ComputeGraph, appName: String) =
    this(computeGraph,
         appName,
         CogDebugger.Defaults.DefaultClockToolbarLearningRate,
         CogDebugger.Defaults.DefaultClockToolbarUpdateIntervalMSec)

  def this(computeGraph: ComputeGraph) =
    this(computeGraph, "Cog Browser v4")

  /////////////////////////////////////////////////////////////////////////////
  /////////////////////////////////////////////////////////////////////////////

  protected[cogdebugger] var debuggerState: Option[DebuggerState] = None

  // This was a hook intended mostly for cleanup of pre-defined user viewers,
  // which never really took off.
  var cleanupOperation: Option[() => Unit] = None

  computeGraphAndAppName match {
    case Some((graph, name)) =>
      val state = new DebuggerState(graph)
      val cogString = "Cog X " + libcog.getClass.getPackage.getImplementationVersion
      name match {
        case ""       => title = cogString
        case appName  => title = appName+" -- "+cogString; state.appName = Some(appName)
      }
      debuggerState = Some(state)
      buildUI(state)
    case None =>
      title = "Cog Browser v4"
      // App was started without
      // TODO Add prompt to load a ComputeGraph file
  }

  if (Cog.verboseDebugger)
    publish(Message("Gui initialized."))

  /** (Re-)Initializes the core components of the UI (run controls, network
    * viewer, probe desktop) and installs them in the main window.
    *
    * The old GUI components and any references to the old graph are destroyed.
    *
    * @param state The DebuggerState this GUI is associated with
    */
  protected def buildUI(state: DebuggerState) {
    // Create the message box early and register components with it as soon as
    // they're created so we don't miss messages. I'd like to give components
    // a handle to the messages box in their constructor so they can publish
    // messages as they initialize, but that breaks the publisher/subscriber
    // paradigm.
    val messages = new MessagesPanel()
    messages.listenTo(this)

    val toolbar = setupCGExecutionControls(state)

    val desktop = setupProbeDesktop(state)
    messages.listenTo(desktop)
    
    val structurePane = setupStructureViewer(state)

    // The graph view may not be available (undefined) if the computegraph
    // defines a large or complicated model...
    structurePane.network match {
      case Some(graphView) =>
        messages.listenTo(graphView)
        desktop.listenTo(graphView)
      case None =>
    }
    // ... however, the buttons view is always available.
    messages.listenTo(structurePane.buttons)
    desktop.listenTo(structurePane.buttons)

    val ui = new Cog3StyleDebuggerUI(
      toolbar,
      structurePane,
      desktop,
      messages
    )
    state.ui = Some(ui)

    //teardownUI() // Disconnect any old GUI components
    contents = ui
  }

  /** Disconnect event transport between any components in the current UI, if
    * it exists (else do nothing). */
  protected def teardownUI() {

    // Is this even necessary? Scala reactors use some kind of weak referencing
    // mechanism, so throwing away our references to debuggerState.ui might
    // be enough to see that everything is garbage collected.

    for (state <- debuggerState; ui <- state.ui) {
      ui match {
        case ui: Cog3StyleDebuggerUI => ui.msgPanel.deafTo(this)
        case _ =>
      }
    }
  }

  /** Builds a toolbar with controls for stepping the compute graph and
    * controlling the rate at which probes refresh. This toolbar is also home
    * to the cycle counter and cycles/sec readout.
    *
    * @return The GUI's main toolbar component
    */
  protected def setupCGExecutionControls(state: DebuggerState) = {
    // Set up toolbars
    val toolbar = new BoxPanel(Orientation.Horizontal)//new GridPanel(1, 0)
    val stepTools = new ClockToolbar(state.cg, clockToolbarLearningRate, clockToolbarUpdateInterval)
    val probeControls = new ProbeToolbar(state.pm)

    // A bit of finagling with components to get them to layout pretty-like.
    // There's got to be a better way to align these.
    //  val wrappers = for (comp <- Seq(stepTools, /*cycleDisplay,*/ probeControls))
    //    yield new BoxPanel(Orientation.Vertical) { contents += comp }
    stepTools.xLayoutAlignment = 0.0
    //cycleDisplay.xLayoutAlignment = 0.5
    probeControls.xLayoutAlignment = 1.0
    probeControls.contents.insert(0, Swing.HGlue)
    toolbar.contents ++= Seq(stepTools, Swing.HGlue, probeControls)//wrappers

    // More Cog-3-esque changes.
    stepTools.ratePanel.rate.preferredSize = new Dimension(80, 0)
    toolbar.contents ++= Seq(Swing.HStrut(20), stepTools.ratePanel, Swing.HStrut(10))

    toolbar.contents ++= {
      val buttons = ArrayBuffer[Button]()
      if (ComputeGraph.defaultCheckpointerType.isAvailable) {
        buttons += Button("Save...")( doSaveComputeGraph(state) )
        // Don't show the Restore button yet - it doesn't work even if the classes and libs are there
//        buttons += Button("Restore...")( doRestoreComputeGraph(state) )
      }
      buttons += Button("Platform")(publish(Message(platformDescription)))
      // Apply the more minimalist button style favored by the Cog 3 debugger
      // to every button on the toolbar
      buttons collect { case b: Button =>
        b.opaque = false
        b.contentAreaFilled = false
        b.borderPainted = false
        b.margin = new Insets(2, 2, 2, 2)
      }
      buttons
    }
    toolbar
  }

  protected def setupStructureViewer(state: DebuggerState) = {
    new StructurePane(state.cg)
  }

  /** Builds the ProbeDesktop. */
  protected def setupProbeDesktop(state: DebuggerState) = {
    new Desktop(state.mh, state.pm)
  }

  /** If an application name is defined, returns a handle to the directory
    * associated with the application (~/.cogexmachine/appnam). */
  private def getAppDir(state: DebuggerState): Option[File] = {
    for (dir   <- CogDebuggerApp.cogGuiDir;
         state <- debuggerState;
         name  <- state.appName) yield {
      new File(dir, Util.sanitizeString(name))
    }
  }

  private var lastSaveFile: Option[File] = None

  /** Build and return a new file chooser dialog box. This dialog can be used
    * to prompt a user to select files for saving/loading by calling the
    * showSaveDialog and showOpenDialog on the returned chooser.
    *
    * @param startDir The initial directory opened by the file chooser. The
    *                 user can navigate to other directories from here.
    * @param extDesc A brief description of the types of files this chooser is
    *                meant to locate
    * @param ext A number of file extensions on which to filter in the file
    *            chooser. Though the user can select to display files of all
    *            types in the chooser, by default it will only show files
    *            with one of the given extensions (in addition to directories)
    * @return
    */
  private def buildFileChooser(startDir: Option[File], extDesc: String, ext: String*) = {
    val chooser = new FileChooser(startDir.orNull)
    val filter = new FileNameExtensionFilter(extDesc, ext: _*)
    chooser.fileFilter = filter
    chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
    chooser.multiSelectionEnabled = false
    chooser
  }

  // For now, the debugger can be hardwired to use ComputeGraph's default save/restore technology.

  /** The file suffix (includes ".") for the save/restore technology. */
  private def suffix = ComputeGraph.defaultCheckpointerType.fileSuffix

  /** The file extension (excludes ".") for the save/restore technology. */
  private def extension = if (suffix.startsWith(".")) suffix.drop(1) else suffix

  /** Builds a file chooser for saving or loading ComputeGraphs. If a file has
    * been previously saved or loaded from, the chooser will start at the parent
    * directory of that file; else it will start in the same directory the
    * debugger uses to save its layouts across runs
    * (usually "~/.cogexmachina/[app_name]"). The file extension filter is set
    * to only display files with the suffix defined in
    * ComputeGraph.defaultCheckpointerType.fileSuffix.
    *
    * @param state A DebuggerState used as a hint to determine the chooser's
    *              starting directory in the event there have been no
    *              computegraph files recently saved/opened.
    * @return
    */
  private def buildSaveRestoreChooser(state: DebuggerState) = {
    val startDir =
      if (lastSaveFile.isDefined) lastSaveFile
      else getAppDir(state)
    buildFileChooser(startDir, "ComputeGraph state", extension)
  }

  /** Prompt the user with a save dialog to select a file into which to save
    * the current ComputeGraph, and if approved by the user, write the
    * computegraph to disk.
    *
    * @param state A DebuggerState used as a hint to determine the chooser's
    *              starting directory in the event there have been no
    *              computegraph files recently saved/opened.
    */
  def doSaveComputeGraph(state: DebuggerState) {
    val cg = state.cg
    val chooser = buildSaveRestoreChooser(state)
    val parentComp = if (!this.contents.isEmpty) this.contents(0) else null
    chooser.showSaveDialog(parentComp) match {
      case FileChooser.Result.Approve =>
        var saveFile = chooser.selectedFile
        val filePath = saveFile.getAbsolutePath
        if (!filePath.endsWith(suffix))
          saveFile = new File(filePath.concat(suffix))
        cg.write(filePath)
        lastSaveFile = Some(saveFile)
        publish(Message(s"Saved compute graph state to: ${saveFile.getName}"))
      case FileChooser.Result.Cancel  => // No action
      case FileChooser.Result.Error   => publish(ErrMsg("Failed to save file"))
    }
  }

  /** Prompt the user with an open dialog to load a saved computegraph file
    * from disk, and if approved, instantiate the computegraph and reinitialize
    * the GUI to display display its contents, throwing out anything pertaining
    * to the old.
    *
    * @param state A DebuggerState used as a hint to determine the chooser's
    *              starting directory in the event there have been no
    *              computegraph files recently saved/opened.
    */
  def doRestoreComputeGraph(state: DebuggerState) {
    val chooser = buildSaveRestoreChooser(state)
    val parentComp = if (!this.contents.isEmpty) this.contents(0) else null
    chooser.showOpenDialog(parentComp) match {
      case FileChooser.Result.Approve =>
        val userFile = chooser.selectedFile
        val newGraph = ComputeGraph.readFromFile(userFile.getAbsolutePath)
        val newState = new DebuggerState(newGraph)
        debuggerState = Some(newState)
        lastSaveFile = Some(userFile)
        buildUI(newState)
        publish(Message(s"Opened compute graph save in: ${userFile.getName}"))
      case FileChooser.Result.Cancel  => // No action
      case FileChooser.Result.Error   => publish(ErrMsg("Failed to open file"))
    }
  }

  override def closeOperation() {

    debuggerState map (_.cg.stop)

    // Hide the GUI immediately - it will *appear* to the user that shutdown
    // is faster. Should also stop further repaints, which spares us some
    // resources and hopefully prevents any open visualizations from trying to
    // reach into the compute graph for data to display.
    visible = false

    // Kill the probe driver thread
    // Wait at most X seconds for the probe driver to terminate.
    debuggerState map { state =>
      state.pm.probeDriver.running = false
      state.pm.probeDriverThread.join(5 * 1000)
    }

    saveApplicationState()

    releaseOpenCLResources()

    // Run any additional cleanup code provided by users.
    cleanupOperation map { op => op() }

    super.closeOperation()

  }

  protected def releaseOpenCLResources() {
    Log.i("CogDebugger closing. Releasing ComputeGraph resources.")
    debuggerState map (_.cg.release)
  }

//  class CogBrowserMenuBar extends MenuBar {
//    val fileMenu = new Menu("File"); fileMenu.mnemonic = event.Key.F
////    val saveItem = new MenuItem(Action("Save Model State")(publish(ErrMsg("TODO: Save model state"))))
////    saveItem.mnemonic = event.Key.S
////    val restoreItem = new MenuItem(Action("Restore Model State")(publish(ErrMsg("TODO: Restore model state"))))
////    restoreItem.mnemonic = event.Key.R
//    val exitItem = new MenuItem(Action("Exit")(closeOperation()))
//    exitItem.mnemonic = event.Key.X
//    //fileMenu.contents ++= Seq(saveItem, restoreItem, new Separator(), exitItem)
//    fileMenu.contents += exitItem
//
//    val viewMenu = new Menu("View"); viewMenu.mnemonic = event.Key.V
//    val messagesItem = new CheckMenuItem("Messages") {
//      selected = true
//      action = Action("Messages") {
//        if (this.selected) {
//          hSplit.topComponent = vSplit
//          mainPanel.layout(hSplit) = BorderPanel.Position.Center
//          mainPanel.revalidate()
//        } else {
//          mainPanel.layout(vSplit) = BorderPanel.Position.Center
//          mainPanel.revalidate()
//        }
//      }
//    }
//    viewMenu.contents += messagesItem
//
//    val helpMenu = new Menu("Help"); helpMenu.mnemonic = event.Key.H
//    val aboutItem = new MenuItem(Action("About"){
//      Dialog.showMessage(
//        mainPanel,
//        title="About",
//        message="Cog ex Machina 4.0\nHP Confidential    © Copyright 2008-2013 Hewlett-Packard Development Company, L.P.")
//    })
//    helpMenu.contents += aboutItem
//
//    //contents ++= Seq(fileMenu, viewMenu, helpMenu)
//    contents ++= Seq(fileMenu, helpMenu)
//  }

  /** Save any layout-related information of the current debugger session to
    * disk so that the debugger can be restored to its current state on a
    * subsequent run. Layout-relted information includes the size and position
    * of the debugger window, the location of any dividers for the various
    * panes, the size and position of probe frames open on the desktop, and
    * the state of any buttons or dropdowns on those probe frames.
    */
  protected def saveApplicationState() {
    for (dir <- CogDebuggerApp.cogGuiDir) {
      for (state <- debuggerState) {
        for (name <- state.appName) {
          val appDir = new File(dir, Util.sanitizeString(name))
          if (appDir.exists() || appDir.mkdir()) {
            val outputFile = new File(appDir, CogDebugger.guiConfigFileName)
            XML.save(outputFile.getAbsolutePath, save)
            Log.d("GUI state saved to directory: "+appDir.getPath)
            return
          } else {
            Log.e("Error saving GUI state (cannot make directory "+appDir.getPath+").")
            return
          }
        }
        Log.e("Failed to save GUI state: Application name is undefined (what to call the save file?)")
        return
      }
      Log.e("Failed to save GUI state: DebuggerState is undefined.")
      return
    }
    Log.e("Failed to save GUI state: Parent directory ~/.cogexmachina doesn't exist.")
    return
  }

  /** Describe the current debugger state in an XML element. This description
    * includes mostly layout-related information (size and position of the
    * debugger window and probe frames), but also includes the state of buttons
    * and dropdowns on most probe frames.
    *
    * @return The root XML node describing this debugger session.
    */
  def save: Elem =
    <CogDebugger>
      <MainWindow>
        { RestorableState.layoutTag(this.bounds) }
        {
        (for (state <- debuggerState; ui <- state.ui) yield {
          ui.save
        }).getOrElse(scala.xml.NodeSeq.Empty)
        }
      </MainWindow>
    </CogDebugger>

  /** Restore the debugger to a state defined by an XML file present on disk.
    * At present, state that can be restored is mostly related to probe frames:
    * which visualizations are open, their size and position, and the state
    * of their buttons and dropdowns.
    */
  def restore() {
    for (dir <- CogDebuggerApp.cogGuiDir;
         state <- debuggerState;
         appName <- state.appName) { // Need yield here?
      val appDir = new File(dir, Util.sanitizeString(appName))
      val guiConfigFile = new File(appDir, CogDebugger.guiConfigFileName)
      if (guiConfigFile.exists()) {
        val rootElem = XML.loadFile(guiConfigFile)
        //val pp = new scala.xml.PrettyPrinter(80, 2)
        //Log.d("Loaded GUI config file: "+guiConfigFile.getAbsolutePath)
        //Log.d(pp.formatNodes(rootElem))
        restore(rootElem)
      } else {
        Log.w("Couldn't find GUI config file to restore from.")
      }
    }
  }

  /** Restore the debugger to a state described in the given XML node. */
  def restore(savedState: Node) {
    (savedState \ "MainWindow" \ "layout" \ "position").headOption.foreach (pos => {
      RestorableState.restoreInt(pos \ "x" head)(x => location = new Point(x, location.getY.toInt))
      RestorableState.restoreInt(pos \ "y" head)(y => location = new Point(location.getX.toInt, y))
    })
    (savedState \ "MainWindow" \ "layout" \ "size").headOption.foreach(sz => {
      RestorableState.restoreInt(sz \ "width" head)(width => size = new Dimension(width, size.getHeight.toInt))
      RestorableState.restoreInt(sz \ "height" head)(height => size = new Dimension(size.getWidth.toInt, height))
    })
    for (state <- debuggerState; ui <- state.ui) {
      (savedState \ "MainWindow").headOption.foreach(node => {
        ui.restore(node)
      })
    }
  }

}

object CogDebugger {

  object Defaults {
    val DefaultClockToolbarLearningRate = 0.5f
    val DefaultClockToolbarUpdateIntervalMSec = 1000
  }

  /** The name of the file used by the debugger's session save/restore
    * facilities. */
  val guiConfigFileName = "gui.cfg"

  /** Change `verbosity` to 0 (or less) to disable all printing to console. */
  object Log {
    var verbosity = 4
    if (!Cog.verboseDebugger)
      verbosity = 0
    def d(msg: String) { if (verbosity > 3) Console.out.println(msg) }
    def i(msg: String) { if (verbosity > 2) Console.out.println(msg) }
    def w(msg: String) { if (verbosity > 1) Console.err.println(msg) }
    def e(msg: String) { if (verbosity > 0) Console.err.println(msg) }
  }
}
