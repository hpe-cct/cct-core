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

package cogdebugger.ui.fieldvisualizations

import cogx.runtime.debugger.ProbedField
import libcog._
import cogdebugger.{ToolFactory, OneOfNProperty, RestorableState}
import cogdebugger.ui.components.{LoadingAnimationPanel, PanPane, ToolBar}
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.xml.Node
import scala.swing._
import scala.language.postfixOps

/** A high-level visualization container that lets a user choose switch between
  * available visualization options for a particular field.
  *
  * Besides a target field, this class also requires as arguments a list of
  * user-friendly visualization names and a factory that uses those names as
  * keys to produce an actual Viewer instance. The superpanel itself is
  * presented as a BorderPanel with a toolbar at the top and the visualization
  * in the center. The selectable viewer's names are presented to the user in a
  * ComboBox installed in the toolbar after any visualiztion specific controls
  * (which are installed if a given Viewer implements the ToolbarItems trait).
  * This combobox will only appear if there is more than one available
  * visualization.
  *
  * The first visualization named in the `options` argument is used as the
  * default and will be immediately installed in the superpanel as part of
  * initialization.
  *
  * While `viewerFactory` can be any map from Strings to Viewers, consider
  * making it some sort of lazy or memoized map, particularly if any of the
  * Viewer instances has substantial startup time. [[cogdebugger.Memoize#apply]]
  * [[cogdebugger.Memoize]]
  *
  * This class was designed with the expectation that we'll wish to implement
  * a subclass specific to each particualar type of field. A companion object
  * with a straightforward factory method can make the instantiation of the
  * subclass cleaner. E.g., for ColorFields (which currently only have one
  * visualization):
  *
  * {{{
  * class ColorFieldSuperPanel protected (target: ProbedField,
  *                                       options: List[String],
  *                                       factory: (String) => Viewer)
  *   extends ViewerSuperPanel2(target, options, factory)
  *
  * object ColorFieldSuperPanel {
  *
  *   val ColorPanelName = "Color Image"
  *   val viewerNames = List(ColorPanelName)
  *
  *   def apply(target: ProbedField) = {
  *     //require( // Check for ColorField //, "Not a ColorField")
  *     val fieldShape  = target.fieldType.fieldShape
  *     val tensorShape = target.fieldType.tensorShape
  *     val memoizer = Memoize[String, EventDrivenViewer] {
  *       case ColorPanelName => new ColorFieldMemoryView(target, fieldShape) with MouseDragZoom
  *     }
  *     new ColorFieldSuperPanel(target, viewerNames, memoizer)
  *   }
  * }
  * }}}
  *
  * Created by gonztobi on 4/10/2014.
  *
  * @param target ProbedField being visualized.
  * @param options User-presentable strings corresponding to the visualizations
  *                available for the `target` field.
  * @param viewerFactory A map from the Strings in `options` to their
  *                      respective visualization instances.
  */
class ViewerSuperPanel(target: ProbedField,
                        options: List[String],
                        viewerFactory: (String) => Viewer)
  extends BorderPanel
  with EventDrivenViewer
  with Zoomable
  with RestorableState
{

  /** Visualizations are created on demand; at any given point in time there
    * may be an available visualization that hasn't been instantiated yet.
    * This set helps us track which visualizations have actually been built so
    * that we can save their state to a file when the debugger closes or
    * restore from a file when they're recreated in a subsequent session. */
  private val instantiated = collection.mutable.Set.empty[Viewer]
  private val restored = collection.mutable.Set.empty[Viewer]

  // Default view is the first in the list.
  protected var currentView: Viewer = viewerFactory(options.head)
  instantiated += currentView

  protected var lastData: AbstractFieldMemory = null
  protected var lastTime: Long = 0L

  val selectedViewerProperty = new OneOfNProperty("viewselection", options.head, options)
  selectedViewerProperty.action = Action("Change Display") {
    changeDisplay(selectedViewerProperty.selection)
  }
  properties += selectedViewerProperty

  /** A toolbar that hosts common controls for the visualizations installed
    * as children in this SuperPanel, as well as the combo box that allows
    * switching between different visualizations. */
  val toolbar = new ToolBar("Field Visualization") {
    val zoomInButton  = Button("+")(zoomIn())
    val zoomOutButton = Button("-")(zoomOut())
    val displayBox = ToolFactory.comboBox(selectedViewerProperty)
    contents += zoomInButton
    contents += zoomOutButton
    contents += Swing.HGlue
    if (options.size > 1) contents += displayBox
    floatable = false
    border = Swing.EmptyBorder
  }

  /** Rebuild the toolbar, populating it with the standard controls (for
    * zooming and switching views) as well as any controls defined by the
    * current view. Used when switching views. */
  private def refreshToolbar() {
    toolbar.contents.clear()
    toolbar.contents += toolbar.zoomInButton
    toolbar.contents += toolbar.zoomOutButton
    currentView match {
      case hasItems: ToolbarItems =>
        val flattened = hasItems.toolbarComponents.foldLeft(Seq.empty[Component]) {
          (listSoFar, nextCompGroup) =>
            (listSoFar :+ Swing.HStrut(10)) ++ nextCompGroup.components.toSeq
        }
        toolbar.contents ++= flattened
      case _ =>
    }
    toolbar.contents += Swing.HGlue
    if (options.size > 1) toolbar.contents += toolbar.displayBox
    toolbar.revalidate()
  }

  private val scroller = new PanPane(currentView)
  scroller.border = Swing.EmptyBorder

  // Install toolbar and default visualization.
  layout(toolbar) = BorderPanel.Position.North
  layout(scroller) = BorderPanel.Position.Center
  refreshToolbar() // Make sure current view's toolbar items are installed

  /** Change the currently active display. Also adds/removes some controls
    * from the toolbar as appropriate (e.g. adds zoom buttons for Zoomable
    * views or removes them for views that cannot be zoomed).
    *
    * @param key Key into the `displayOptions` map for the desired view.
    */
  def changeDisplay(key: String) {
    // Building and updating the new view could take a while, so we do it off
    // the Event-Dispatch Thread with a future.
    // ---
    // It occurs to me that this introduces the possiblity of a race - we
    // changeDisplay rapidly, we'll spawn a bunch of futures, all of which
    // will on completion try to install the view they built. Whoever finishes
    // last wins. Is there a way to safely cancel a future if another
    // changeDisplay comes in the meantime? For now, I'm just going to disable
    // the combobox while switching views (I suspect I'll be asked to revert
    // this as soon as somebody switches to an expensive view, decides they'd
    // rather not wait, and wants to change back).
    toolbar.displayBox.enabled = false
    val progressBar = new LoadingAnimationPanel("Switching visualizations...")
    layout(progressBar) = BorderPanel.Position.Center
    revalidate(); repaint()
    val f = Future {
      val newView = viewerFactory(key)
      // We don't update hidden views in the background, so we probably have to
      // update/redraw the newly switched to view.
      if (lastData != null)
        newView.update(target, lastData, lastTime)
      newView
    }

    f.onComplete {
      case Success(newView) => Swing.onEDT {

        // If this is the first time we've switched to this particular
        // visualization, restore any state saved from a previous debugger
        // session.
        restoreIfNecessary(newView)
        instantiated += newView
        currentView = newView

        // Add/remove zoom buttons as necessary
        currentView match {
          case zoomable: Zoomable =>
            toolbar.zoomInButton.visible = true
            toolbar.zoomOutButton.visible = true
          case _ =>
            toolbar.zoomInButton.visible = false
            toolbar.zoomOutButton.visible = false
        }

        //        toolbar.subPanelTools.contents.clear()
        //        currentView match {
        //          case tbi: ToolbarItems =>
        //            println("Switched to viewer with toolbar items")
        //            val tools = tbi.toolbarComponents
        //            val flattened = tools.foldLeft(Seq.empty[Component]) {
        //              (listSoFar, nextGroup) =>
        //                (listSoFar :+ Swing.HStrut(10)) ++ nextGroup.components.toSeq
        //            }
        //            toolbar.subPanelTools.contents ++= flattened
        //          case _ =>
        //        }
        //        toolbar.revalidate()
        refreshToolbar()

        scroller.contents = currentView
        //layout(currentView) = BorderPanel.Position.Center
        layout(scroller) = BorderPanel.Position.Center
        revalidate(); repaint()
        toolbar.displayBox.enabled = true
      }

      case Failure(throwable) =>
        Console.err.println("Debugger error: Failed to build visualization "+key)
        Console.err.println("  "+throwable.getMessage)
        toolbar.displayBox.enabled = true
    }

  }

  /** Used for really lousy flow control. Basically, when it comes time to
    * update the current view, we create a new future with currentView's update
    * method as the body and save a reference to the future here. When that
    * future completes, it sets this reference to null. Thus, as long as this
    * reference is non-null, we know there's a currently running update op
    * in the background and we won't issue another. */
  @volatile
  private var currentUpdateOp: Future[Unit] = null

  def updateCurrentView(data: AbstractFieldMemory, step: Long) {
    if (currentView != null && currentUpdateOp == null) {
      //currentView.update(target, newData, step) // old impl, runs on EDT
      currentUpdateOp = scala.concurrent.Future(currentView.update(target, data, step))
      currentUpdateOp.onComplete {
        case Success(_) => currentUpdateOp = null
        case Failure(_) => currentUpdateOp = null
      }
    }
  }

  /** Updates the visualization based on the contents of `data`. */
  def update(newData: AbstractFieldMemory, step: Long) {
    updateCurrentView(newData, step)
    lastData = newData
    lastTime = step
  }

  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
    update(data, simTime)
  }

  override def zoomIn() {
    currentView match {
      case z: Zoomable => z.zoomIn()
      case _ =>
    }
  }

  override def zoomOut() {
    currentView match {
      case z: Zoomable => z.zoomOut()
      case _ =>
    }
  }

  /** Encode the state of this panel into an XML tag. */
  def save =
  // TODO Copy old saved state, but overwrite with details of current session.
  // Copy saved state of viewers that didn't get instantiated in this
  // session, but did in the last.
    <ViewerSuperPanel>
      <SubPanels>{
        instantiated collect { case restorable: RestorableState => restorable.save }
        }</SubPanels>
    </ViewerSuperPanel>

  protected var savedState: Node = null
  def restore(tag: Node) {
    savedState = (tag \ "ViewerSuperPanel").head
    restoreIfNecessary(currentView)
  }

  /** Restores a viewer if `savedState` is not null and we haven't already
    * called this method for the viewer.
    * @param viewer Target viewer for the saved state restore attempt.
    */
  private def restoreIfNecessary(viewer: Viewer) {
    if (!restored.contains(viewer) && savedState != null) {
      viewer match {
        case restorable: RestorableState =>
          if (Cog.verboseDebugger)
            println("[ViewerSuperPanel] Restoring viewer: "+viewer+" ("+viewer.getClass.getSimpleName+")")
          restorable.restore(savedState \ "SubPanels" head)
        case _ =>
      }
    }
    restored += viewer
  }
}
