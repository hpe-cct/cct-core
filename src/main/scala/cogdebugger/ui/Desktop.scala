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

import scala.swing._
import scala.xml.{Elem, Node}
import scala.language.postfixOps
import libcog._
import cogdebugger._
import cogx.runtime.debugger.ProbedField
import cogdebugger.coggui3ports.{FramePackedEvent, ProbeDesktop, WrappedInternalDesktopPane}
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.structure.{EdgeLeftClick, EdgeRightClick, VertexLeftClick, VertexRightClick}
import org.interactivemesh.scala.swing.InternalFrame
import org.interactivemesh.scala.swing.event.InternalFrameClosing

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/14/13
 * Time: 3:27 PM
 */

/** A desktop that hosts windows/frames that in turn host visualizations for
  * fields.
  *
  * The Desktop reacts to [[cogdebugger.ui.structure.InteractiveGraphEvent]]s
  * (produced by one of the classes in cogdebugger.ui.structure) that indicate
  * that a user is interested in some field. Please remember to actually
  * subscribe this class to an event source! In response to an event, a window
  * is launched on the Desktop that contains some manner of visualization of
  * the data within the field. Windows can be resized, repositioned, and closed
  * by users.
  *
  * If requested, the Desktop can encode the state of any open visualizations
  * (size, position, installed Viewer implementation) into an XML tag. If
  * peristed to disk, this tag can then later be used to restore the state of
  * the Desktop in a subsequent debugger session.
  *
  * @param moduleHierarchy A tree describing the module hierarchy of the
  *                        model being debugged.
  * @param probeManager The ProbeManager responsible for tracking and reading
  *                     probes attached to the model being debugged.
  */
class Desktop(moduleHierarchy: ModuleHierarchyTree, probeManager: ProbeManager)
    extends ProbeDesktop(probeManager)
    with RestorableState {
  import Desktop.Log

  private val probableToViewerMap = new Util.IdentityHashMap[AnyRef, InternalFrame]()

  override protected lazy val desktop = new WrappedInternalDesktopPane {
    override def paintComponent(g: scala.swing.Graphics2D) {
      super.paintComponent(g)
      g.setColor(java.awt.Color.GRAY)
      val sBounds = g.getFontMetrics.getStringBounds(Desktop.copyrightString, g)
      g.drawString(Desktop.copyrightString,
        (desktopWidth - sBounds.getWidth.toFloat) / 2,
        desktopHeight - sBounds.getHeight.toFloat - 10)
    }
  }

  reactions += {

    ///////////////////////////////////////////////////////////////////////////
    // Respond to events from some compute graph structure viewer (e.g. the
    // NetworkViewer or ButtonsPanel)

    // Current behavior for left-clicks is to bring any viewer currently
    // associated with the click's target to the foreground, rather than
    // open a second viewer for it.

    case VertexLeftClick(v) =>
      probableToViewerMap.get(v) match {
        case Some(viewer) => viewer.selected = true
        case None => buildViewerFor(v)
      }
    case VertexRightClick(v) =>
      probableToViewerMap.get(v) match {
        case Some(viewer) => viewer.selected = true
        case None =>
      }
    case EdgeLeftClick(v) =>
    case EdgeRightClick(v) =>

    ///////////////////////////////////////////////////////////////////////////
    // De-register event-driven probe frames from their event sources.

    case InternalFrameClosing(frame, param) =>
      frame.contents.collect { case panel: EventDrivenViewer =>
        // Un-couple this viewer from its probes (otherwise the next click on
        // an associated field will try to foreground this dead viewer rather
        // than open a new once)
        probableToViewerMap --= probableToViewerMap.collect { case (key, `frame`) => key }
        //panel.myNotifiers.foreach(panel.deafTo(_))
//        panel.unsubscribeAll()

        // We need to destroy each probe attached to the closing viewer.
      }

      frame match {
        case f: DesktopFrame =>
          Log.d("A frame is closing. Destroying its probes.")
          f.probes foreach { probe => probeManager -= probe }
        case _ =>
      }

      deafTo(frame)

    ///////////////////////////////////////////////////////////////////////////

  }

  /** Constructs a Viewer for the target field, installs it in a frame, and
    * adds that frame to the desktop. */
  def buildViewerFor(target: AnyRef) {
    target match {
      case pf: ProbedField =>

        def addViewer(title: String, viewer: Viewer) =
          this.addViewer(title, viewer, pf)

        val title = pf.simpleName + " " + pf.fieldType
        pf.fieldType.elementType match {
          case Float32 =>
            (pf.fieldType.dimensions, pf.fieldType.tensorOrder) match {
              case (f, 0) if f < 4 => addViewer(title, ScalarFieldSuperPanel(pf))
              case (f, 1) if f < 3 => addViewer(title, new VectorFieldSuperPanel(pf))
              case (f, 2) if f < 4 => addViewer(title, MatrixFieldSuperPanel(pf))
              case (f, t) => Console.err.println(s"[Desktop] No viewers " +
                      s"defined for float fields of field dimension $f and " +
                      s"tensor order $t")
            }
          case Complex32 =>
            (pf.fieldType.dimensions, pf.fieldType.tensorOrder) match {
              case (2, 0) => addViewer(title, new ComplexFieldSuperPanel(pf))
              case (f, 1) if f < 3 => addViewer(title, ComplexVectorSuperPanel(pf))
              case (f, t) => Console.err.println(s"[Desktop] No viewers " +
                      s"defined for complex float fields of field " +
                      s"dimension $f and tensor order $t")
            }
          case Uint8Pixel =>
            (pf.fieldType.dimensions, pf.fieldType.tensorOrder) match {
              case (2, 1) => addViewer(title, ColorFieldSuperPanel(pf))
              case (f, t) => Console.err.println(s"[Desktop] No viewers " +
                      s"defined for byte3pixel fields of field dimension $f " +
                      s"and tensor order $t")
            }
          case e => Console.err.println(s"[Desktop] No viewers defined for " +
                  s"fields of type $e")
        }
      case o => Console.err.println(s"[Desktop] No viewers defined for " +
              s"fields of type $o")
    }
  }


  /** Builds a [[cogdebugger.ui.DesktopFrame]] with the given `title` to host
    * the given `viewer` and adds it to the desktop. If the viewer is an
    * EventDrivenViewer, this method will create a [[cogdebugger.Probe]] to
    * drive it and subscribe the viewer to the probe before making the frame
    * visible.
    *
    * All frames created by this method are listened to by the
    * [[cogdebugger.coggui3ports.Tiler]]. As soon as they pack themselves,
    * the Tiler will attempt to position them such they don't overlap any
    * existing panels (note that this position is immediately overriden by
    * restoreFrame if the viewer was added as part of the Desktop's restore
    * process).
    *
    * @param title Title for the desktop frame that will be shown
    * @param viewer The viewer to be installed in a frame and shown on the
    *               desktop
    * @param viewerTarget The ProbedField the viewer is visualizing
    */
  protected def addViewer(title: String, viewer: Viewer, viewerTarget: ProbedField) = {

    // We shouldn't be able to open a second viewer for a field that's
    // already probed.
    require(!probableToViewerMap.contains(viewerTarget),
      "Trying to create a new viewer for a field that's already being viewed.\n" +
              "We should have popped the existing viewer to the front instead.")

//    probableToViewerMap.get(viewerTarget) match {
//      case Some(viewer) =>
//        viewer.selected = true
//        //return
//      case None =>
//    }

    val viewerFrame = new DesktopFrame(maxWidth=desktopWidth,
                                       maxHeight=desktopHeight)
    tiler.listenTo(viewerFrame.frame)
    viewerFrame.title = title
    viewerFrame.contents = viewer
    viewerFrame.resizable = true
    viewerFrame.iconifiable = true
    viewerFrame.maximizable = true
    viewerFrame.closable = true
    viewerFrame.pack

    // Subscribe event-driven viewers to their event sources.
    viewer match {
      case eventDrivenView: EventDrivenViewer =>
        // Build a probe for this field and hook it up to the viewer.
        val probe = viewerTarget match {
          case pf: ProbedField => new ProbedFieldProbe(pf)
          case x => throw new RuntimeException("Don't know how to probe target: "+x)
        }
        probe.subscribe(eventDrivenView)
        viewerFrame.probes += probe
        probeManager += probe
        probableToViewerMap(viewerTarget) = viewerFrame
      case _ =>
    }

    listenTo(viewerFrame.frame) // Watch for frame closing events
    this.addFrame(viewerFrame)

    viewerFrame
  }

  /** Produces an XML element describing the state of the Desktop, with the
    * intent that this tag will be saved to disk and later used during the
    * startup of a subsequent debugger session to arrange any open
    * visualizations just as the user had left them.
    *
    * The desktop itself cannot save much information aside from which fields
    * are being visualized and the sizes and postitions of thier visualizations
    * on the desktop, as the inner details of any given viewer are unknown to
    * the desktop. Thus, individual Viewer implementations must also be able to
    * save to (and restore from) XML details about their internal state if
    * the user's debugger environment is to actually remain consistent across
    * runs. Each Viewer that implements [[cogdebugger.RestorableState]] will
    * have its own `save` function called and its tag nested within Desktop's.
    * The viewer's tag will be used as an argument to its `restore` method the
    * next time the debugger is launched (for the same app).
    *
    * The tag produced by this function appears thusly:
    * {{{
    * <Desktop>
    *   <frame>
    *     <layout>
    *       <size width=... height=.../>
    *       <position x=... y=.../>
    *     </layout>
    *     <probes>
    *       <probe>#Name of field being probed#</probe>
    *       ...
    *     </probes>
    *     <visualization>
    *       <viewerclass class=.../>
    *       #Installed viewer's XML tag goes here#
    *     </visualization>
    *   </frame>
    *   ...
    * </Desktop>
    * }}}
    */
  def save: Elem =
    <Desktop>
      { desktop.contents.collect {
        case df: DesktopFrame =>
          <frame>
            { RestorableState.layoutTag(df.bounds) }
            <probes>
              { df.probes.map(p =>
              <probe elementType={p.target.fieldType.elementType.getClass.getSimpleName}>{
                p.target match {
                  case pf: ProbedField => pf.name.mkString(".")
                  case x => x.toString
                }
              }</probe>)
              }
            </probes>
            { df.contents match {
                case (v: Viewer) :: Nil =>
                  <visualization>
                    <viewerclass class={v.getClass.getSimpleName}/>
                    { df.contents collect { case rs: RestorableState => rs.save } }
                    { v.propertiesTag }
                  </visualization>
                case _ =>
            }}
          </frame>
      }}
    </Desktop>

  /** Restores the state of the desktop to that described in the given xml
    * node `savedState`. It's expected that `savedState` describes the sizes,
    * positions, and viewers installed in any frames left open on the desktop
    * the last time the debugger was shut down. This method will recreate those
    * windows with the same sizes, positions, and installed viewers.
    *
    * Note that any exception encountered while attempting to restore the
    * desktop will immediately abort the remainder of the operation. Any
    * visualizations that were successfully restored prior to the exception
    * will remain, but everything else is simply dropped.
    *
    * See [[cogdebugger.ui.Desktop#save]] for the expected format of the xml.
    */
  def restore(savedState: Node) {
    savedState \ "Desktop" match {
      case Seq(desktopNode @ <Desktop>{ _* }</Desktop>) =>
        try {
          (desktopNode \ "frame") foreach restoreFrame
        } catch {
          case x: Exception =>
            // indent error msg
            val msg = x.getMessage.split("\n").mkString("\n  ")
            println("[Desktop] Internal error while " +
                    "restoring probe windows:\n  "+msg)
        }
      case _ =>
    }
  }

  /** Attempts to restore an individual frame as described in an XML node.
    *
    * The XML node contains the long name of the field that was being
    * visualized. If the field still exists in the current compute graph (it
    * may have been removed or renamed in between debugger sessions), a
    * Viewer implementation is instantiated based on the class name saved in
    * the XML tag. If the Viewer implements [[cogdebugger.RestorableState]],
    * it's handed the `visualization` subtree/tag of the XML and can attempt to
    * restore its own saved state (see java.awt.Desktop#save for an example
    * of the XML). Lastly, the frame is sized and positioned according to the
    * details saved in the XML. */
  def restoreFrame(frameTag: Node) {
    (frameTag \ "probes" \ "probe") foreach(probeTag => {
      val probeName = probeTag.text
      val savedProbeElementType = (probeTag \ "@elementType").text

      if (Cog.verboseDebugger)
        cogdebugger.Util.Log.d("Restoring probe: "+probeName)

      moduleHierarchy.get(probeName) match {
        case Some(fieldNode) =>
          val probedField = fieldNode.wrappedField
          val elementType = probedField.fieldType.elementType.getClass.getSimpleName 
          if (elementType != savedProbeElementType) {
            Log.w("[Desktop] A probed field's element type has changed " +
                    "since last debugger invocation. Its viewer will not " +
                    "be restored.")
            return
          }

          val viewer = (frameTag \ "visualization" \ "viewerclass" \ "@class").text match {
            case Desktop.scalarSuperPanelClassName  => ScalarFieldSuperPanel(probedField)
            case Desktop.vectorSuperPanelClassName  => VectorFieldSuperPanel(probedField)
            case Desktop.matrixSuperPanelClassName  => MatrixFieldSuperPanel(probedField)
            case Desktop.complexSuperPanelClassName => ComplexFieldSuperPanel(probedField)
            case Desktop.colorSuperPanelClassName   => ColorFieldSuperPanel(probedField)
            case Desktop.complexVecSuperPanelClassName => ComplexVectorSuperPanel(probedField)
          }

          // Run the viewer's own restore routine, if it defines one.
          viewer match {
            case rs: RestorableState => rs.restore(frameTag \ "visualization" head)
            case _ => // Nothing to do
          }

          // Restore the viewer's saved size and position.
          frameTag \ "visualization" \ "properties" foreach (tag => viewer.xmlToProperties(tag))
          val frame = addViewer(probedField.simpleName+" "+probedField.fieldType, viewer, probedField)
          val (x, y, width, height) = RestorableState.readLayoutTag(frameTag \ "layout" head)
          frame.bounds = (x, y, width, height)

        case None => // Can't find the original ProbedField; no visualization
                     // to restore.
          Log.w("[Desktop] Warning: Failed to restore a field " +
                  "visualization - Can't find field "+probeName)
      }
    })
  }

}

/** An InternalFrame intended to hold an instance of one of our various Viewer
  * classes. It's been augmented to keep track of its associated probes and to
  * raise an event when it's packed itself (this event can be used by
  * [[cogdebugger.coggui3ports.Tiler]] to intelligently size and place the
  * frame on the desktop). */
class DesktopFrame(
        minWidth: Int = 100,  minHeight: Int = 100,
        maxWidth: Int = 1920, maxHeight: Int = 1200)
    extends InternalFrame {
  val probes = collection.mutable.Set[ProbedFieldProbe]()
  override def pack {
    super.pack
    // Enforce min and max size for windows
    val width  = minWidth max (size.width min maxWidth)
    val height = minHeight max (size.height min maxHeight)
    peer.setSize(new Dimension(width, height))
    peer.validate()
    frame.publish(FramePackedEvent(this))
  }
}

object Desktop {
  object Log {
    var verbosity = 3
    def e(msg: String) { if (verbosity > 0) Console.err.println(msg) }
    def w(msg: String) { if (verbosity > 1) Console.err.println(msg) }
    def i(msg: String) { if (verbosity > 2) Console.out.println(msg) }
    def d(msg: String) { if (verbosity > 3) Console.out.println(msg) }
  }

  // We need to have stable identifiers to be able to match against class
  // types (which we do when restoring from XML).
  val scalarSuperPanelClassName  = classOf[ScalarFieldSuperPanel].getSimpleName
  val vectorSuperPanelClassName  = classOf[VectorFieldSuperPanel].getSimpleName
  val matrixSuperPanelClassName  = classOf[MatrixFieldSuperPanel].getSimpleName
  val complexSuperPanelClassName = classOf[ComplexFieldSuperPanel].getSimpleName
  val complexVecSuperPanelClassName = classOf[ComplexVectorSuperPanel].getSimpleName
  val colorSuperPanelClassName   = classOf[ColorFieldSuperPanel].getSimpleName

  val copyrightString = "© Copyright 2016 Hewlett Packard Enterprise Development LP"

}