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

import libcog.AbstractFieldMemory
import cogdebugger._
import scala.collection.mutable.ArrayBuffer
import scala.swing.Component
import scala.xml.{NodeSeq, Node}
import javax.swing.SwingWorker

/** Base trait for classes that visualize field data.
  *
  * One of the design goals for Viewers is to have any of their user
  * configurable options persist across invocations of the debugger. To this
  * end, all Viewers maintain a list of properties that wrap the values they
  * need to save across debugger sessions. Each [[cogdebugger.Property]] is
  * capable of generating an XML tag describing the state of that property,
  * and Viewers are capable of collecting those tags and nesting them inside
  * another tag specific to the Viewer instance.
  */
trait Viewer extends Component {

  /** A list of properties that should be persisted when the app closes, and
    * restored the next time it's launched. A common example of a persistent is
    * the zoom/magnification level of the viewer. Be sure to add any relevenant
    * properties to this list in your Viewer subclasses! */
  val properties = new ArrayBuffer[Property[_]]()

  /** Updates the visualization based on the contents of `data`.
    *
    * The `src` argument was orignally meant to reference the
    * kernel/field/object that generated the data, in order to support
    * composite visualizations (that is, viewers that produce a visual based on
    * the data from several different sources), but launching such viewers in
    * the current UI is clunky at beset, so this feature isn't used. Viewers
    * that only visualize a single field's data can probably safely ignore this
    * arument (and indeed, most of the current ones do).
    *
    * @param src The field or object that generated the `data` argument
    * @param data New field data that needs to be rendered by this viewer
    * @param simTime The ComputeGraph's step count at the time the `data`
    *                argument was generated
    */
  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long)

  /** Reset the visualization. An optional operation; subclasses must override
    * this method or else it does nothing. */
  def reset(): Unit = {}

  /** Returns the XML representation of this viewer's properties, suitable
    * for saving into a file. */
  def propertiesTag: NodeSeq = {
    if (properties.isEmpty) {
      NodeSeq.Empty
    } else {
      <properties>
        { properties.map(prop => prop.xmlTag) }
      </properties>
    }
  }

  /** Parses the XML tag produced by the `propertiesTag` method and restores
    * any saved valued to this Viewer. */
  def xmlToProperties(node: Node) {
    node match {
      case <properties>{ xmlPropTags @ _* }</properties> =>
        val propNameToXMLTag =
          Map(xmlPropTags.map(tag => (tag \ "@name").text -> tag): _*)
        for (property <- properties) {
          propNameToXMLTag.get(property.name) foreach (tag => property.restore(tag))
        }
      case _ =>
    }
  }

}

/** A field viewer that can be subscribed to an event source so as to reacively
  * update its visualizaion in response to new data becoming available.
  *
  * While you can call the viewer's `update` method directly, in the typical
  * usage scenario an EventDrivenViewer will be subscribed to a
  * [[cogdebugger.Probe]] that has been registered with the debugger's
  * [[cogdebugger.ProbeManager]]. The probe manager will read out the data
  * from any probed fields at regular intervals, raise NewProbeData events,
  * and the viewers will then update themselves. Note however, that the viewer
  * is NOT guaranteed to respond to all NewProbeData events, see more below.
  *
  * The task of visualizing a field can be an expensive operation. Updating
  * and rendering all viewers on a single thread makes for an unresponsive
  * debugger. For this reaon, each EventDrivenViewer contains a Scala Actor
  * that performs the `update` operation in response to probe date events
  * (calling `update` directly on this class does NOT use the actor). Because
  * there is no way of knowing in advance how long an update will take or the
  * rate at which NewProbeData events will be raised, the actor only ever
  * operates on the most recently received probe data, discarding any older
  * messages that may have enqueued in its mailbox. While the actor is busy
  * in the `update` method, the volatile `busy` variable will be set to true;
  * a Probe implemenation may take advantage of this to perform additional
  * flow control.
  *
  * ---
  *
  * The intent was to allow for a single viewer to listen to multiple probes,
  * so that it could compose their data in some way. The original motivation
  * for this was Ben+Matthew's Virage tracker, in which they overlayed a
  * moving target on a color image. The target's size, position and color was
  * determined by one field, and the background by another. I don't know
  * exactly what sort of hackery the resorted to go get that to work, but I
  * know it took more effort than it ought to have.
  *
  * I'd really like this to be an abstract class taking the visualization
  * targets as arguments, but then subclasses can't extend the different Panel
  * classes.
  */
trait EventDrivenViewer extends Viewer with ProbeListener {

  @volatile protected var busy = false
  def isBusy = busy

  def notify(event: ProbeEvent) {
    event match {
      case npd: NewProbeData =>
        //worker ! npd
        if (!busy) {
          busy = true
          new Worker(npd).execute()
        }
      case ProbeReset => reset()
    }
  }

  class Worker(data: NewProbeData) extends SwingWorker[Unit, Unit] {
    def doInBackground(): Unit = {
      update(data.src, data.data, data.simTick)
    }
    override def done(): Unit = {
      busy = false
    }
  }

}

// One of the problems with the above design is that while Viewers obviously
// have a GUI component to them, requiring that they extend some Component
// subclass prevents us from forcing them to take a field as an argument
// (that's what led to the awkward `targets` def in EventDrivenFieldViewer). It
// also made GUI save/restore a bit awkward, as extending Component means that
// the whole UI is built in a default state at construction time, then
// tweaked when it comes time to restore - it's easy for stuff to get desynched
// that way.
//
// I wonder if the better approach wouldn't have been composition, rather than
// inheritance... something like the below.
//import cogx.runtime.debugger.ProbedField
//import scala.xml.Node
//import scala.swing._
//abstract class FieldViewer[T <: AbstractFieldMemory](target: ProbedField,
//                                                     savedState: Option[Node])
//        extends ProbeListener {
//  val ui: Component // Probably best implemented as a lazy val in subclasses
//  def preRender(data: T)
//  /** Guaranteed to be executed on the Swing Event-Dispatch Thread.*/
//  def render(data: T)
//  def postRender(data: T)
//  def update(data: T) {
//    require(data.fieldType == target.fieldType)
//    preRender(data)
//    Swing.onEDT(render(data))
//    postRender(data)
//  }
//}
//abstract class EventDrivenFieldViewer[T <: AbstractFieldMemory](
//        target: ProbedField,
//        savedState: Option[Node])
//    extends FieldViewer[T](target, savedState) with ProbeListener {
//  def notify(event: ProbeEvent) {
//    event match {
//      case NewProbeData(src, data, time) =>
//        require(src eq(target))
//        update(data.asInstanceOf[T])
//      case ProbeReset =>
//    }
//  }
//}
