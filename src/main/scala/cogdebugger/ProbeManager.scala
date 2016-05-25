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

import libcog.{ComputeGraph, AbstractFieldMemory, FieldMemory}
import cogx.runtime.debugger.ProbedField

class ProbeManager(graph: ComputeGraph) {
  private val probes = scala.collection.mutable.Set[ProbedFieldProbe]()
  def addProbe(probe: ProbedFieldProbe) { probes += probe };    def +=(probe: ProbedFieldProbe) { addProbe(probe) }
  def removeProbe(probe: ProbedFieldProbe) { probes -= probe }; def -=(probe: ProbedFieldProbe) { removeProbe(probe) }
  def clear() { probes.clear() }
  def readProbes() {
    for (probe <- probes)
      if (!probe.isBusy)
        //graph.read(probe.target, probe.memory, probe.publishProbeData(_, _, graph.simStep))
        // TODO Need to get simulation time associated with this probe data
        graph.read(probe.target, probe.memory, probe.publishProbeData(_, _, 0L))
  }
  def reset() { probes foreach (probe => probe.reset()) }

  val probeDriver = new ProbeDisplayDriver(graph, 20)
  val probeDriverThread = new Thread(probeDriver)
  probeDriverThread.setName("ProbeDriverThread")
  probeDriverThread.start()

  /** Drives Cog's probing mechanism by making regular calls to a
    * [[cogdebugger.ProbeManager]]'s readProbes method. */
  class ProbeDisplayDriver(computeGraph: ComputeGraph,
                           @volatile var updatesPerSec: Double)
    extends Runnable {
    @volatile var running = true
    val MaxSleepTimeMS = 1000       // Min response time to GUI change of probeUpdatesPerSec
    override def run() {
      var timeAlreadySlept = 0L
      try {
        while (!Thread.interrupted() && running) {
          val interProbeTime = (1000.0 / updatesPerSec).toLong
          val remainingSleepTime = interProbeTime - timeAlreadySlept
          val thisSleepTime = math.min(remainingSleepTime,MaxSleepTimeMS)
          if (thisSleepTime > 0) {
            try { Thread.sleep(thisSleepTime) } catch { case e: InterruptedException => return }
            timeAlreadySlept += thisSleepTime
          }
          if (timeAlreadySlept >= interProbeTime) {
            readProbes()
            timeAlreadySlept = 0
          }
        }
      } finally { running = false }
    }
  }

}

sealed trait ProbeEvent
case class NewProbeData(src: ProbedField, data: AbstractFieldMemory, simTick: Long) extends ProbeEvent
case object ProbeReset extends ProbeEvent

trait ProbeListener {
  def isBusy: Boolean
  def notify(event: ProbeEvent): Unit
}

abstract class Probe(val target: AnyRef) {
  protected val subscribers = collection.mutable.Buffer[ProbeListener]()
  def isBusy = subscribers.exists(sub => sub.isBusy)
  def subscribe(subscriber: ProbeListener) { subscribers += subscriber }
  def unsubscribe(subscriber: ProbeListener) { subscribers -= subscriber }
  def publish(evt: ProbeEvent) { subscribers foreach (sub => sub.notify(evt)) }
  def reset() { publish(ProbeReset) }
}

class ProbedFieldProbe(override val target: ProbedField) extends Probe(target) {
  val memory = FieldMemory.direct(target.fieldType)
  def publishProbeData(src: ProbedField, data: AbstractFieldMemory, time: Long) {
    require(src eq target)
    publish(NewProbeData(src, data, time))
  }
}