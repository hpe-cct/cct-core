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

package cogx.runtime.allocation

import cogx.runtime.resources.{GPU, Node, ComputeNode, Cluster}
import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.opencl.OpenCLPlatform
import cogx.runtime.allocation.circuit.partitioner.Partitioner
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel
import cogx.platform.types.AbstractKernel
import cogx.runtime.execution.Schedule

/** Manages allocation of all compute resources for the cluster. Its
  * responsibilities are:
  *
  * 1. Allocates ComputeNodes for the computation.
  *
  * 2. Spawns off a ComputeNodeServer for each compute node in the cluster.
  *
  * 3. Collects a list of resources available from each ComputeNodeServer.
  *
  * 4. Creates and destroys actors on the ComputeNodes.
  *
  * @author Greg Snider
  */
private[runtime]
object AllocateCluster {

  /** Allocate compute resources of the specified devices to the needs of the `circuit`.
    *
    * @param circuit The DAG of kernels to be evaluated.
    * @param platform The platform upon which the circuit is evaluated.
    * @param mode The specific machines and devices upon which the circuit is evaluated.
    */
  def apply(circuit: KernelCircuit, platform: OpenCLPlatform, mode: AllocationMode): Cluster = {

    // For now, the node that does the cluster allocation is always the head node.
    val hostname = "127.0.0.1"
    val headNode = new Node(hostname)

    val computeNodes = mode match {

      case AllocationMode.SingleGPU(deviceIndex) =>
        require(deviceIndex >= 0 && deviceIndex < platform.devices.length,
          s"Specified device index $deviceIndex out of range for platform with ${platform.devices.length} devices.")
        val gpu = new GPU(deviceIndex)
        gpu.circuit = circuit
        gpu.orderedKernels = Schedule(circuit)
        Array(new ComputeNode(hostname, Array(gpu)))

      case AllocationMode.MultiGPU =>
        Console.err.println("[AllocateCluster] Warning: Operating in experimental multi-GPU mode.")
        // A debugging aid; displays unpartitioned KernelCircuit as a directed
        // graph, but requires the Jung library be added as a dependency
        //CircuitVisualizer.apply(circuit)

        val gpus = Array.tabulate(platform.devices.length) { index => new GPU(index) }

        println("[DEBUG] Arbitraritly breaking up and placing the KernelCircuit. Expect horrible performance!")
        println("[DEBUG] Starting KernelCircuit partitioning...")
        val start = System.currentTimeMillis()
        val partitioner = new Partitioner(circuit)
        import partitioner.SubCircuitSpec
        val specs = for (gpu <- gpus) yield partitioner.addSubCircuitSpec() // Use all available GPUs
        var visitedCount = 0

        /** In an equal partitioning of a circuit amongst the GPUs, how many
          * kernels per gpu (rounded up) */
        val kernelsPerGPU = math.ceil(circuit.size.toDouble / gpus.size).toInt
        /** The subCircuit for the ith kernel  */
        def subCircuit(i: Int) = specs(i / kernelsPerGPU)

        def placeKernelToSubCircuit(k: AbstractKernel, subCkt: SubCircuitSpec) {
          println("Adding hyperkernel " + k + " to sub-circuit " + subCkt)
          subCkt.add(k)
          visitedCount += 1
        }

        // Place the kernels, splitting the kernels roughly evenly over the gpus,
        // making sure that the Recurrences and Actuators are placed to the same
        // SubCircuits as the kernels they are linked to "under the covers".

        circuit.traversePostorder {
          _ match {
            case rfk: RecurrentFieldKernel =>
              partitioner.getSubCircuitOf(rfk.recurrence.source) match {
                case Some(subCkt) =>            // Already placed
                  placeKernelToSubCircuit(rfk, subCkt)
                case None =>                    // Not already placed
                  val subCkt = subCircuit(visitedCount)
                  placeKernelToSubCircuit(rfk.recurrence.source, subCkt)
                  placeKernelToSubCircuit(rfk, subCkt)
              }
            case kernel =>
              partitioner.getSubCircuitOf(kernel) match {
                case Some(subCkt) =>            // Already placed, do nothing.
                case None =>                    // Not already placed
                  placeKernelToSubCircuit(kernel, subCircuit(visitedCount))
              }
          }
        }

        val subCircuits = partitioner.construct() // Generate the subcircuits, complete with proxies.
        for (i <- 0 until gpus.size) {
          gpus(i).circuit = subCircuits(i)
          // Kernel ordering for each sub-circuit must respect the global
          // circuit ordering, else deadlock may result.  With a smarter
          // GPUSupervisor scheduling approach, this may not be necessary.
          gpus(i).orderedKernels = Schedule(subCircuits(i), circuit)
        }
        val end = System.currentTimeMillis()
        println( "[DEBUG] KernelCircuit partitioning complete.")
        println(s"[DEBUG} Partitioning took ${end - start} ms")

        Array(new ComputeNode(hostname, gpus))

      case AllocationMode.Cluster =>
        throw new NotImplementedError("CLuster mode not implemented yet")
    }

    val cluster = new Cluster(headNode, computeNodes)

    cluster

  }
}

/** This is a debugging aid I built to show the partioned kernel circuits.
  * It uses Jung to build and display a graph, but that's not a dependency we
  * can keep in the core.
  *
  * This code has not been kept up-to-date with recent multi-output kernel
  * changes.
  *
  * */
//import cogx.platform.types.AbstractKernel
//import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel
//import org.apache.commons.collections15.Transformer
//import edu.uci.ics.jung
//import edu.uci.ics.jung.graph._
//import edu.uci.ics.jung.visualization._
//import scala.swing._
//
//object CircuitVisualizer {
//
//  trait Vertex
//  trait Edge
//  class KernelVertex(val kernel: AbstractKernel) extends Vertex
//  class ForwardEdge extends Edge
//  class RecurrentEdge extends Edge
//
//  def apply(cluster: Cluster) {
//    val circuits = for (node <- cluster.computeNodes; gpu <- node.gpus) yield gpu.circuit
//    val graphs = circuits map makeGraph
//    Swing.onEDT {
//      val panels = graphs map makePanel
//
//      val boxPanel = new BoxPanel(Orientation.Horizontal)
//      val comps = panels map Component.wrap
//      comps foreach (_.border = Swing.EtchedBorder)
//      boxPanel.contents ++= comps
//
//      val frame = new Frame
//      frame.title = "Sub-circuits"
//      frame.contents = boxPanel
//
//      frame.visible = true
//    }
//  }
//
//  def apply(circuit: KernelCircuit) {
//    val graph = makeGraph(circuit)
//    Swing.onEDT {
//      val panel = makePanel(graph)
//
//      val boxPanel = new BoxPanel(Orientation.Horizontal)
//      val comp = Component.wrap(panel)
//      comp.border = Swing.EtchedBorder
//      boxPanel.contents += comp
//
//      val frame = new Frame
//      frame.title = "Complete Circuit"
//      frame.contents = boxPanel
//
//      frame.visible = true
//    }
//  }
//
//  private def makePanel(graph: DirectedGraph[Vertex, Edge]) = {
//    val forest = new DelegateForest(graph)
//    val layout = new jung.algorithms.layout.KKLayout(forest)
//    layout.setSize(new Dimension(1280, 720)) // 720p default
//
//    val panel = new VisualizationViewer[Vertex, Edge](layout)
//
//    val gm = new control.DefaultModalGraphMouse()
//    gm.setMode(control.ModalGraphMouse.Mode.TRANSFORMING)
//    panel.setGraphMouse(gm)
//
//    val vertexLabeler = new Transformer[Vertex, String]() {
//      def transform(v: Vertex): String = v match {
//        case kv: KernelVertex =>
//          kv.kernel match {
//            case rfk: RecurrentFieldKernel => kv.kernel.name+" (ID: "+rfk.id+") [RFK]"
//            case k => kv.kernel.name + "(ID: "+k.id+")"
//          }
//          //kv.kernel.name
//        case _ => "Vertex"
//      }
//    }
//
//    val edgePainter = new Transformer[Edge, java.awt.Paint]() {
//      def transform(e: Edge): Paint = e match {
//        case e: RecurrentEdge => java.awt.Color.LIGHT_GRAY
//        case _ =>                java.awt.Color.BLACK
//      }
//    }
//
//    panel.getRenderContext.setVertexLabelTransformer(vertexLabeler)
//    panel.getRenderContext.setEdgeDrawPaintTransformer(edgePainter)
//    panel
//  }
//
//  private def makeGraph(circuit: KernelCircuit) = {
//    val graph = new DirectedSparseGraph[Vertex, Edge]()
//    val kernelToNode = collection.mutable.Map.empty[AbstractKernel, Vertex]
//    // Make all vertices
//    circuit.traversePostorder { kernel =>
//      val vertex = new KernelVertex(kernel)
//      kernelToNode(kernel) = vertex
//      graph.addVertex(vertex)
//    }
//    // Make all edges
//    circuit.traversePostorder { kernel =>
//      // Handle backward/recurrent edges
//      kernel match {
//        case rfk: RecurrentFieldKernel =>
//          if (rfk.recurrence != null) {
//            val edge = new RecurrentEdge
//            //print("adding a recurrent edge from driver: "+rfk.recurrence.name+"... ")
//            graph.addEdge(edge, kernelToNode(rfk.recurrence), kernelToNode(rfk))
//            //println("done.")
//          }
//        case _ =>
//      }
//      // Handle forward/output edges
//      for (output <- kernel.outputs) {
//        val edge = new ForwardEdge
//        //print("adding a forward edge from "+kernel.name+" to "+output.name+"... ")
//        graph.addEdge(edge, kernelToNode(kernel), kernelToNode(output))
//        //println("done.")
//      }
//    }
//    graph
//  }
//
//}