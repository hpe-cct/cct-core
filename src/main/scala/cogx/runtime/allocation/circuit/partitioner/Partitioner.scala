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

package cogx.runtime.allocation.circuit.partitioner

import cogx.compiler.codegenerator.KernelCircuit
import cogx.platform.types.{VirtualFieldRegister, AbstractKernel}
import cogx.cogmath.collection.{IdentityHashSet, IdentityHashMap}
import cogx.runtime.allocation.circuit.{OutputProxyKernel, InputProxyKernel}
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel

/** Utility class for breaking a KernelCircuit apart into disjoint
  * sub-circuits - useful for binding different parts of a computation to
  * different nodes in a distributed app.
  *
  * A Partitioner maintains a list of SubCircuitSpec objects, each of which
  * represents a portion of the complete circuit and maintains a list of the
  * AbstractKernels belonging to that sub-circuit.
  *
  * As kernels from the complete circuit are added to SubCircuitSpecs, the
  * partitioner checks that the kernel is in fact from the circuit being
  * partitioned and that it doesn't already belong to a different
  * SubCircuitSpec. Once all kernels have been added to the 'specs managed by
  * this partitioner, the construct method will add input and output proxy
  * kernels to each spec as necessary (whenever a kernel in one sub-circuit
  * sources or sinks a kernel in a different sub-circuit) and translate each
  * SubCircuitSpec into an actual circuit, ready for binding to a compute
  * device.
  *
  * Some code-cleanup possibilities:
  *
  * Can remove logic that puts proxies between recurrences and their paired
  * kernels, or likewise between ActuatorKernels and their paired sources.
  *
  * Should check that subcircuit placement hasn't split recurrences and
  * Actuators from their paired kernels.
  *
  * @param circuit The circuit to be partitioned into sub-circuits
  *
  * @author Tobin Gonzalez
  */
private[cogx]
class Partitioner(circuit: KernelCircuit) {

  import Partitioner._

  // We can use path-dependent types here to make sure that sub-circuits from
  // this partitioning don't co-mingle with those from another partioning.

  /** A specification for a partition or subcircuit of a larger circuit. */
  class SubCircuitSpec {

    /** Sub-circuit's kernels: IdentityHashSet needed to implement 'contains' */
    private val _kernels = new IdentityHashSet[AbstractKernel]()
    def kernels = _kernels.toSeq

    /** In the context of a subcircuit, a "root" is any kernel with no outputs
      * in this subcircuit. This is a hacky definition to make translateKernel
      * work. In reality, kernels with outputs to other subciruits will
      * eventually have to be attached to OutputProxyKernels, which are true
      * roots in that they don't list any kernels as outputs at all. */
    def roots  = kernels.filter { kernel =>
      (kernel.outputs.size == 0) || kernel.outputs.forall(output => output.sinks.forall(!contains(_)))
    }

    /** Inputs to the subcircuit are those kernels that list no kernels as
      * inputs. If the subcircuit has already been augmented with input proxy
      * kernels, they should appear in this sequence. */
    def inputs = {
      kernels.filter(_.inputs.size  == 0)
    }

    def contains(kernel: AbstractKernel) = _kernels.contains(kernel)
    def add(kernel: AbstractKernel) {
      require(kernel.circuit eq circuit, "Can't add a kernel from Circuit A to one of Circuit B's sub-circuits. ")
      require(kernelToSubCircuitMap.get(kernel) == None, "Can't add a single kernel to multiple sub-circuits.")
      _kernels += kernel
      kernelToSubCircuitMap(kernel) = this
    }

  }

  /** Utility class for helping with the building and linking of proxy
    * kernels. Output and input proxies have a one-to-many relationship.
    * @param proxiedOutput The kernel who's output needs to be shipped across a
    *                (sub)circuit boundary. */
  private class ProxySet(val proxiedOutput: VirtualFieldRegister) {
    var outputProxy: OutputProxyKernel = null
    val inputProxies = collection.mutable.Buffer.empty[InputProxyKernel]
  }

  /** All the subcircuits managed by this partitioner. They must collectively
    * represent all kernels in the original, unpartitioned circuit before a call
    * to contruct() is legal. */
  private val subCircuits = collection.mutable.Buffer[SubCircuitSpec]()

  /** Map from a kernel in the original, unpartitioned circuit to the subcircuit
    * that will own in the context of this Partitioner. */
  private val kernelToSubCircuitMap = new IdentityHashMap[AbstractKernel, SubCircuitSpec]

  /** Which SubCircuitSpec does this kernel belong to, if any */
  def getSubCircuitOf(k: AbstractKernel): Option[SubCircuitSpec] =
    kernelToSubCircuitMap.get(k)

  /** Create a new, empty SubCircuitSpec and add it to this partioner. */
  def addSubCircuitSpec() = {
    val spec = new SubCircuitSpec
    subCircuits += spec
    spec
  }

  /** Remove a SubCircuitSpec from this Partitioner. All kernels that had been
    * added to that Spec will be freed for adding to other 'Specs (and must
    * be re-added before calling construct). */
  def removeSubCircuitSpec(spec: SubCircuitSpec) {
    if (subCircuits.contains(spec)) {
      subCircuits -= spec
      for (kernel <- spec.kernels) kernelToSubCircuitMap -= kernel
    }
  }

  /** Check that the current partitioning is sane. At present, this checks that
    * every kernel in the original circuit undergoing partitioning belongs to
    * exactly one subcircuit/partition. */
  def validate(): Boolean = {
    // Make sure every kernel in the complete circuit belongs to exactly one
    // sub-circuit.
    circuit.traversePostorder { kernel =>
      var refCount = 0
      subCircuits foreach { subCircuit =>
        if (subCircuit.contains(kernel)) {
          println("kernel " + kernel + " found in subcircuit " + subCircuit)
          refCount += 1
        }
      }
      if (refCount != 1) {
        Log.e("kernel " + kernel + "found " + refCount + " times in subcircuits, expecting once.")
        return false
      }
    }

    // Other checks ?

    true
  }

  /** Validate the current partitioning and produce a new KernelCircuit for
    * each partition. */
  def construct(): Seq[KernelCircuit] = {
    require(validate(), "Invalid partioning of circuit: "+circuit)

    /* The current Circuit API only allows us to modify the most recently
     * created circuit. This wouldn't be an issue if not for recurrences, which
     * can hide instances where proxying is required. We need to pre-compute
     * which kernels need proxies, lest we start building a subcircuit and
     * encounter a RecurrentFieldKernel driven by kernel in a previously
     * handled subcircuit. */
    //val proxyMap = collection.mutable.HashMap[AbstractKernel, ProxyPair]()
    val proxyMap = computeProxies()
    val kernelCircuits = subCircuits.map(constructSubCircuit(_, proxyMap))

    Log.debugOnlyBlock {
      for (i <- 0 until subCircuits.length) {
        Log.d("Circuit "+i+" kernels:")
        Log.d("  Inputs:                 "+subCircuits(i).inputs.map(_.name).mkString(", "))
        Log.d("  Outputs (sans proxies): "+subCircuits(i).roots.map(_.name).mkString(", "))
        Log.d("  Should own copies of:   "+subCircuits(i).kernels.map(_.name).mkString(", "))
        Log.d("  Owns copies:            "+kernelCircuits(i).flatten.map(_.name).mkString(", "))
      }
    }


    // Link up the proxies.
    proxyMap.values foreach { proxySet =>
      require(proxySet.outputProxy != null,     "Missing the output proxy for kernel output: "+proxySet.proxiedOutput.name)
      require(proxySet.inputProxies.length > 0, "No input proxies corresponding to output proxy: "+proxySet.outputProxy.name)
      proxySet.outputProxy.targetInputProxyIds = proxySet.inputProxies map (_.id)
      proxySet.inputProxies foreach (_.sourceProxyId = proxySet.outputProxy.id)
    }

    Log.debugOnlyBlock {
      Log.d("Begin: Generated partitions ---")
      for ((circuit, i) <- kernelCircuits.zipWithIndex) {
        Log.d(s"Partition ${i + 1}:")
        circuit.traversePostorder { kernel =>
          Log.d(s"  ${kernel.name}")
        }
      }
      Log.d("End: Generated partitions -----")
    }

    // TODO Produce and return a kernel directory of some sort?
    // Every kernel's ID is unique - when we copy kernels to a subcircuit, they
    // get a new ID that's different from the original. It might be useful to
    // have some sort of map from original kernel to copied kernel (or vice
    // versa). If nothing else, it'd be useful information to present in the
    // Cog Debugger.
    kernelCircuits
  }

  /** Identifies every kernel output that needs to be proxied (i.e. that sends its
    * output outside its subcircuit) in the current partitioning, and allocates
    * a ProxyPair to help track and link the proxy kernels that will need to be
    * created. This method takes into account that some kernels may drive
    * recurrences, and that such recurrences are not listed in the kernel's
    * "outputs" (In an unpartioned circuit, the recurrence is handled by
    * sharing registers, rather than an explicit input/output relationship).
    * @return A map of kernels outputs that require proxies to their representative
    *         ProxyPair object */
  private def computeProxies() = {

    val proxyMap = new IdentityHashMap[VirtualFieldRegister, ProxySet]()

    subCircuits foreach { subCircuit =>
      subCircuit.kernels foreach { kernel =>
        kernel.outputs foreach { output =>
          if (output.sinks.exists(sink => !subCircuit.contains(sink))) {
            Log.d(s"Kernel output ${output.name} has a sink outside its subcircuit.")
            proxyMap.getOrElseUpdate(output, new ProxySet(output))
          }
        }
        // Check sure that the RecurrentFieldKernels and Actuators are in the
        // same subcircuit as the kernels they're linked to
        kernel match {
          case rfk: RecurrentFieldKernel =>
            val recurrenceVirtualFieldRegister = rfk.recurrence
            require(subCircuit.contains(recurrenceVirtualFieldRegister.source))
//          case ack: LegacyActuatorKernel =>
//            val sourceVirtualFieldRegister = ack.source
//            require(subCircuit.contains(sourceVirtualFieldRegister.source))
          case _ =>
        }
      }
    }

    Log.debugOnlyBlock {
      Log.d("Begin: Computed proxies: ---")
      for (proxyPair <- proxyMap.values) {
        val proxiedOutput = proxyPair.proxiedOutput
        if (proxiedOutput.name != "")
          Log.d(s"  ${proxiedOutput.name}")
        else
          Log.d(s"  $proxiedOutput")
      }
      Log.d("End: Computed proxies ------")
    }

    proxyMap
  }

  /** Build a new KernelCircuit based on the given SubCircuitSpec.
    * @param spec     The SubCircuitSpec for which to produce a KernelCircuit.
    * @param proxyMap A map from a kernel that needs its output to be proxied
    *                 to a ProxySet object. This map is to be reused in
    *                 subsequent method calls (on other SubCircuitSpecs) to
    *                 help hook up Input/Output proxies across subcircuits. */
  private def constructSubCircuit(spec: SubCircuitSpec,
                                  proxyMap: IdentityHashMap[VirtualFieldRegister, ProxySet]): KernelCircuit = {

    // Constructing a new KernelCircuit sets it as the current circuit in the
    // Circuit companion object. All kernels constructed from this point on
    // will automatically pick up this circuit as their owner.
    val subCircuit = new KernelCircuit

    //** A debugging aid; requires Jung library */ 
    //val graph = new JungKernelGraph()

    /** Map from a kernel in the complete circuit to its copy in this subcircuit. */
    val copiedKernels  = new IdentityHashMap[AbstractKernel, AbstractKernel]()

    /** Map from a virtual field register in the complete circuit to its copy in this
      * subcircuit. In some caes, a kernel's "copy" is really its proxy. */
    val copiedVirtualFieldRegisters  = new IdentityHashMap[VirtualFieldRegister, VirtualFieldRegister]()

    // Much of this logic is copied directly from Greg's OpenCLCodeGenerator
    def copyKernelToSubCircuit(kernel: AbstractKernel) {
      val stack = new collection.mutable.Stack[AbstractKernel]()
      stack.push(kernel)
      while (stack.nonEmpty) {
        val cursor = stack.pop()
        if (!copiedKernels.contains(cursor)) {
          val queue = cursor.inputs.filter(!copiedVirtualFieldRegisters.contains(_))
          if (queue.length == 0) {

            require(spec.contains(cursor),
              "About to copy a kernel that doesn't belong to this subcircuit.")

            // All inputs have been handled; copy this kernel to the subcircuit.
            cursor match {
              case k: InputProxyKernel  =>
                throw new RuntimeException("Partitioner error: Unexpected input proxy.")
              case k: OutputProxyKernel =>
                throw new RuntimeException("Partitioner error: Unexpected output proxy.")

              case k: AbstractKernel =>
                Log.d(s"Copying kernel: '${cursor.name}' - $cursor")
                val copy = cursor.copyWithNewInputs(fetchRegisterCopies(cursor.inputs).toArray)
                for (i <- 0 until k.outputs.length) {
                  val output = k.outputs(i)
                  copy.outputs(i).name =
                          s"CopiedKernel("+(if (output.name != "") output.name else output.toString)+")"
                  copiedVirtualFieldRegisters(output) = copy.outputs(i)
                  // Add an OutputProxyKernel if necessary. An earlier step has
                  // computed which kernels need to be proxied. We don't do
                  // that here because it's a difficult place to check if this
                  // kernel drives a recurrence outside this subcircuit.
                  proxyMap.get(output) match {
                    case Some(proxySet) =>
                      Log.d(" ^Kernel needs an output proxy.")
                      proxySet.outputProxy  = new OutputProxyKernel(copy.outputs(i), output)
                    //graph.addKernel(proxyPair.outputProxy) // Requires Jung library
                    //graph.addOutputEdge(copy, proxyPair.outputProxy) // Requires Jung library
                    case None =>
                  }
                }
                copiedKernels(cursor) = copy
                k.addAlias(copy.id)
              case x => throw new RuntimeException("Unexpected kernel: "+x)

            }

          } else {
            // Some inputs still need to be copied to this subcircuit.
            stack.push(cursor)
            for (virtualRegister <- queue) {
              if (spec.contains(virtualRegister.source)) {
                stack.push(virtualRegister.source)
              }
              // Input from another SubCircuit: create an InputProxy
              else if (!copiedVirtualFieldRegisters.contains(virtualRegister)) {
                val proxy = new InputProxyKernel(virtualRegister)
                proxyMap(virtualRegister).inputProxies += proxy
                copiedVirtualFieldRegisters(virtualRegister) = proxy.outputs(0)
              }
            }
          }
        }
      }
    }

    def fetchRegisterCopies(kernels: Seq[VirtualFieldRegister]): Seq[VirtualFieldRegister] = {
      kernels map copiedVirtualFieldRegisters
    }

    // Copy the kernels to the subcircuit
    spec.roots.foreach(copyKernelToSubCircuit(_))

    // Finally, we need to wire up the recurrences.
    spec.kernels.foreach {
      case rfk: RecurrentFieldKernel => {
        require(rfk.inputs.length == 0,
          "Compiler error: A RecurrentFieldKernel shouldn't have any inputs!")
        require(spec.contains(rfk.recurrence.source),
          "Partioner error: A RecurrentFieldKernel is linked to a kernel outside of its SubCircuit.")
        require(rfk.recurrence != null, "Compiler error: expecting non-null recurrence.")
        val copyRfk = copiedKernels(rfk).asInstanceOf[RecurrentFieldKernel]
        copyRfk.recurrence = copiedVirtualFieldRegisters(rfk.recurrence)
      }
      case _ =>
    }

    //graph.showGUI() // Requires Jung library
    require(validatePartition(spec, copiedKernels),
      "Failed to copy all kernels in the SubCircuitSpec to a subcircut.")

    subCircuit
  }

  /** Checks that every kernel included in `spec` has a copy. */
  private def validatePartition(spec: SubCircuitSpec, copies: IdentityHashMap[AbstractKernel, AbstractKernel]) = {
    var valid = true
    for (kernel <- spec.kernels) {
      copies.get(kernel) match {
        case Some(kernel) => // Good, kernel was copied
        case None => valid = false
          Log.e("No copy of kernel: "+kernel.name+" --- "+kernel.toString())
      }
    }
    valid
  }

}

private[cogx]
object Partitioner {
  object Log {
    val DebugLevel = 2
    def e(msg: String) { if (DebugLevel > 0) Console.err.println("[Partioner error] "+msg)}
    def w(msg: String) { if (DebugLevel > 1) Console.err.println("[Partioner warning] "+msg)}
    def i(msg: String) { if (DebugLevel > 2) Console.out.println("[Partioner] "+msg)}
    def d(msg: String) { if (DebugLevel > 3) Console.out.println("[Partioner] "+msg)}
    def debugOnlyBlock(fn: => Unit) { if (DebugLevel > 3) fn }
  }
}


///// DEBUGGING UTILITIES /////

//import org.apache.commons.collections15.Transformer
//import edu.uci.ics.jung
///** This class uses Jung to display the circuit paritions as directed graphs.
//  * Input proxies are colored red, kernels cloned into a subcircuit are yellow,
//  * and output proxies are blue. Light gray edges are recurrences. */
//class JungKernelGraph {
//  import JungKernelGraph._
//  val graph = new jung.graph.DirectedSparseGraph[Vertex, Edge]
//  val vertexMap = new IdentityHashMap[AbstractKernel, KernelVertex]()
//  def addKernel(kernel: AbstractKernel) {
//    if (!vertexMap.contains(kernel)) {
//      val vertex = new KernelVertex(kernel)
//      vertexMap(kernel) = vertex
//      graph.addVertex(vertex)
//    }
//  }
//  def addOutputEdge(from: AbstractKernel, to: AbstractKernel) {
//    graph.addEdge(new ForwardEdge, vertexMap(from), vertexMap(to))
//  }
//  def addRecurrence(driver: AbstractKernel, driven: AbstractKernel) {
//    graph.addEdge(new RecurrentEdge, vertexMap(driver), vertexMap(driven))
//  }
//
//  private var gui: scala.swing.Frame = null
//  def showGUI() { showGUI(None) }
//  def showGUI(title: Option[String]) {
//    swing.Swing.onEDT {
//      if (gui == null) gui = createGUI(title.getOrElse("KernelCircuit Graph"))
//      gui.visible = true
//    }
//  }
//  
//  private def createGUI(title: String) = {
//    import scala.swing._
//    val layout = new jung.algorithms.layout.KKLayout(graph)
//    layout.setSize(new Dimension(800, 600))
//    val vertexLabeler = new Transformer[Vertex, String]() {
//      def transform(v: Vertex): String = v match {
//        case kv: KernelVertex =>
//          val kernel = kv.kernel
//          kernel match {
//            case rfk: RecurrentFieldKernel => s"${kernel.name} (ID: ${kernel.id}) [RFK]"
//            case _ =>                         s"${kernel.name} (ID: ${kernel.id})"
//          }
//      }
//    }
//    val edgePainter = new Transformer[Edge, java.awt.Paint]() {
//      def transform(e: Edge): Paint = e match {
//        case fe: ForwardEdge =>   java.awt.Color.BLACK
//        case re: RecurrentEdge => java.awt.Color.LIGHT_GRAY
//      }
//    }
//    val vertexFiller = new Transformer[Vertex, java.awt.Paint]() {
//      def transform(v: Vertex): Paint = v match {
//        case kv: KernelVertex => kv.kernel match {
//          case ipk: InputProxyKernel => java.awt.Color.RED
//          case opk: OutputProxyKernel => java.awt.Color.BLUE
//          case _ => java.awt.Color.YELLOW
//        }
//      }
//    }
//    val control = new jung.visualization.control.DefaultModalGraphMouse()
//    val vv = new jung.visualization.VisualizationViewer(layout)
//    vv.getRenderContext.setVertexLabelTransformer(vertexLabeler)
//    vv.getRenderContext.setEdgeDrawPaintTransformer(edgePainter)
//    vv.getRenderContext.setVertexFillPaintTransformer(vertexFiller)
//    vv.setGraphMouse(control)
//    val frame = new Frame
//    frame.title = title
//    frame.contents = Component.wrap(vv)
//    frame
//  }
//}
//object JungKernelGraph {
//  sealed trait Vertex
//  sealed trait Edge
//  class KernelVertex(val kernel: AbstractKernel) extends Vertex
//  class ForwardEdge extends Edge
//  class RecurrentEdge extends Edge
//}