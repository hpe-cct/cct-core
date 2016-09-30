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

package cogx.runtime

import _root_.akka.actor.{ActorSystem, TypedActor, TypedProps}
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel
import cogx.compiler.parser.syntaxtree._
import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.codegenerator.opencl.generator.OpenCLCodeGenerator
import cogx.platform.checkpoint._
import cogx.platform.opencl._
import cogx.compiler.optimizer.KernelCircuitOptimizer
import cogx.platform.cpumemory.AbstractFieldMemory
import cogx.runtime.checkpoint.hdf5.{Hdf5ObjectRestorer, Hdf5ObjectSaver}
import cogx.runtime.checkpoint.javaserialization.{JavaObjectRestorer, JavaObjectSaver}
import cogx.runtime.debugger.{ProbedCircuit, ProbedField, UserFieldNames}
import java.util.concurrent.Semaphore

import cogx.platform.types._
import cogx.runtime.execution.{CircuitEvaluator, CogActorSystem, Profiler}
import java.lang.reflect.{InvocationTargetException, UndeclaredThrowableException}

import cogx.platform.cpumemory.readerwriter.FieldReader
import cogx.compiler.codegenerator.opencl.fragments.HyperKernel
import cogx.parameters.Cog
import cogx.runtime.allocation.AllocationMode

import scala.collection.mutable.ArrayBuffer

/** A Cog computation graph.
  *
  * @param optimize When false, turns off the optimizer, allowing the graph to
  *              be more easily debugged, since all fields declared by the
  *              user remain visible.
  * @param device The index of the device to run the ComputeGraph on
  * @param forceProfiling Should profiling be forced, thereby bypassing and refreshing the Profiler's cache.
  * @param fftUse policy for FFT use in fast convolution.
  * @param convolveSmallTensorUse policy for when to use "small tensor addressing" in kernel generation.
  *
  * @author Greg Snider and Dick Carter
  */
class ComputeGraph(val optimize: Boolean = true,
                   device: Option[Int] = None,
                   forceProfiling: Boolean = Cog.forceProfiling,
                   fftUse: ConvolutionFFTUsePolicy = UseFFTWhenBest,
                   convolveSmallTensorUse: ConvolutionSmallTensorUsePolicy = UseSmallTensorWhenBest)
  extends Saveable
{
  /** Debug flag */
  def Verbose = Cog.verboseOptimizer

  /** Turns on printing of the compiled circuit for debugging. */
  def VerbosePrint = Cog.printOptimized

  /** Syntax tree for the computation. This is added to when the user creates
    * computations. The syntax tree acts nearly like a singleton in that it
    * is discovered implicitly by a call to a companion object. However, it's
    * possible to create multiple syntax trees as long as they are not nested.
    * Scala doesn't provide an easy way of discovering when a constructor
    * finishes execution, so we must use this implicit approach.
    */
  private[cogx] var syntaxTree = new SyntaxTree

  /** When true, user has requested that all fields be probed. */
  private var userProbeAllRequest = false

  /** Is this a ComputeGraph created by reading a File?  If so, it will lack a SyntaxTree. */
  private var restoredFromFile = false

  /** If this ComputeGraph was read from a file, this is the restored KernelCircuit */
  private var restoredCircuit: KernelCircuit = null

  /** If this ComputeGraph was read from a file, this is the partial restored SyntaxTree populated by Sensors and Actuators */
  private var restoredSyntaxTree: SyntaxTree = null

  /** Circuit of kernels compiled from syntaxTree. */
  private lazy val circuit = {
    if (restoredFromFile)
      restoredCircuit
    else {
      syntaxTree.seal
      compileCircuit(syntaxTree)
    }
  }

  /** Optimized circuit that exposes only probed fields (for debugger). */
  lazy val probedCircuit = new ProbedCircuit(circuit)

  /** Actor system, used only for actor-based evaluators. */
  private lazy val actorSystem: ActorSystem = CogActorSystem()

  /** Selects the OpenCLPlatform to use and creates a unique context for this ComputeGraph. */
  private lazy val platform = OpenCLPlatform()

  /** Selects the OpenCLPlatform to use for profiling and creates a unique context for this ComputeGraph. */
  private lazy val profilerPlatform = new OpenCLPlatform(profilerUse = true)

  /** The specific device(s) that will evaluate the ComputeGraph, now bound at ComputeGraph creation time. */
  val mode: AllocationMode = device match {
    case Some(deviceIndex) => AllocationMode.SingleGPU(deviceIndex)
    case None => AllocationMode.default
  }

  /** Evaluator that can execute the compiled circuit. */
  private lazy val evaluator: EvaluatorInterface =  {
    // Touch circuit to force its lazy instantiation. If this throws an
    // exception, we do not want to create the circuit evaluator.
    circuit

    val profileSize = if (Cog.profile) Cog.profileSize else 0

    var typedProps = TypedProps(classOf[EvaluatorInterface], new CircuitEvaluator(circuit, platform, mode, profileSize))

    // If we're not sharing threads, then add our Cog-custom dispatcher
    if (!Cog.threadSharing)
      typedProps = typedProps.withDispatcher("CogActors.one-actor-per-thread-dispatcher")

    val ta = TypedActor(actorSystem).typedActorOf(typedProps, name="CircuitEvaluator").
      asInstanceOf[EvaluatorInterface]

    val evaluatorActor = TypedActor(actorSystem).getActorRefFor(ta)
    ta.tellIdentity(evaluatorActor)
    ta
  }

  /** The number of HyperKernels in the circuit, useful for merger testing. */
  private[cogx] def hyperkernelCount =
    circuit.filteredSize(_.isInstanceOf[HyperKernel])

  /** Default constructor for users, forces optimization (though they can
    * disable it to varying degrees using probes or probeAll.)
    */
  def this() = this(true)

  /** If the compute graph may span multiple devices, then use code gen params
    * that will conservatively work across all devices.  Otherwise use the
    * params that work for the chosen device.
    *
    * @return code gen params for the device or devices that the ComputeGraph may use.
    */
  def kernelCodeGenParams = mode match {
    case AllocationMode.SingleGPU(deviceIndex) => platform.devices(deviceIndex).kernelCodeGenParams
    case AllocationMode.MultiGPU => platform.kernelCodeGenParams
    case AllocationMode.Cluster => platform.kernelCodeGenParams
  }

  /** Translator from SyntaxTree to KernelCircuit */
  lazy val codeGenerator = {
    if (Verbose)
      println("ComputeGraph: creating code generator")
    val profiler = new Profiler(profilerPlatform, mode, actorSystem, forceProfiling)
    new OpenCLCodeGenerator(kernelCodeGenParams, fftUse, convolveSmallTensorUse, profiler)
  }

  /** Compile a syntax tree to a circuit that can be executed by an evaluator.
   *
   * @param syntaxTree The syntax tree to be compiled.
   */
  private def compileCircuit(syntaxTree: SyntaxTree): KernelCircuit = {
    if (Verbose)
      println("ComputeGraph: compiling circuit")
    // Bind user names for Fields to syntax tree nodes.
    UserFieldNames.extract(this)
    if (Verbose)
      println("ComputeGraph: generating code")
    // Generate a kernel circuit for the syntax tree. This will mark each
    // field in the syntax tree with the kernel that represents it.
    val circuit = codeGenerator.generateCircuit(syntaxTree)
    if (Verbose)
      println("ComputeGraph: marking probes")

    // Mark probes on circuit nodes (kernels) from the probe marks on the
    // corresponding fields. Note that sensors, actuators, constants and
    // recurrences are automatically probed.
    syntaxTree.traversePostorder {
      operation => {
        operation.outputs.foreach( field => {
          val virtualFieldRegister = field.getVirtualFieldRegister
          virtualFieldRegister.name = field.name
          if (field.isProbed || field.hasRecurrence ||
            userProbeAllRequest || !optimize)
            virtualFieldRegister.markProbed

          field.recurrence match {
            case Some(field) => field.getVirtualFieldRegister.markProbed
            case None =>
          }
        })
      }
    }
    if (!userProbeAllRequest && optimize) {
      if (Verbose)
        println("ComputeGraph: optimizing")
      KernelCircuitOptimizer.optimize(circuit, kernelCodeGenParams)
    }
    if (VerbosePrint)
      circuit.print
    circuit
  }

  /** Probe all fields; this effectively disables the optimizer.
    */
  def probeAll {
    userProbeAllRequest = true
  }

  /** Step the computation one cycle, returning the resulting simulation time
    * (synchronous call).
    *
    * @return the number of steps taken from the last reset
    */
  def step: Long = {
    val time =
      try {
        evaluator.step
      } catch {
        // Either of the following 2 exceptions mask the underlying cause.
        case e: UndeclaredThrowableException =>
          throw e.getCause
        case e: InvocationTargetException =>
          throw e.getCause
          
        case e: Exception =>
          throw e
      }
    time
  }

  /** Step the computation `count` cycles, returning the resulting simulation
    *  time (synchronous call).
    *
    * @param count Number of steps to take.
    * @return the number of steps taken from the last reset
    */
  def step(count: Long): Long = {
    require(count >= 0L, "Negative step count seen: " + count)
    // step(0L) should still return the time
    var time = if (count > 0L) 0L else evaluator.step(0L)
    try {
      // Break up the stepping into chunks so the Akka actor system doesn't
      // timeout.  Also, we get quicker response to a hung system if we can
      // live with the default Actor message timeout.
      val StepBatchSize = 100L
      var stepsRemaining = count
      while (stepsRemaining > 0) {
        val stepsThisPass = math.min(stepsRemaining, StepBatchSize)
        time = evaluator.step(stepsThisPass)
        stepsRemaining -= stepsThisPass
      }
    } catch {
      // Either of the following 2 exceptions mask the underlying cause.
      case e: UndeclaredThrowableException =>
        throw e.getCause
      case e: InvocationTargetException =>
        throw e.getCause

      case e: Exception =>
        release
        throw e
    }
    time
  }

  /** Reset the computation to an initial state defined by the user (synchronous
    * call).
    *
    * If the computation is running, it is stopped before the initialization
    * takes place.
    *
    * @return Zero (the simulation time after reset).
    */
  def reset: Long = {
    try {
      evaluator.reset
    } catch {
      // Either of the following 2 exceptions mask the underlying cause.
      case e: UndeclaredThrowableException =>
        throw e.getCause
      case e: InvocationTargetException =>
        throw e.getCause

      case e: Exception =>
        throw e
    }
  }

  /** Start the computation running until `stop` is called (asynchronous call) */
  def run {
    evaluator.run
  }

  /** Stop the computation, returning the simulation time (synchronous call). */
  def stop: Long = {
    evaluator.stop
  }

  /** Get the current simulation time (asynchronous call).
    *
    * @param done Callback function with the simulation time
    */
  def time(done: (Long) => Unit) {
    evaluator.time(done)
  }

  def debugTouch {
    circuit
    println("ComputeGraph: KernelCircuit: ---------------")
    circuit.print
    println("--------------------------------------------")
    println("ComputeGraph creating evaluator:")
    evaluator
  }

  /** Read a field (user access to the field data).
    */
  def read(field: Field): FieldReader = {
    val virtualFieldRegister = field.getVirtualFieldRegister
    require (virtualFieldRegister != null, s"Compiler error: null virtual field register seen for field $field")
    require (virtualFieldRegister.probed, s"Attempted read of non-probed field $field." +
      s"  Add 'probe(${field.name})' within the ComputeGraph to correct this problem.")
    readVirtualRegister(virtualFieldRegister)
  }

  /** Read a field given the name.  Used typically for restored compute graphs that no longer have fields.
    */
  def readByName(fieldName: String): FieldReader = {
    // Get the kernels, leaf nodes before roots
    val kernels = circuit.flatten.toArray

    val matchingVfrs = new ArrayBuffer[VirtualFieldRegister]

    kernels.foreach(kernel => {
      val ios = kernel.outputs
      ios.foreach(reg => if (reg.name == fieldName) matchingVfrs += reg)
    })
    matchingVfrs.length match {
      case 1 => readVirtualRegister(matchingVfrs(0))
      case 0 => throw new RuntimeException(s"Could not locate field with probe name '${fieldName}'.")
      case x => throw new RuntimeException(s"Found $x fields with probe name '${fieldName}'.")
    }
  }

  /** Retrieve an Unpipelinedactuator from a restored compute graph using the name of the source field.
    */
  def getUnpipelinedActuator(sourceFieldName: String): UnpipelinedActuator = {

    val matchingUnpipelinedActuators = new ArrayBuffer[UnpipelinedActuator]

    // If one uses this method on an original (i.e. not restored) ComputeGraph, the names found
    // must be explicitly set as in A.probe("A"), not A.probe().
    val sTree = if (restoredFromFile) restoredSyntaxTree else syntaxTree

    sTree.traversePostorder {
      operation => {
        operation match {
          case x: UnpipelinedActuator =>
            operation.inputs.foreach(in =>
              if (in.name == sourceFieldName)
                matchingUnpipelinedActuators += x
            )
          case _ =>
        }
      }
    }

    matchingUnpipelinedActuators.length match {
      case 1 => matchingUnpipelinedActuators(0)
      case 0 => throw new RuntimeException(s"Could not locate unpipelined actuator with source field name '${sourceFieldName}'.")
      case x => throw new RuntimeException(s"Found $x unpipelined actuators with source field name '${sourceFieldName}'.")
    }
  }

  /** Produce a FieldReader for this virtual field register. Used by user read(field),
    * but also the ComputeGraph save/restore mechanism.
    */
  private[cogx] def readVirtualRegister(vfr: VirtualFieldRegister): FieldReader = {
    val semaphore = new Semaphore(0)
    var memory: AbstractFieldMemory = null
    def done(mem: AbstractFieldMemory) {
      memory = mem
      semaphore.release
    }
    evaluator.readField(vfr, done)
    semaphore.acquire

    // One can eliminate the inheritance star pattern:
    //
    // ScalarFieldMemory -> ScalarFieldReader -> FieldReader
    // ScalarFieldMemory -> AbstractFieldMemory -> FieldReader
    //
    // by having AbstractFieldMemory no longer inherit from FieldReader
    // and have the return value below dynamically cast as shown:
    //
    //    memory.asInstanceOf[FieldReader]
    //
    // However, there would be no record in the codebase that subclasses
    // of AbstractFieldMemory had better derive from FieldReader.
    memory
  }

  /** Read a field, calling `done` when the field data is available
    * (asynchronous call). This primarily used by the debugger, but may be
    * used by arbitrary user code.
    *
    * @param field The field that is being read
    * @param to Pre-allocated field memory, passed to the `done` function, that
    *        receives the read field data.
    * @param done Callback which returns memory holding the field data; this
    *        memory must be released before this field can be read again.
    */
  def read(field: ProbedField, to: AbstractFieldMemory,
           done: (ProbedField, AbstractFieldMemory) => Unit)
  {
    val virtualFieldRegister = field.virtualFieldRegister
    require (virtualFieldRegister != null, s"Compiler error: null virtual field register seen for field $field")
    require (virtualFieldRegister.probed, s"Attempted read of non-probed field $field." +
      "  Add 'probe(${field.name})' within the ComputeGraph to correct this problem.")
    evaluator.readField(virtualFieldRegister, to, done(field, _))
  }

  /** Future interface for debugger. Note that the AbstractFieldMemory
    * referenced in done must be released before the field can be read again.
    */
  def read(field: ProbedField, done: (ProbedField, AbstractFieldMemory) => Unit)
  {
    throw new RuntimeException("not implemented yet.")
  }

  /** Release all resources for this (and ALL) compute graphs.
    *
    * This attempts to minimize memory leaks due to the JVM garbage
    * collector not being able to handle direct memory very well.
    *
    * After release of a ComputeGraph, we still have references to the Direct Buffers
    * from singleton objects like HyperCircuit, which prevents the direct buffer freeing
    * from being immediately successful.  Making a new ComputeGraph should overwrite those
    * references, thus freeing up the direct memory for the next ComputeGraph.  Making a tiny
    * ComputeGraph and releasing it between 'real' ComputeGraphs would be one hack at the user
    * level to combat direct buffer freeing problems.
    */
  def release {
    synchronized {
      // Remove references to VirtualFieldRegisters within Fields, since user code may still reference Fields.
      if (syntaxTree != null) {
        codeGenerator.releaseCircuit(syntaxTree)
        syntaxTree = null
      }
      actorSystem.shutdown
      // Releasing OpenCL resources also removes pointers so that OpenCL
      // memory objects could, in principle, be reclaimed by the garbage
      // collector.
      // The platform currently owns the fieldmemory allocator, so will
      // attempt to reclaim direct buffer memory
      // In practice, though, direct buffers are very difficult
      // to reclaim, so we do a direct buffer "cleaning" here. A hack, but
      // widely done.
      platform.release()
      profilerPlatform.release()
      System.gc
    }
  }

  /** Print out the ComputeGrap for debugging. */
  def print() {
    // Print out SyntaxTree
    println("Syntax tree:")
    syntaxTree.print

    // Print out KernelCircuit
    println
    println("Kernel circuit:")
    probedCircuit.print
  }

  def printEvaluator {
    evaluator.print
  }

  /** Make sure release is called, even if an exception is thrown. */
  def withRelease(operations: => Unit) {
    try {
      operations
    }
    finally
      release
  }

  /** A user-friendly name for this computegraph. Used in logs and for
    * debugging. */
  var computeGraphName = "unnamed"

  /** The user's description of this computegraph. */
  def description: Option[String] = _description

  /** Set a description for the computegraph. Used in logs and for
    * debugging. */
  def description_=(desc: String) { _description = Some(desc) }

  /** An optional, user-settable string describing the purpose of this
    * compute graph. If set, this string is saved in the serialized version
    * of this graph when it is written to disk. It may also be used to enrich
    * logs.
    */
  private var _description: Option[String] = None

  /** Write the compute graph to a file with the given checkpoint technology (e.g. hdf5) */
  def write(filename: String,
            checkpointerType: CheckpointerType = ComputeGraph.defaultCheckpointerType): Unit = {
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    /** A saver object for the chosen serialization approach. */
    val saver = checkpointerType match {
      case Hdf5CheckpointerType => new Hdf5ObjectSaver(suffixedFilename) with ComputeGraphSaverState
      case JavaCheckpointerType => new JavaObjectSaver(suffixedFilename) with ComputeGraphSaverState
    }
    try {
      ComputeGraph.Log.info("Saving ComputeGraph to file "+filename)
      saver.writeObject("ComputeGraph", this)
    } finally {
      ComputeGraph.Log.debug("Saving operation complete. Closing saver.")
      saver.close()
    }
  }

  /** Write this compute graph instance with the facilities offered by an
    * ObjectSaver.
    */
  def save(saver: ObjectSaver): Unit = {
    val saverState = saver.asInstanceOf[ComputeGraphSaverState]
    import saverState._

    // The ComputeGraph File Spec describes what can be found in the saved
    // file. The spec hasn't really been formalized, but revision 1.0 fields
    // include:
    //   Mandatory:
    //     ComputeGraph File Spec version major number [int]
    //     ComputeGraph File Spec version minor number [int]
    //     The release version of Cog that generated this file [string]
    //     The name of the saved compute graph [string]
    //     OpenCL kernel source codes [string array]
    //     Virtual Field Register information [object array]
    //     OpenCL kernels [object array]
    //   Optional:
    //     User description of the compute graph [string]
    val ComputeGraphFileSpecMajorVersion = 1
    val ComputeGraphFileSpecMinorVersion = 0

    // Write out some header info
    saver.writeInt(ComputeGraph.ComputeGraphFileSpecMajorVersionKey, ComputeGraphFileSpecMajorVersion)
    saver.writeInt(ComputeGraph.ComputeGraphFileSpecMinorVersionKey, ComputeGraphFileSpecMinorVersion)
    saver.writeString("releaseVersion", Cog.releaseVersion)
    saver.writeString("name", computeGraphName)
    for (desc <- _description) saver.writeString(ComputeGraph.UserDescriptionKey, desc)

    // Get the kernels, leaf nodes before roots
    val kernels = circuit.flatten.toArray

    // Number the set of unique kernel codes and write this out.
    // Retain a map of kernel codes to their numeric ID.
    val kernelCodes = ArrayBuffer[KernelSourceCode]()
    kernels.foreach { _ match {
      case k: OpenCLDeviceKernel =>
        val ksc = new KernelSourceCode(k.kernelCode)
        if (!kernelCodeToIndex.isDefinedAt(ksc)) {
          val nextIndex = kernelCodes.size
          kernelCodes += ksc
          kernelCodeToIndex(ksc) = nextIndex
        }
      case _ =>
    }}
    saver.writeObjectArray("kernelCodes", kernelCodes.toArray)

    // Number the virtual field registers (vfr's).
    // Retain a map of vfr's to their numeric ID.
    val vfrs = ArrayBuffer[VirtualFieldRegister]()
    kernels.foreach(kernel => {
      val ios = kernel.inputs ++ kernel.outputs
      ios.foreach(reg =>
        if (!vfrToIndex.isDefinedAt(reg)) {
          val nextIndex = vfrs.length
          vfrToIndex(reg) = nextIndex
          vfrs += reg
        }
      )
    })
    val virtualFieldRegisters = vfrs.toArray
    saver.writeObjectArray("fields", virtualFieldRegisters)

    // Exposing the ComputeGraph is a bit of overkill, but RecurrenceFieldKernels
    // need to read the state of their registers, and all reads should go through
    // the ComputeGraph interface (remember multi-GPU and multi-node considerations).
    computeGraph = this

    saver.writeObjectArray("kernels", kernels)
  }
}

/** Factory object for making compute graphs from a ComputeGraphCheckpoint instance or a
  * file-saved version of one.
  */
object ComputeGraph extends RestoreFactory {
  /** Controls verbosity of deserialization routines. The levels are:
    * {{{
    *   0  - disable all logging
    *   1  - errors only
    *   2  - errors and warnings
    *   3  - errors, warning, and info messages
    *   4+ - enable all logging (errors, warnings, info, debug)
    * }}}
    */
  var logLevel = 3
  private object Log {
    private val Prefix = "ComputeGraph RestoreFactory"
    def debug(msg: String) { if (logLevel > 3) println(Prefix+" [Debug]: "+msg) }
    def info(msg: String) { if (logLevel > 2) println(Prefix+" [Info]: "+msg) }
    def warn(msg: String) { if (logLevel > 1) println(Prefix+" [Warn]: "+msg) }
    def error(msg: String) { if (logLevel > 0) println(Prefix+" [Error]: "+msg) }
  }

  val defaultCheckpointerType = Hdf5CheckpointerType
  private val ComputeGraphFileSpecMajorVersionKey = "computeGraphFileSpecMajorVersion"
  private val ComputeGraphFileSpecMinorVersionKey = "computeGraphFileSpecMinorVersion"
  private val UserDescriptionKey                  = "description"

  /** Create a ComputeGraph from a representation read from a file.
    *
    * @param filename The name of the file to read.
    * @param checkpointerType The checkpointing technology, e.g. hdf5, java, etc.
    * @return The created ComputeGraph, suitable for further stepping.
    */
  def readFromFile(filename: String,
                   checkpointerType: CheckpointerType = defaultCheckpointerType): ComputeGraph = {
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    /** A restorer object for the chosen serialization approach. */
    val restorer = checkpointerType match {
      case Hdf5CheckpointerType => new Hdf5ObjectRestorer(suffixedFilename) with ComputeGraphRestorerState
      case JavaCheckpointerType => new JavaObjectRestorer(suffixedFilename) with ComputeGraphRestorerState
    }
    Log.info("Restoring graph from file: "+filename)
    var cg: ComputeGraph = null
    try {
      cg = restorer.readRestorable("ComputeGraph", ComputeGraph).asInstanceOf[ComputeGraph]
    } finally {
      Log.debug("Closing restorer.")
      restorer.close()
    }
    cg
  }

  /** Create a ComputeGraph from its checkpointed state. */
  def restore(restorer: ObjectRestorer): ComputeGraph = {
    val restorerState = restorer.asInstanceOf[ComputeGraphRestorerState]
    import restorerState._

    // Unfortunately, we didn't have a fileSpec version starting out, so it has
    // to be treated as 'optional' to read those old files.
    Log.debug("Looking for fileSpecMajor...")
    val fileSpecMajor   = restorer.readOptionalInt(ComputeGraphFileSpecMajorVersionKey)
    Log.debug("Looking for fileSpecMinor...")
    val fileSpecMinor   = restorer.readOptionalInt(ComputeGraphFileSpecMinorVersionKey)
    Log.debug("Looking for releaseVersion...")
    val releaseVersion  = restorer.readString("releaseVersion")
    Log.debug("Looking for name (of graph)")
    val name            = restorer.readString("name")
    Log.debug("Looking for user description (of graph)...")
    val userDescription = restorer.readOptionalString(UserDescriptionKey)

    //    // Could take version-specific actions like this
    //    fileSpecMajor match {
    //      case Some(majorVersion) =>
    //        majorVersion match {
    //          case 1 =>  // do something for v1.+
    //          case _ =>
    //        }
    //      case None =>  // do something for un-versioned (aka really old) files
    //    }

    // Log some stuff
    for (major <- fileSpecMajor; minor <- fileSpecMinor) {
      Log.info("ComputeGraph File Spec is v"+major+"."+minor)
    }
    Log.info("Restoring graph '"+name+",' built with Cog release version '"+releaseVersion+"'")
    for (desc <- userDescription) { Log.info("Graph description: "+desc) }

    // Read the kernel codes from the file and make this part of the restorer's global state
    restorer.readRestorableArray("kernelCodes", KernelSourceCode).foreach( kernelCode =>
      kernelCodes += kernelCode.asInstanceOf[KernelSourceCode]
    )
    // Read the field infos from the file and make this part of the restorer's global state
    restorer.readRestorableArray("fields", VirtualFieldRegister).foreach( fieldInfo =>
      fieldInfos += fieldInfo.asInstanceOf[VirtualFieldRegisterInfo]
    )
    // Read in the kernel-defining data, which will add kernels and virtual field registers to the
    // just-created empty KernelCircuit.
    val kernels = restorer.readRestorableArray("kernels", AbstractKernel).map(_.asInstanceOf[AbstractKernel])
    // Tell RecurrentFieldKernels where there recurrence vfrs are.  This had to wait until all kernels and their
    // associated vfr outputs were created.
    kernels.foreach(kernel => kernel match {
      case rfk: RecurrentFieldKernel => rfk.recurrence = vfrs(recurrences(rfk))
      case _ =>
    })

    val cg = new ComputeGraph()
    cg.restoredFromFile = true
    cg.restoredCircuit = restoredCircuit
    // Preclude adding more fields to this restored ComputeGraph- it's mostly a compiled KernelCircuit already.
    restoredSyntaxTree.seal
    cg.restoredSyntaxTree = restoredSyntaxTree
    cg.computeGraphName = name
    cg
  }
}
