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

package cogx.runtime.execution

import akka.actor.{ActorSystem, TypedActor, TypedProps}
import cogx.cogmath.geometry.Shape
import cogx.cogmath.hypercircuit.Hypercircuit
import cogx.compiler.codegenerator.KernelCircuit
import cogx.compiler.codegenerator.opencl.hyperkernels.ConstantHyperKernel
import cogx.compiler.parser.op.ConstantScalarOp
import cogx.parameters.Cog
import cogx.platform.opencl.{KernelSourceCode, OpenCLAbstractKernel, OpenCLDeviceKernel, OpenCLPlatform}
import cogx.platform.types.ElementTypes.Float32
import cogx.platform.types._
import cogx.runtime.EvaluatorInterface
import cogx.runtime.allocation.AllocationMode
import cogx.runtime.allocation.AllocationMode.SingleGPU
import cogx.utilities.{CogDir, MD5}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Provides kernel profiling information to aid code generators and optimizers.
  *
  * @param platform The OpenCL platform (with queues enabled for profiling) to use for timing the kernels.
  * @param mode A description of which device(s) will be used to run the ComputeGraph.
  * @param actorSystem The Akka actor system to use when creating Actors for profiling.
  * @param forceProfiling Should profiling always be performed even if prior cached profiling samples exist.
  *
  * @author Dick Carter
  */
class Profiler(platform: OpenCLPlatform, mode: AllocationMode, actorSystem: ActorSystem, forceProfiling: Boolean) {

  // An id to create unique names for the Profiler evaluator actors
  private var _id = 0

  private def id = synchronized {
    _id += 1
    _id
  }

  val deviceName = mode match {
    case SingleGPU(deviceIndex: Int) => platform.devices(deviceIndex).fullNameNoSpaces
    case _ => throw new RuntimeException("Profiler sees multi-GPU platform, aborting.")
  }

  /** Creates the circuit-driving input kernels and L2-cache-flushing kernel to surround a KernelCircuit completed
    * by the provided `circuitMaker` routine, which is given a set of inputs.  This routine then runs the completed
    * KernelCircuit for `runSteps` steps, after a warmup of `warmupSteps` steps.  The result is passed back as a
    * `ProfileSample` that includes whether the information was determined real-time, or whether it was a cached
    * result from an earlier compile.
    *
    * @param inputFieldTypes The FieldTypes of the kernel (or more generally the circuit) to be profiled.
    * @param circuitMaker A function that completes the circuit to be profiled, given its inputs.
    * @param warmupSteps The number of steps before clearing the execution time statistics.
    * @param runSteps The number of steps to execute while accumulating runtime statistics.
    * @return The execution time of the circuit created by `circuitMaker`, wrapped with other info in a `ProfileSample`.
    */
  def profile(inputFieldTypes: Array[FieldType], circuitMaker: (Array[VirtualFieldRegister]) => Unit,
              warmupSteps: Int = Cog.profilerWarmupSteps, runSteps: Int = Cog.profilerSteps): ProfileSample = {

    // Save away the primary circuit the generators are building.  The profiler makes its own circuit on the side.
    val mainCircuit = Hypercircuit.current
    val numInputs = inputFieldTypes.length
    try {
      // Create a new circuit of just the kernel(s) to profile, plus the input drivers and L2 cache flush kernel.
      val nonProfiledKernels = ArrayBuffer[OpenCLAbstractKernel]()
      val profiledCircuit = new KernelCircuit
      // Create a kernel to flush the L2 cache between steps of the kernel-of-interest
      val cacheFlushWords = (Cog.profilerCacheFlushSizeMB * 1024 * 1024 / 4).toInt
      nonProfiledKernels += ConstantHyperKernel(ConstantScalarOp(0f), new FieldType(Shape(cacheFlushWords), Shape(), Float32))
      // Each input kernel will be a unique kernel not subject to CSE
      val inputKernels = Array.tabulate(numInputs) { i =>
        val inputVal = (i + 1).toFloat / numInputs
        val inputKernel = ConstantHyperKernel(ConstantScalarOp(inputVal), inputFieldTypes(i))
        nonProfiledKernels += inputKernel
        inputKernel
      }
      val inputRegisters = inputKernels.map(_.outputs(0))

      // Now invoke the provided circuitMaker function to complete the circuit to be profiled.
      circuitMaker(inputRegisters)

      // Look up the hash of the circuits code
      val circuitKey = Profiler.circuitToKey(profiledCircuit)

      // Determine whether a prior cached profiling result can be used
      val usefulPriorSample =
        if (forceProfiling)
          None
        else {
          Profiler.get(deviceName, circuitKey) match {
            case Some(cachedSample) =>
              if (cachedSample.runSteps >= runSteps ||
                  cachedSample.runSteps == runSteps && cachedSample.warmupSteps >= warmupSteps)
                Some(cachedSample)
              else
                None
            case None =>
              None
          }
        }
      // Pass back the cached profiling result, or perform a new profiling test.
      usefulPriorSample match {
        case Some(sample) => sample
        case None => timeKernelCircuit(circuitKey, profiledCircuit, nonProfiledKernels, warmupSteps, runSteps)
      }
    }
    finally
      Hypercircuit.setCurrent(mainCircuit)
  }

  /** Creates the circuit-driving input kernels and L2-cache-flushing kernel to surround a KernelCircuit completed
    * by the provided `circuitMaker` routine, which is given a set of inputs.  This routine then runs the completed
    * KernelCircuit for `runSteps` steps, after a warmup of `warmupSteps` steps.  The result is passed back as a
    * `ProfileSample` that includes whether the information was determined real-time, or whether it was a cached
    * result from an earlier compile.
    *
    * @param circuitKey A hash of the kernels in the circuit (harmlessly includes input driving kernels).
    * @param profiledCircuit The KernelCircuit to profile.
    * @param nonProfiledKernels The "infrastructure" kernels added by the profiler that should be excluded from timing.
    * @param warmupSteps The number of steps before clearing the runtime statistics.
    * @param runSteps The number of steps to execute while accumulating runtime statistics.
    * @return The execution time of the `profiledCircuit`, wrapped with other info in a `ProfileSample`.
    */
  private def timeKernelCircuit(circuitKey: String, profiledCircuit: KernelCircuit,
                        nonProfiledKernels: ArrayBuffer[OpenCLAbstractKernel], warmupSteps: Int, runSteps: Int): ProfileSample = {
    val evaluator = makeEvaluator(profiledCircuit)

    try {
      // Reset the KernelCircuit, which evaluates all kernels once (we'll throw these timings away).
      evaluator.reset
      // Do additional warmup steps as requested
      val furtherWarmupSteps = warmupSteps - 1
      if (furtherWarmupSteps > 0)
        evaluator.step(furtherWarmupSteps)
      // Reset profile statistics so they will only include the ensuing 'runSteps'
      profiledCircuit.traversePostorder { kernel =>
        kernel match {
          case k: OpenCLAbstractKernel => k.stats.reset
          case _ =>
        }
      }
      // Now run the circuit, accumulating execution time statistics.
      evaluator.step(runSteps)

      // Execution time for non-profiled kernels
      val nonProfiledKernelsTime = nonProfiledKernels.map(_.stats.avg).foldLeft(0.0)(_ + _)
      val totalTime = profiledCircuit.flatten.map(_.asInstanceOf[OpenCLAbstractKernel].stats.avg).foldLeft(0.0)(_ + _)
      // Execution time for the kernels of interest.
      val profiledKernelsTime = totalTime - nonProfiledKernelsTime
      val sample = ProfileSample(circuitKey, System.currentTimeMillis(), warmupSteps, runSteps, profiledKernelsTime, false)
      Profiler.put(deviceName, sample)
      sample
    }
    finally {
      platform.devices.foreach(_.releaseForReuse())
      TypedActor(actorSystem).stop(evaluator)
    }
  }

  // Create a temporary set of Actors to perform a single profiling experiment
  private def makeEvaluator(circuit: KernelCircuit): EvaluatorInterface = {
    var typedProps = TypedProps(classOf[EvaluatorInterface], new CircuitEvaluator(circuit, platform, mode, 0))

    // If we're not sharing threads, then add our Cog-custom dispatcher
    if (!Cog.threadSharing)
      typedProps = typedProps.withDispatcher("CogActors.one-actor-per-thread-dispatcher")

    val ta = TypedActor(actorSystem).typedActorOf(typedProps, name="ProfilerCircuitEvaluator" + id).
      asInstanceOf[EvaluatorInterface]

    val evaluatorActor = TypedActor(actorSystem).getActorRefFor(ta)
    ta.tellIdentity(evaluatorActor)
    ta
  }
}

/** A JVM-wide manager of cached profiling samples. */
object Profiler {

  /** The subdir of the Cog dir that holds the Profiler state. */
  private val ProfilerSubdir = "profiler"

  /** The directory that holds the Profiler state (e.g. $HOME/.cogexmachina/profiler). */
  val profilerDir = CogDir() match {
    case Some(dir) =>
      val profilerSubDir = new java.io.File(dir.getPath, ProfilerSubdir)
      if (profilerSubDir.isDirectory() || profilerSubDir.mkdir()) {
        Some(profilerSubDir)
      } else {
        Console.err.println(s"Unable to find or create user ${profilerSubDir.getPath} " +
          "directory.  Unable to save or restore Profiler state.")
        None
      }
    case None => None
  }

  private val caches = mutable.HashMap[String, ProfilerCache]()

  /** Translate a circuit to be profiled into a hash used as a key into a ProfileSample cache. */
  def circuitToKey(profiledCircuit: KernelCircuit): String = {
    val hashCodes = profiledCircuit.flatten.map(kernel =>
      new KernelSourceCode(kernel.asInstanceOf[OpenCLDeviceKernel].kernelCode).hashCode
    )
    val circuitString = new StringBuilder()
    hashCodes.foreach(circuitString append _)
    MD5(circuitString.toString())
  }

  // Synchronization protects database from simultaneous updates from multiple threads

  /** Check for prior profiling data for a circuit in the cache for `deviceName` based on the circuit's hash. */
  def get(deviceName: String, circuitHashCode: String): Option[ProfileSample] = synchronized {
    val cache = caches.getOrElseUpdate(deviceName, new ProfilerCache(deviceName))
    cache.get(circuitHashCode)
  }

  /** Record profiling data for a circuit in the cache for `deviceName` (circuit hash is part of the ProfileSample). */
  def put(deviceName: String, sample: ProfileSample): Unit = synchronized {
    val cache = caches.getOrElseUpdate(deviceName, new ProfilerCache(deviceName))
    cache.put(sample)
  }
}
