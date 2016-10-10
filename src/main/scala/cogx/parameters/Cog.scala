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

package cogx.parameters

/** Controls compilation process, allowing various levels of verbosity for
  * insight into the process. No module should print out information without
  * consulting this. The user may modify the values of the fields here within
  * their program.  Alternatively, the user can set a JVM command line arg
  * to modify the behavior of an already compiled program.
  *
  * @author Greg Snider
  */
object Cog {
  /** Display optimizer results. */
  var verboseOptimizer = setBoolean("verboseOptimizer")
  /** Print out optfimized circuit for ComputeGraph. */
  var printOptimized = setBoolean("printOptimized")
  /** Print out details of merging GPU kernels. */
  var verboseKernelMerging = setBoolean("verboseKernelMerging")
  /** Make the code generator chatty. */
  var verboseCodeGenerator = setBoolean("verboseCodeGenerator")
  /** Print OpenCL resources allocated (device kernels, CPU kernels, buffers).*/
  var verboseOpenCLResources = setBoolean("verboseOpenCLResources", default=true)
  /** Print out details of OpenCL device allocation. */
  var verboseOpenCLDevices = setBoolean("verboseOpenCLDevices")
  /** Print out details of OpenCL platform. */
  var verboseOpenCLPlatform = setBoolean("verboseOpenCLPlatform")
  /** Filter string for pruning down list of available OpenCL platforms. */
  val platformFilter = setString("platformFilter")
  /** Display debugger steps. */
  var verboseDebugger = setBoolean("verboseDebugger")
  /** Enable profiling. */
  var profile = setBoolean("profile")
  /** Number of steps between writing profile statistics to the console. */
  var profileSize = setInt("profileSize", 10000)
  /** Set this true for additional compiler information. This deletes any
    * cached programs, forcing a recompilation that will provide the information.
    * This should always be set false except when debugging.
    */
  var deleteProgramCache = setBoolean("deleteProgramCache")
  /** Enable printing of OpenCL source code during compilation */
  var printSource = setBoolean("printSource")
  /** Enable printing of OpenCL "binary" code. For Nvidia, the ptx assembly. */
  var printAssembly = setBoolean("printAssembly")
  /** Chatty version of User GPU Operators to ease debugging. */
  var verboseUserGPUOperators = setBoolean("verboseUserGPUOperators")
  /** Chatty version of User GPU Expressions to ease debugging. */
  var verboseUserGPUExpressions = setBoolean("verboseUserGPUExpressions")
  /** Enable multi-plane/workgroup kernel optimization for frame projection followed by block reduce */
  var fastProjectFrame = setBoolean("fastProjectFrame", default=true)
  /** Enable merging of the two kernels involved in the sequence blockReduceSum(convolve(...ProjectFrame...))  */
  var projectFrameMerging = setBoolean("projectFrameMerging", default=true)
  /** Enable merging of the two kernels involved in the sequence blockReduceSum(convolve(...BackProjectFrame...))  */
  var backProjectFrameMerging = setBoolean("backProjectFrameMerging", default=true)
  /** Enable merging of the two kernels involved in the sequence blockReduceSum(convolve(...FilterAdjoint...))  */
  var filterAdjointMerging = setBoolean("filterAdjointMerging", default=true)
  /** Enable experimental merger optimizations (e.g. forward-merging of kernels that use local memory) */
  var localMemoryMerging = setBoolean("localMemoryMerging", default = true)
  /** Enables the core platform Akka actors to share threads, rather than use a custom dispatcher that gives each actor
    * its own thread.  The system has not been extensively tested with this set to "true" and so may deadlock. */
  var threadSharing = setBoolean("threadSharing")
  /** Enables the cpu kernel Akka actors to share threads, rather than use a custom dispatcher that gives each actor
    * its own thread.  The system has not been extensively tested with this set to "true" and so may deadlock. */
  var cpuKernelThreadSharing = setBoolean("cpuKernelThreadSharing")
  /** Allow the kernels to execute in a different order from which they were submitted to the GPU command queues.
    * This will result in bigger model GPU memory usage with sadly no performance boost on current OpenCL platforms. */
  var outOfOrderExecution = setBoolean("outOfOrderExecution")
  /** Enable virtual field registers to share OpenCl buffers */
  val bufferSharing = setBoolean("bufferSharing", default=true)
  /** Enable cpu DirectBuffer memory to be pinned. Only verified to work with NVidia, so leave off by default. */
  val pinnedBuffers = setBoolean("pinnedBuffers", default=false)
  /** Enable warning about user CPU Operators having apply() invoked multiple times- bad if they have mutable state. */
  val checkUserOperators = setBoolean("checkUserOperators", default=true)
  /** The InOrderSharedLatchAllocator before Cog 4.3.6 mishandled some models.  Alert the user if they were effected. */
  val checkLegacyBufferSharing = setBoolean("checkLegacyBufferSharing", default=true)
  /** The Reshape operator before Cog 4.3 had a different behavior for some uses.  Alert the user if they were effected. */
  val checkLegacyReshape = setBoolean("checkLegacyReshape", default=false)
  /** Maximum number of probed fields that the graphical layout pane will attempt to display. */
  var maxLayoutProbedFields = setInt("maxLayoutProbedFields", 100)
  /** Kernel CommandQueue flush frequency (users should generally leave this alone). */
  var kernelFlushFrequency = setInt("kernelFlushFrequency", 100)
  // One can use -Xcheck:jni to diagnose the need for libjsig.so.
  /** Check that LD_PRELOAD has libjsig.so.  This was the remedy for JVM crashes under NVIDIA drivers circa 2010. */
  var checkForLibjsig = setBoolean("checkForLibjsig", default=false)
  /** Instructs code generators to make best single guess for a kernel, rather than generate variants to be profiled. */
  var noVariants = setBoolean("noVariants", default=false)
  /** Output activities of the compile-time Profiler. */
  var verboseProfiler = setBoolean("verboseProfiler", default=false)
  /** Profiler should touch this much memory (in MB) to ensure a flushed L2 cache. */
  var profilerCacheFlushSizeMB = setDouble("profilerCacheFlushSizeMB", default=4.0)
  /** Number of steps the profiler executes prior to timing its kernels. */
  var profilerWarmupSteps = setInt("profilerWarmupSteps", default=1)
  /** Number of steps the profiler executes to time its kernels (after the warmup steps). */
  var profilerSteps = setInt("profilerSteps", default=1)
  /** Directs the profiler to always profile the kernels, ignoring (but nonetheless updating) the profiler cache. */
  var forceProfiling = setBoolean("forceProfiling", default=false)
  /** Directs the profiler to nuke the profiler cache. */
  var deleteProfilerCache = setBoolean("deleteProfilerCache", default=false)
  /** Release version, is not automatically discernible from the release jar manifest */
  val defaultReleaseVersion = "5.0.0"

  /** return the release version as a String.  When this is invoked out of the libcog jar,
    * the getImplementationVersion will return the value from the file ./META-INF/MANIFEST.MF
    * as put there by sbt, for example.  When invoked from the development environment, this
    * approach returns null, so we supply a value whose major and minor release number are
    * correct.
    * @return The release version as a string
    */
  def releaseVersion = cogx.getClass.getPackage.getImplementationVersion match {
    case null => defaultReleaseVersion
    case s => s
  }

  /** Looks up and sets a boolean parameter based on a JVM arg, e.g. -Dcog.verboseOptimizer */
  def setBoolean(parameter: String, default: Boolean = false) = {
    val parameterStr = System.getProperty("cog." + parameter, default.toString)
    parameterStr == "true" || parameterStr == "1" || parameterStr == ""
  }

  /** Looks up and sets an integer parameter based on a JVM arg, e.g. -Dcog.profileSize=100 */
  def setInt(parameter: String, default: Int = 0) = {
    val parameterStr = System.getProperty("cog." + parameter, default.toString)
    parameterStr.toInt
  }

  /** Looks up and sets a Double parameter based on a JVM arg, e.g. -Dcog.profilerCacheFlushSizeMB=1.5 */
  def setDouble(parameter: String, default: Double = 0.0) = {
    val parameterStr = System.getProperty("cog." + parameter, default.toString)
    parameterStr.toDouble
  }

  /** Looks up and sets a string parameter based on a JVM arg, e.g. -Dcog.platform="amd" */
  def setString(parameter: String, default: String = null.asInstanceOf[String]) = {
    System.getProperty("cog." + parameter, default)
  }

}