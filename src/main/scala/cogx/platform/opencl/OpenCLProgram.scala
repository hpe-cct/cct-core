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

package cogx.platform.opencl

import scala.collection.mutable.Set
import com.jogamp.opencl.{CLContext, CLDevice, CLProgram}
import cogx.utilities.{DeleteDirectory, Timer}
import cogx.parameters.Cog

/** An OpenCL program (a collection of separately runnable kernels)
  *
  * @author Greg Snider
  *
  * @param device The wrapped OpenCL device on which this program will be run.
  */
private[cogx]
class OpenCLProgram(device: OpenCLDevice) {

  /** The platform of the device. */
  private def platform = device.platform
  /** Source code for each kernel in the program. */
  private val sources = Set[KernelSourceCode]()
  /** OpenCL compilation options. */
  private val buildOptions = "-cl-mad-enable -cl-no-signed-zeros" +
          (if (platform.isAMD) " -w" else "") +
          (if (platform.isNVidia) " -cl-nv-verbose" else "")
  /** The context of this program. */
  private def clContext = platform.clContext
  /** The underlying OpenCL resource for this device. */
  private def clDevice = device.clDevice

  // Other NVidia flags that are available:
  //
  // -cl-nv-op-level=3             // The default must be at the highest level. Playing with this only makes things worse.
  //
  // -cl-nv-maxrregcount=32        // Applies to all the kernels that are compiled together, so not that useful. Creates
  //                               // register spilling if you set it too low (although occupancy improvements may accrue).

  /** The built program for this device. */
  private var builtProgram: CLProgram = null

  /** Compile all source code added using `addKernelSourceCode` into
    * a single CLProgram.
    *
    * @param resourceDescriptor String to tack onto program build messages to indicate model resources.
    */
  def getProgram(resourceDescriptor: String = ""): CLProgram = {
    synchronized {
      if (builtProgram == null) {
        val timer = new Timer
        if (Cog.verboseOpenCLPlatform) {
          println("OpenCLPlatform.getProgram.  Building source string...")
          timer.start
        }
        //val sourceSet: Set[KernelSourceCode] = sources(device)
        val sourceStringBuffer = new StringBuffer
        sources.toArray.map(_.code).
                sortWith((s1, s2) => s1.compareTo(s2) < 0).foreach{
          sourceStringBuffer append _
        }
        val sourceString = sourceStringBuffer.toString
        if (Cog.verboseOpenCLPlatform)
          timer.stop

        if (Cog.verboseOpenCLPlatform) {
          println("    Building program for " + clContext)
          for (k <- sources) {
            val kernelNames = k.names
            println("       kernel: " + kernelNames)
          }
          println()
        }
        if (Cog.printSource)
          println(sourceString)

        if (Cog.verboseOpenCLPlatform) {
          println("    Creating program...")
          timer.start
        }
        val program = clContext.createProgram(sourceString)
        if (Cog.verboseOpenCLPlatform)
          timer.stop
        // The register usage for a kernel is present in the build log, but
        // only if the compilation is performed.  NVidia caches kernels it
        // has already compiled (with identical compiler args) under ~/.nv
        // Thus, to ensure compilation, we must nuke the directory.
        val binaryCache = OpenCLNvidia.binaryCacheDirectory
        if (Cog.deleteProgramCache && platform.isNVidia && binaryCache != null) {
          println("    Deleting " + binaryCache +
                  " to force OpenCL kernel compilation.")
          DeleteDirectory(binaryCache)
        }
        if (platform.isNVidia)
          OpenCLNvidia.checkEnvironmentVariablesForLD_PRELOAD()
        try {
          if (Cog.verboseOpenCLPlatform) {
            println("    Building program...")
            timer.start
          }
          program.build(buildOptions, clDevice)
          if (Cog.verboseOpenCLPlatform)
            timer.stop
          if (Cog.verboseOpenCLPlatform)
            println("        device " + clDevice + ": "+ sources.size + " unique kernels compiled.")
        } catch {
          case e: Exception =>
            println("    OpenCL program compile failed.")
            println(sourceString)
            println(e)
            System.exit(1)
        }
        if (Cog.verboseOpenCLPlatform) {
          System.out.print("    OpenCL program build log ------------------------")
          val buildLog = program.getBuildLog(clDevice)
          if (buildLog == "")
            println("      <empty>")
          else
            println(buildLog.replaceAll("\n", "\n      "))
          println("    -------------------------------------------------")
        }
        if (Cog.verboseOpenCLPlatform)
          println()

        if (Cog.printAssembly) {
          val ptxAsMapOfByteArrays = program.getBinaries
          val ptxAsCharArray =
            ptxAsMapOfByteArrays.get(clDevice).toArray.map( byte => byte.toChar )
          println(ptxAsCharArray.mkString(""))
        }

        if (Cog.verboseOpenCLResources)
          print(resourceDescriptor)

        builtProgram = program
      }
    }
    builtProgram
  }

  /** Make a KernelSourceCode object for this kernel code string and add it to the program
    * for this device.
    *
    * @param source  The OpenCL code for the kernel (as a string).
    * @return The created KernelSourceCode object.
    */
  def addKernelSourceCode(source: String): Unit = {
    val sourceCode = new KernelSourceCode(source, Cog.verboseOpenCLPlatform)
    synchronized {
      val kernelSuppliedName = sourceCode.kernelSuppliedName
      val nameAsRun = sourceCode.nameAsRun
      //if (Cog.verboseOpenCLPlatform)
      //  println("    ATTEMPTING to register KERNEL " + kernelSuppliedName +
      //          " (nameAsRun: " + nameAsRun + ")")
      if (!(sources contains sourceCode)) {
        //if (Cog.verboseOpenCLPlatform)
        //  println("    ...REGISTERED kernel " + kernelSuppliedName)
        sources += sourceCode
        // We've changed sources, so invalidate program so it's rebuilt
        builtProgram = null
      } else {
//        var aliasesMsg = ""
        if (Cog.verboseOpenCLPlatform) {
          val prototype = sources.find(that => that == sourceCode) match {
            case Some(prototypeCode) => prototypeCode
            case None => null.asInstanceOf[KernelSourceCode]
          }
          prototype.addAlias(kernelSuppliedName)
//          aliasesMsg = ", aliases = " + prototype.names
        }
        //if (Cog.verboseOpenCLPlatform)
        //  println("    ...KERNEL " + kernelSuppliedName + " already registered")
      }
    }
  }
}
