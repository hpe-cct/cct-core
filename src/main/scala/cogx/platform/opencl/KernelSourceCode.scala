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

import java.util.StringTokenizer
import cogx.platform.checkpoint.{ObjectRestorer, RestoreFactory, Saveable, ObjectSaver}
import cogx.utilities.MD5

/** Source code for an OpenCL kernel.
  *
  * @author Greg Snider and Dick Carter
  *
  * @param kernelSuppliedCode source code with a unique numbered name
  * @param maintainAliases flag to enable human-readable name in kernel source
  *
  * Setting maintainAliases = false will preserve the NVidia compiled-kernel
  * cache better across changes in the program and different programs.  It
  * should only be enabled if a human is going to look at the source, and try
  * to reconcile the source against the compiler-assigned names assigned
  * upstream.
  *
  */
private[cogx]
final class KernelSourceCode(kernelSuppliedCode: String, maintainAliases: Boolean = false) extends Saveable {

  /** Parse the beginning of the kernel source code and return the kernel's function name. */
  private def extractKernelName(kernelSource: String): String = {
    // Search for first occurrence of "__kernel"
    val tokenizer =
      new StringTokenizer(kernelSource, " ()[]+-*/~!@#$%^&={}':;,<>?.\n\t\r")
    require(tokenizer.hasMoreTokens)
    var token = tokenizer.nextToken
    while (!token.equals("__kernel")) {
      require(tokenizer.hasMoreTokens, "OpenCL source code has no kernel.")
      token = tokenizer.nextToken
    }
    require(tokenizer.hasMoreTokens, "OpenCL source code ends with '__kernel'.")
    val second = tokenizer.nextToken
    require(second.equals("void"), "Non-void OpenCL kernel.")
    require(tokenizer.hasMoreTokens, "OpenCL kernel name missing.")
    val third = tokenizer.nextToken
    third
  }

  private val aliases = collection.mutable.Set[String]()

  val kernelSuppliedName = extractKernelName(kernelSuppliedCode)
  aliases += kernelSuppliedName

  // Kernel will be given a unique name based on its contents.  Strip off any
  // kernel and GPUOpcode numbers that might vary from run to run so we hit
  // the NVidia compiled-kernel cache as much as possible.
  val nameWithIdRemoved = kernelSuppliedName.replaceFirst("_[a-fA-F0-9]+$", "")
  private val codeWithIdRemoved = kernelSuppliedCode.replaceFirst("__kernel[ \t\n\t\f]+void[ \t\n\t\f]+" + kernelSuppliedName, "__kernel void " + nameWithIdRemoved)
  private val codeMD5 = MD5(codeWithIdRemoved)

  val nameAsRun = nameWithIdRemoved + "_" + codeMD5
  val codeAsRun = codeWithIdRemoved.replaceFirst("__kernel void " + nameWithIdRemoved, "__kernel void " + nameAsRun)

  def code = if (maintainAliases)
    aliasesHeader + codeAsRun
  else
    codeAsRun

  def names = aliases.toArray.sortWith( (s1, s2) => (s1 < s2) ).mkString(", ")

  private def aliasesHeader = "// Kernel name aliases: " + names + "\n"

  def addAlias(alias: String) {
    if (maintainAliases)
      aliases += alias
  }

  /** Two KernelSourceCodes are equal if they have the same MD5 hash of the
    * source text, ignoring the kernel name.  */
  override def equals(other: Any): Boolean =
    other match {
      case that: KernelSourceCode =>
        codeMD5.equals(that.codeMD5)
      case _ => false
    }

  override val hashCode = codeMD5.hashCode

  /** Save this instance using the facilities of the ObjectSaver */
  def save(saver: ObjectSaver) {
    saver.writeString("functionName", nameAsRun)
    saver.writeString("aCompilerName", kernelSuppliedName)
    saver.writeString("code", code)
  }
}

/** Factory object for creating KernelSourceCode instances from their stored representations. */
object KernelSourceCode extends RestoreFactory {
  /** Create a KernelSourceCode instance through use of the provided ObjectRestorer
    * @param restorer The restorer through which to read the new object state.
    * @return The created KernelSourceCode based on the read information.
    */
  def restore(restorer: ObjectRestorer): KernelSourceCode = {
    val functionName = restorer.readString("functionName")
    val aCompilerName = restorer.readString("aCompilerName")
    val code = restorer.readString("code")
    val kernelSuppliedCode = code.replace(functionName, aCompilerName)
    val retVal = new KernelSourceCode(kernelSuppliedCode)
    retVal.code // Instantiate data members
    retVal
  }
}