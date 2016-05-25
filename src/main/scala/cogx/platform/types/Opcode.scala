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

package cogx.platform.types

/** Intrinsic operations performed by kernels.
  *
  * Every opcode has a "name" which must be unique
  * for each opcode. By default the short version of the opcode's class name
  * is used, but this may be augmented by a suffix if the class name is
  * insufficient to distinguish the opcode.
  *
  * @param nameSuffix Optional suffix to add to the Opcodes class name in order
  *        to create a unique name for the opcode.
  *
  * This complicated naming may have been put in to try and protect the system
  * from someone adding an Opcode and forgetting to give it a unique name.
  * The 'name' field became the kernel name and having different kernels by
  * the same name was bad.  Now, we hash the entire kernel code contents and
  * add the hash to the kernel name, so different kernels will always have
  * different names.  Bottom line: we can experiment with simplifying this
  * down the road, as in:
  *
  * abstract class Opcode(val name: String)
  *
  * or
  *
  * abstract class Opcode() { name: String }
  *
  * Second thought: perhaps this was set up to give a sensible default name for
  * an Opcode with less repetitive info. Rather than:
  *
  * case class UpSampleOp(factor: Int) = UnaryOperator("upsample_" + factor)
  *
  * we need only:
  *
  * case class UpSampleOp(factor: Int) = UnaryOperator(factor.toString)
  *
  * The latter will stay correct, even if someone renames the Opcode.  We are
  * not taking advantage of this sensible default currently.
  *
  * @author Greg Snider
  */
private[cogx]
abstract class Opcode(nameSuffix: String = "") {

  // The name appears in the OpenCL kernel name and so must be filtered of illegal
  // characters the user may have inserted as part of a GPUOperator name

  /** String used to name the opcode */
  lazy val name =
    getClass.toString.split('.').last.split('$')(0).stripSuffix("Op").stripSuffix("Opcode") +
            (if (nameSuffix != "") "_" + nameSuffix else "").filter(c => c.isLetterOrDigit || c == '_')
}

