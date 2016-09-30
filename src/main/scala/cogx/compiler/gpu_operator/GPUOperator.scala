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

package cogx.compiler.gpu_operator

import cogx.cogmath.geometry.Shape
import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, BigTensorAddressing}
import cogx.compiler.parser.syntaxtree._
import cogx.platform.types._
import cogx.compiler.parser.op.{UserGPUOpcode, UserGPUWithVariantsOpcode}
import cogx.parameters.Cog
import cogx.compiler.gpu_operator.statement._
import cogx.compiler.gpu_operator.function.FieldBuffer

/** Represents a single GPU operator.
  *
  * @param resultTypes The types of the output fields.
  * @param names An array of kernel variant names used by the profiler and other Cog-internal debugging tools.
  * @param kernel A code generator function that takes a variant index and calls code-generating primitives.
  *
  * @author Greg Snider and Dick Carter
  */
private[cogx]
class GPUOperator private[GPUOperator](resultTypes: Array[FieldType], val names: Array[String], kernel: Int => Unit)
  extends UserGPULibrary with GPUOperatorParsingUtils
{
  val numVariants = names.size

  /** The operation for this user GPU operator (its HyperNode in the SyntaxTree) */
  val operation = GPUOperator.synchronized {

    val variantOpcodes = Array.tabulate(numVariants) { i =>
      // Execute the user code for the ith variant to set the global state of the GPUOperator parser
      kernel(i)

      /** Statements comprising the operator. */
      val statements = Statement.getStatements()
      /** Thread model for operator: big tensor vs. small tensor. */
      val addressing: AddressingMode = inferAddressingMode(statements)

      // Correctness checks
      checkBlockSemantics(statements)
      checkReadsAndWrites(statements, addressing == BigTensorAddressing)

      /** The code for the operator. No longer 'lazy' as this is used for opcode generation. */
      val code = toString(statements, addressing, resultTypes)
      /** Optional overrides of local and global thread allocation. */
      val (localThreads: Option[Shape], globalThreads: Option[FieldType]) =
        getThreadAllocationOverrides(statements)

      // Note that a multi-output GPUOperator will have multiple output fields
      // all with the same opcode.  This will be problematic until a new
      // underlying Node/Edge data structure is implemented for the SyntaxTree.

      /** The opcode for this user GPU operator */
      val opcode = UserGPUOpcode(addressing, code, localThreads, globalThreads, names(i))
      opcode
    }

    if (Cog.verboseUserGPUOperators) {
      for (i <- 0 until numVariants) {
        println()
        if (names(i) != "")
          print(names(i) + " ")
        if (numVariants == 1)
          println(s"GPUOperator code:")
        else
          println(s"GPUOperator code for variant $i:")
        println(variantOpcodes(i).code)
        println()
      }
    }

    /** The input fields read by the operator variants. */
    val inputFields = FieldBuffer.getFieldBuffers.map(_.field)

    val opcode =
      if (numVariants == 1)
        variantOpcodes(0)
      else
        new UserGPUWithVariantsOpcode(variantOpcodes, variantOpcodes(0).nameSuffix + "_et_al")

    /** The operation for this user GPU operator (its HyperNode in the SyntaxTree) */
    Operation(opcode, inputFields, resultTypes)
  }

  /** The output fields written by the operator. */
  val outputFields = resultTypes.map(t => Field(operation, t))
}

/** Creates a Field produced by a user-defined GPU operator.
  *
  * @author Greg Snider and Dick Carter
  */
object GPUOperator
        extends UserGPULibrary
{

  /** Produce a field from a user-defined named GPU operator. Example:
    * {{{
    *    def myfunction(f1: Field, f2: Field): Field =
    *      GPUOperator(f1.fieldType, "myfunction") {
    *         // semantic checks here
    *         kernel code here, accessing f1 and f2
    *      }
    * }}}
    *
    * @param resultType The type of field produced by the operator.
    * @param name An optional name, used by the profiler and other Cog-internal debugging tools.
    * @return A field produced by the user-defined operator.
    */
  def apply(resultType: FieldType, name: String)(kernel: => Unit): Field = {
    val operator = new GPUOperator(Array(resultType), Array(name), wrap(kernel))
    operator.outputFields(0)
  }

  /** Produce a field from a user-defined unnamed GPU operator. Example:
    * {{{
    *    def myfunction(f1: Field, f2: Field): Field =
    *      GPUOperator(f1.fieldType) {
    *         // semantic checks here
    *         kernel code here, accessing f1 and f2
    *      }
    * }}}
    *
    * Scala does not support multiple method signatures with default values,
    * hence the multitude of apply() signatures to support a default "" name.
    *
    * @param resultType The type of field produced by the operator.
    * @return A field produced by the user-defined operator.
    */
  def apply(resultType: FieldType)(kernel: => Unit): Field = apply(resultType, "")(kernel)

  /** Create a named GPU Operator that produces two fields. */
  def apply(t0: FieldType, t1: FieldType, name: String)
           (kernel: => Unit): (Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1))
  }

  /** Create an unnamed GPU Operator that produces two fields. */
  def apply(t0: FieldType, t1: FieldType)
           (kernel: => Unit): (Field, Field) = apply(t0, t1, "") (kernel)

  /** Create a named GPU Operator that produces three fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2))
  }

  /** Create an unnamed GPU Operator that produces three fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType)
           (kernel: => Unit): (Field, Field, Field) = apply(t0, t1, t2, "")(kernel)

  /** Create a named GPU Operator that produces four fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3))
  }

  /** Create an unnamed GPU Operator that produces four fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field) = apply(t0, t1, t2, t3, "")(kernel)

  /** Create a named GPU Operator that produces five fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4))
  }

  /** Create an unnamed GPU Operator that produces five fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field) = apply(t0, t1, t2, t3, t4, "")(kernel)

  /** Create a named GPU Operator that produces six fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, t5: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5))
  }

  /** Create an unnamed GPU Operator that produces six fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, "")(kernel)

  /** Create a named GPU Operator that produces seven fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, t5: FieldType, t6: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6))
  }

  /** Create an unnamed GPU Operator that produces seven fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, t6, "")(kernel)

  /** Create a named GPU Operator that produces eight fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7))
  }

  /** Create an unnamed GPU Operator that produces eight fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, t6, t7, "")(kernel)

  /** Create a named GPU Operator that produces nine fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
             t8: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7), out(8))
  }

  /** Create an unnamed GPU Operator that produces nine fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
            t8: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, t6, t7, t8, "")(kernel)

  /** Create a named GPU Operator that produces ten fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
             t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
             t8: FieldType, t9: FieldType, name: String)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9), Array(name), wrap(kernel))
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7), out(8), out(9))
  }

  /** Create an unnamed GPU Operator that produces ten fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
            t8: FieldType, t9: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, "")(kernel)

  /** Maps a GPUOperator `kernel` function into the form expected by GPUOperatorWithVariants. */
  private def wrap(kernel: => Unit)(i: Int): Unit = {
    require(i == 0, "Internal compiler error in treatment of single-variant GPUOperator.")
    kernel
  }

  // Begining of GPUOperators with variants

  /** Produce a field from a user-defined named GPU operator, with variants. Example:
    * {{{
    *    def myfunction(f1: Field, f2: Field): Field =
    *      val myVariantParams: Seq[MyParam] = ...
    *      val myVariantNames: Array[String] = myVariantParams.map("myFunction_" + _.toString)
    *      GPUOperator(f1.fieldType, myVariantNames) { i =>
    *         // semantic checks here
    *         val param = myVariantParams(i)
    *         kernel code (a function of `param`) goes here, accessing f1 and f2
    *      }
    * }}}
    *
    * @param resultType The type of field produced by the operator.
    * @param names The kernel variant names, used by the profiler and other Cog-internal debugging tools.
    * @return A field produced by the user-defined operator.
    */
  def apply(resultType: FieldType, names: Array[String])(kernel: Int => Unit): Field = {
    // GPUOPeratorSpec concurrency test has shown hanging behavior without `synchronized`.
    // Synchronized here keeps creation and reset of Statements Array together.
    val operator = new GPUOperator(Array(resultType), names, kernel)
    operator.outputFields(0)
  }
  /** Create a named GPU Operator that produces two fields. */
  def apply(t0: FieldType, t1: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1), names, kernel)
    val out = operator.outputFields
    (out(0), out(1))
  }

  /** Create a named GPU Operator that produces three fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2))
  }

  /** Create a named GPU Operator that produces four fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3))
  }

  /** Create a named GPU Operator that produces five fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4))
  }

  /** Create a named GPU Operator that produces six fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5))
  }

  /** Create a named GPU Operator that produces seven fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6))
  }

  /** Create a named GPU Operator that produces eight fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7))
  }

  /** Create a named GPU Operator that produces nine fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
            t8: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7), out(8))
  }

  /** Create a named GPU Operator that produces ten fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
            t8: FieldType, t9: FieldType, names: Array[String])
           (kernel: Int => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field, Field) =
  {
    val operator = new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9), names, kernel)
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7), out(8), out(9))
  }
}
