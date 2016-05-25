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
import cogx.compiler.codegenerator.opencl.fragments.{TensorElementAddressing, SmallTensorAddressing, BigTensorAddressing, AddressingMode}
import cogx.compiler.parser.syntaxtree._
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import cogx.compiler.parser.op.UserGPUOpcode
import cogx.parameters.Cog
import cogx.compiler.gpu_operator.statement._
import cogx.compiler.gpu_operator.statement.IfBlock
import cogx.compiler.gpu_operator.statement.ElseBlock
import cogx.compiler.gpu_operator.function.FieldBuffer

/** Represents a single GPU operator.
  *
  * @param resultTypes The types of the output fields.
  * @param name Name used by the profiler and other Cog-internal debugging tools.
  */
private[cogx]
class GPUOperator(resultTypes: Array[FieldType], val name: String)
  extends UserGPULibrary
{
  /** Statements comprising the operator. */
  private val statements = Statement.getStatements()
  /** Thread model for operator: big tensor vs. small tensor. */
  private var addressing: AddressingMode = inferAddressingMode(statements)

  // Correctness checks
  checkBlockSemantics(statements)
  checkReadsAndWrites(statements, addressing == BigTensorAddressing)

  /** The code for the operator. No longer 'lazy' as this is used for opcode generation. */
  val code = toString(statements)
  /** Hashcode is based on the code text */
  override val hashCode = code.hashCode
  /** Optional overrides of local and global thread allocation. */
  val (localThreads: Option[Shape], globalThreads: Option[FieldType]) =
    getThreadAllocationOverrides(statements)
  /** The input fields read by the operator. */
  private val inputFields = FieldBuffer.getFieldBuffers.map(_.field)

  // Note that a multi-output GPUOperator will have multiple output fields
  // all with the same opcode.  This will be problematic until a new
  // underlying Node/Edge data structure is implemented for the SyntaxTree.

  /** The opcode for this user GPU operator */
  val opcode = UserGPUOpcode(this)

  /** The operation for this user GPU operator (its HyperNode in the SyntaxTree) */
  val operation = Operation(opcode, inputFields, resultTypes)

  /** The output fields written by the operator. */
  private val outputFields = resultTypes.map(t => createResultField(t, operation))

  if (Cog.verboseUserGPUOperators) {
    println()
    if (name != "")
      print(name + " ")
    println("GPUOperator code:")
    println(code)
    println()
  }

  /** If possible, "superthread" the kernel, creating more work groups, by
    * allocating a thread for each tensor element. This only applies to
    * BigTensorAddressing.
    *
    * @return True if kernel can be "superthreaded."
    */
  def superthread(): Boolean = {
    addressing match {
      case SmallTensorAddressing =>
        false
      case BigTensorAddressing =>
        addressing = TensorElementAddressing
        true
      case TensorElementAddressing =>
        true
    }
  }

  /** Get the thread addressing mode for this kernel.
    *
    * @return Thread addressing mode.
    */
  def threadAddressing: AddressingMode =
    addressing

  /** Create a field of a given type as an output of the given (possibly multi-output) operation.
    *
    * @param resultType The type of the desired field.
    * @return The field.
    */
  private def createResultField(resultType: FieldType, operation: Operation): Field = {
    // Create the correct type of result field.
    val resultField = resultType.elementType match {
      case Float32 =>
        resultType.tensorOrder match {
          case 0 =>
            new ScalarField(operation, resultType)
          case 1 =>
            new VectorField(operation, resultType)
          case 2 =>
            new MatrixField(operation, resultType)
          case x =>
            throw new Exception("Unsupported tensor order: " + x)
        }
      case Complex32 =>
        resultType.tensorOrder match {
          case 0 =>
            new ComplexField(operation, resultType)
          case 1 =>
            new ComplexVectorField(operation, resultType)
          case x =>
            throw new Exception("Unsupported tensor order: " + x)
        }
      case Uint8Pixel =>
        new ColorField(operation, resultType)
      case x =>
        throw new Exception("Unsupported element type: " + x)
    }
    resultField
  }

  /** Deduce the addressing mode from the GPUOperator statements
    *
    * @param statements The statements comprising the operator.
    * @return Addressing mode for the operator.
    */
  private def inferAddressingMode(statements: Array[Statement]): AddressingMode = {
    // We first check for a _globalThreads statement. If that statement has
    // a non-zero-dimension tensor shape, we use TensorElementAddressing.
    for (statement <- statements) {
      statement match {
        case GlobalThreadsStatement(workFieldType) =>
          if (workFieldType.tensorShape != Shape())
            return TensorElementAddressing
        case _ =>
      }
    }

    // If we get here, we must continue analysis.
    // The first two statements could be _localThreads and/or _globalThreads
    // which we skip over.
    def isThreadsStatement(statement: Statement): Boolean = {
      statement match {
        case GlobalThreadsStatement(workFieldType) => true
        case LocalThreadsStatement(shape) => true
        case _ => false
      }
    }
    def isTensorElementLoop(statement: Statement): Boolean = {
      statement match {
        case block: ForEachTensorElementBlock => true
        case _ => false
      }
    }
    var index = 0
    if (isThreadsStatement(statements(index)))
      index += 1
    if (isThreadsStatement(statements(index)))
      index += 1
    if (isTensorElementLoop(statements(index)))
      BigTensorAddressing
    else
      SmallTensorAddressing
  }

  /** Check semantics of statements, e.g. that EndIf has a matching If */
  private def checkBlockSemantics(statements: Seq[Statement]) {
    val blockStack = new collection.mutable.Stack[Block]
    var lastPoppedBlock: Block = null
    var tensorElementBlocks = 0
    for (index <- 0 until statements.length) {
    //for (statement <- statements) {
      val statement = statements(index)
      statement match {
        case block: FunctionBlock =>
          blockStack.push(block)
        case block: ForEachTensorElementBlock =>
          tensorElementBlocks += 1
          if (tensorElementBlocks > 1)
            error("only one _forEachTensorElement block may appear " +
              "in a GPUOperator.")
          blockStack.push(block)
        case block: EndForEachTensorElement =>
          lastPoppedBlock = blockStack.pop()
        case block: IfBlock =>
          blockStack.push(block)
        case block: EndIf =>
          lastPoppedBlock = blockStack.pop()
        case block: ElseBlock =>
          lastPoppedBlock match {
            case _: IfBlock =>
            case _: ElseIfBlock =>
            case x =>
              error("unexpected _else, does not match an _if or _elseif")
          }
          blockStack.push(block)
        case block: EndElse =>
          blockStack.top match {
            case _: ElseBlock =>
              lastPoppedBlock = blockStack.pop()
            case x =>
              error("unexpected }, does not match an _else")
          }
        case block: ElseIfBlock =>
          lastPoppedBlock match {
            case _: IfBlock =>
            case _: ElseIfBlock =>
            case x =>
              error("unexpected _elseif, does not match an _if or _elseif")
          }
          blockStack.push(block)
        case block: EndElseIf =>
          blockStack.top match {
            case _: ElseIfBlock =>
              lastPoppedBlock = blockStack.pop()
            case x =>
              error("unexpected }, does not match an _elseif")
          }
        case block: ForBlock =>
          blockStack.push(block)
        case block: EndFor =>
          blockStack.top match {
            case _: ForBlock =>
              lastPoppedBlock = blockStack.pop()
            case x =>
              error("unexpected _endfor, does not match a _for")
          }
        case block: WhileBlock =>
          blockStack.push(block)
        case block: AnonymousBlock =>
          blockStack.push(block)
        case block: EndAnonymous =>
          blockStack.top match {
            case _: AnonymousBlock =>
              lastPoppedBlock = blockStack.pop()
            case x =>
              error("unexpected }, does not match a {")
          }
        case block: EndWhile =>
          blockStack.top match {
            case _: WhileBlock =>
              lastPoppedBlock = blockStack.pop()
            case x =>
              error("unexpected }, does not match a _while")
          }
        case nonblock: Statement =>
      }
    }
    if (blockStack.size != 0)
      error("unbalanced block nesting, missing a }: " + blockStack.size)
  }

  /** Check that read and write statements in a GPUOperator are legal for the
    * given thread addressing mode. See the "User-defined GPU operators on
    * Cog-X" document for details.
    *
    * @param statements Statements for the operator.
    * @param bigTensor True if big tensor addressing is being used.
    */
  private def checkReadsAndWrites(statements: Array[Statement], bigTensor: Boolean) {
    if (bigTensor) {
      for (statement <- statements) {
        statement match {
          case x: WriteTensor =>
            error("_writeTensor(...) is illegal in a 'big tensor' GPUOperator ")
          case x: WriteTensorNonlocal =>
            error("_writeTensor(...) is illegal in a 'big tensor' GPUOperator ")
          case _ =>
        }
      }
    } else {
      val lastStatement = statements.last
      /*
      lastStatement match {
        case _: WriteTensor =>
        case _: WriteTensorNonlocal =>
        case x =>
          error("the last statement in a 'small tensor' GPUOperator " +
            "must be _writeTensor(...). Found " + x)
      }
      */
      // Check that all WriteTensor statements occur at the end
      var sawWriteStatements = false
      for (statement <- statements) {
        statement match {
          case _: WriteTensor =>
            sawWriteStatements = true
          case _: WriteTensorNonlocal =>
            sawWriteStatements = true
          case nonWriteStmt => if (sawWriteStatements)
            error("Non-write statement appears after write statements " +
              s"in a GPUOperator: $nonWriteStmt")
        }
      }
    }
  }

  /** Extract optional _localThreads and _globalThreads statements.
    *
    * @param statements All statements for the operator.
    * @return A Tuple2 containing the optional localThreads work group shape,
    *         followed by the optional global threads working field type.
    */
  private def getThreadAllocationOverrides(statements: Seq[Statement]):
    (Option[Shape], Option[FieldType]) =
  {
    var localOverride: Option[Shape] = None
    var globalOverride: Option[FieldType] = None
    for (statement <- statements) {
      statement match {
        case LocalThreadsStatement(workGroupShape) =>
          if (localOverride == None)
            localOverride = Some(workGroupShape)
          else
            error("more than one _localThreads statement in a GPUOperator")
        case GlobalThreadsStatement(fieldType) =>
          if (globalOverride == None)
            globalOverride = Some(fieldType)
          else
            error("more than one _globalThreads statement in a GPUOperator")
        case _ =>
      }
    }
    (localOverride, globalOverride)
  }

  /** Convert a function to an OpenCL string. */
  private def toString(statements: Seq[Statement]): String = {
    val buffer = new StringBuffer
    def indent(level: Int) {
      for (i <- 0 until level)
        buffer append "    "
    }
    var level = 2
    for (statement <- statements) {
      statement match {
        case block: ForEachTensorElementBlock =>
          addressing match {
            case SmallTensorAddressing =>
              error("internal error: illegal thread addressing mode")
            case BigTensorAddressing =>
              // Normal block processing of the outer _forEachTensorElement(...)
              indent(level)
              buffer append block.toString
              level += 1
              buffer append "\n"
            case TensorElementAddressing =>
              // The "for" loop goes away, _tensorElement is supplied by
              // the HyperKernel framework in this case.
              indent(level)
              buffer append "{"
              level += 1
              buffer append "\n"
          }
        case block: Block =>
          indent(level)
          buffer append block.toString
          level += 1
          buffer append "\n"
        case end: BlockEnd =>
          level -= 1
          indent(level)
          buffer append end.toString
          buffer append "\n"
        case write: WriteStatement =>
          indent(level)
          val outputIndex: Int = write.index.index
          buffer append write.toString(resultTypes(outputIndex))
          buffer append ";\n"
        case x =>
          indent(level)
          buffer append x.toString
          buffer append ";\n"
      }
    }
    buffer.toString
  }
  /** Two GPUOperators are equal if they have the same code text. */
  override def equals(other: Any): Boolean =
    other match {
      case that: GPUOperator =>
        this.code == that.code
      case _ => false
    }
}

/** Creates a Field produced by a user-defined GPU operator.
  *
  * @author Greg Snider
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
    // GPUOPeratorSpec concurrency test has shown hanging behavior without `synchronized`.
    // Synchronized here keeps creation and reset of Statements Array together.
    val operator = synchronized {
      kernel
      new GPUOperator(Array(resultType), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4, t5), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8), name)
    }
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
    val operator = synchronized {
      kernel
      new GPUOperator(Array(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9), name)
    }
    val out = operator.outputFields
    (out(0), out(1), out(2), out(3), out(4), out(5), out(6), out(7), out(8), out(9))
  }

  /** Create an unnamed GPU Operator that produces ten fields. */
  def apply(t0: FieldType, t1: FieldType, t2: FieldType, t3: FieldType,
            t4: FieldType, t5: FieldType, t6: FieldType, t7: FieldType,
            t8: FieldType, t9: FieldType)
           (kernel: => Unit): (Field, Field, Field, Field, Field, Field, Field, Field, Field, Field) =
    apply(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, "")(kernel)
}
