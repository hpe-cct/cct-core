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
import cogx.compiler.codegenerator.opencl.fragments.{AddressingMode, BigTensorAddressing, SmallTensorAddressing, TensorElementAddressing}
import cogx.compiler.gpu_operator.statement._
import cogx.platform.types.FieldType
import cogx.compiler.parser.semantics.SemanticError

/** Stand-alone functions for analysis of GPUOperator Statements.
  *
  * @author Greg Snider
  */
trait GPUOperatorParsingUtils extends SemanticError {
  /** Deduce the addressing mode from the GPUOperator statements
    *
    * @param statements The statements comprising the operator.
    * @return Addressing mode for the operator.
    */
  def inferAddressingMode(statements: Array[Statement]): AddressingMode = {
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
  def checkBlockSemantics(statements: Seq[Statement]) {
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
  def checkReadsAndWrites(statements: Array[Statement], bigTensor: Boolean) {
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
  def getThreadAllocationOverrides(statements: Seq[Statement]):
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
  def toString(statements: Seq[Statement], addressing: AddressingMode, resultTypes: Array[FieldType]): String = {
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

}
