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

package cogx.compiler.cpu_operator

import cogx.parameters.Cog
import cogx.platform.cpumemory._
import cogx.platform.types._
import cogx.platform.types.ElementTypes._
import java.lang.reflect.{InvocationTargetException, Method}
import cogx.compiler.parser.semantics.SemanticError
import cogx.compiler.parser.syntaxtree._
import cogx.compiler.parser.op.UserOpcode
import scala.collection.mutable.ArrayBuffer
import cogx.platform.cpumemory.readerwriter.{MatrixFieldWriter, VectorFieldWriter, ScalarFieldWriter, ComplexFieldWriter, ColorFieldWriter}

/** A user-defined operator, allowing the user to write Cog code that can't
  * be easily parallelized and must be executed on the CPU.
  *
  * To define an Operator, create an object which extends this class and define
  * a 'compute' method in it. Here's an example:
  * {{{
  *
  * /** An operator which flips a picture upside down. */
  * object upsideDown extends Operator {
  *   def compute(in: ScalarFieldReader, out: ScalarFieldWriter) {
  *     out.setShape(in.fieldShape)
  *     for (row <- 0 until in.rows; col <- 0 until in.columns) {
  *       val pixel = in.read(row, col)
  *       result.write(in.rows - row - 1, col, pixel)
  *     }
  *   }
  * }
  *
  * }}}
  * To invoke this operator, use it like a function:
  * {{{
  *
  *  val graph = new ComputeGraph {
  *     val field = ScalarField(...)
  *     val flipped = upsideDown(field)
  *  }
  *
  * }}}
  * This class does the necessary reflective magic to connect up the `compute`
  * method, which is written using *FieldReader and *FieldWriter objects, to
  * Fields.
  *
  * @author Greg Snider
  */
abstract class Operator
        extends SemanticError
{
  /** Debugging flag. */
  val Verbose = false

  /** User-defined name of the operator. */
  val name: String = {
    val rawOperatorName = getClass.getSimpleName
    if (rawOperatorName endsWith "$")
      rawOperatorName.substring(0, rawOperatorName.length - 1)
    else
      rawOperatorName
  }
  /** User-defined `compute` method that executes the operator. */
  private val computeMethod: Method = findComputeMethod

  /** Number of instances of this operator in the compute graph. */
  private var numInstances = 0

  /** Create an instance of the operator.
    *
    * @param inputs Fields that are inputs to the operator.
    * @return Field resulting from execution of the operator.
    */
  def apply(inputs: Field*): Field = {
    numInstances += 1
    if (Cog.checkUserOperators && numInstances == 2)
      println(s"Warning: user CPU Operator $this has multiple instances.  Ensure it has no state altered by compute().")
    val in = inputs.toArray
    val result: FieldWriterProxy = checkParameters(computeMethod, in)
    val outputFieldType = findResultType(computeMethod, in, result)
    val outResult = allocateFieldResult(outputFieldType)
    val executor =
        new OperatorExecutor(name, this, computeMethod, outputFieldType, outResult)
    val opcode = UserOpcode(executor)
    createResultField(opcode, in, outputFieldType)
  }

  /** Find the user-defined `compute` method using reflection (one and only one
    * compute method is required).
    *
    * @return The user's compute method.
    */
  private def findComputeMethod: Method = {
    val methods: Array[Method] = getClass.getDeclaredMethods
    var computeMethod: Method = null
    for (method <- methods) {
      if (method.getName == "compute") {
        if (computeMethod == null)
          computeMethod = method
        else
          semanticError("contains more than one 'compute' method")
      }
    }
    if (computeMethod == null)
      semanticError("has no 'compute' method")
    computeMethod
  }

  /** Check that the parameters to the `compute` function are legal.
    *
    * The user defines the compute function with N FieldReaders and one
    * FieldWriter, but it is invoked with only N arguments (the input fields).
    *
    * @param method The user compute method, found by reflection.
    * @param arguments Arguments to the operator as invoked by the user at
    *        runtime.
    * @return The field result from the computation.
    */
  private def checkParameters(method: Method, arguments: Array[Field]): FieldWriterProxy = {
    val parameterTypes = method.getParameterTypes
    if (parameterTypes.length < 2)
      semanticError("must have at least one input FieldReader and only one" +
              " FieldWriter (as the last parameter).")
    if (parameterTypes.length != (arguments.length + 1))
      semanticError("invoked with " +
              arguments.length + " arguments, but was defined to have " +
              parameterTypes.length + " input field parameters.")
    for (p <- 0 until arguments.length) {
      val parmType = parameterTypes(p)
      if (!parmType.isAssignableFrom(memoryTypeFor(arguments(p).fieldType)))
        semanticError("'compute' argument " + p +
                "is not type compatible with parameter " + p)
    }
    // Now look at the last parameter so we can allocate an appropriate
    // FieldResult proxy.
    val outParameter = parameterTypes.last
    if (Verbose) {
      println("Operator.checkParameters:")
      println("  outParameter = " + outParameter)
      for (i <- 0 until arguments.length) {
        println("  parm " + i + " = " + parameterTypes(i))
        println("  arg  " + i + " = " + arguments(i).fieldType)
      }
    }
    // match statements don't seem to work with classOf[_] values.
    if (outParameter == classOf[ScalarFieldWriter])
      new ScalarFieldWriterProxy
    else if (outParameter == classOf[VectorFieldWriter])
      new VectorFieldWriterProxy
    else if (outParameter == classOf[MatrixFieldWriter])
      new MatrixFieldWriterProxy
    else if (outParameter == classOf[ColorFieldWriter])
      new ColorFieldWriterProxy
    else if (outParameter == classOf[ComplexFieldWriter])
      new ComplexFieldWriterProxy
    else
      throw new RuntimeException("bad field type: " + outParameter)
  }

  /** Get the class of AbstractFieldMemory required for `fieldType`. */
  private def memoryTypeFor(fieldType: FieldType): Class[_] = {
    fieldType.elementType match {
      case Uint8Pixel =>
        classOf[ColorFieldMemory]
      case Complex32 =>
        classOf[ComplexFieldMemory]
      case Float32 =>
        fieldType.tensorShape.dimensions match {
          case 0 =>
            classOf[ScalarFieldMemory]
          case 1 =>
            classOf[VectorFieldMemory]
          case 2 =>
            classOf[MatrixFieldMemory]
          case x =>
            throw new RuntimeException("bad field type")
        }
      case x =>
        throw new RuntimeException("bad field type")
    }
  }

  /** Allocate an appropriate FieldResult for `fieldType`. */
  private def allocateFieldResult(fieldType: FieldType): FieldWriterProxy = {
    fieldType.elementType match {
      case Uint8Pixel =>
        new ColorFieldWriterProxy
      case Complex32 =>
        new ComplexFieldWriterProxy
      case Float32 =>
        fieldType.tensorShape.dimensions match {
          case 0 =>
            new ScalarFieldWriterProxy
          case 1 =>
            new VectorFieldWriterProxy
          case 2 =>
            new MatrixFieldWriterProxy
          case x =>
            throw new RuntimeException("bad field type")
        }
      case x =>
        throw new RuntimeException("bad field type")
    }
  }

  /** Find the result type of the user compute method -- this is the last
    * argument of the argument list.
    *
    * This field type is discovered in a tricky way. The compute method is
    * executed once and the type of result is examined.
    *
    * @return Field type of the result.
    */
  private def findResultType(method: Method, inputs: Array[Field],
                             outWriter: FieldWriterProxy): FieldType = {
    val inReaders =
      inputs.map(field => FieldMemory.indirect(field.fieldType))
    if (Verbose) {
      println("Operator.findResultType:")
      println("  out = " + outWriter.getClass.getSimpleName)
      for (i <- 0 until inputs.length) {
        println("  arg " + i + " = " + inputs(i).getClass.getSimpleName)
      }
    }
    val arguments = new ArrayBuffer[Object] {
      for (in <- inReaders)
        this += in
      this += outWriter
    }.toArray

    try {
      method.invoke(this, arguments :_*)
    } catch {
      case e: InvocationTargetException =>
        println("Operator.findResultType exception: " + e.getTargetException)
        throw e
    }
    outWriter.fieldType
  }

  /** Create the output field for a given set of inputs. */
  private def createResultField(opcode: Opcode,
                                in: Array[Field],
                                outputFieldType: FieldType): Field = {
    outputFieldType.elementType match {
      case Uint8Pixel =>
        new ColorField(opcode, in, outputFieldType)
      case Complex32 =>
        new ComplexField(opcode, in, outputFieldType)
      case Float32 =>
        outputFieldType.tensorShape.dimensions match {
          case 0 =>
            new ScalarField(opcode, in, outputFieldType)
          case 1 =>
            new VectorField(opcode, in, outputFieldType)
          case 2 =>
            new MatrixField(opcode, in, outputFieldType)
          case x =>
            throw new RuntimeException("bad field type")
        }
      case x =>
        throw new RuntimeException("bad field type")
    }
  }

  /** User error. */
  private def semanticError(message: String) {
    error("user operator '" + name + "': " + message)
  }
}
