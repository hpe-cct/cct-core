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

package cogx.compiler.codegenerator.opencl.generator

import cogx.compiler.codegenerator.{KernelCircuit, CodeGenerator}
import cogx.compiler.parser.syntaxtree._
import cogx.platform.types._
import cogx.platform.opencl.{OpenCLKernelCodeGenParams, OpenCLAbstractKernel}
import cogx.compiler.codegenerator.opencl.cpukernels.RecurrentFieldKernel
import cogx.cogmath.collection.IdentityHashMap
import cogx.parameters.Cog

/** Generates a Circuit of OpenCL kernels from a syntax tree.
  *
  * @param codeGenParams A bundle of device parameters that affect kernel code generation and optimization.
  * @param fftUse policy for FFT use in fast convolution.
  * @param smallTensorUse policy for when to use SmallTensorAddressing in convolution.
  *
  * Note: if we start adding more compiler parameters like `fftUse`, then
  * we should probably create a wrapper class to pass around instead of
  * many separate parameters.
  *
  * The default platformParam value of null should only be used in testing when it is known
  * that there are no kernels that require platform parameters (e.g. warpSize).
  *
  * @author Greg Snider
  */
private[cogx]
class OpenCLCodeGenerator(codeGenParams: OpenCLKernelCodeGenParams = null,
                          fftUse: ConvolutionFFTUsePolicy = UseFFTWhenBest,
                          smallTensorUse: ConvolutionSmallTensorUsePolicy = UseSmallTensorWhenBest)
        extends CodeGenerator[OpenCLAbstractKernel] {
  /** A map from an Operation to the Kernel it's associated with in the KernelCircuit */
  private val operationToKernel = new IdentityHashMap[Operation, OpenCLAbstractKernel]()
  /** A map from a Field to the VirtualFieldRegister it's associated with in the KernelCircuit */
  private val fieldToRegister = new IdentityHashMap[Field, VirtualFieldRegister]()

  /** Map Fields of `syntaxTree` to a Circuit of OpenCL code (AbstractKernels). */
  def generateCircuit(syntaxTree: SyntaxTree): KernelCircuit = {
    if (Cog.verboseCodeGenerator)
      printf("OpenCLCodeGenerator: creating circuit for %d fields.\n",
        syntaxTree.size)
    val circuit = new KernelCircuit
    // We compile from the (possibly multiple) roots of the syntax tree.
    for (root <- syntaxTree.roots) {
      translateOperation(root)
    }

    // Handle recurrences. These must be handled out-of-band so that both
    // the syntax tree and the kernel circuit are acyclic.

    syntaxTree.traversePostorder {
      (operation: Operation) => {
        val numOutputs = operation.numOutputs
        operationToKernel(operation) match {
          case r: RecurrentFieldKernel =>
            require(numOutputs == 1, s"Expecting single output RecurrenceKernel, found $numOutputs outputs.")
            val recurrenceField: Field = operation.outputs(0).recurrence match {
              case Some(field) => field
              case None => throw new RuntimeException("Expecting field recurrence for RecurrentFieldKernel")
            }
            r.recurrence = fieldToRegister(recurrenceField)
          case _ =>
        }
      }
    }
    circuit
  }

  /** GenerateCircuit back-annotates each field in a syntaxTree with the
    * compiled kernel that implements its computation. This removes those
    * annotations so that there are no dangling pointers to OpenCL resources,
    * allowing the garbage collector to dispose of them after they have been
    * deallocated on the OpenCL platform.
    *
    * @param syntaxTree Syntax tree to be de-annotated with the results of
    *        generateCircuit.
    */
  def releaseCircuit(syntaxTree: SyntaxTree) {
    syntaxTree.traversePostorder {
      (operation: Operation) => { operation.outputs.foreach(_.release) }
    }
  }

  /** Translate an operation to an abstract kernel. This routine could seemingly
    * be written more cleanly using recursion, rather than a stack.  However, the stack
    * approach is better in that it does not risk overflowing the process' call stack
    * for huge chains of kernels. */
  private def translateOperation(operation: Operation) {
    if (!operationToKernel.contains(operation)) {
      val stack = new scala.collection.mutable.ArrayStack[Operation]
      stack.push(operation)

      while (stack.nonEmpty) {
        val cursor = stack.pop()

        /** Don't operate on nodes we have already visited. */
        if (!operationToKernel.contains(cursor)) {
          val unprocessedInputFields = cursor.inputs.filter(!fieldToRegister.contains(_))

          if (unprocessedInputFields.length == 0) {
            // All the inputs for this field have been translated. Proceed to kernel generation.
            val kernel =
              if (cursor.numOutputs == 1) {
                val outField = cursor.outputs(0)
                outField match {
                  case f: ScalarField =>
                    ScalarFieldGenerator(f, translateFields(f.inputs),
                      codeGenParams, fftUse, smallTensorUse)
                  case f: ColorField =>
                    ColorFieldGenerator(f, translateFields(f.inputs))
                  case f: VectorField =>
                    VectorFieldGenerator(f, translateFields(f.inputs),
                      codeGenParams, fftUse, smallTensorUse)
                  case f: MatrixField =>
                    MatrixFieldGenerator(f, translateFields(f.inputs),
                      codeGenParams, fftUse, smallTensorUse)
                  case f: ComplexField =>
                    ComplexFieldGenerator(f, translateFields(f.inputs),
                      codeGenParams, fftUse, smallTensorUse)
                  case f: ComplexVectorField =>
                    ComplexVectorFieldGenerator(f, translateFields(f.inputs),
                      codeGenParams, fftUse, smallTensorUse)
                  case x =>
                    throw new RuntimeException("oops, bad field type: " + x.getClass.getSimpleName)
                }
              }
              else {
                MultipleFieldGenerator(cursor, translateFields(cursor.inputs), fftUse, smallTensorUse)
              }
            val clKernel = kernel.asInstanceOf[OpenCLAbstractKernel]
            operationToKernel(cursor) = clKernel
            for (outputIndex <- 0 until clKernel.numOutputs) {
              val outField = cursor.outputs(outputIndex)
              val outRegister = clKernel.outputs(outputIndex)
              outField.setVirtualFieldRegister(outRegister)
              outRegister.name = outField.name // Get debugging name defined by user.
              fieldToRegister(outField) = outRegister
            }
          } else {
            /** This field still has one or more inputs that haven't been
              * translated yet. First, push the cursor on to the stack so we can
              * revisit it after the inputs have been translated. Next, push
              * all the operations associated with these inputs onto the stack.
              */
            stack.push(cursor)
            for (f <- unprocessedInputFields) {
              stack.push(f.operation)
            }
          }
        }
      }
    }
  }

  /** Translate an array of fields to an array of virtual field registers. */
  private def translateFields(fields: Seq[Field]): Array[VirtualFieldRegister] = {
    fields.map(field => {
      if (!fieldToRegister.contains(field))
        translateOperation(field.operation)
      fieldToRegister(field)
    }).toArray
  }
}