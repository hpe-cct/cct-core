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

/**
 *
 * Here are notes on how to add a language keyword in Cog-4.0:
 *
 * Lets say the new keyword is "foo".  The steps are:
 *
 * 1. Create the opcode for foo, which should subclass the operator class
 *    that is appropriate for the number of inputs (UnaryOpcode, BinarOpcode
 *    etc.).  Let's assume foo is a unary function, then one should add a line
 *    to ./cogx/compiler/parser/op/UnaryOpcode.scala like:
 *
 *    case object FooOp extends UnaryOpcode("foo")
 *
 *    The "foo" String above merely names the opcode for debugging.
 *
 * 2. The next step depends on whether the 'foo' function is a simple "point
 *    op" that can be handled by the standard UnaryHyperKernel (or, for a
 *    binary op, the BinaryHyperKernel), or whether 'foo' requires its own
 *    kernel.  The UnaryHyperkernel makes use of the mapping function
 *    OpcodeToFunction(op: Opcode), which maps the opcode to the macro that
 *    appears in the OpenCL code.  Let's say 'foo' performs the cube of each
 *    input element.  Then one would add a line to
 *    ./cogx/compiler/codegenerator/opencl/OpcodeToFunction.scala within the
 *    UnaryOpcode match case:
 *
 *    case op: UnaryOpcode =>
 *      op match {
 *      ...
 *        case FooOp => "cube"
 *      ...
 *      }
 *
 *    Then, in ./cogx/compiler/codegenerator/opencl/CogCLFunctions.scala, add
 *
 *    #define cube(a) ((a) * (a) * (a))
 *
 *    Alternatively, if the kernel that performs the foo function does not need
 *    to invoke OpcodeToFunction on it, generally nothing needs to be done,
 *    since the default is:
 *
 *     case _ => internalError("opcode has no function"); ""
 *
 * 3. Two files are involved to enable "foo" as a keyword of the Cog language.
 *    To enable foo as a method on a field, such as in myField.foo(), add to
 *    ./cogx/compiler/parser/syntaxtree/Field.scala:
 *
 *    def foo = UnaryOperator(FooOp, this)
 *
 *    To enable foo to operate as a function that takes a field, such as with
 *    foo(myField), add to ./cogx/compiler/parser/syntaxtree/Intrinsics.scala:
 *
 *    def foo(f: Field) = UnaryOperator(FooOp, f)
 *
 * 4. One has to teach the parser what the result FieldType of the operation is,
 *    given the input, in ./cogx/compiler/parser/syntaxtree/UnaryOperator.scala.
 *    The default for unary operators is that the output has the same FieldType
 *    as the input.  If the function doesn't adhere to the default, add a new
 *    field defining function as in:
 *
 *    val opType = operation match {
 *       ...
 *       case op: FooOp =>
 *         new FieldType(<resultShape>,<tensorShape>, <elementType>)
 *       ...
 *       case default =>
 *         inType
 *    }
 *
 *    In addition, if the foo function cannot operate on all field types, add
 *    a line to UnaryOperator.scala (in this case).  If foo() only operated
 *    on ScalarFields with a 0D tensor at each field point (i.e. a single float),
 *    then the addition would be:
 *
 *    // Semantic check: is the operation permitted for this size/type of input?
 *    operation match {
 *      ...
 *      case FooOp =>
 *        if (!tensor0RealField(inType)) typeError(operation, inType)
 *      ...
 *      case default => // OK by default
 *    }
 *
 * 5. To map the FooOp opcode to the kernel that performs the function,
 *    edit the "generator" files for each field type that applies to the
 *    function.  For ScalarFields for example, one would add to the file
 *    ./cogx/compiler/codegenerator/generator/ScalarFieldGenerator.scala:
 *
 *    case FooOp =>
 *       FooHyperKernel(inputs(0), FooOp, fieldType)
 *
 * 6.  And finally, if the function uses its own unique kernel, write the
 *    kernel (e.g. FooHyperKernel)!
 *
 * The following is the *OLD* pre-Cog-4.0 approach, left here to make sure
 * that the new description above hasn't left out any steps:
 *
{{{
The Cog compiler takes Cog programs written in Scala using the Cog primitives
and transforms them to run on a cluster of GPUs. It is structured much like
a conventional compiler: parser, code generator, optimizer.

Cog types are defined in the FieldType class. Each contains three attributes
which make up a Cog type:
   fieldShape
   tensorShape
   tensorType

The parser captures Cog computations and uses those to build "expression trees."
The nodes of those trees are Expr's (e.g. ScalarFieldExpr, VectorFieldExpr)
defined in the parser package. The Expr's also extend the Operation class
which define the computation at each expression tree node. Each expression holds
three pieces of information:

  1. An opcode defining the operation for the Expr, defined in the
     operations.Opcode class.
  2. An array of operations that serve as inputs to the nod.
  3. The type of the result produced by the node

After parsing, the resulting Expr trees are passed to the code generator,
GPUEvaluator, which translates those trees into KernelTrees which represent
the computation in terms of "kernels"--an OpenCL term that describes a GPU
function that takes 1 or more device buffers containing data as inputs, and
writes 1 or more device buffers as outputs.

Cog has generalized the notion of "kernels" and allows CPUKernels (kernels which
run on the CPU) to be intermixed with device kernels. The classes in
opencl.buffers manages the CPU and device buffers, allowing them to
interoperate. The package opencl.compute contains abstractions for kernels
themselves, allowing CPU and device kernels to interoperate.

CPU kernels are kept in the opencl.cpukernels package and the opencl.userkernels
packages. Device kernels are kept in the kernels package.

The code in the optimizer package transforms kernel trees into more efficient
forms using classical optimizations, such as common subexpression elimination.

Adding a new field operation into Cog requires some additions in the compiler
package. Here's the general steps you must take to add a new operation called
"foo" for "ScalarFieldExpr":

  1. Create a new opcode, FooOp, in Opcode.scala

  2. In ScalarFieldExpr, write a  method called “foo” that invokes foo on
     a scalar field:

        def foo =
           ScalarFieldOperation(FooOp, Array[Operation](this), this.fieldType)

  3. Write the kernel for foo, FooKernel, in the opencl.kernels package. and
     outputs.

  4. In ScalarFieldGenerator, add a line that maps the opcode to the kernel. For example:

       case FooOp => new FooKernel(…)


  5. In ScalarFieldOperation, add FooOp to the list of checked-for match alternatives,
     along with any additional checks to the ScalarFieldOperation constructor args.

The compiler also allows the compiler writer to add additional kernels that
use the OpenCL Image API. This API is not visible to the Cog application
writer. Here's how to take advantage of this.

To create a new method (call it “foo”) on a ScalarField that requires a kernel
written using the Image API, the procedure is:

  1. Create a new opcode, FooOp, in Opcode.

  2. Write the kernel for foo, FooKernel, in the opencl.kernels package.
     This kernel uses the Image API, meaning that it can use the texture cache
     for inputs and outputs.

  3. In GPUEvaluator, add a case in compileImage() that maps the opcode to the
     kernel. For example:

        case FooOp => new FooKernel(…)

  4. In ImageExpr, write a private[cog] method called “foo” that invokes foo on
     an image:

        private[cog] def foo =
          ImageOperation(FooOp, Array[Operation](this), this.fieldType)

  5. In ScalarFieldExpr, write a method called “foo” that (1) converts scalar
     field to image, (2) calls “foo” on the image, (3) converts the result
     back to a scalar field:

       def foo: ScalarFieldExpr = {
         val functions = new Object with ExpressionFunctions
         val image = functions.image(this)
         val result = image.foo
         result.toScalarField
       }

The Image API can also be accessed to process higher order fields. The
instructions above still apply, but there are some extensions:

  1. There are two different image types: ScalarImage and QuadImage.
     ScalarImages hold 1 Float / pixel, QuadImages hold 4 Floats / pixel.
     These types are implicit since they are not user visible and appear as
     ScalarImageExpr and QuadImageExpr.

  2. There are multiple “image( )” functions you can call to convert fields to
     images. These functions are defined in the file ExpressionFunctions.scala:

        image(ScalarFieldExpr): ScalarImageExpr
        image(ScalarFieldExpr, ScalarFieldExpr, ScalarFieldExpr,
              ScalarFieldExpr): QuadImageExpr
        image(VectorFieldExpr): QuadImageExpr
        image(ColorFieldExpr): QuadImageExpr

     You can access the above functions like this:

         val functions = new Object with ExpressionFunctions
         val image = functions.image(…)

   3. You can convert Images to Fields like this:

        ScalarImageExpr.toScalarField: ScalarFieldExpr
        QuadImageExpr.toVectorField: VectorFieldExpr
        QuadImageExpr.toColorField: ColorFieldExpr

      There are examples of using this to create new Image kernels in
      DyadFieldExpr and ColorFieldExpr.

 }}}

 @author Greg Snider

  *
  * @author Greg Snider
  */
package object compiler {

  // Test
}
