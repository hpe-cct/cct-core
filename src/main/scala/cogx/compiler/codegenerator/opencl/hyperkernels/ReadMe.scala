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

package cogx.compiler.codegenerator.opencl.hyperkernels

/** This package contains HyperKernel drivers used by the Cog compiler to
  * synthesize OpenCL code.
  *
  * ********   RULES for writing code for a HyperKernel: ***********
  *
  * 1. Input fields to the kernel are named "@in0", "@in1", "@in2", ...
  *
  * 2. Output fields produced by the kernel are named "@out0", "@out1", "@out2", ...
  *
  * 3. Every HyperKernel uses one of three addressing modes (necessary because
  *    of limitations of OpenCL data structures):
  * {{{
  *    1. TensorElementAddressing: each OpenCL work-item (thread) addresses
  *       exactly one point in the result field, for any dimension tensor.
  *
  *    2. SmallTensorAddressing: each work-item addresses exactly one point in
  *       the field, allowing complete tensors (scalars, vectors, matrices,
  *       complex numbers, pixels) to be read and written as single objects.
  *       This is an efficient addressing mode since hyperkernels using this
  *       mode can be merged, but this only works for "small" tensors containing
  *       four or fewer floats.
  *
  *    3. BigTensorAddressing: each work-item addresses exactly one
  *       point in the field, but only single elements (floats) can be written
  *       and read.
  *       This is a less efficient addressing mode since such hyperkernels
  *       cannot be merged, but allows you to process fields with large tensors
  *       (containing more than four floats).
  * }}}
  *
  *    [Editorial comment: I find the most important concept tied to 'addressing
  *    mode' is the number and purpose of threads.  Thus, I suggest renaming this
  *    concept 'thread model', as in:
  *
  *    val threadModel = OneThreadPerFieldPoint   // or OneThreadPerTensorElement
  *    new HyperKernel(operation, in, resultType, threadModel)
  *
  *    There shouldn't then be many restrictions on how these threads access
  *    their inputs (i.e. addressing modes), except that no addressing mode
  *    that relies on _tensorElement can be used in OneThreadPerFieldPoint
  *    mode.  Greg should implement this change, perhaps as part of a rework
  *    of Hyperkernel addressing syntax.  -RJC
  *    ]
  *
  * 4. Every kernel has thread-private variables automatically defined that
  *    can be accessed by threads. These variables define the point in the field
  *    being processed by that work item. These are:
  * {{{
  *        `_layer`
  *        `_row`
  *        `_column`
  * }}}
  *
  * There is an additional variable defined only for
  *    TensorElementAddressing:
  * {{{
  *        `_tensorElement`
  * }}}
  *
  *    which tells the thread which element in the tensor at the current point
  *    is being processed by that thread.
  *
  *    There are also corresponding constants defined that threads may read to
  *    find the actual dimensions of the field:
  * {{{
  *        `_layers`
  *        `_rows`
  *        `_columns`
  *        `_tensorElements`
  * }}}
  *    For some reading and writing "nonlocally" (which is not very efficient
  *    but sometimes necessary) you will need to declare one or more of the
  *    following, all of type int:
  * {{{
  *        `layers`
  *        `rows`
  *        `columns`
  *        `tensorElements`
  * }}}  *
  *
  * 5. SmallTensorAddressing is preferred when possible since kernels with this
  *    addressing mode can be merged more efficiently. The HyperHelper trait
  *    contains a method 'bestAddressingMode' which you can use to see if
  *    SmallTensorAddressing is possible given the inputs and result type.
  *
  * 6. Rules for reading and writing fields
  *
  * '''TensorElementAddressing field reads and writes'''
  * {{{
  *   `read(@in1)`:
  *       reads @in1 returning a float,
  *       indexed by (_layer, _row, _column, _tensorElement)
  *
  *   `readNonlocal(@in1)`:
  *       reads @in1 returning a float,
  *       indexed by (layer, row, column, tensorElement)
  *
  *   `@out0 = float`:
  *       writes the first output field (@out0) with a float
  *       indexed by (_layer, _row, _column, _tensorElement)
  *
  *   `@outNonlocal0 = float`:
  *       writes the first output field (@out0) with a float
  *       indexed by (layer, row, column, tensorElement)
  * }}}
  *    The writes in this addressing mode may only be done once at the end of
  *    the computation, and they may not appear inside of a loop.
  *
  *
  * '''SmallTensorAddressing field reads and writes'''
  * {{{
  *   `read(@in1)`:
  *       reads @in1 returning a CLType
  *           (CLFloat, CLFloat2, CLFloat3, CLFloat4, CLPixel)
  *       indexed by (_layer, _row, _column)
  *
  *   `readNonlocal(@in)`:
  *       reads @in1 returning a CLType
  *       indexed by (layer, row, column)
  *
  *   `readElement(@in1)`:
  *       reads @in1 returning a float
  *       indexed by (_layer, _row, _column, tensorElement)
  *
  *   `readElementNonlocal(@in)`:
  *       reads @in1 returning a float
  *       indexed by (layer, row, column, tensorElement)
  *
  *   `@out0 = CLType`:
  *       writes the first output field (@out0) with a CLType
  *       indexed by (_layer, _row, _column)
  *
  *   `@outNonlocal0 = CLType`:
  *       writes the first output field (@out0) with a float
  *       indexed by (layer, row, column)
  * }}}
  *    The writes in this addressing mode may only be done once at the end of
  *    the computation, and they may not appear inside of a loop.
  *
  *    Also note that readElement(@in1) may be dangerous to use in SmallTensor-
  *    Addressing mode if the kernel driving @in1 can be merged into the kernel
  *    using the readElement() call.  See SliceVectorsHyperKernel for a
  *    work-around.
  *
  *
  * '''BigTensorAddressing field reads and writes'''
  * {{{
  *   `readElement(@in1)`:
  *       reads @in1 returning a float
  *       indexed by (_layer, _row, _column, tensorElement)
  *
  *   `readElementNonlocal(@in)`:
  *       reads @in1 returning a float
  *       indexed by (layer, row, column, tensorElement)
  *
  *   `@outElement0 = float`:
  *       writes the first output field (@out0) with a float
  *       indexed by (_layer, _row, _column, tensorElement)
  *
  *   `@outElementNonlocal0 = float`:
  *       writes the first output field (@out0) with a float
  *       indexed by (layer, row, column, tensorElement)
  * }}}
  *    The writes in this addressing mode may be done multiple times within
  *    a loop.
  *
  *
  *    There are two additional read functions for reading 0D fields:
  * {{{
  *        `readScalar(@in1)`
  *            reads the single point in a 0D scalar field.
  *
  *        `readPoint(@in1)`
  *            reads the single point (vector, matrix, ...) in a 0D tensor field.
  * }}}
  *
  *
  * 7. Scalar fields have a special case in TensorElementAddressing to make it
  *    easier to combine tensor fields and scalar fields. The read() (but not
  *    readNonlocal()) function in that addressing mode acts as though the
  *    _tensorElement parameter is always equal to zero, regardless of the
  *    state of the variable _tensorElement.
  *
  * 8. The HyperKernel merger has certain checks that must be satisfied before
  *    merging is permitted, however these may not be exhaustive.  For example,
  *    a kernel that operates in TensorFieldAddressing mode on inputs of the
  *    same fieldType, but makes nonlocal references (via readNonlocal)
  *    will be incorrectly merged unless the kernel is
  *    explicitly marked with the NonlocalOperator trait.
  *
  * 9. For consistency, Hyperkernels should be written as a class with a private
  *    constructor and a companion object implementing apply().  The apply()
  *    method should take the result FieldType as an argument and perform checks
  *    of this 'actual' result type with the type expected by the kernel.
  *
  * 10. Hyperkernels should not reference the underlying address generation
  *    variables such as in_field_0_rowStride, _out_field_0, etc. since kernels
  *    are not aware of merging that can renumber those I/O's.  The FFT and DCT
  *    Hyperkernels currently reference some properties of their I/O fields, and
  *    to protect those kernels, we replace  partStride(@out0), fieldName(@out0),
  *    and fieldName(@in0) (for example) with the correctly numbered actual
  *    variable.  Note that the "part stride" is the number of words between the
  *    real and imaginary components in a complex field.
  *
  * @author Greg Snider
  */
