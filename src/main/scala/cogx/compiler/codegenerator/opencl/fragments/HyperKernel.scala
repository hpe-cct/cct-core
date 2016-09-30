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

package cogx.compiler.codegenerator.opencl.fragments

import HyperKernel._
import cogx.compiler.codegenerator.opencl.CogCLFunctions
import cogx.cogmath.collection.{IdentityHashSet, IdentityHashSetDeterministic}
import cogx.compiler.codegenerator.common.{InputReduced, MergedOp}
import java.util
import cogx.cogmath.hypergraph.acyclic.DAG
import cogx.parameters.Cog
import cogx.platform.types._
import cogx.platform.opencl.{OpenCLDeviceKernel, WorkGroupParameters}
import cogx.compiler.codegenerator.common.FieldPolicies._
import scala.collection.mutable.ArrayBuffer


/** Synthesizes OpenCL kernel code from an abstract user specification. Handles
  * all of the boilerplate and, more importantly, allows compatible kernels
  * to be merged together into a single kernel to avoid the high kernel launch
  * overhead and reduced bandwidth through the GPU.
  *
  * Note that most fields of the class are vars rather than vals. This is
  * because we do not want to instantiate this HyperKernel during compilation,
  * but want to wait until it has been serialized into the cluster onto the
  * actual node where it will execute. The first call to startComputation
  * forces allocation of all OpenCL resources.
  *
  * Some thoughts on the future evolution of this class:
  *
  * I find the "addressing mode" concept confusing frankly.  What is at issue
  * is whether each thread of the kernel is outputting the full field point, or
  * just a tensor element of the field point.  Perhaps better would be calling
  * it the "threading model."  The assumption here is that the threading model
  * is dictated by the Shape of the output field.  2D VectorFields of vector
  * length 2 are considered TensorFields, and operate in FieldPointAddressingMode,
  * while 3D VectorFields of length 2 operate in TensorPointAddressingMode.  One
  * plus is that in many simple cases, a single representation of the kernel code
  * can handle both addressing modes (e.g. @out0 = read(@in0) is the kernel code
  * for a copy kernel in both modes).  However, in more complicated cases, I
  * believe we want the demands of the kernel to dictate whether FieldPointAddressing
  * or TensorElementAddressing is used.  Since the addressing mode is set during
  * the code synthesis operation, a kernel can generate code based on the mode.
  * Perhaps the kernel should have a mixed-in trait specifying whether it can
  * run in FieldPointAddressing or TensorElementAddressing mode (to be used by
  * the HyperKernel merger, which makes that choice for the entire kernel DAG).
  *
  * Right now, the code generated looks like:
  * {{{
  * <cltype> _temp_123;
  * {
  *    <kernel code>
  *    _temp_123 = <RHS of @out0 statement>;
  * }
  * out[_column + _row * _out_field_0_rowStride] = _temp_123; // if DAG-top kernel
  * }}}
  *
  * Some enhanced merging capabilities may be possible if the code instead looks
  * like:
  * {{{
  * <cltype> _temp_123_func(int _row, int _column) {     // can adapt to 3D, 4D.
  *     <kernel code>
  *     return <RHS of @out0 statement>;
  * }
  *  out[_column + _row * _out_field_0_rowStride] = _temp_123_func(_row, _column);
  * }}}
  *  In particular, merging backwards from kernels that do indexed reads may
  *  be possible.  A readNonlocal(@in0) may result in the code:
  * {{{
  *  {
  *        // kernel code...
  *
  *         row = ...
  *         column = ...
  *         ... = _temp_123(row, column)     // readNonlocal(@in0) where in0 has been merged
  *
  *        ...  =
  *  }
  *  }}}
  *  The Shape of the "workField" can already be dictated by the kernel,
  *  irrespective of the shape of its input or output.  Perhaps the HyperKernel
  *  merger can take over responsibility for picking the workField shape, choosing
  *  a super-set of dimensions and sizes to cover the needs of all merged kernels.
  *
  *  Each kernel would then generate code with the workField given as an input,
  *  making do with what threads there were to create the output field.  This
  *  might involve stepping the workField Shape across the outputField Shape,
  *  for example.
  *
  *  The "NonLocalOperator" trait currently kills merging in both direction.
  *  This trait needs to be better defined.  One aspect of kernels that is likely
  *  to affect merging is whether each thread works independently, or needs
  *  information or help from other threads in the thread block (as is typically
  *  true if a local (a.k.a. shared) memory is involved.  Is this
  *  "InterdependentThreads" trait truly the same as "HasLocalMemory"?
  *
  *  Currently, if a kernel's output goes to 2 or more places, that kernel
  *  is not merged forward.  Perhaps alternatively, a general global memory
  *  bandwidth metric could be used:
  *  {{{
  *  this <== in.nonMergableOp1(A+B) + in.nonMergableO2(A+B)
  *
  *  Assume that nonMergableOp1,2 cannot be merged forward.  Then 4 kernels would
  *  be created by the current merger:
  *
  *  temp <- add(A, B)
  *  temp1 <- nonMergableOp1(in, temp)
  *  temp2 <- nonMergableOp2(in, temp)
  *  result <- add(temp1, temp2)
  *
  *  However, if the A+B kernel was duplicated and merged into its two outputs,
  *  we'd have 3 kernels:
  *
  *  temp1 <- CompoundOp1(in, A, B)
  *  temp2 <- CompoundOp2(in, A, B)
  *  result <- add(temp1, temp2)
  *  }}}
  *  Let's call the size of all the fields (A, B, temp, ...) 'X'.  Then the
  *  4 kernel approach moves 12X bytes, while the 3-kernel approach moves 11X.
  *  The 3-kernel approach is likely to be faster, even though it duplicates
  *  the A+B calculation.  Note that the bandwidth-driven merger would correctly
  *  not merge forward the output of a reduction, because that would duplicate
  *  the large reduction input, with little benefit gained from avoiding the
  *  writing out and re-reading the single-float reduction result.
  *
  *  The operation of this class has not yet been updated to handle
  *  multi-output kernels.   5/30/2014 -RJC
  *
  * @param opcode Opcode for this particular kernel.
  * @param in Array of virtual field registers driving this kernel.
  * @param resultTypes Types of the fields produced by this kernel.
  * @param addressing Addressing mode: dictates how field reads/writes are done.
  * @param samplingMode True if positions outside the domain of the field should
  *        return the value of the nearest border, otherwise return 0. This is
  *        valid only for image fields.
  *
  * @author Greg Snider
  */
private[cogx]
class HyperKernel(opcode: Opcode,
                  in: Array[VirtualFieldRegister],
                  resultTypes: Array[FieldType],
                  val addressing: AddressingMode,
                  val samplingMode: SamplingMode = DontCare)
  extends OpenCLDeviceKernel(opcode, in, resultTypes)
{
  /** Alternate constructor for common single-output case, using default SamplingMode */
  def this(opcode: Opcode,
           in: Array[VirtualFieldRegister],
           resultType: FieldType,
           address: AddressingMode) =
    this(opcode, in, Array(resultType), address)

  /** Alternate constructor for common single-output case, specifying a SamplingMode */
  def this(opcode: Opcode,
           in: Array[VirtualFieldRegister],
           resultType: FieldType,
           address: AddressingMode,
           samplingMode: SamplingMode) =
    this(opcode, in, Array(resultType), address, samplingMode)

  in.foreach(input => require(input != null))

  // Previously, the choice of addressing mode was overridden for ScalarFields
  // that were not in SmallTensorMode.  Changing the mode "under the covers"
  // seems dangerous (and overly restictive because BigTensorAddressing might
  // be reasonable).  Instead, we just enforce that kernels in TensorElementAddressing
  // mode (where _tensorElement is defined) are indeed writing Vector or Matrix Fields.
  // Put another way, if we're writing ScalarFields, we should not be in
  // TensorElementAddressing mode.
  //
  // On the other hand, there are valid reasons why one would want to use
  // BigTensorAddressing or TensorElementAddressing when writing a scalar field.
  // So, the following check is removed since, even if the condition is true,
  // it does no harm. GSS.
  //
  //  if (addressing == TensorElementAddressing)
  //    resultTypes.foreach(ft => require(ft.tensorShape.dimensions > 0))

  /** Type of element (scalar or tensor) produced by this kernel. */
  val elementTypes = resultTypes.map(addressing.clType(_))
  /** Which field determines the work group parameters?  Result type by default . */
  protected lazy val workFieldType = resultTypes(0)
  /** Scheduling parameters for this kernel driver. */
  lazy val workGroup = computeWorkGroupParameters(workFieldType, addressing)
  /** Distinct name for this kernel.  The system coalesces kernels based on
    * their source code (minus the kernel name), so this name is mostly useful
    * for debugging the OpenCL source: set Verbose = 2 in OpenCLPlatform and
    * search for the unique kernel name generated below.
    */
  val kernelName = opcode.name + "_" + this.id
  /** Input field arguments to kernel. */
  private[HyperKernel] val inFieldFragments = new Array[InputFieldFragment](in.length)
  /** Output argument from the kernel. */
  private val outFieldFragments = new Array[OutputFieldFragment](resultTypes.length)
  /** Do we need to define a sampler? */
  private lazy val needsSampler = in.map(kernel => isColorField(kernel.fieldType)).foldLeft(false)(_ || _)
  /** Prolog for kernel, extracts work group parameters. */
  private lazy val prolog: Prolog =
    new Prolog(workFieldType, workGroup, addressing, _usesLocalMemory, samplingMode, needsSampler)
  /** User-defined code for the kernel. */
  private var codeFragmentDAG: DAG[Fragment] = null
  /** Synthesized code for the kernel. */
  lazy val kernelCode: String = synthesizeCode
  /** User-supplied OpenCL code for body of kernel. */
  private var rawUserCode: String = null
  /** Useful name for debugging. */
  override def toString: String = {
    getClass.getSimpleName + "_" + id + "(" + opcode + ") => " + fieldTypesString +
            " '" + name + "'" + ", inputs = " + inputFieldTypesString
  }

  // Flags that help with kernel merging:

  /** Does this kernel's code use a local memory? */
  protected var _usesLocalMemory = false
  /** Set of input indices read non-locally by this kernel's code */
  protected val nonlocallyReadInputIndices = new scala.collection.mutable.HashSet[Int]()

  /** Does this kernel's code do a non-local write of the output indexed by `outputIndex`? */
  def doesNonlocalWrite(outputIndex: Int) = {
    require(outputIndex < codeFragmentDAG.roots.length)
    val answer =
    codeFragmentDAG.roots(outputIndex).asInstanceOf[CodeFragmentOutput].getWriteType match {
      case WriteLocal => false
      case _ => true
    }
    answer
  }

  /** The non-locally read inputs are remembered by their input indices, because
  * that is how they are represented in the kernel source, plus the input kernels
  * can change during merging with no notification to this kernel.  However,
  * during merging and input de-duplication, it is best to work with the input
  * kernels in effect at the time.  The following methods translate between
  * a HashSet[AbstractKernel] and the underlying stored HashSet[Int].
  */
  def nonlocallyReadInputs = nonlocallyReadInputIndices.map(i => in(i).source)
  def nonlocallyReadInputs_=(kernels: scala.collection.mutable.HashSet[AbstractKernel]) {
    require(nonlocallyReadInputIndices.size == 0)
    for (i <- 0 until in.length)
      if (kernels contains in(i).source)
        nonlocallyReadInputIndices += i
  }

  /** Does this kernel's code do a non-local read of this input? */
  def doesNonlocalRead(input: AbstractKernel) = {
    nonlocallyReadInputs.contains(input)
  }

  /** The FieldType of the single output of this kernel */
  def fieldType = {
    require(fieldTypes.length == 1,"Expecting single-output kernel.")
    fieldTypes(0)
  }

  // Initialize
  for (i <- 0 until in.length) {
    // Check that all inputs are legal.
    require(in(i) != null)
    val fieldType = in(i).fieldType

    // HyperKernels pass values in registers using the OpenCL fundamental types
    // (float, float2, float3, ...). Here we determine the OpenCL type to
    // use for the result field.
    val elementType = addressing match {
      case SmallTensorAddressing => SmallTensorAddressing.clType(fieldType)
      case TensorElementAddressing => CLFloat
      case BigTensorAddressing => CLFloat  // actually null, no values passed
    }

    // Create the fragments for the input fields
    inFieldFragments(i) =
            new InputFieldFragment(in(i).fieldType, i, in, elementType)
    require(inFieldFragments(i).id == in(i).source.id)
  }
  //if (addressing == FieldPointAddressing)
  //  require(workFieldType.isTensorField)

  /** Attach a code string to the hyperkernel; this must be the last thing
    * called by subclasses in order to create a viable hyper kernel.
    *
    * @param codeString OpenCL code for the kernel.
    */
  protected def addCode(codeString: String) {
    require(rawUserCode == null)
    rawUserCode = codeString
    analyzeCode(rawUserCode)
    val codeFragment = new CodeFragment(
      inFieldFragments.toArray, addressing, rawUserCode, elementTypes, resultTypes)
    // Create the CodeFragmentOutputs, all pointing back to the same CodeFragment, but with different output indices
    val codeOutputFragments = Array.tabulate[Fragment](resultTypes.length){ i => new CodeFragmentOutput(codeFragment, i) }
    addCodeFragment(new DAG[Fragment](codeOutputFragments))
  }

  /** Analyze source code for it's "non-nice" properties: local memory,
    * non-local reads and writes.
    *
    * @param code Source code for kernel.
    */
  private def analyzeCode(code: String) {
    // Create a set of inputs that are read nonlocally
    // By using RE's, the runtime does not scale with the number of inputs
    for (keyword <- Array("readNonlocal", "readElement", "readElementNonlocal",
      "readScalar", "readPoint", "_readTensorNonlocal", "_readTensorElementNonlocal")) {
      val keywordRE = (keyword + "\\(@in(\\d+)\\)").r   // e.g. readElement(@in0)
      for (keywordRE(i) <- keywordRE.findAllIn(code)) {
        nonlocallyReadInputIndices += i.toInt
      }
    }
    _usesLocalMemory =
            code.contains("__local")
  }

  /** Add a code fragment DAG directly to the hyperkernel.
    *
    * @param dag The code fragment DAG to attach.
    */
  private[HyperKernel] def addCodeFragment(dag: DAG[Fragment]) {
    require(codeFragmentDAG == null)
    codeFragmentDAG = dag
    require(codeFragmentDAG.roots.length == resultTypes.length)
    for (i <- 0 until resultTypes.length) {
      outFieldFragments(i) = new OutputFieldFragment(codeFragmentDAG.roots(i),
        resultTypes(i), i, this, elementTypes(i))
    }
  }

  /** Mark input field "index" as constant. */
  protected def makeInputConstant(index: Int) {
    inFieldFragments(index).constant = true
  }

  /** Print out a HyperKernel for debugging. */
  def print(withCode: Boolean = false) {
    println("--------------------------------------------------------")
    println("HyperKernel " + kernelName)
    println("    inputKernels:")
    in.map(_.source).foreach(kernel => {
      if (kernel == null)
        println("        null")
      else
        println("        " + kernel.opcode.toString + " " + kernel.id)
    })
    if (withCode)
      println(kernelCode)
    println("end HyperKernel " + kernelName)
    println("--------------------------------------------------------")
  }


  /** Synthesize OpenCL code for this kernel.
    *
    * @return Synthesized code.
    */
  private def synthesizeCode: String = {
    val buffer = new StringBuffer
    // Pull in Cog functions
    buffer.append(CogCLFunctions.real)
    buffer.append(CogCLFunctions.complex)
    buffer.append(CogCLFunctions.realToComplex)
    buffer.append(CogCLFunctions.complexToReal)

    // Kernel declaration
    buffer.append("__kernel void " + kernelName +"(\n")

    // Input arguments. If none, don't add a trailing comma.
    buffer.append(inFieldFragments.map(_.inDeclaration).mkString(",\n"))
    if (inFieldFragments.length > 0)
      buffer.append(",\n")

    // Output arguments.
    buffer.append(outFieldFragments.map(_.outDeclaration).mkString(",\n"))
    buffer.append(")\n")

    // Start of kernel's basic block
    buffer.append("{\n")

    // Field parameters: input, then output
    inFieldFragments.foreach(field => buffer.append(field.inParameters))
    outFieldFragments.foreach(field => buffer.append(field.outParameters))

    // Field parameters for the field driving the work item/group structure
    buffer.append(FieldDefines("", workFieldType))

    // Local work group parameters and prolog. If there are no cooperating
    // threads, we can exit this kernel early if out of bounds.
    buffer.append(LocalIndexParameters(workGroup))
    if (this._usesLocalMemory) {
      buffer.append(prolog.code)
    } else {
      buffer.append(prolog.code)
      buffer.append(prolog.returnIfOutOfBounds())
    }

    // Code body. This requires a post-order traversal of the Fragment DAG.
    // First set output index for root code fragments, which might be writing
    // outputs directly from the fragment
    for(i <- 0 until codeFragmentDAG.roots.length)
      codeFragmentDAG.roots(i).asInstanceOf[CodeFragmentOutput].setOutputIndex(i)
    codeFragmentDAG.traversePostorder {
      fragment => buffer.append(fragment.code)
    }

    // Escape if out of bounds. We can't do this at the beginning for Nonlocal
    // operators because we might lose some threads needed for cooperative
    // processing.
    if (this._usesLocalMemory)
      buffer.append(prolog.returnIfOutOfBounds())

    // Generate code for output
    outFieldFragments.foreach(out => buffer.append(out.writeResult(addressing)))

    // Cleanup #defines in code (necessary because the OpenCL compiler allocates
    // registers for consts, creating too much register pressure).
    inFieldFragments.foreach(in => buffer.append(in.cleanup))
    outFieldFragments.foreach(out => buffer.append(out.cleanup))
    // Cleanup #defines for the field driving the work item/group structure
    buffer.append(FieldDefines.cleanup("", workFieldType))
    buffer.append(LocalIndexParameters.cleanup(workGroup))

    // End of kernel.
    buffer.append("}\n\n")

//    println("HyperKernel DEBUG:")
//    println(buffer.toString)
//    System.exit(1)

    renumberFragmentIDs(buffer.toString)
  }

  /** Change globalIDs in the _tempNNN_ form to consecutively numbered
    * IDs _temp_1, _temp_2, etc.  This reduces the number of different
    * kernels by putting them in a standard form. */
  private def renumberFragmentIDs(code: String, nextId: Int = 1): String = {
    val s = UniqueID.findFirstIn(code)
    if (s == null)
      code
    else {
      val codeWith1Substitution = code.replaceAll(s, "_temp_" + nextId)
      renumberFragmentIDs(codeWith1Substitution, nextId+1)
    }
  }

  def returnIfOutOfOutputFieldBounds(index: Int): String =
    prolog.returnIfOutOfOutputFieldBounds(index, resultTypes(index))

  /** Release all memory held by this kernel.
    */
  private def destroy() {
    for (i <- 0 until in.length)
      in(i) = null
    for (i <- 0 until outFieldFragments.length)
      outFieldFragments(i) = null
    codeFragmentDAG = null
  }

  /** Check if two kernels can be merged.
    *
    * @param kernel The kernel that this hyperkernel would be merged into;
    *        this hyperkernel must be driving an input on kernel.
    * @return True if they can be merged.
    */
  private def canMergeWithSink(kernel: AbstractKernel): Boolean = {
    kernel match {
      case hypersink: HyperKernel =>
        // There are several conditions for mergeability:
        //   1. The connection from source to sink must be a pure edge,
        //      not a hyperedge with multiple sinks. In the future this might
        //      be done by cloning the source and absorbing it into the sink.
        //   2. The two kernels must generate fields with the same shape. This
        //      is because the work group sizes must be identical for ease of
        //      merging. Non-identical work group sizes can be done, at least
        //      in principle, but would require a more sophisticated merger.
        //   3. The two kernels must have the same addressing mode.
        //   4. The sink cannot tile the input field using local memory. Tiles
        //      cannot be conveyed through registers as required by the merging
        //      algorithm.
        //
        // NOTE: we add one more condition below: that "this" cannot
        // use local memory either. Empirically this speeds things up a lot,
        // though I don't understand why. Perhaps the local memory allocation
        // creates greater memory pressure. E.g. perhaps registers are spilled
        // to local memory, and if a lot of it is allocated, there's less
        // "spill space." This deserves some more investigation. XXX
        // Note that a this -> hypersink connection can be probed: it results
        // in an additional output of the merged kernel.
        val ok = (this.addressing != BigTensorAddressing) &&
                // We have to be careful that an input of the new merged hyperkernel does not depend on an output.
                // In the absence of a full-blown dependency analysis, the following ensures this.
                (this.drivesOnly(hypersink) || this.drivesAllOf(hypersink)) &&
                (this.workFieldType.fieldShape == hypersink.workFieldType.fieldShape) &&
                (this.addressing != TensorElementAddressing || this.workFieldType.tensorShape == hypersink.workFieldType.tensorShape) &&
                (this.addressing == hypersink.addressing) &&
                (!this._usesLocalMemory || Cog.localMemoryMerging) &&
                !hypersink._usesLocalMemory &&
                !hypersink.doesNonlocalRead(this) &&
                // No outputs can do a non-local write (which sets row, column globally, but may not perform
                // the write until the final output section
                (0 until this.outputs.length).forall(i => {!this.doesNonlocalWrite(i)}) &&
                !mergedKernelWillExceedMaxArgs(hypersink, this) &&
                this.samplingMode.isCompatibleWith(hypersink.samplingMode) &&
                // Covers cases where the kernels have overridden the local workgroup size
                this.workGroup == hypersink.workGroup

        ok
      case _ =>
        false
    }
  }

  /** Report if this kernel has inputs driven by the same kernel output
    * (i.e. virtual field register)
    *
    * @return True if there are any duplicated inputs
    */
  private[compiler] def hasDuplicatedInputs: Boolean = {
    val uniqueInputsSet = new IdentityHashSet[VirtualFieldRegister]()
    // If we can find an element that is not newly added, we have duplicates.
    inputs.exists( !uniqueInputsSet.add(_) )
  }

  /** Report if this kernel drives only the specified sink kernel (i.e. no other kernels are sinks).
    *
    * @return True if there are no other kernel sinks.
    */
  private[compiler] def drivesOnly(sink: HyperKernel): Boolean = {
    outputs.forall(output => {
      output.sinks.length == 0 ||
        output.sinks.length == 1 && (output.sinks.head eq sink)
    })
  }

  /** Report if this kernel drives all inputs of the specified sink kernel.
    *
    * @return True if there are no other drivers of `sink`.
    */
  private[compiler] def drivesAllOf(sink: HyperKernel): Boolean = {
    sink.inputs.forall(input => {input.source eq this})
  }

  /** Search for an input that this kernel can be merged into.
    *
    * @return The first mergeable input (an AbstractKernel), else `None`.
    */
  def findMergeableInput: Option[HyperKernel] = {
    for (input <- inputs) {
      input.source match {
        case hyperIn: HyperKernel =>
          if (hyperIn.canMergeWithSink(this))
            return Some(hyperIn)
        case _ =>
      }
    }
    None
  }

  /** Compile the hyperkernel and print out source code for debugging, then
    * exit.
    */
  def debugCompile() {
    println("Compiling " + getClass.getName)
    val source = kernelCode
    try {
      val program = clContext.createProgram(source)
      program.build(clDevice)
      println("...success.")
    } catch {
      case e: Exception => println("...failed.")
    }
    println("Code:\n" + source)
    System.exit(1)
  }

  /** Create a clone of this kernel that uses a new set of virtual field registers
    *  as inputs.  Useful for breaking a large circuit apart into smaller subcircuits.
    */
  def copyWithNewInputs(inputs: Array[VirtualFieldRegister]): AbstractKernel = {
    val originalWorkFieldType = workFieldType
    val originalWorkGroup = workGroup
    val copy = new HyperKernel(opcode, inputs, resultTypes, addressing, samplingMode) {
      // The kernel being copied may have overridden the defaults for
      // workFieldType and workGroup.  Copies these values over:
      override lazy val workFieldType = originalWorkFieldType
      override lazy val workGroup = originalWorkGroup
    }
    require(copy.workFieldType == this.workFieldType)
    require(copy.workGroup == this.workGroup)
    copy._usesLocalMemory     = this._usesLocalMemory
    //copy.nonlocallyReadInputs = this.nonlocallyReadInputs // This won't work; we have new inputs!
    if (this.rawUserCode != null) {
      copy.addCode(this.rawUserCode)
    } else {
      copy.addCodeFragment(this.codeFragmentDAG)
    }
    copy
  }

  /** Can this kernel merge into `that`, creating a multi-output kernel?  The
    * consideration of whether this is a performance win should be considered
    * externally.  This just reflects whether the code can be generated for the
    * merged kernel and that it will function correctly.
    *
    * @param that The kernel that would be merged into this kernel.
    * @return True if they can be merged.
    *
    * */
  def canShareMultiOutputKernel(that: HyperKernel) =

  // both kernels currently can't use local memory since there is a
  // premature return executed by some of the threads after the local memory
  // read-in

  // We don't want to compare Workgroup sizes here, because the workgroup is
  // a rounded-up version of the workfield.  Ultimately we might be able to
  // merge based on workgroup, but not until the early-return of threads based
  // on the workfield is redone.  Note that we don't want to compare the workfield
  // directly, because in SmallTensorAddressing and BigTensorAddressing, different
  // tensor shapes may be compatible (the thread organization is based on the
  // fieldShape only).

    (this.addressing == that.addressing) &&
    (this.workFieldType.fieldShape == that.workFieldType.fieldShape) &&
    (this.addressing != TensorElementAddressing ||
            this.workFieldType.tensorShape == that.workFieldType.tensorShape) &&
    !(this._usesLocalMemory && that._usesLocalMemory) &&
    // No outputs can do a non-local write (which sets row, column globally, but may not perform
    // the write until the final output section
    (0 until this.outputs.length).forall(i => {!this.doesNonlocalWrite(i)}) &&
    !mergedKernelWillExceedMaxArgs(this, that) &&
     this.samplingMode.isCompatibleWith(that.samplingMode) &&
    // Covers cases where the kernels have overridden the local workgroup size
    this.workGroup == that.workGroup
}


/** Companion object for merging hyperkernels.
  */
private[cogx]
object HyperKernel {

  /** computeWorkGroupParameters() uses the following defaults to set
    * the size of the workgroup:
    * 1D fields LxRxC = 1 x 1 x (DefaultLocalRows * DefaultLocalColumns)
    * 2D fields LxRxC = 1 x DefaultRows x DefaultColumns
    */
  val DefaultLocalRows = 16
  val DefaultLocalColumns = 16

  /** Replace `kernel`, which has redundant (duplicated) inputs with another
    * kernel with unique inputs.
    *
    * @param kernel Kernel with redundant inputs
    */
  private[compiler] def removeRedundantInputs(kernel: HyperKernel): HyperKernel = {
    val inputSet = new IdentityHashSetDeterministic[VirtualFieldRegister]
    kernel.inputs.foreach(inputSet += _)
    require(inputSet.size < kernel.inputs.length,
      "Expecting duplicated inputs, found none.")
    val resultOpcode = InputReduced(kernel.opcode)
    val resultSamplingMode = kernel.samplingMode
    val resultKernel = new CompoundHyperKernel(resultOpcode, inputSet.toArray,
      kernel.resultTypes, kernel.addressing, resultSamplingMode)
    for (i <- 0 until kernel.numOutputs)
      resultKernel.outputs(i).name = kernel.outputs(i).name

    // Update some flags of the new kernel based on the old
    resultKernel._usesLocalMemory = kernel._usesLocalMemory
    resultKernel.nonlocallyReadInputs = kernel.nonlocallyReadInputs

    // Link old inFieldFragments in source and sink to new inFieldFragments
    // in mergedKernel.  First we need a map to get from the new set of unique
    // input kernels to their InFieldFragments:
    val registerToFragment = new util.IdentityHashMap[VirtualFieldRegister, InputFieldFragment]
    resultKernel.inFieldFragments.foreach{frag =>
      registerToFragment.put(frag.registerDriver, frag)}

    // Find the input field fragment on the sink that will be merged with the
    // output of the source and link it to that source's CodeFragment.  Link
    // the other sink inputs to the new InFieldFragments.
    for (inFragment <- kernel.inFieldFragments) {
      val remappedInFragment =
          registerToFragment.get(inFragment.registerDriver)
      inFragment.bindDrivingInput(remappedInFragment)
    }

    // Attached the merged code DAG to the new kernel
    resultKernel.addCodeFragment(kernel.codeFragmentDAG)

    // Destroy the old kernel, return the new.
    resultKernel stealOutputsFrom kernel
    resultKernel
  }


  /**
    * Create the set of unique inputs driving two kernels to be merged.
    * A possible connection from source -> sink is considered.
    *
    * @param sink Sink hyperkernel.
    * @param source Source hyperkernel.
    * @return Inputs that would drive the merged source and sink.
    */
  private[this] def mergedInputSet(sink: HyperKernel, source: HyperKernel): Array[VirtualFieldRegister] = {
    // Merge the input virtual field registers, excluding the source --> sink connection, and
    // eliminate duplicates.
    val inputSet = new IdentityHashSetDeterministic[VirtualFieldRegister]
    // Add sink input edges, but not iif the edge comes from the source
    sink.inputs.foreach(input =>
      if (!(input.source eq source))
        inputSet += input
    )
    source.inputs.foreach(inputSet += _)
    inputSet.toArray
  }

  /**
    * Create the set of unique inputs driving two kernels to be merged.
    * A possible connection from source -> sink is considered.
    *
    * @param sink Sink hyperkernel.
    * @param source Source hyperkernel.
    * @return Inputs that would drive the merged source and sink.
    */
  private[this] def mergedOutputSet(sink: HyperKernel, source: HyperKernel): Array[VirtualFieldRegister] = {
    // Will the merge totally enclose this source -> sink connection?
    def isBuriedEdge(edge: VirtualFieldRegister) =
      (edge.source eq source) && edge.sinks.length == 1 && (edge.sinks(0) eq sink)

    // Combine the output virtual field registers, excluding the source --> sink connection
    // Also create the Array of CodeFragments for the merged kernel
    val outputSet = new ArrayBuffer[VirtualFieldRegister]
    sink.outputs.foreach(outputSet += _)
    // Add source output edges, but not if the merge would enclose the edge
    for (outIndex <- 0 until source.numOutputs) {
      val output = source.outputs(outIndex)
      if (!isBuriedEdge(output) || output.probed) {
        outputSet += output
      }
      else
        require(!output.probed, "Invalid merge: removing a probed field!")
    }
    outputSet.toArray
  }

  /**
    * Merge two hyperkernels into one. This routine no longer requires a
    * source -> sink connection.
    *
    * @param sink Sink hyperkernel.
    * @param source Source hyperkernel.
    * @return Hyperkernel which has merged source and sink.
    */
  private[compiler] def doMerge(sink: HyperKernel, source: HyperKernel): HyperKernel = {

    def inFragString(in: InputFieldFragment) =
      in.toString + "  " + in.registerDriver.toString

    //println("HyperKernel.doMerge...")
    //println("      sink:   " + sink)
    //println("      source: " + source)
    require(sink != null && source != null)

    // To simplify the logic, we preclude a sink -> source connection
    source.inputs.foreach( input => require(!(input.source eq sink)))

    // Will the merge totally enclose this source -> sink connection?
    def isBuriedEdge(edge: VirtualFieldRegister) =
      (edge.source eq source) && edge.sinks.length == 1 && (edge.sinks(0) eq sink)

    val inputSet = mergedInputSet(sink, source)

    val outputSet = mergedOutputSet(sink, source)

    // DEBUG
//    println("sink in fragments for " + sink.toString)
//    sink.inFieldFragments.foreach(frag => println("     " + inFragString(frag)))
//    println("source in fragments for " + source.toString)
//    source.inFieldFragments.foreach(frag => println("     " + inFragString(frag)))
//    println("merged in kernels:")
//    inputSet.foreach(kernel => println("     " + kernel.toString))

    val mergedOpcode =
      if (sink.opcode.isInstanceOf[MergedOp])
        MergedOp(source.opcode :: sink.opcode.asInstanceOf[MergedOp].opcodes)
      else
        MergedOp(List(source.opcode, sink.opcode))

    val mergedSamplingMode = sink.samplingMode merge source.samplingMode

    // Create the merged kernel using the merged input fields.

    val mergedKernel =
      new CompoundHyperKernel(mergedOpcode, inputSet.toArray, outputSet.map(_.fieldType).toArray,
        sink.addressing, mergedSamplingMode) {

        // The kernels being merged may have overridden the defaults for
        // workFieldType and workGroup.  Copies these values over:
        require(sink.addressing == source.addressing)
        require(sink.workFieldType.fieldShape == source.workFieldType.fieldShape)
        sink.addressing match {
          case SmallTensorAddressing =>
          case TensorElementAddressing =>
            require(sink.workFieldType.tensorShape == source.workFieldType.tensorShape)
          case BigTensorAddressing =>
          case _ =>
        }
        require(sink.workGroup == source.workGroup)
        override lazy val workFieldType = sink.workFieldType
        override lazy val workGroup = sink.workGroup
      }

    for (i <- 0 until outputSet.length)
      mergedKernel.outputs(i).stealProbeAndNameFrom(outputSet(i))

    // Update some flags of the new merged kernel based on the source and sink

    mergedKernel._usesLocalMemory =
            source._usesLocalMemory || sink._usesLocalMemory

    mergedKernel.nonlocallyReadInputs =
            sink.nonlocallyReadInputs union source.nonlocallyReadInputs

    // Link old inFieldFragments in source and sink to new inFieldFragments
    // in mergedKernel.  First we need a map to get from the new set of unique
    // input virtual field registers to their InFieldFragments:
    val registerToInputFragment = new util.IdentityHashMap[VirtualFieldRegister, InputFieldFragment]
    mergedKernel.inFieldFragments.foreach {fragment =>
      registerToInputFragment.put(fragment.registerDriver, fragment)
    }

    // Consistency check:
//    println("merged in fragments")
//    mergedKernel.inFieldFragments.foreach {fragment =>
//      println("    " + inFragString(fragment))
//    }

    // Find the input field fragment on the sink that will be merged with the
    // output of the source and link it to that source's CodeFragment.  Link
    // the other sink inputs to the new InFieldFragments.
    for (inFragment <- sink.inFieldFragments) {
//      println("getting sink inFragment: " + inFragString(inFragment))
      val remappedInFragment: Fragment = {
//        println("     source: " + source.toString)
//        println("     kernelDriver: " + inFragment.kernelDriver.toString)
        if (source eq inFragment.registerDriver.source) {
          val outputIndex = inFragment.registerDriver.sourceOutputIndex match {
            case Some(i) => i
            case None => throw new RuntimeException("Compiler error: kernel merging problem.")
          }
          val in = source.outFieldFragments(outputIndex).input
          require(in != null)
          in
        } else {
          val in = registerToInputFragment.get(inFragment.registerDriver)
          require(in != null)
          in
        }
      }
      inFragment.bindDrivingInput(remappedInFragment)
    }
    // Also link the sources InFieldFragments to those of the new merged kernel.
    source.inFieldFragments.foreach(inFragment =>  {
//      println("     source: " + source.toString)
//      println("     registerDriver: " + inFragment.registerDriver.toString)
      inFragment.bindDrivingInput(
        registerToInputFragment.get(inFragment.registerDriver))
    })

    // Attached the merged code DAG to the new kernel
    val mergedCodeFragments = new ArrayBuffer[Fragment]

    outputSet.foreach(output => {
      val drivingKernel = output.source.asInstanceOf[HyperKernel]
      val outputIndex = output.sourceOutputIndex match {
        case Some(i) => i
        case _ => throw new RuntimeException("Compiler internal merge error.")
      }
      mergedCodeFragments += drivingKernel.codeFragmentDAG.roots(outputIndex)
    })

    mergedKernel.addCodeFragment(new DAG[Fragment](mergedCodeFragments.toArray))

    // move the sinks on the sink and source kernels over to the new merged
    // kernel, respecting that a buried source -> sink connection will not
    // appear as an output of the new merged kernel
    var mergedOutputIndex = 0
    for (outIndex <- 0 until sink.outputs.length) {
      val output = sink.outputs(outIndex)
      mergedKernel.outputs(mergedOutputIndex).stealSinksFrom(output)
      mergedOutputIndex += 1
    }
    for (outIndex <- 0 until source.outputs.length) {
      val output = source.outputs(outIndex)
      if (!isBuriedEdge(output) || output.probed) {
        mergedKernel.outputs(mergedOutputIndex).stealSinksFrom(output, exceptSink=sink)
        mergedOutputIndex += 1
      }
    }
    sink.removeFromCircuit(mustDo = true)
    // sink.removeFromCircuit may have recursively removed the source, but
    // there's no harm in doing it twice.  If there is no source -> sink
    // connection, as with horizontal merges, this removal is necessary
    source.removeFromCircuit(mustDo = true)
    mergedKernel
  }

  /** Determine if the kernel that would result from merging sink and source
    * will have too many args.
    *
    * @param sink Sink hyperkernel.
    * @param source Source hyperkernel.
    * @return Boolean, will merged kernel have too many args.
    */
  private def mergedKernelWillExceedMaxArgs(sink: HyperKernel,
                                            source: HyperKernel): Boolean = {
    // OpenCL 1.1 guarantees at least 1024 bytes for the kernel parameters.
    // Since our parameters are all 4-byte pointers, this translates to 256
    // parameters maximum (conservatively).

    // The following parameters could be read from the OpenCL devices, but the
    // compiler currently runs before the OpenCLPlatform is created.  We
    // currently see a parameter size of 4352 in practice, with an address size
    // of 32 bits (so the true limit is 1088 args)
    val MaxKernelParameterSizeBytes = 1024
    val AddressBits = 32
    val AddressBytes = AddressBits/8

    val MaxKernelParameters = MaxKernelParameterSizeBytes/AddressBytes

    // quickly calculate a pessimistically large estimate for the merged kernel
    // arg count, assuming no shared inputs between the source and sink and no
    // source outputs that get buried by the merge.
    val maxMergedKernelArgCount =
      sink.numInputs + source.numInputs + sink.numOutputs + source.numOutputs

    if (maxMergedKernelArgCount <= MaxKernelParameters)
      false
    else {
      val exactMergedKernelArgCount =
        mergedInputSet(sink, source).length + mergedOutputSet(sink, source).length
      exactMergedKernelArgCount > MaxKernelParameters
    }
  }

  /** Compute work group parameters for a kernel based on a FieldType (typically
    * the output field of the kernel) and an addressing mode.  Overriding the
    * default work group rows and columns has an impact on mergability, so should
    * only be done by kernels using local memory (for now).
    */
  def computeWorkGroupParameters(workFieldType: FieldType,
                                 addressing: AddressingMode,
                                 defaultLocalRows: Int = DefaultLocalRows,
                                 defaultLocalColumns: Int = DefaultLocalColumns):
       WorkGroupParameters =
  {

    val fieldDimensions = workFieldType.dimensions

    // The following parameters should depend on the target GPU. XXX
    // Note: local workGroup threads = localLayers * localRows * localColumns

    val (localLayers, localRows, localColumns): Tuple3[Long, Long, Long] =
      if (fieldDimensions > 1)
        (1, defaultLocalRows, defaultLocalColumns)
    else
        (1, 1, defaultLocalRows * defaultLocalColumns)

    // Compute work group parameters. Note that OpenCL supports only dimensions
    // (1, 2, 3), so we must map the Cog computation to that range. Note that
    // this algorithm is coupled with the algorithms in FieldIndex,
    // FieldParameters, and PrologFragment.

    // Note: 0D vector fields are processed by 1D kernels, so different
    // vector elements are not guaranteed to be processed in different work
    // groups for this case.  This caused a problem with the ScalarReduceHyperKernel's
    // handling of 0D input, so kernel code should pay attention to this fact.

    val tensorPoints = workFieldType.tensorShape.points
    var workGroupDimensions = fieldDimensions
    val layout = new FieldMemoryLayoutImpl(workFieldType)
    var columns = layout.columns
    var rows = layout.rows
    var layers = layout.layers
    addressing match {
      case TensorElementAddressing =>
        workGroupDimensions = (workGroupDimensions + 1) min 3
        fieldDimensions match {
          case 3 => layers *= tensorPoints
          case 2 => layers = tensorPoints
          case 1 => rows = tensorPoints
          case 0 => columns = tensorPoints
        }
      case SmallTensorAddressing =>
        workGroupDimensions = workGroupDimensions max 1
        fieldDimensions match {
          case 3 =>
          case 2 =>
          case 1 =>
          case 0 => columns = 1
        }
      case BigTensorAddressing =>
        workGroupDimensions = workGroupDimensions max 1
        fieldDimensions match {
          case 3 =>
          case 2 =>
          case 1 =>
          case 0 => columns = 1
        }
    }

    val parms = new WorkGroupParameters(workGroupDimensions,
      globalLayers = roundUp(localLayers, layers),
      globalRows = roundUp(localRows, rows),
      globalColumns = roundUp(localColumns, columns),
      localLayers, localRows, localColumns)
    parms
  }

  /** Round up "value" to the nearest multiple of "toMultipleOf".   */
  def roundUp(toMultipleOf: Long, value: Long): Long = {
    val remainder = value % toMultipleOf
    if (remainder == 0)
      value
    else
      value + toMultipleOf - remainder
  }

  /** Helper function to set up definition of layer, row and column variables
    * as needed for the dimensionality of a field, as is needed prior to
    * use of the readNonlocal() fragment generators.
    *
    * This flexible routine cannot be called directly, but is accessed by
    * some special-purpose versions below for defining, setting, or
    * defining-and-setting the layer, row and column variables.
    *
    * @param fieldType Field to use for the dimensionality of the field.
    * @param layerDef int layer = 'layerDef';
    * @param rowDef int row = 'rowDef';
    * @param columnDef int column = 'columnDef';
    * @return Code string to define layer, row and column.
    */
  private def lrcHelper(prefix: String, fieldType: FieldType, layerDef: String,
          rowDef: String, columnDef: String, indent: String) = {
    val code = new StringBuffer
    if (fieldType.dimensions > 2) {
      code append indent + prefix + "layer"
      if (layerDef != null)
        code append " = " + layerDef
      code append ";\n"
    }
    if (fieldType.dimensions > 1) {
      code append indent + prefix + "row"
        if (rowDef != null)
          code append " = " + rowDef
      code append ";\n"
    }
    if (fieldType.dimensions > 0) {
      code append indent + prefix + "column"
      if (columnDef != null)
        code append " = " + columnDef
      code append ";\n"
    }
    code.toString
  }

  // Use this method to define, but not set row, column and layer
  def defineLayerRowColumn(fieldType: FieldType, indent: String = "        ") =
    lrcHelper("int ", fieldType, null, null, null, indent)

  // Use this method to set an already defined row, column and layer
  def setLayerRowColumn(fieldType: FieldType, layerDef: String,
                        rowDef: String, columnDef: String, indent: String = "        ") =
    lrcHelper("", fieldType, layerDef, rowDef, columnDef, indent)

  // Use this method to both define and set row, column and layer
  def defineAndSetLayerRowColumn(fieldType: FieldType, layerDef: String,
              rowDef: String, columnDef: String, indent: String = "        ") =
    lrcHelper("int ", fieldType, layerDef, rowDef, columnDef, indent)
}
