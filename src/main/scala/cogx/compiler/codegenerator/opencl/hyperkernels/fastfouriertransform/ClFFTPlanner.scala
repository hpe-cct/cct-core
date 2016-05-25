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

package cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform

import scala.collection.mutable.ArrayBuffer

/** A "Planner" for synthesizing OpenCL kernels that implement the FFT.
  *
  * @param deviceMaxWorkItemsPerWorkGroup Maximum number of work items in a work group.
  * @param size The dimensions of the input to be transformed.
  * @param dataFormat Data format, InterleavedComplex (array of complex) or
  *                   SplitComplex (separate planar arrays).
  */
private[cogx]
class ClFFTPlanner private (deviceMaxWorkItemsPerWorkGroup: Int, val size: ClFFTDim3,
                            dataFormat: ClFFTDataFormat) {

  // dimension of transform must be either 1, 2 or 3
  require(size.dimension >= 1 && size.dimension <= 3)

  // linked list of kernels which needs to be executed for this fft
  var kernel_infos = ArrayBuffer[ClFFTKernelInfo]()

  // Maximum size of signal for which local memory transposed based
  // fft is sufficient i.e. no global mem transpose (communication)
  // is needed
  val max_localmem_fft_size = 2048

  // Maximum work items per work group allowed. This, along with max_radix below controls
  // maximum local memory being used by fft kernels of this plan. Set to 256 by default
  val max_work_item_per_workgroup = deviceMaxWorkItemsPerWorkGroup

  // Maximum base radix for local memory fft ... this controls the maximum register
  // space used by work items. Currently defaults to 16
  val max_radix = 16

  // Device depended parameter that tells how many work-items need to be read consecutive
  // values to make sure global memory access by work-items of a work-group result in
  // coalesced memory access to utilize full bandwidth e.g. on NVidia tesla, this is 16
  val min_mem_coalesce_width = 16

  // Number of local memory banks. This is used to generate kernel with local memory
  // transposes with appropriate padding to avoid bank conflicts to local memory
  // e.g. on NVidia it is 16.
  val num_local_mem_banks = 16

  createKernels()

  def this(deviceMaxWorkItemsPerWorkGroup: Int, columns: Int,
           dataFormat: ClFFTDataFormat) =
    this(deviceMaxWorkItemsPerWorkGroup, new ClFFTDim3(columns), dataFormat)

  def this(deviceMaxWorkItemsPerWorkGroup: Int, rows: Int, columns: Int,
           dataFormat: ClFFTDataFormat) =
    this(deviceMaxWorkItemsPerWorkGroup, new ClFFTDim3(columns, rows), dataFormat)

  def this(deviceMaxWorkItemsPerWorkGroup: Int, layers: Int, rows: Int, columns: Int,
           dataFormat: ClFFTDataFormat) =
    this(deviceMaxWorkItemsPerWorkGroup, new ClFFTDim3(columns, rows, layers), dataFormat)

  /** Get the OpenCL source code for the synthesized FFT, one pass each dimension. */
  def clSourceCode: Array[String] =
    kernel_infos.toArray.map(_.kernel_string.toString())

  def workDimensions: Array[WorkDimensions] =
    kernel_infos.toArray.map(kernelInfo => getKernelWorkDimensions(kernelInfo))

  def allSourceCode = clSourceCode.reduceLeft(_ + _)

  def kernelNames: Array[String] = kernel_infos.toArray.map(_.kernel_name)

  private def getKernelWorkDimensions(kernelInfo: ClFFTKernelInfo): WorkDimensions = {
    val lWorkItems: Int = kernelInfo.num_workitems_per_workgroup
    val batchSize = kernelInfo.dir match {
      case X =>
        size.y * size.z
      case Y =>
        size.z
      case Z =>
        1
    }
    val workGroupMultiplier = kernelInfo.dir match {
      case X =>
        Math.ceil(batchSize.toDouble / kernelInfo.num_xforms_per_workgroup).toInt
      case Y =>
        batchSize
      case Z =>
        batchSize
    }
    val numWorkGroups = kernelInfo.num_workgroups * workGroupMultiplier
    val gWorkItems = numWorkGroups * lWorkItems

    WorkDimensions(batchSize, gWorkItems, lWorkItems)
  }

  private def createKernels() {
    size.dimension match {
      case 1 =>
              FFT1D(X)
      case 2 =>
              FFT1D(X)
              FFT1D(Y)
      case 3 =>
              FFT1D(X)
              FFT1D(Y)
              FFT1D(Z)
      case _ =>
        throw new RuntimeException("Illegal FFT dimensions.")
    }
  }

  private def log2(x: Int): Int = if (x <= 1) 0 else 1 + log2(x / 2)

  private def isPowerOf2(x: Int) = x == (1 << log2(x))

  // For any n, this function decomposes n into factors for loacal memory tranpose
  // based fft. Factors (radices) are sorted such that the first one (radixArray[0])
  // is the largest. This base radix determines the number of registers used by each
  // work item and product of remaining radices determine the size of work group needed.
  // To make things concrete with and example, suppose n = 1024. It is decomposed into
  // 1024 = 16 x 16 x 4. Hence kernel uses float2 a[16], for local in-register fft and
  // needs 16 x 4 = 64 work items per work group. So kernel first performance 64 length
  // 16 ffts (64 work items working in parallel) following by transpose using local
  // memory followed by again 64 length 16 ffts followed by transpose using local memory
  // followed by 256 length 4 ffts. For the last step since with size of work group is
  // 64 and each work item can array for 16 values, 64 work items can compute 256 length
  // 4 ffts by each work item computing 4 length 4 ffts.
  // Similarly for n = 2048 = 8 x 8 x 8 x 4, each work group has 8 x 8 x 4 = 256 work
  // iterms which each computes 256 (in-parallel) length 8 ffts in-register, followed
  // by transpose using local memory, followed by 256 length 8 in-register ffts, followed
  // by transpose using local memory, followed by 256 length 8 in-register ffts, followed
  // by transpose using local memory, followed by 512 length 4 in-register ffts. Again,
  // for the last step, each work item computes two length 4 in-register ffts and thus
  // 256 work items are needed to compute all 512 ffts.
  // For n = 32 = 8 x 4, 4 work items first compute 4 in-register
  // lenth 8 ffts, followed by transpose using local memory followed by 8 in-register
  // length 4 ffts, where each work item computes two length 4 ffts thus 4 work items
  // can compute 8 length 4 ffts. However if work group size of say 64 is choosen,
  // each work group can compute 64/ 4 = 16 size 32 ffts (batched transform).
  // Users can play with these parameters to figure what gives best performance on
  // their particular device i.e. some device have less register space thus using
  // smaller base radix can avoid spilling ... some has small local memory thus
  // using smaller work group size may be required etc
  private def getRadixArray(size: Int): Array[Int] = {
    size match {
      case 2 => Array(2)
      case 4 => Array(4)
      case 8 => Array(8)
      case 16 => Array(8, 2)
      case 32 => Array(8, 4)
      case 64 => Array(8, 8)
      case 128 => Array(8, 4, 4)
      case 256 => Array(4, 4, 4, 4)
      case 512 => Array(8, 8, 8)
      case 1024 => Array(16, 16, 4)
      case 2048 => Array(8, 8, 8, 4)
      case _ => throw new RuntimeException("Illegal FFT dimension size.")
    }
  }

  private def getRadixArray(size: Int, maxRadix: Int): Array[Int] = {
    require(maxRadix > 1)
    val buffer = ArrayBuffer[Int]()
    var n = size
    while (n > maxRadix) {
      buffer.append(maxRadix)
      n /= maxRadix
    }
    buffer.append(n)
    buffer.toArray
  }

  // Approach for supporting split real/imag buffer layout prior to actually passing in two buffers for each field.
  private def insertCogCode(kernelString: StringBuilder): Unit = {
    kernelString.append("    const int dir = %dirVal%;\n")
    kernelString.append("    const int S = %batchSize%;\n")
  }

  private def insertVariables(kStream: StringBuilder, maxRadix: Int): Unit = {
    kStream.append(
      s"""|    int i, j, r, indexIn, indexOut, index, tid, bNum, xNum, k, l;
          |    int s, ii, jj, offset;
          |    float2 w;
          |    float ang, angf, ang1;
          |    __local float *lMemStore, *lMemLoad;
          |    float2 a[$maxRadix];
          |    int lId = get_local_id(0);
          |    int groupId = get_group_id(0);
          |""".stripMargin
    )
  }


  /** code snippet to read in a value from memory to the 'a' array */
  private def formattedLoad(kernelString: StringBuilder, aIndex: Int, gIndex: Int) {
    // #if used because the code within the 'if' and 'else' may reference
    // non-existent variables and hence would not compile.
    kernelString.append(
      s"""|        {
          |            column = $gIndex;
          |            tensorElement = %plane%;
          |#if %realInput%
          |            a[$aIndex] = (float2)(readElementNonlocal(@in0), 0.0f);
          |#elif %splitRealImaginary%
          |            a[$aIndex] = (float2)(readElementNonlocal(@in0), readElementNonlocal(@in1));
          |#else
          |            a[$aIndex] = readElementNonlocal(@in0);
          |#endif
          |        }
          |""".stripMargin
    )
  }

  /** code snippet to write an 'a' array value to memory */
  private def formattedStore(kernelString: StringBuilder, aIndex: Int, gIndex: Int) {
    // #if used because the code within the 'if' and 'else' may reference
    // non-existent variables and hence would not compile.
    // It's best to keep this definition entirely within curly braces so it looks like one statement.  In doing so,
    // we're protected against a usage that omits the braces, such as:
    //
    // kernelString.append(" if (threadId < threshHold)\n")
    // formattedStore(...)

    kernelString.append(
      s"""|        {
          |            column = $gIndex;
          |            tensorElement = %plane%;
          |#if %realOutput%
          |            @outElementNonlocal0 = %scalingMultiply%a[$aIndex].x;
          |#elif %splitRealImaginary%
          |            @outElementNonlocal0 = %scalingMultiply%a[$aIndex].x;
          |            @outElementNonlocal1 = %scalingMultiply%a[$aIndex].y;
          |#else
          |            @outElementNonlocal0 = %scalingMultiply%a[$aIndex].x;
          |            column += partStride(@out0);
          |            @outElementNonlocal0 = %scalingMultiply%a[$aIndex].y;
          |#endif
          |        }
          |""".stripMargin
    )
  }

  // These 2 routines clean up the code generation quite a bit and further localize
  // the I/O handling, making it easier to adapt to changes in Cog HyperKernel I/O.  -RJC

  /** Bump the input pointer by an offset (measured in elements) */
  private def adjustInPtr(kernelString: StringBuilder, offsetStr: String) {
    kernelString.append(
      s"""|        fieldName(@in0) += $offsetStr;
          |#if %splitRealImaginary% && !%realInput%
          |        fieldName(@in1) += $offsetStr;
          |#endif
          |""".stripMargin
    )
  }

  /** Bump the output pointer by an offset (measured in elements) */
  private def adjustOutPtr(kernelString: StringBuilder, offsetStr: String) {
    kernelString.append(
      s"""|        fieldName(@out0) += $offsetStr;
          |#if %splitRealImaginary% && !%realOutput%
          |        fieldName(@out1) += $offsetStr;
          |#endif
          |""".stripMargin
    )
  }

  private def insertGlobalLoadsAndTranspose(kernelString: StringBuilder,
                                            N: Int,
                                            numWorkItemsPerXForm: Int,
                                            numXFormsPerWG: Int,
                                            R0: Int,
                                            mem_coalesce_width: Int): Int =
  {
    import kernelString.append
    def appendLocalBarrier = { append("    barrier( CLK_LOCAL_MEM_FENCE );\n") }

    val log2NumWorkItemsPerXForm: Int = log2(numWorkItemsPerXForm)
    val groupSize: Int = numWorkItemsPerXForm * numXFormsPerWG
    var lMemSize = 0
    if (numXFormsPerWG > 1)
      append(s"        s = S & ${numXFormsPerWG - 1};\n")

    if (numWorkItemsPerXForm >= mem_coalesce_width) {
      if (numXFormsPerWG > 1) {
        append(
          s"""|    ii = lId & ${numWorkItemsPerXForm - 1};
              |    jj = lId >> $log2NumWorkItemsPerXForm;
              |    if ( !s || (groupId < get_num_groups(0)-1) || (jj < s) ) {
              |         offset = mad24( mad24(groupId, $numXFormsPerWG, jj), $N, ii );
              |""".stripMargin
        )
        adjustInPtr(kernelString, "offset")
        adjustOutPtr(kernelString, "offset")
        for (i <- 0 until R0)
          formattedLoad(kernelString, i, i*numWorkItemsPerXForm)
        append("    }\n")
      } else {
        append(
          s"""|    ii = lId;
              |    jj = 0;
              |    offset = mad24(groupId, $N, ii );
              |""".stripMargin
        )
        adjustInPtr(kernelString, "offset")
        adjustOutPtr(kernelString, "offset")
        for (i <- 0 until R0)
          formattedLoad(kernelString, i, i*numWorkItemsPerXForm)
      }
    } else if ( N >= mem_coalesce_width ) {
      val numInnerIter = N / mem_coalesce_width
      val numOuterIter = numXFormsPerWG / ( groupSize / mem_coalesce_width )

      append(
        s"""|    ii = lId & ${mem_coalesce_width - 1};
            |    jj = lId >> ${log2(mem_coalesce_width)};
            |    lMemStore = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |    offset = mad24( groupId, $numXFormsPerWG, jj);
            |    offset = mad24( offset, $N, ii);
            |""".stripMargin
      )
      adjustInPtr(kernelString, "offset")
      adjustOutPtr(kernelString, "offset")

      append("if((groupId == get_num_groups(0)-1) && s) {\n")
      for (i <- 0 until numOuterIter) {
        append("    if( jj < s ) {\n")
        for (j <- 0 until numInnerIter) {
          formattedLoad(kernelString, i * numInnerIter + j,
            j * mem_coalesce_width + i * ( groupSize / mem_coalesce_width ) * N)
        }
        append("    }\n")
        if (i != numOuterIter - 1)
          kernelString.append(s"    jj += ${groupSize / mem_coalesce_width};\n")
      }
      append("}\n ")
      append("else {\n")
      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          formattedLoad(kernelString, i * numInnerIter + j,
            j * mem_coalesce_width + i * (groupSize / mem_coalesce_width) * N)
      append("}\n")

      append(
        s"""|    ii = lId & ${numWorkItemsPerXForm - 1};
            |    jj = lId >> $log2NumWorkItemsPerXForm;
            |    lMemLoad = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii);
            |""".stripMargin
      )

      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          append(s"    lMemStore[${j * mem_coalesce_width + i * ( groupSize / mem_coalesce_width ) * (N + numWorkItemsPerXForm )}] = a[${i * numInnerIter + j}].x;\n")
      appendLocalBarrier

      for (i <- 0 until R0)
        append(s"    a[$i].x = lMemLoad[${i * numWorkItemsPerXForm}];\n")
      appendLocalBarrier

      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          append(s"    lMemStore[${j * mem_coalesce_width + i * ( groupSize / mem_coalesce_width ) * (N + numWorkItemsPerXForm )}] = a[${i * numInnerIter + j}].y;\n")
      appendLocalBarrier

      for (i <- 0 until R0)
        append(s"    a[$i].y = lMemLoad[${i * numWorkItemsPerXForm}];\n")
      appendLocalBarrier

      lMemSize = (N + numWorkItemsPerXForm) * numXFormsPerWG
    } else {
      append(s"    offset = mad24( groupId,  ${N * numXFormsPerWG}, lId );\n")
      adjustInPtr(kernelString, "offset")
      adjustOutPtr(kernelString, "offset")

      append(
        s"""|    ii = lId & ${N - 1};
            |    jj = lId >> ${log2(N)};
            |    lMemStore = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |""".stripMargin
      )

      append("if((groupId == get_num_groups(0)-1) && s) {\n")
      for (i <- 0 until R0) {
        append("    if(jj < s ) {\n")
        formattedLoad(kernelString, i, i * groupSize)
        append("    }\n")
        if (i != R0 - 1)
          append(s"    jj += ${groupSize / N};\n")
      }
      append("}\n")
      append("else {\n")
      for (i <- 0 until R0)
        formattedLoad(kernelString, i, i * groupSize)
      append("}\n")

      if (numWorkItemsPerXForm > 1) {
        append(
          s"""|    ii = lId & ${numWorkItemsPerXForm - 1};
              |    jj = lId >> ${log2NumWorkItemsPerXForm};
              |    lMemLoad = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
              |""".stripMargin
        )
      } else {
        append(
          s"""|    ii = 0;
              |    jj = lId;
              |    lMemLoad = sMem + mul24( jj, ${N + numWorkItemsPerXForm});
              |""".stripMargin
        )
      }

      for (i <- 0 until R0)
        append(s"    lMemStore[${i * ( groupSize / N ) * ( N + numWorkItemsPerXForm )}] = a[$i].x;\n")
      appendLocalBarrier

      for (i <- 0 until R0)
        append(s"    a[$i].x = lMemLoad[${i * numWorkItemsPerXForm}];\n")
      appendLocalBarrier

      for (i <- 0 until R0)
        append(s"    lMemStore[${i * ( groupSize / N ) * ( N + numWorkItemsPerXForm )}] = a[$i].y;\n")
      appendLocalBarrier

      for (i <- 0 until R0)
        append(s"    a[$i].y = lMemLoad[${i * numWorkItemsPerXForm}];\n")
      appendLocalBarrier

      lMemSize = (N + numWorkItemsPerXForm) * numXFormsPerWG
    }
    lMemSize
  }

  private def insertGlobalStoresAndTranspose(kernelString: StringBuilder,
                                             N: Int,
                                             maxRadix: Int,
                                             Nr: Int,
                                             numWorkItemsPerXForm: Int,
                                             numXFormsPerWG: Int,
                                             mem_coalesce_width: Int): Int =
  {
    import kernelString.append
    def appendLocalBarrier = { append("    barrier( CLK_LOCAL_MEM_FENCE );\n") }

    val groupSize = numWorkItemsPerXForm * numXFormsPerWG
    var lMemSize = 0
    val numIter = maxRadix / Nr
    var indent = ""

    if (numWorkItemsPerXForm >= mem_coalesce_width) {
      if (numXFormsPerWG > 1) {
        append("    if( !s || (groupId < get_num_groups(0)-1) || (jj < s) ) {\n")
        indent = "    "
      }
      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        formattedStore(kernelString, ind, i*numWorkItemsPerXForm)
      }
      if (numXFormsPerWG > 1)
        append("    }\n")
    } else if (N >= mem_coalesce_width) {
      val numInnerIter = N / mem_coalesce_width
      val numOuterIter = numXFormsPerWG / (groupSize / mem_coalesce_width)

      append(
        s"""|    lMemLoad  = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |    ii = lId &  ${mem_coalesce_width - 1};
            |    jj = lId >> ${log2(mem_coalesce_width)};
            |    lMemStore = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |""".stripMargin
      )

      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        append(s"    lMemLoad[${i*numWorkItemsPerXForm}] = a[$ind].x;\n")
      }
      appendLocalBarrier

      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          append(s"    a[${i*numInnerIter + j}].x = lMemStore[${j*mem_coalesce_width + i*( groupSize / mem_coalesce_width )*(N + numWorkItemsPerXForm)}];\n")
      appendLocalBarrier

      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        append(s"    lMemLoad[${i*numWorkItemsPerXForm}] = a[$ind].y;\n")
      }
      appendLocalBarrier

      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          append(s"    a[${i*numInnerIter + j}].y = lMemStore[${j*mem_coalesce_width + i*( groupSize / mem_coalesce_width )*(N + numWorkItemsPerXForm)}];\n")
      appendLocalBarrier

      append("if((groupId == get_num_groups(0)-1) && s) {\n")
      for (i <- 0 until numOuterIter) {
        append("    if( jj < s ) {\n")
        for (j <- 0 until numInnerIter)
          formattedStore(kernelString, i*numInnerIter + j, j*mem_coalesce_width + i*(groupSize/mem_coalesce_width)*N)
        append("    }\n")
        if (i != numOuterIter - 1)
          append(s"    jj += ${groupSize / mem_coalesce_width};\n")
      }
      append("}\n")
      append("else {\n")
      for (i <- 0 until numOuterIter)
        for (j <- 0 until numInnerIter)
          formattedStore(kernelString, i*numInnerIter + j, j*mem_coalesce_width + i*(groupSize/mem_coalesce_width)*N)
      append("}\n")

      lMemSize = (N + numWorkItemsPerXForm) * numXFormsPerWG
    } else {
      append(
        s"""|    lMemLoad  = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |    ii = lId & ${N - 1};
            |    jj = lId >> ${log2(N)};
            |    lMemStore = sMem + mad24( jj, ${N + numWorkItemsPerXForm}, ii );
            |""".stripMargin
      )

      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        append(s"    lMemLoad[${i*numWorkItemsPerXForm}] = a[$ind].x;\n")
      }
      appendLocalBarrier

      for (i <- 0 until maxRadix)
        append(s"    a[$i].x = lMemStore[${i*( groupSize / N )*( N + numWorkItemsPerXForm )}];\n")
      appendLocalBarrier

      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        append(s"    lMemLoad[${i*numWorkItemsPerXForm}] = a[$ind].y;\n")
      }
      appendLocalBarrier

      for (i <- 0 until maxRadix)
        append(s"    a[$i].y = lMemStore[${i*( groupSize / N )*( N + numWorkItemsPerXForm )}];\n")
      appendLocalBarrier

      append("if((groupId == get_num_groups(0)-1) && s) {\n")
      for (i <- 0 until maxRadix) {
        append("    if(jj < s ) {\n")
        formattedStore(kernelString, i, i*groupSize)
        append("    }\n")
        if (i != maxRadix - 1)
          append(s"    jj += ${groupSize / N};\n")
      }
      append("}\n")
      append("else {\n")
      for (i <- 0 until maxRadix)
        formattedStore(kernelString, i, i * groupSize)
      append("}\n")

      lMemSize = (N + numWorkItemsPerXForm) * numXFormsPerWG
    }

    lMemSize
  }

  private def insertfftKernel(kernelString: StringBuilder, Nr: Int, numIter: Int) {
    for (i <- 0 until numIter)
      kernelString.append(s"    fftKernel$Nr(a+${i*Nr}, dir);\n")
  }

  private def insertTwiddleKernel(kernelString: StringBuilder, Nr: Int,
                                  numIter: Int, Nprev: Int, len: Int,
                                  numWorkItemsPerXForm: Int)
  {
    import kernelString.append
    val logNPrev: Int = log2(Nprev)
    for (z <- 0 until numIter) {
      if (z == 0) {
        if (Nprev > 1)
          append(s"    angf = (float) (ii >> $logNPrev);\n")
        else
          append(s"    angf = (float) ii;\n")
      } else {
        if (Nprev > 1)
          append(s"    angf = (float) ((${z*numWorkItemsPerXForm} + ii) >> $logNPrev);\n")
        else
          append(s"    angf = (float) (${z*numWorkItemsPerXForm} + ii);\n")
      }

      for (k <- 1 until Nr) {
        val ind: Int = z * Nr + k
        append(
          s"""|    ang = dir * ( 2.0f * M_PI * $k.0f / $len.0f ) * angf;
              |    w = (float2)(native_cos(ang), native_sin(ang));
              |    a[$ind] = complexMul(a[$ind], w);
              |""".stripMargin
        )
      }
    }
  }

  /** Returns a tuple of the lMemSize, offset and midPad. */
  private def getPadding(numWorkItemsPerXForm: Int, Nprev: Int,
                         numWorkItemsReq: Int, numXFormsPerWG: Int,
                         Nr: Int, numBanks: Int): (Int, Int, Int) =
  {
    val offset =
      if ((numWorkItemsPerXForm <= Nprev) || (Nprev >= numBanks))
        0
      else {
        val numRowsReq = Math.min(numWorkItemsPerXForm, numBanks) / Nprev
        val numColsReq = Nprev * Math.min(1, numRowsReq / Nr)
        numColsReq
      }

    val midPad =
      if (numWorkItemsPerXForm >= numBanks || numXFormsPerWG == 1)
        0
      else {
        val bankNum = ( (numWorkItemsReq + offset) * Nr ) & (numBanks - 1)
        Math.max(0, numWorkItemsPerXForm - bankNum)
      }

    val lMemSize = ( numWorkItemsReq + offset) * Nr * numXFormsPerWG + midPad * (numXFormsPerWG - 1)
    (lMemSize, offset, midPad)
  }

  private def insertLocalStores(kernelString: StringBuilder,
                                numIter: Int, Nr: Int,
                                numWorkItemsPerXForm: Int,
                                numWorkItemsReq: Int, offset: Int,
                                comp: String)
  {
    import kernelString.append

    for (z <- 0 until numIter) {
      for (k <- 0 until Nr) {
        val index = k*(numWorkItemsReq + offset) + z*numWorkItemsPerXForm
        append(s"    lMemStore[$index] = a[${z*Nr + k}].$comp;\n")
      }
    }
    append("    barrier(CLK_LOCAL_MEM_FENCE);\n")
  }

  private def insertLocalLoads(kernelString: StringBuilder, n: Int, Nr: Int,
                               Nrn: Int, Nprev: Int, Ncurr: Int,
                               numWorkItemsPerXForm: Int, numWorkItemsReq: Int,
                               offset: Int, comp: String)
  {
    import kernelString.append

    val numWorkItemsReqN: Int = n / Nrn
    val interBlockHNum: Int = math.max( Nprev / numWorkItemsPerXForm, 1)
    val interBlockHStride: Int = numWorkItemsPerXForm
    val vertWidth: Int = Math.min(Nr, math.max(numWorkItemsPerXForm / Nprev, 1))
    val vertNum: Int = Nr / vertWidth
    val vertStride: Int = ( n / Nr + offset ) * vertWidth
    val iter: Int = Math.max( numWorkItemsReqN / numWorkItemsPerXForm, 1)
    val intraBlockHStride: Int = Nprev * Math.max( numWorkItemsPerXForm / (Nprev*Nr), 1)
    val stride: Int = numWorkItemsReq / Nrn

    for (i <- 0 until iter) {
      val ii: Int = i / (interBlockHNum * vertNum)
      val zz: Int = i % (interBlockHNum * vertNum)
      val jj: Int = zz % interBlockHNum
      val kk: Int = zz / interBlockHNum
      for (z <- 0 until Nrn) {
        val st: Int = kk * vertStride + jj * interBlockHStride + ii * intraBlockHStride + z * stride
        append(s"    a[${i*Nrn + z}].$comp = lMemLoad[$st];\n")
      }
    }
    append("    barrier(CLK_LOCAL_MEM_FENCE);\n")
  }

  private def insertLocalLoadIndexArithmatic(kernelString: StringBuilder,
                                             Nprev: Int, Nr: Int,
                                             numWorkItemsReq: Int,
                                             numWorkItemsPerXForm: Int,
                                             numXFormsPerWG: Int, offset: Int,
                                             midPad: Int)
  {
    import kernelString.append

    val Ncurr: Int = Nprev * Nr
    val logNcurr: Int = log2(Ncurr)
    val logNprev: Int = log2(Nprev)
    val incr: Int = (numWorkItemsReq + offset) * Nr + midPad
    if (Ncurr < numWorkItemsPerXForm) {
      if (Nprev == 1)
        append(s"    j = ii & ${Ncurr - 1};\n")
      else
        append(s"    j = (ii & ${Ncurr - 1}) >> $logNprev;\n")
      if (Nprev == 1)
        append(s"    i = ii >> $logNcurr;\n")
      else
        append(s"    i = mad24(ii >> $logNcurr, $Nprev, ii & ${Nprev - 1});\n")
    }
    else {
      if (Nprev == 1)
        append("    j = ii;\n")
      else
        append(s"    j = ii >> $logNprev;\n")
      if (Nprev == 1)
        append("    i = 0;\n")
      else
        append(s"    i = ii & ${Nprev - 1};\n")
    }
    if (numXFormsPerWG > 1)
      append(s"    i = mad24(jj, $incr, i);\n")
    append(s"    lMemLoad = sMem + mad24(j, ${numWorkItemsReq + offset}, i);\n")
  }

  private def insertLocalStoreIndexArithmatic(kernelString: StringBuilder,
                                              numWorkItemsReq: Int,
                                              numXFormsPerWG: Int,
                                              Nr: Int,
                                              offset: Int,
                                              midPad: Int)
  {
    import kernelString.append

    if (numXFormsPerWG == 1)
      append(s"    lMemStore = sMem + ii;\n")
    else
      append(s"    lMemStore = sMem + mad24(jj, ${(numWorkItemsReq + offset)*Nr + midPad}, ii);\n")
  }

  private def createLocalMemfftKernelString() {
    val n: Int = size.x
    require(n <= max_work_item_per_workgroup * max_radix, "signal length too big for local mem fft.")

    val radixArray = {
      val noMaxRadixArray = getRadixArray(n)
      require(noMaxRadixArray.length > 0, "no radix array supplied.")
      if (n/noMaxRadixArray(0) > max_work_item_per_workgroup)
        getRadixArray(n, max_radix)
      else
        noMaxRadixArray
    }
    def numRadix = radixArray.length

    require(radixArray(0) <= max_radix, "max radix choosen is greater than allowed.")
    require(n/radixArray(0) <= max_work_item_per_workgroup,
      "required work items per xform greater than maximum work items allowed per work group for local mem fft.")

    radixArray.foreach(i => require(i > 0 && isPowerOf2(i), s"Expecting power of two radix, found $i."))
    val tmpLen = radixArray.foldLeft(1)(_ * _)
    require(tmpLen == n, s"product of radices $tmpLen choosen doesn't match the length of signal $n.")

    val localString: StringBuilder = new StringBuilder

    val kCount: Int = kernel_infos.size
    val kernelName = "fft" + kCount + size + radixArrayToString(radixArray) + "_S%batchSize%_%dirName%"
    val numWorkItemsPerXForm: Int = n / radixArray(0)
    val numWorkItemsPerWG: Int =
      if (numWorkItemsPerXForm <= 64) 64 else numWorkItemsPerXForm
    require(numWorkItemsPerWG <= max_work_item_per_workgroup, "max work items per workgroup exceeded.")
    val numXFormsPerWG: Int = numWorkItemsPerWG / numWorkItemsPerXForm

    val kInfo: ClFFTKernelInfo = new ClFFTKernelInfo(kernelName, dir = X,
      num_workgroups = 1,
      num_xforms_per_workgroup = numXFormsPerWG,
      num_workitems_per_workgroup = numWorkItemsPerWG,
      in_place_possible = true)
    kernel_infos.append(kInfo)
    val kernelString = kInfo.kernel_string
    import kernelString.append

    val N: Array[Int] = radixArray
    val maxRadix: Int = N(0)

    insertVariables(localString, maxRadix)

    val lMemSize1 = insertGlobalLoadsAndTranspose(localString, n,
      numWorkItemsPerXForm, numXFormsPerWG, maxRadix, min_mem_coalesce_width)
    kInfo.setMinLMemSize(lMemSize1)

    val xcomp: String = "x"
    val ycomp: String = "y"

    var Nprev: Int = 1
    var len: Int = n
    for (r <- 0 until numRadix) {
      val numIter: Int = N(0) / N(r)
      val numWorkItemsReq: Int = n / N(r)
      val Ncurr: Int = Nprev * N(r)
      insertfftKernel(localString, N(r), numIter)
      if (r < (numRadix - 1)) {
        insertTwiddleKernel(localString, N(r), numIter, Nprev, len, numWorkItemsPerXForm)
        val (padLMemSize, offset, midPad) = getPadding(numWorkItemsPerXForm, Nprev, numWorkItemsReq,
          numXFormsPerWG, N(r), num_local_mem_banks)
        kInfo.setMinLMemSize(padLMemSize)
        insertLocalStoreIndexArithmatic(localString, numWorkItemsReq,
          numXFormsPerWG, N(r), offset, midPad)
        insertLocalLoadIndexArithmatic(localString, Nprev, N(r),
          numWorkItemsReq, numWorkItemsPerXForm, numXFormsPerWG, offset, midPad)
        insertLocalStores(localString, numIter, N(r),
          numWorkItemsPerXForm, numWorkItemsReq, offset, xcomp)
        insertLocalLoads(localString, n, N(r), N(r+1), Nprev, Ncurr,
          numWorkItemsPerXForm, numWorkItemsReq, offset, xcomp)
        insertLocalStores(localString, numIter, N(r),
          numWorkItemsPerXForm, numWorkItemsReq, offset, ycomp)
        insertLocalLoads(localString, n, N(r), N(r+1), Nprev, Ncurr,
          numWorkItemsPerXForm, numWorkItemsReq, offset, ycomp)
        Nprev = Ncurr
        len = len / N(r)
      }
    }
    val lMemSize2 = insertGlobalStoresAndTranspose(localString, n, maxRadix,
      N(numRadix - 1), numWorkItemsPerXForm, numXFormsPerWG, min_mem_coalesce_width)
    kInfo.setMinLMemSize(lMemSize2)

    append("{\n")
    // Support of split real/imag buffer layout prior to actually passing in two buffers for each field.
    insertCogCode(kernelString)

    if (kInfo.lmem_size > 0)
      append(s"    __local float sMem[${kInfo.lmem_size}];\n")
    append(localString)
    append("}\n")
  }

  // For n larger than what can be computed using local memory fft, global transposes
  // multiple kernel launces is needed. For these sizes, n can be decomposed using
  // much larger base radices i.e. say n = 262144 = 128 x 64 x 32. Thus three kernel
  // launches will be needed, first computing 64 x 32, length 128 ffts, second computing
  // 128 x 32 length 64 ffts, and finally a kernel computing 128 x 64 length 32 ffts.
  // Each of these base radices can futher be divided into factors so that each of these
  // base ffts can be computed within one kernel launch using in-register ffts and local
  // memory transposes i.e for the first kernel above which computes 64 x 32 ffts on length
  // 128, 128 can be decomposed into 128 = 16 x 8 i.e. 8 work items can compute 8 length
  // 16 ffts followed by transpose using local memory followed by each of these eight
  // work items computing 2 length 8 ffts thus computing 16 length 8 ffts in total. This
  // means only 8 work items are needed for computing one length 128 fft. If we choose
  // work group size of say 64, we can compute 64/8 = 8 length 128 ffts within one
  // work group. Since we need to compute 64 x 32 length 128 ffts in first kernel, this
  // means we need to launch 64 x 32 / 8 = 256 work groups with 64 work items in each
  // work group where each work group is computing 8 length 128 ffts where each length
  // 128 fft is computed by 8 work items. Same logic can be applied to other two kernels
  // in this example. Users can play with difference base radices and difference
  // decompositions of base radices to generates different kernels and see which gives
  // best performance. Following function is just fixed to use 128 as base radix

  // Rather than generate 3 arrays, we generate a single radix array and have the
  // R1 and R2 arrays functionally derivable from it.
  private def getGlobalRadixInfo(n: Int): Array[Int] =
  {
    //    Changing the baseRadix to 256 showed a 10% speedup in ProjectionRegression,
    //    which does a lot of vector convolutions, but the convolution regression
    //    mostly showed a slowdown for FFT-based convolution.  Leaving this at 128
    //    until more study is done.
    val baseRadix: Int = math.min(n, 128)

    getRadixArray(n, baseRadix)

//    val radixArray = ArrayBuffer[Int]()
//
//    def buildRadixArray(N: Int): Unit = {
//      if (N > baseRadix) {
//        radixArray += baseRadix
//        buildRadixArray(N/baseRadix)
//      } else {
//        radixArray += N
//      }
//    }
//
//    buildRadixArray(n)
//    radixArray
  }

  // Helper function to convert a primary radix into the `R1` radix value
  private def radixToR1(B: Int) = {
    if (B <= 8)
      B
    else {
      var r1: Int = 2
      var r2: Int = B / r1
      while (r2 > r1) {
        r1 *= 2
        r2 = B / r1
      }
      r1
    }
  }

  // Helper function to convert a primary radix into the `R2` radix value
  private def radixToR2(B: Int) = B / radixToR1(B)

  /**
    * Converts a radix array to a string by grabbing the non-zero values
    * of "radixArray" and concatenating them into a string. For example,
    * the array (4, 2, 1, 0, 0) would be converted to: "_radix_4_2_1". Note that
    * zeros in the array are ignored.
    */
  private def radixArrayToString(radixArray: Seq[Int]): String = {
    var string = "_radix"
    for (radix <- radixArray) {
      if (radix != 0)
        string += "_" + radix.toString
    }
    string
  }

  private def createGlobalFFTKernelString(n: Int, BS: Int, dir: ClFFTKernelDir, vertBS: Int) {
    var radix: Int = 0
    val radixArr = getGlobalRadixInfo(n)
    val R1Arr = radixArr.map(radix => radixToR1(radix))
    val R2Arr = radixArr.map(radix => radixToR2(radix))
    val numRadices = radixArr.length

    val maxThreadsPerBlock: Int = max_work_item_per_workgroup
    val maxArrayLen: Int = max_radix
    var batchSize: Int = min_mem_coalesce_width
    val vertical: Boolean = if (dir == X) false else true

    val numPasses: Int = numRadices

    def kCount: Int = kernel_infos.size

    var N: Int = n
    val m: Int = log2(n)
    val Rinit: Int = if (vertical) BS else 1
    batchSize = if (vertical) Math.min(BS, batchSize) else batchSize

    for (passNum <- 0 until numPasses) {

      val localString: StringBuilder = new StringBuilder

      radix = radixArr(passNum)
      val R1 = R1Arr(passNum)
      val R2 = R2Arr(passNum)

      var strideI: Int = Rinit
      for (i <- 0 until numPasses)
        if (i != passNum)
          strideI *= radixArr(i)

      var strideO: Int = Rinit
      for (i <- 0 until passNum)
        strideO *= radixArr(i)

      val threadsPerXForm: Int = R2
      batchSize = if (R2 == 1) max_work_item_per_workgroup else batchSize
      batchSize = Math.min(batchSize, strideI)
      var threadsPerBlock: Int = batchSize * threadsPerXForm
      threadsPerBlock = Math.min(threadsPerBlock, maxThreadsPerBlock)
      batchSize = threadsPerBlock / threadsPerXForm
      require(R2 <= R1)
      require(R1*R2 == radix)
      require(R1 <= maxArrayLen)
      require(threadsPerBlock <= maxThreadsPerBlock)

      var numIter: Int = R1 / R2
      val gInInc: Int = threadsPerBlock / batchSize

      var lgStrideO: Int = log2(strideO)
      var numBlocksPerXForm: Int = strideI / batchSize
      var numBlocks: Int = numBlocksPerXForm
      if (!vertical)
        numBlocks *= BS
      else
        numBlocks *= vertBS

      val kernelName = "fft" + kCount + size + radixArrayToString(radixArr) + "_S%batchSize%_%dirName%"
      val kInfo: ClFFTKernelInfo =
        new ClFFTKernelInfo(kernelName, dir,
          num_workgroups = numBlocks,
          num_xforms_per_workgroup = 1,
          num_workitems_per_workgroup = threadsPerBlock,
          in_place_possible = (passNum == (numPasses - 1)) && ((numPasses % 2) != 0))
      if (R2 != 1) {
        if (strideO == 1)
          kInfo.setMinLMemSize((radix + 1)*batchSize)
        else
          kInfo.setMinLMemSize(threadsPerBlock*R1)
      }

      insertVariables(localString, R1)

      import localString.append
      def appendLocalBarrier = { append("barrier( CLK_LOCAL_MEM_FENCE );\n") }

      if (vertical) {
        append(
          s"""|xNum = groupId >> ${log2(numBlocksPerXForm)};
              |groupId = groupId & ${numBlocksPerXForm - 1};
              |indexIn = mad24(groupId, $batchSize, xNum << ${log2(n*BS)});
              |tid = mul24(groupId, $batchSize);
              |i = tid >> $lgStrideO;
              |j = tid & ${strideO - 1};
              |""".stripMargin
        )
        var stride: Int = radix*Rinit
        for (i <- 0 until passNum)
          stride *= radixArr(i)
        append(
          s"""|indexOut = mad24(i, $stride, j + (xNum << ${log2(n*BS)}));
              |bNum = groupId;
              |""".stripMargin
        )
      } else {
        val lgNumBlocksPerXForm: Int = log2(numBlocksPerXForm)
        append(
          s"""|bNum = groupId & ${numBlocksPerXForm - 1};
              |xNum = groupId >> $lgNumBlocksPerXForm;
              |indexIn = mul24(bNum, $batchSize);
              |tid = indexIn;
              |i = tid >> $lgStrideO;
              |j = tid & ${strideO - 1};
              |""".stripMargin
        )
        var stride: Int = radix*Rinit
        for (i <- 0 until passNum)
          stride *= radixArr(i)
        append(
          s"""|indexOut = mad24(i, $stride, j);
              |indexIn += (xNum << $m);
              |indexOut += (xNum << $m);
              |""".stripMargin
        )
      }

      // Load Data
      val lgBatchSize: Int = log2(batchSize)
      append(
        s"""|tid = lId;
            |i = tid & ${batchSize - 1};
            |j = tid >> $lgBatchSize;
            |indexIn += mad24(j, $strideI, i);
            |""".stripMargin
      )

      adjustInPtr(localString, "indexIn")
      for (j <- 0 until R1)
        formattedLoad(localString, j, j * gInInc * strideI)

      localString.append("fftKernel").append(R1).append("(a, dir);\n")

      if (R2 > 1) {
        // twiddle
        for (k <- 1 until R1) {
          append(
            s"""|ang = dir*(2.0f*M_PI*$k/$radix)*j;
                |w = (float2)(native_cos(ang), native_sin(ang));
                |a[$k] = complexMul(a[$k], w);
                |""".stripMargin
          )
        }

        // shuffle
        numIter = R1 / R2
        append(
          s"""|indexIn = mad24(j, ${threadsPerBlock*numIter}, i);
              |lMemStore = sMem + tid;
              |lMemLoad = sMem + indexIn;
              |""".stripMargin
        )
        for (k <- 0 until R1)
          append(s"lMemStore[${k*threadsPerBlock}] = a[$k].x;\n")
        appendLocalBarrier
        for (k <- 0 until numIter)
          for (t <- 0 until R2)
            append(s"a[${k*R2+t}].x = lMemLoad[${t*batchSize + k*threadsPerBlock}];\n")
        appendLocalBarrier
        for (k <- 0 until R1)
          append(s"lMemStore[${k*threadsPerBlock}] = a[$k].y;\n")
        appendLocalBarrier
        for (k <- 0 until numIter)
          for (t <- 0 until R2)
            append(s"a[${k*R2+t}].y = lMemLoad[${t*batchSize + k*threadsPerBlock}];\n")
        appendLocalBarrier

        for (j <- 0 until numIter)
          append(s"fftKernel$R2(a + ${j*R2}, dir);\n")
      }

      // twiddle
      if (passNum < (numPasses - 1)) {
        append(
          s"""|l = ((bNum << $lgBatchSize) + i) >> $lgStrideO;
              |k = j << ${log2(R1/R2)};
              |ang1 = dir*(2.0f*M_PI/$N)*l;
              |""".stripMargin
        )
        for (t <- 0 until R1) {
          append(
            s"""|ang = ang1*(k + ${(t%R2)*R1 + (t/R2)});
                |w = (float2)(native_cos(ang), native_sin(ang));
                |a[$t] = complexMul(a[$t], w);
                |""".stripMargin
          )
        }
      }

      // Store Data
      if (strideO == 1) {
        append(
          s"""|lMemStore = sMem + mad24(i, ${radix + 1}, j << ${log2(R1/R2)});
              |lMemLoad = sMem + mad24(tid >> ${log2(radix)}, ${radix+1}, tid & ${radix-1});
              |""".stripMargin
        )

        for (i <- 0 until R1 / R2)
          for (j <- 0 until R2)
            append(s"lMemStore[${i + j*R1}] = a[${i*R2+j}].x;\n")
        appendLocalBarrier
        if (threadsPerBlock >= radix) {
          for (i <- 0 until R1)
            append(s"a[$i].x = lMemLoad[${i*(radix+1)*(threadsPerBlock/radix)}];\n")
        } else {
          val innerIter: Int = radix/threadsPerBlock
          val outerIter: Int = R1/innerIter
          for (i <- 0 until outerIter)
            for (j <- 0 until innerIter)
              append(s"a[${i*innerIter+j}].x = lMemLoad[${j*threadsPerBlock + i*(radix+1)}];\n")
        }
        appendLocalBarrier

        for (i <- 0 until R1 / R2)
          for (j <- 0 until R2)
            append(s"lMemStore[${i + j*R1}] = a[${i*R2+j}].y;\n")
        appendLocalBarrier
        if (threadsPerBlock >= radix) {
          for (i <- 0 until R1)
            append(s"a[$i].y = lMemLoad[${i*(radix+1)*(threadsPerBlock/radix)}];\n")
        } else {
          val innerIter: Int = radix/threadsPerBlock
          val outerIter: Int = R1/innerIter
          for (i <- 0 until outerIter)
            for (j <- 0 until innerIter)
              append(s"a[${i*innerIter+j}].y = lMemLoad[${j*threadsPerBlock + i*(radix+1)}];\n")
        }
        appendLocalBarrier

        append("indexOut += tid;\n")
        adjustOutPtr(localString, "indexOut")
        for (k <- 0 until R1)
          formattedStore(localString, k, k * threadsPerBlock)
      } else {
        append(s"indexOut += mad24(j, ${numIter*strideO}, i);\n")
        adjustOutPtr(localString, "indexOut")
        for (k <- 0 until R1)
          formattedStore(localString, k, ((k % R2) * R1 + (k / R2)) * strideO)
      }

      // No insertHeader: HyperKernel framework takes care of this
      val kernelString = kInfo.kernel_string
      kernelString.append("{\n")
      // Support for split real/imag buffer layout prior to actually passing in two buffers for each field.
      insertCogCode(kernelString)
      if (kInfo.lmem_size > 0)
        kernelString.append(s"    __local float sMem[${kInfo.lmem_size}];\n")
      kernelString.append(localString)
      kernelString.append("}\n")

      N /= radix
      kernel_infos.append(kInfo)
    }
  }

  private def FFT1D(dir: ClFFTKernelDir) {
    dir match {
      case X =>
        if (size.x > max_localmem_fft_size) {
          createGlobalFFTKernelString(size.x, 1, X, 1)
        }
        else if (size.x > 1) {
          if (size.x / getRadixArray(size.x)(0) <= max_work_item_per_workgroup) {
            createLocalMemfftKernelString()
          } else {
            if (size.x / getRadixArray(size.x, max_radix)(0) <= max_work_item_per_workgroup)
              createLocalMemfftKernelString()
            else
              createGlobalFFTKernelString(size.x, 1, X, 1)
          }
        }

      case Y =>
        if (size.y > 1)
          createGlobalFFTKernelString(size.y, size.x, Y, 1)

      case Z =>
        if (size.z > 1)
          createGlobalFFTKernelString(size.z, size.x * size.y, Z, 1)

      case _ =>
    }
  }

}
