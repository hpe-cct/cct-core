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

package cogx.compiler.codegenerator.opencl.hyperkernels.discretecosinetransform

import cogx.compiler.codegenerator.opencl.hyperkernels.fastfouriertransform._

import scala.collection.mutable.ArrayBuffer

/** A "Planner" for synthesizing OpenCL kernels that implement a DCT that
  * transforms the rows of a 2D scalar field, where the rows have been
  * interleaved as described in the paper:
  *
  * "Image compression using the discrete
  * cosine transform," Watson, Mathematica Journal, 4(1), 1994
  *
  * Compared to the fastfouriertransform package, this discretecosinetransform
  * package involves the addition of extra "twiddling" required
  * as described in the above paper. However, a reimplemenation could reduce
  * a lot of the computation here and perhaps eliminate the need for the
  * interleaving. Perhaps a 2X performance improvement could be achieved.
  *
  * @param deviceMaxWorkItemsPerWorkGroup The maximum number of work items that
  *        the OpenCL platform will support within a work group.
  * @param rows The number of rows in the input field.
  * @param columns The number of columns in the input field.
  * @param forward True for a forward DCT transform, false for an inverse
  *        transform.
  */
private[cogx]
class DCTPlanner(deviceMaxWorkItemsPerWorkGroup: Int,
                 rows: Int,
                 columns: Int,
                 forward: Boolean)
{
  val sizes = Array(rows, columns)
  val size = new ClFFTDim3(rows, columns)

  // Only tested currently on 2D transform
  require(size.dimension == 2)

  // kernel for the dct
  var dctKernel: ClFFTKernelInfo = null

  // Maximum size of signal for which local memory transposed based
  // fft is sufficient i.e. no global mem transpose (communication)
  // is needed
  val max_localmem_fft_size = 2048

  // Maximum work items per work group allowed. This, along with max_radix below controls
  // maximum local memory being used by fft kernels of this plan. Set to 256 by default
  var max_work_item_per_workgroup = deviceMaxWorkItemsPerWorkGroup

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

  // Create the kernel.
  DCT1D(X)

  // Debugging junk
  val Verbose = false
  if (Verbose) {
    println("DCTPlanner: forward = " + forward)
    println("  dimensions = " + sizes.length)
    println("  sizes: " + sizes(0) + " x " + sizes(1))
    println(clSourceCode(0))
  }

  /** Get the OpenCL source code for the synthesized FFT, one pass each dimension. */
  def clSourceCode: String =
    dctKernel.kernel_string.toString()

  def workDimensions: WorkDimensions =
    getKernelWorkDimensions(dctKernel)

  def allSourceCode = clSourceCode

  def kernelNames: String = dctKernel.kernel_name

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

  private def insertHeader(kernelString: StringBuilder, kernelName: String): Unit =
  {
    // The HyperKernel framework controls the header generation, so do nothing.
  }

  // Cog approach to supporting split real/imag buffer layout prior to actually
  // passing in two buffers for each field.
  private def insertCogDefines(kernelString: StringBuilder): Unit =
  {
    kernelString.append(
      s"""|#define dir (%dirVal%)
          |#define S (%batchSize%)
          |for (int tensorElement = 0;
          |  tensorElement < _tensorElements;
          |  tensorElement++) {
          |""".stripMargin
    )
  }

  private def insertCogUndefines(kernelString: StringBuilder): Unit =
  {
    kernelString.append(
      s"""|    barrier(CLK_LOCAL_MEM_FENCE);
          |#undef dir
          |#undef S
          |}
          |""".stripMargin
    )
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


  /** Twiddles the values in the a[] arrays (each thread has it's own) required
    * by the DCT when doing the forward transform on rows. For an explanation,
    * see "Image compression using the discrete cosine transform," Watson,
    * Mathematica Journal, 4(1), 1994.
    *
    * NOTE: This only works for sizes 256 and larger, reason unknown.
    */
  private def rowTwiddle(kernelString: StringBuilder,
                         aIndex: Int,
                         gIndex: Int,
                         inverse: Boolean)
  {
    require(size.x.toInt >= 256, "DCT can only handle sizes 256 and larger (BUG)")
    val column = gIndex.toString + " + lId"
    val twiddleString =
      """
        |  {
        |    float2 twiddleFactor;
        |    if ((%column%) == 0)
        |        twiddleFactor = (float2)(1.0f, 0.0f);
        |    else {
        |        float realPart = 0.0f;
        |        float imagPart = (%sign%M_PI * (%column%)) / (2 * %lineLength%);
        |        float2 exp =
        |             complexExp((float2)(realPart, imagPart));
        |        twiddleFactor =
        |          complexMultiplyReal(exp, %sqrt2%);
        |    }
        |    a[%index%] = complexMultiply(a[%index%], twiddleFactor);
        |  }
        |  barrier(CLK_LOCAL_MEM_FENCE);
        |        """.stripMargin.
              replaceAll("%index%", aIndex.toString).
              replaceAll("%column%", column.toString).
              replaceAll("%lineLength%", size.x.toInt.toString).
              replaceAll("%sign%", if (inverse) "" else "-").
              replaceAll("%sqrt2%", math.sqrt(2).toFloat.toString + "f")

    kernelString.append(twiddleString)
  }

  /** Write out a real or complex value to global memory.
    */
  private def formattedStore(kernelString: StringBuilder, aIndex: Int, gIndex: Int) {

    //**************************************************************************
    // We add additional twiddle factors for the DCT just before writing out.
    if (forward)
      rowTwiddle(kernelString, aIndex, gIndex, inverse = false)

    // End of DCT twiddling
    //**************************************************************************

    kernelString.append(
      s"""|        column = $gIndex;
          |        @outElementNonlocal0 = a[$aIndex].x;
          |""".stripMargin
    )
  }

  /** Read a real or complex value from global memory.
    *
    * We do the normalization required by the DFT here, dividing each input pixel
    * by the square root of the number of columns.
    */
  private def formattedLoad(kernelString: StringBuilder, aIndex: Int, gIndex: Int) {
    val normalize = (1f / math.sqrt(columns).toFloat).toString + "f"
    kernelString.append(
      s"""|        column = $gIndex;
          |        a[$aIndex] = (float2) (readElementNonlocal(@in0) * $normalize, 0.0f);
          |""".stripMargin
    )
    //**************************************************************************
    // We add additional twiddle factors for the DCT just after reading in.
    if (!forward)
      rowTwiddle(kernelString, aIndex, gIndex, inverse = true)

    // End of DCT twiddling
    //**************************************************************************
  }

  // These 2 routines clean up the code generation quite a bit and further localize
  // the I/O handling, making it easier to adapt to changes in Cog HyperKernel I/O.  -RJC

  /** Bump the input pointer by an offset (measured in elements) */
  private def adjustInPtr(kernelString: StringBuilder, offsetStr: String) {
      kernelString.append(s"        fieldName(@in0) += $offsetStr;\n")
  }

  /** Bump the output pointer by an offset (measured in elements) */
  private def adjustOutPtr(kernelString: StringBuilder, offsetStr: String) {
    kernelString.append(s"        fieldName(@out0) += $offsetStr;\n")
  }

  private def insertGlobalLoadsAndTranspose(kernelString: StringBuilder,
                                            N: Int,
                                            numWorkItemsPerXForm: Int,
                                            numXFormsPerWG: Int,
                                            R0: Int,
                                            mem_coalesce_width: Int): Int =
  {
    val lMemSize = 0
    if (numXFormsPerWG > 1) {
      kernelString.append(s"        s = S & ${numXFormsPerWG - 1};\n")
    }
    if (numWorkItemsPerXForm >= mem_coalesce_width) {
      kernelString.append(
        s"""|    ii = lId;
            |    jj = 0;
            |    offset =  mad24(groupId, $N, ii);
            |""".stripMargin
      )
      adjustInPtr(kernelString, "offset")
      adjustOutPtr(kernelString, "offset")
      for (i <- 0 until R0) {
        formattedLoad(kernelString, i, i * numWorkItemsPerXForm)
      }
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
    val lMemSize = 0
    val numIter = maxRadix / Nr
    var indent = ""

    if (numWorkItemsPerXForm >= mem_coalesce_width) {
      if (numXFormsPerWG > 1) {
        kernelString.append("    if( !s || (groupId < get_num_groups(0)-1) || (jj < s) ) {\n")
        indent = "    "
      }
      for (i <- 0 until maxRadix) {
        val j = i % numIter
        val k = i / numIter
        val ind = j * Nr + k
        formattedStore(kernelString, ind, i * numWorkItemsPerXForm)
      }
      if (numXFormsPerWG > 1)
        kernelString.append("    }\n")
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
//  private def getPadding(numWorkItemsPerXForm: Int, Nprev: Int,
//                         numWorkItemsReq: Int, numXFormsPerWG: Int,
//                         Nr: Int, numBanks: Int): fftPadding =
//  {
//    var offset = 0
//    var midPad = 0
//    if ((numWorkItemsPerXForm <= Nprev) || (Nprev >= numBanks))
//      offset = 0
//    else {
//      val numRowsReq =
//        (if (numWorkItemsPerXForm < numBanks) numWorkItemsPerXForm else numBanks) / Nprev
//      var numColsReq = 1
//      if (numRowsReq > Nr)
//        numColsReq = numRowsReq / Nr
//      numColsReq = Nprev * numColsReq
//      offset = numColsReq
//    }
//    if (numWorkItemsPerXForm >= numBanks || numXFormsPerWG == 1) {
//      midPad = 0
//    }
//    else {
//      val bankNum = ((numWorkItemsReq + offset) * Nr) & (numBanks - 1)
//      if (bankNum >= numWorkItemsPerXForm) {
//        midPad = 0
//      }
//      else {
//        midPad = numWorkItemsPerXForm - bankNum
//      }
//    }
//    val lMemSize = (numWorkItemsReq + offset) * Nr * numXFormsPerWG + midPad * (numXFormsPerWG - 1)
//    new fftPadding(lMemSize, offset, midPad)
//  }

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

    val kCount: Int = 1
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
    dctKernel = kInfo
    val kernelString = kInfo.kernel_string
    import kernelString.append


    val N: Array[Int] = radixArray
    val maxRadix: Int = N(0)

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

    if (kInfo.lmem_size > 0)
      append(s"    __local float sMem[${kInfo.lmem_size}];\n")
    insertCogDefines(kernelString)
    insertVariables(kernelString, maxRadix)
    append(localString)

    // This is ugly. The original code hacks the input parameters by incrementing
    // them, meaning we need to reset them to their original values before
    // beginning the next iteration of the loop. Ugly.
    val inputParameterReset =
      """
        |    {
        |        // Reset input parameters to their entry values.
        |        int offset = mad24((int) get_group_id(0), (int) %n%, (int) get_local_id(0));
        |        fieldName(@in0) -= offset;
        |        fieldName(@out0) -= offset;
        |    }
      """.stripMargin.replaceAll("%n%", n.toString)
    append(inputParameterReset)

    insertCogUndefines(kernelString)
    append("}\n")
  }

  /**
    * Converts a radix array to a string by grabbing the non-zero values
    * of "radixArray" and concatenating them into a string. For example,
    * the array (4, 2, 1, 0, 0) would be converted to: "_radix_4_2_1". Note that
    * zeros in the array are ignored.
    */
  private def radixArrayToString(radixArray: Array[Int]): String = {
    var string = "_radix"
    for (radix <- radixArray) {
      if (radix != 0)
        string += "_" + radix.toString
    }
    string
  }

  def DCT1D(dir: ClFFTKernelDir) {
    createLocalMemfftKernelString()
  }
}
